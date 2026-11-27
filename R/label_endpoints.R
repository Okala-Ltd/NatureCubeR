#' @title Get project labels for either bioacoustics or camera
#'
#' @description
#' Labels are derived by using either suggested labels on the platform or
#' by manually adding labels from the wider database.
#'
#' @param hdr A base URL provided and valid API key returned by the
#'   function \link{auth_headers}
#' @param labeltype A character vector specifying the label type
#'   ('Bioacoustic' or 'Camera')
#'
#' @return A tibble containing project labels
#'
#' @examples
#' \dontrun{
#'   labels <- get_project_labels(headers, labeltype='Camera')
#' }
#'
#' @author
#' Adam Varley
#' @export
get_project_labels <- function(hdr,
                               labeltype = c('Bioacoustic', 'Camera')) {
  urlreq_ap <- httr2::req_url_path_append(
    hdr$root, "getProjectLabels", labeltype, hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp) %>% tibble::as_tibble())
}

#' @title Add project labels for either bioacoustics or camera
#'
#' @description
#' Add labels so labellers have access to them in the Dashboard. Labels are
#' derived by using either suggested labels on the platform or by manually
#' adding labels from the wider database.
#'
#' @param hdr A base URL provided and valid API key returned by the
#'   function \link{auth_headers}
#' @param labeltype A character vector specifying the label type
#'   ('Bioacoustic' or 'Camera')
#' @param labels A label object list specifying the labels to be added
#'
#' @return A success message as a list
#'
#' @examples
#' \dontrun{
#'   add_project_labels(headers, labeltype='Camera', labels=my_labels)
#' }
#'
#' @author
#' Adam Varley
#' @export
add_project_labels <- function(hdr,
                               labeltype = c('Bioacoustic', 'Camera'),
                               labels) {
  urlreq_ap <- httr2::req_url_path_append(hdr$root, "addProjectLabels", labeltype, hdr$key)
  urlreq_ap <- urlreq_ap %>% httr2::req_method("POST") %>% httr2::req_body_json(data = labels)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq)

  return(resp)
}

# Utility function to replace NULLs and empty (zero-length) fields - e.g. an
# empty JSON array like `tags: []` parsed to list() - with NA. A zero-length
# field left as-is makes tibble::as_tibble() recycle every column down to 0
# rows, silently dropping the record.
replace_nas <- function(df) {
  df[sapply(df, function(x) is.null(x) || length(x) == 0)] <- NA
  return(df)
}

#' @title Get labels from the wider IUCN database (all species)
#'
#' @description
#' Retrieve labels from the wider IUCN database, optionally filtered by a search term.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param offset An integer specifying the offset for the query
#' @param search_term A character vector specifying the search term to be used (optional)
#'
#' @return A list containing tabular data and pagination information for iterative calls
#'
#' @examples
#' \dontrun{
#'   getIUCNLabels(headers, offset=0, search_term="horse")
#' }
#'
#' @author
#' Adam Varley
#' @export
getIUCNLabels <- function(hdr, offset, search_term = NULL) {
  if (is.null(search_term)) {
    search_term <- ""
  }

  limit = API_MAX_LIMIT

  urlreq_ap <- httr2::req_url_path_append(hdr$root, "getIUCNLabels", hdr$key)
  urlreq_ap <- urlreq_ap %>%
    httr2::req_method("GET") %>%
    httr2::req_url_query("offset" = offset, "limit" = limit, "search_term" = search_term) %>%
    httr2::req_retry(
      max_tries = 5,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
    )

  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq)
  resp_table <- lapply(resp$table, function(x) {
    x %>% replace_nas() %>% tibble::as_tibble()
  }) %>% dplyr::bind_rows()

  return(list(
    data = resp_table,
    total = resp$pagination_state$total,
    offset = resp$pagination_state$offset,
    limit = resp$pagination_state$limit
  ))
}


#' @title Add labels from the wider IUCN database (all species)
#'
#' @description
#' Add labels from the wider IUCN database in chunks.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param labels A data frame of labels to add
#' @param chunksize An integer specifying the chunk size for the submission
#'
#' @return A success message as a list
#'
#' @examples
#' \dontrun{
#'   add_IUCN_labels(headers, labels=my_labels, chunksize=200)
#' }
#'
#' @author
#' Adam Varley, Cristobal Salamé
#' @export
add_IUCN_labels <- function(hdr, labels, chunksize) {

  if (nrow(labels) < 100) {
    message('Data is too small to chunk, submitting all data')
    chunksize <- nrow(labels)
    spl.dt <- list(labels)
  } else {

    if (chunksize > nrow(labels)) {
      message('chunksize is bigger than length of data altering chunksize to ', nrow(labels))
      chunksize <- nrow(labels) / 2
    } else {
      spl.dt <- split(labels, cut(seq_len(nrow(labels)), round(nrow(labels) / chunksize)))

    }
  }


  pb <- cli::cli_progress_bar(
    format = "Submitting {cli::pb_current}/{cli::pb_total} label(s) | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    total  = nrow(labels),
    clear  = FALSE
  )
  # on.exit (not tryCatch) so the bar is always closed - including on a user
  # interrupt (e.g. Escape/Ctrl+C) - see .check_label_values() in
  # push_phone_observations.R for the same pattern and why tryCatch's `error`
  # handler alone isn't enough.
  on.exit(cli::cli_progress_done(id = pb), add = TRUE)

  submitted <- 0
  for (i in seq_along(spl.dt)) {

    urlreq_ap <- httr2::req_url_path_append(hdr$root, "addIUCNLabels", hdr$key)
    urlreq_ap <- urlreq_ap %>% httr2::req_method("POST") %>% httr2::req_body_json(data = spl.dt[[i]])

    preq <- httr2::req_perform(urlreq_ap, verbosity = 3)
    resp <- httr2::resp_body_json(preq)

    submitted <- submitted + nrow(spl.dt[[i]])
    cli::cli_progress_update(id = pb, set = submitted)
  }

  return(resp)
}


# Internally used function to send updated labels in chunks

.send_updated_labels <- function(hdr, datachunk) {

  urlreq_ap <- httr2::req_url_path_append(hdr$root, "updateSegmentLabels", hdr$key)
  urlreq_ap <- urlreq_ap %>%
    httr2::req_method("PUT") %>%
    httr2::req_body_json(data = datachunk)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp))
}

# Map plain column names from get_media_assets() / get_project_labels() onto
# the updateSegmentLabels schema names the API expects.
.normalize_label_submission_columns <- function(submission_records) {
  if (!is.data.frame(submission_records)) {
    return(submission_records)
  }

  rename_map <- c(
    segment_record_id = "segment_record_id_fk",
    label_id = "label_id_fk"
  )
  for (from in names(rename_map)) {
    to <- rename_map[[from]]
    if (!(from %in% names(submission_records))) {
      next
    }
    if (to %in% names(submission_records)) {
      # Keep the schema column; drop the alias so both are not sent.
      submission_records[[from]] <- NULL
    } else {
      names(submission_records)[names(submission_records) == from] <- to
    }
  }
  submission_records
}

# Validate the subset of the API's Label schema used for new labels.
.validate_label_submission <- function(submission_records) {
  if (!is.data.frame(submission_records)) {
    stop("`submission_records` must be a data frame or tibble.")
  }

  submission_records <- .normalize_label_submission_columns(submission_records)

  required <- c("segment_record_id_fk", "label_id_fk")
  optional <- c(
    "number_of_individuals",
    "label_record_uuid",
    "prediction_accuracy",
    "label_created_at",
    "label_record_id"
  )
  missing <- setdiff(required, names(submission_records))

  if (length(missing) > 0L) {
    problems <- paste0("missing required column(s): ", paste(missing, collapse = ", "))
    stop(
      "`submission_records` does not match the updateSegmentLabels schema (",
      paste(problems, collapse = "; "),
      "). Expected columns are: ",
      paste(c(required, optional), collapse = ", "),
      ". Columns `segment_record_id` and `label_id` are also accepted and renamed automatically."
    )
  }

  is_integer_column <- function(x) {
    is.numeric(x) &&
      all(is.finite(x)) &&
      all(x == floor(x))
  }

  integer_columns <- intersect(
    c("segment_record_id_fk", "label_id_fk", "number_of_individuals", "label_record_id"),
    names(submission_records)
  )
  for (column in integer_columns) {
    values <- submission_records[[column]]
    if (anyNA(values) || !is_integer_column(values)) {
      stop(
        "Column `", column, "` must contain non-missing whole numbers, as required by the updateSegmentLabels schema."
      )
    }
  }
  invisible(submission_records)
}

#' @title Push new labels using a chunked process
#'
#' @description
#' Push new labels to the platform in chunks. The endpoint expects a JSON
#' array of label objects.
#'
#' Each row in \code{submission_records} must contain
#' \code{segment_record_id_fk} and \code{label_id_fk}. The plain names
#' \code{segment_record_id} and \code{label_id} (as returned by
#' \link{get_media_assets} and \link{get_project_labels}) are accepted and
#' renamed automatically. The optional columns supported by the endpoint are
#' \code{number_of_individuals}, \code{label_record_uuid},
#' \code{prediction_accuracy}, \code{label_created_at}, and
#' \code{label_record_id}. Extra columns are rejected before any request is
#' made.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param submission_records A data frame or tibble with one row per label.
#'   Required columns are \code{segment_record_id_fk} (integer) and
#'   \code{label_id_fk} (integer), or the aliases \code{segment_record_id}
#'   and \code{label_id}. Optional columns are
#'   \code{number_of_individuals} (integer, default 1),
#'   \code{label_record_uuid} (character), \code{prediction_accuracy}
#'   (numeric), \code{label_created_at} (date-time), and
#'   \code{label_record_id} (integer).
#' @param chunksize A positive whole number specifying the maximum number of
#'   records sent per request.
#'
#' @return The API response from the final chunk, typically a list containing
#'   the message \code{"Labels updated successfully"}.
#'
#' @examples
#' \dontrun{
#'   push_new_labels(headers, submission_records, chunksize=30)
#' }
#'
#' @author
#' Adam Varley, Cristobal Salamé
#' @export
push_new_labels <- function(hdr, submission_records, chunksize) {

  submission_records <- .validate_label_submission(submission_records)

  if (chunksize > nrow(submission_records)) {
    message('chunksize is bigger than length of data altering chunksize to ', nrow(submission_records))
    chunksize <- nrow(submission_records)
  }

  spl.dt <- split(
    submission_records,
    ceiling(seq_len(nrow(submission_records)) / chunksize)
  )

  pb <- cli::cli_progress_bar(
    format = "Submitting {cli::pb_current}/{cli::pb_total} label(s) | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    total  = nrow(submission_records),
    clear  = FALSE
  )
  
  # on.exit (not tryCatch) so the bar is always closed - including on a user
  # interrupt (e.g. Escape/Ctrl+C) - see .check_label_values() in
  # push_phone_observations.R for the same pattern and why tryCatch's `error`
  # handler alone isn't enough.
  on.exit(cli::cli_progress_done(id = pb), add = TRUE)

  submitted <- 0
  resp <- NULL
  for (i in seq_along(spl.dt)) {
    resp <- .send_updated_labels(hdr, datachunk = spl.dt[[i]])
    submitted <- submitted + nrow(spl.dt[[i]])
    cli::cli_progress_update(id = pb, set = submitted)
  }

  return(resp)
}



#' @title Set blank status for segment labels
#'
#' @description
#' Marks or unmarks segment labels as blank for a given list of segment record IDs.
#' IDs are sent in chunks so a large batch does not hit the gateway timeout.
#' A chunk that still returns HTTP 504 is split in half and retried.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}.
#' @param blank_status A boolean value indicating whether to mark as blank (TRUE) or unblank (FALSE).
#' @param segment_record_ids A numeric vector of segment record IDs to update.
#' @param chunksize An integer specifying how many segment record IDs to send per request.
#'   Defaults to 50. Raise it if requests finish quickly; lower it if they time out.
#'
#' @return A list containing the API response message from the last chunk submitted.
#'
#' @examples
#' \dontrun{
#'   # Mark segments as blank
#'   set_segment_blank_status(headers, blank_status = TRUE, segment_record_ids = c(101, 102, 103))
#'   # Unmark segments as blank
#'   set_segment_blank_status(headers, blank_status = FALSE, segment_record_ids = c(101, 102, 103))
#' }
#'
#' @author
#' Adam Varley
#' @export
set_segment_blank_status <- function(hdr, blank_status, segment_record_ids, chunksize = 50) {
  status_str <- tolower(as.character(blank_status))
  segment_record_ids <- unname(as.numeric(segment_record_ids))
  n <- length(segment_record_ids)

  if (n == 0L) {
    stop("`segment_record_ids` must contain at least one ID.", call. = FALSE)
  }
  if (length(chunksize) != 1L || is.na(chunksize) || chunksize < 1) {
    stop("`chunksize` must be a positive number.", call. = FALSE)
  }

  chunksize <- min(as.integer(chunksize), n)
  queue <- split(segment_record_ids, ceiling(seq_along(segment_record_ids) / chunksize))

  pb <- cli::cli_progress_bar(
    format = "Updating blank status {cli::pb_current}/{cli::pb_total} segment record(s) | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    total  = n,
    clear  = FALSE
  )
  # on.exit (not tryCatch) so the bar is always closed - including on a user
  # interrupt (e.g. Escape/Ctrl+C) - see .check_label_values() in
  # push_phone_observations.R for the same pattern and why tryCatch's `error`
  # handler alone isn't enough.
  on.exit(cli::cli_progress_done(id = pb), add = TRUE)

  submitted <- 0
  resp <- NULL
  while (length(queue) > 0L) {
    ids <- unname(queue[[1]])
    queue <- queue[-1]

    # A JSON array even for one ID. auto_unbox would send a bare number.
    req <- httr2::req_url_path_append(hdr$root, "segmentLabelsBlankStatus", hdr$key, status_str) %>%
      httr2::req_method("PUT") %>%
      httr2::req_body_json(data = as.list(ids)) %>%
      httr2::req_retry(
        max_tries = 4,
        is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503)
      ) %>%
      # 504 means this chunk is too big for the gateway. Handle it below by
      # splitting, instead of failing the whole batch.
      httr2::req_error(is_error = function(resp) {
        status <- httr2::resp_status(resp)
        status >= 400 && status != 504
      })

    preq <- httr2::req_perform(req)
    if (httr2::resp_status(preq) == 504L) {
      if (length(ids) == 1L) {
        stop(
          "HTTP 504 Gateway Timeout updating blank status for segment record ID ",
          ids[[1]], ". ", submitted, " segment record(s) were already updated.",
          call. = FALSE
        )
      }
      mid <- ceiling(length(ids) / 2)
      cli::cli_alert_warning(
        "Request of {length(ids)} segment record(s) timed out; retrying as two smaller requests."
      )
      queue <- c(list(ids[seq_len(mid)], ids[(mid + 1L):length(ids)]), queue)
      next
    }

    resp <- httr2::resp_body_json(preq)
    submitted <- submitted + length(ids)
    cli::cli_progress_update(id = pb, set = submitted)
  }

  message(resp$message)
  return(resp)
}

#' @title Set publish status for segment records
#'
#' @description
#' Marks or unmarks segment records as published for a given list of segment record IDs.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}.
#' @param publish_status A boolean value indicating whether to publish (TRUE) or unpublish (FALSE).
#' @param segment_record_ids A numeric vector of segment record IDs to update.
#' @param chunksize An integer specifying how many segment record IDs to send per request.
#'   Defaults to 500.
#'
#' @return A list containing the API response message from the last chunk submitted.
#'
#' @examples
#' \dontrun{
#'   # Publish segments
#'   publish_segments(headers, publish_status = TRUE, segment_record_ids = c(101, 102, 103))
#'   # Unpublish segments
#'   publish_segments(headers, publish_status = FALSE, segment_record_ids = c(101, 102, 103))
#' }
#'
#' @author
#' Adam Varley, Cristobal Salamé
#' @export
publish_segments <- function(hdr, publish_status, segment_record_ids, chunksize = 500) {
  status_str <- tolower(as.character(publish_status))

  if (chunksize > length(segment_record_ids)) {
    chunksize <- length(segment_record_ids)
  }

  chunks <- split(segment_record_ids, ceiling(seq_along(segment_record_ids) / chunksize))

  pb <- cli::cli_progress_bar(
    format = "Publishing {cli::pb_current}/{cli::pb_total} segment record(s) | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    total  = length(segment_record_ids),
    clear  = FALSE
  )
  # on.exit (not tryCatch) so the bar is always closed - including on a user
  # interrupt (e.g. Escape/Ctrl+C) - see .check_label_values() in
  # push_phone_observations.R for the same pattern and why tryCatch's `error`
  # handler alone isn't enough.
  on.exit(cli::cli_progress_done(id = pb), add = TRUE)

  submitted <- 0
  for (i in seq_along(chunks)) {
    urlreq_ap <- httr2::req_url_path_append(hdr$root, "segmentRecordsPublishStatus", hdr$key, status_str) %>%
      httr2::req_method("PUT") %>%
      httr2::req_body_json(data = chunks[[i]])

    preq <- httr2::req_perform(urlreq_ap)
    resp <- httr2::resp_body_json(preq)

    submitted <- submitted + length(chunks[[i]])
    cli::cli_progress_update(id = pb, set = submitted)
  }

  return(resp)
}
