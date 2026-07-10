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
  urlreq_ap <- urlreq_ap |> httr2::req_method("POST") |> httr2::req_body_json(data = labels)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq)

  return(resp)
}

# Utility function to replace NULLs with NAs in a data frame
replace_nas <- function(df) {
  df[sapply(df, function(x) is.null(x))] <- NA
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
  urlreq_ap <- urlreq_ap |>
    httr2::req_method("GET") |>
    httr2::req_url_query("offset" = offset, "limit" = limit, "search_term" = search_term)

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
#' Adam Varley
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


  for (i in seq_along(spl.dt)) {

    urlreq_ap <- httr2::req_url_path_append(hdr$root, "addIUCNLabels", hdr$key)
    urlreq_ap <- urlreq_ap |> httr2::req_method("POST") |> httr2::req_body_json(data = spl.dt[[i]])

    preq <- httr2::req_perform(urlreq_ap, verbosity = 3)
    resp <- httr2::resp_body_json(preq)

    message('submitted ', i * chunksize, ' labels of ', nrow(labels))
  }

  return(resp)
}


# Internally used function to send updated labels in chunks

send_updated_labels <- function(hdr, datachunk) {

  datachunk <- jsonlite::toJSON(datachunk, pretty = TRUE)

  urlreq_ap <- httr2::req_url_path_append(hdr$root, "updateSegmentLabels", hdr$key)
  urlreq_ap <- urlreq_ap |> httr2::req_method("PUT") |> httr2::req_body_json(jsonlite::fromJSON(datachunk))
  preq <- httr2::req_perform(urlreq_ap, verbosity = 3)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp))
}

#' @title Push new labels using a chunked process
#'
#' @description
#' Push new labels to the platform in chunks.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param submission_records A tibble containing the records to be submitted
#' @param chunksize An integer specifying the chunk size for the submission
#'
#' @return A list containing tabular data and pagination information for iterative calls
#'
#' @examples
#' \dontrun{
#'   push_new_labels(headers, submission_records, chunksize=30)
#' }
#'
#' @author
#' Adam Varley
#' @export
push_new_labels <- function(hdr, submission_records, chunksize) {

  if (chunksize > nrow(submission_records)) {
    message('chunksize is bigger than length of data altering chunksize to ', nrow(submission_records))
    chunksize <- nrow(submission_records)
  }

  spl.dt <- split(submission_records, cut(seq_len(nrow(submission_records)), round(nrow(submission_records) / chunksize)))
  for (i in seq_along(spl.dt)) {

    send_updated_labels(hdr, datachunk = spl.dt[[i]])
    message('submitted ', i * chunksize, ' labels of ', nrow(submission_records))
  }
}



#' @title Set blank status for segment labels
#'
#' @description
#' Marks or unmarks segment labels as blank for a given list of segment record IDs.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}.
#' @param blank_status A boolean value indicating whether to mark as blank (TRUE) or unblank (FALSE).
#' @param segment_record_ids A numeric vector of segment record IDs to update.
#'
#' @return A list containing the API response message.
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
set_segment_blank_status <- function(hdr, blank_status, segment_record_ids) {
  status_str <- tolower(as.character(blank_status))

  urlreq_ap <- httr2::req_url_path_append(hdr$root, "segmentLabelsBlankStatus", hdr$key, status_str) %>%
    httr2::req_method("PUT") %>%
    httr2::req_body_json(data = segment_record_ids)

  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq)

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
#' Adam Varley
#' @export
publish_segments <- function(hdr, publish_status, segment_record_ids, chunksize = 500) {
  status_str <- tolower(as.character(publish_status))

  if (chunksize > length(segment_record_ids)) {
    chunksize <- length(segment_record_ids)
  }

  chunks <- split(segment_record_ids, ceiling(seq_along(segment_record_ids) / chunksize))

  for (i in seq_along(chunks)) {
    urlreq_ap <- httr2::req_url_path_append(hdr$root, "segmentRecordsPublishStatus", hdr$key, status_str) %>%
      httr2::req_method("PUT") %>%
      httr2::req_body_json(data = chunks[[i]])

    preq <- httr2::req_perform(urlreq_ap)
    resp <- httr2::resp_body_json(preq)

    message('Chunk ', i, '/', length(chunks), ' (', length(chunks[[i]]), ' records) - ', resp$message)
  }

  return(resp)
}
