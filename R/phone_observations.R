# Internal null-coalescing helper.
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Internal: performs an httr2 request; on a >=400 response, stops with the
# status and response body so the API's own error message is visible instead
# of a generic httr2 error.
.perform_or_stop <- function(req) {
  resp   <- httr2::req_perform(req |> httr2::req_error(is_error = \(r) FALSE))
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "<no response body>")
    stop(sprintf("HTTP %d from %s\n%s", status, resp$url, body), call. = FALSE)
  }
  resp
}

#' @title Request signed URLs for field media upload
#'
#' @description
#' Calls \code{POST /getFieldMediaUploadUrls/{api_key}} and returns
#' signed PUT URLs for direct-to-GCS uploads.
#'
#' @param hdr Auth headers from \link{auth_headers}.
#' @param files List of lists, each with \code{filename}, \code{data_type},
#'   and \code{context} (typically \code{"field_record"}).
#'
#' @return A list of file descriptors with \code{filename}, \code{blob_path},
#'   \code{signed_url}, and \code{content_type}.
#'
#' @keywords internal
get_field_media_upload_urls <- function(hdr, files) {
  if (length(files) == 0) {
    return(list())
  }

  # Batch in chunks of 50 (API limit)
  all_results <- list()
  batch_size <- 50L
  n <- length(files)
  for (start in seq(1L, n, by = batch_size)) {
    end <- min(start + batch_size - 1L, n)
    batch <- files[start:end]

    urlreq <- httr2::req_url_path_append(
      hdr$root,
      "getFieldMediaUploadUrls",
      hdr$key
    )
    urlreq <- urlreq |>
      httr2::req_method("POST") |>
      httr2::req_body_json(list(files = unname(batch)))

    response <- httr2::req_perform(urlreq)
    body <- httr2::resp_body_json(response)
    all_results <- c(all_results, body$files)
  }

  return(all_results)
}


#' @title Upload local media files via signed URLs
#'
#' @description
#' Presigns upload URLs then PUTs each local file directly to cloud storage.
#'
#' @param hdr Auth headers from \link{auth_headers}.
#' @param media_files Named list from \code{.upload_pending_media()}.
#'
#' @return Invisibly, the list of presign response entries.
#'
#' @keywords internal
upload_field_media_files <- function(hdr, media_files) {
  if (length(media_files) == 0) {
    return(invisible(list()))
  }

  file_requests <- lapply(names(media_files), function(filename) {
    list(
      filename = filename,
      data_type = media_files[[filename]]$data_type,
      context = "field_record"
    )
  })

  signed <- get_field_media_upload_urls(hdr, file_requests)

  pb <- cli::cli_progress_bar(
    format = "Uploading {cli::pb_current}/{cli::pb_total} media file(s) | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    total  = length(signed),
    clear  = FALSE
  )
  # on.exit (not tryCatch) so the bar is always closed - including on a user
  # interrupt (e.g. Escape/Ctrl+C) - see .check_label_values() for the same
  # pattern and why tryCatch's `error` handler alone isn't enough.
  on.exit(cli::cli_progress_done(id = pb), add = TRUE)

  for (entry in signed) {
    filename <- entry$filename
    info <- media_files[[filename]]
    if (is.null(info)) {
      stop("Presign returned unexpected filename: ", filename)
    }

    content_type <- if (!is.null(entry$content_type)) entry$content_type else info$content_type
    put_req <- httr2::request(entry$signed_url)  %>% 
      httr2::req_method("PUT")  %>% 
      httr2::req_headers(`Content-Type` = content_type)  %>% 
      httr2::req_body_raw(
        readBin(info$filepath, what = "raw", n = file.info(info$filepath)$size),
        type = content_type
      )

    httr2::req_perform(put_req)
    cli::cli_progress_update(id = pb, inc = 1)
  }

  invisible(signed)
}


#' @title Get Project Schema
#'
#' @description
#' Retrieves the project schema (codebook) used to populate `project_system_id`,
#' `procedure_id`, and valid item UUIDs for observation uploads.
#'
#' @param hdr A base URL and API key returned by \link{auth_headers} or
#'   \link{auth_headers_dev}.
#'
#' @return A parsed JSON list containing the project schema.
#'
#' @examples
#' \dontrun{
#'   hdr <- auth_headers("your_api_key")
#'   schema <- get_project_systems(hdr)
#' }
#'
#' @author Adam Varley
#' @export
get_project_systems <- function(hdr) {
  urlreq <- httr2::req_url_path_append(hdr$root, "getProjectSchema", hdr$key)
  httr2::resp_body_json(.perform_or_stop(urlreq))
}


#' @title List Systems and Procedures
#'
#' @description
#' Displays a summary of all systems and their procedures available in the
#' project schema. Use this to discover valid system/procedure name combinations
#' before submitting observations.
#'
#' @param schema List. Project schema returned by \code{get_project_systems()}.
#'
#' @return A tibble (invisibly) with columns \code{system_index},
#'   \code{system_name}, \code{system_id}, \code{procedure_index},
#'   \code{procedure_name}, and \code{procedure_id}. The tibble is also
#'   printed to the console.
#'
#' @examples
#' \dontrun{
#'   hdr <- auth_headers("your_api_key")
#'   schema <- get_project_systems(hdr)
#'   list_systems(schema)
#' }
#'
#' @author Adam Varley
#' @export
list_systems <- function(schema) {
  if (is.null(schema$systems) || length(schema$systems) == 0) {
    message("No systems found in schema.")
    return(invisible(tibble::tibble()))
  }

  rows <- lapply(seq_along(schema$systems), function(si) {
    sys      <- schema$systems[[si]]
    sys_name <- as.character(sys$system_name %||% "")
    sys_id   <- if (!is.null(sys$project_system_id)) as.integer(sys$project_system_id) else NA_integer_
    procs    <- sys$procedures

    if (length(procs) == 0) {
      return(tibble::tibble(
        system_index = si, system_name = sys_name, system_id = sys_id,
        procedure_index = NA_integer_, procedure_name = NA_character_,
        procedure_id = NA_integer_, form = NA
      ))
    }

    tibble::tibble(
      system_index    = si,
      system_name     = sys_name,
      system_id       = sys_id,
      procedure_index = seq_along(procs),
      procedure_name  = vapply(procs, function(p) as.character(p$procedure_name %||% ""), character(1)),
      procedure_id    = vapply(procs, function(p) as.integer(p$procedure_id %||% NA_integer_), integer(1)),
      form            = vapply(procs, function(p) as.logical(p$form %||% NA), logical(1))
    )
  })

  result <- dplyr::bind_rows(rows)
  print(result)
  return(invisible(result))
}


#' @title Describe Procedure Items
#'
#' @description
#' Returns a detailed table of all items (fields) in a selected procedure,
#' including item names, UUIDs, types, and valid choices where applicable.
#' Use this to understand exactly what data to submit and how to structure it
#' before calling \code{upload_observations_from_csv()}.
#'
#' @param schema List. Project schema returned by \code{get_project_systems()}.
#' @param system_name Character. Name of the system. Optional; takes precedence
#'   over \code{system_index} when provided.
#' @param system_index Integer. Index of the system. Default \code{NULL}
#'   (resolves to 1 if \code{system_name} is also absent).
#' @param procedure_name Character. Name of the procedure. Optional; takes
#'   precedence over \code{procedure_index} when provided.
#' @param procedure_index Integer. Index of the procedure. Default \code{NULL}
#'   (resolves to 1 if \code{procedure_name} is also absent).
#'
#' @return A named list (invisibly) with elements:
#'   \describe{
#'     \item{system_id}{Integer project system ID.}
#'     \item{procedure_id}{Integer procedure ID.}
#'     \item{system_name}{Character system name.}
#'     \item{procedure_name}{Character procedure name.}
#'     \item{form}{Logical; whether the procedure is a form.}
#'     \item{items}{Tibble with one row per item: \code{item_id}, \code{item_uuid},
#'       \code{item_name}, \code{item_description}, \code{data_type}, \code{nullable},
#'       \code{choices}.}
#'   }
#'   The items table is also printed to the console.
#'
#' @examples
#' \dontrun{
#'   hdr <- auth_headers("your_api_key")
#'   schema <- get_project_systems(hdr)
#'
#'   # Explore using names
#'   get_procedure(schema,
#'     system_name = "Plante Ivindo",
#'     procedure_name = "Arbre")
#'
#'   # Or by index
#'   get_procedure(schema, system_index = 1, procedure_index = 2)
#' }
#'
#' @author Adam Varley
#' @export
get_procedure <- function(schema,
                               system_name = NULL,
                               system_index = NULL,
                               procedure_name = NULL,
                               procedure_index = NULL) {

  idx <- resolve_schema_indices(
    schema          = schema,
    system_index    = system_index,
    procedure_index = procedure_index,
    system_name     = system_name,
    procedure_name  = procedure_name
  )

  system    <- schema$systems[[idx$system_index]]
  procedure <- system$procedures[[idx$procedure_index]]

  item_nodes <- collect_item_nodes(procedure)

  if (length(item_nodes) == 0) {
    message("No items found in selected procedure.")
    return(invisible(tibble::tibble()))
  }

  rows <- lapply(seq_along(item_nodes), function(i) {
    node <- item_nodes[[i]]

    # Resolve choices for choice-type items
    choice_vals <- node$choices %||% node$options %||% node$items
    choices_str <- ""
    if (!is.null(choice_vals) && length(choice_vals) > 0) {
      choice_labels <- vapply(choice_vals, function(ch) {
        if (!is.list(ch)) return(as.character(ch))
        lbl <- ch$label %||% ch$value %||% ch$name %||% ch$choice_label
        if (is.null(lbl)) lbl <- as.character(ch)
        as.character(lbl)
      }, character(1))
      choices_str <- paste(choice_labels, collapse = " | ")
    }

    tibble::tibble(
      item_id          = if (!is.null(node$item_id)) as.integer(node$item_id) else NA_integer_,
      item_uuid        = as.character(node$item_uuid),
      item_name        = as.character(node$item_name %||% ""),
      item_description = as.character(node$item_description %||% NA_character_),
      data_type        = as.character(node$data_type %||% ""),
      nullable         = if (!is.null(node$nullable)) as.logical(node$nullable) else NA,
      choices          = choices_str
    )
  })

  result <- dplyr::bind_rows(rows)

  sys_name  <- as.character(system$system_name %||% paste("System", idx$system_index))
  proc_name <- as.character(procedure$procedure_name %||% paste("Procedure", idx$procedure_index))
  proc_form <- if (!is.null(procedure$form)) as.logical(procedure$form) else NA
  sys_id    <- if (!is.null(system$project_system_id)) as.integer(system$project_system_id) else NA_integer_
  proc_id   <- if (!is.null(procedure$procedure_id))   as.integer(procedure$procedure_id)   else NA_integer_

  out <- list(
    system_id      = sys_id,
    procedure_id   = proc_id,
    system_name    = sys_name,
    procedure_name = proc_name,
    form           = proc_form,
    items          = result
  )

  message("System: ", sys_name, " (id: ", sys_id, ")")
  message("Procedure: ", proc_name, " (id: ", proc_id, ", form: ", proc_form, ")")
  message("Items (", nrow(result), "):")
  print(result)
  return(invisible(out))
}


# Taxonomic rank columns recognised for a "label" data_type item, ordered
# most specific to least. Not every organism is identified all the way to
# species - when `species` is blank for a row/observation, the first
# non-blank rank moving up this list is used instead (e.g. one only
# identified to genus is still uploaded, using that genus). All of these are
# optional; looked up by fixed column name, not by matching the label item's
# own item_name.
.taxonomy_ranks <- c("species", "genus", "family", "order", "class", "phylum", "kingdom")

# Internal: the coalesced taxonomic label for one row - the value of the
# most specific non-blank rank column present in `data`. Returns NA if every
# rank column is blank or absent for this row.
.coalesce_taxonomic_label <- function(data, r) {
  for (rank in .taxonomy_ranks) {
    if (!rank %in% names(data)) next
    val <- trimws(as.character(data[[rank]][[r]]))
    if (!is.na(val) && nzchar(val) && !identical(val, "NA")) return(val)
  }
  NA_character_
}


#' @title Validate CSV Against Procedure
#'
#' @description
#' Checks a wide-format observation table (one row per feature; procedure
#' item names spread as column headers) against a procedure object returned
#' by \code{get_procedure()}, and mandatorily validates every row the same
#' way \code{upload_observations_from_csv()} would build it - including
#' checking any \code{label} data_type value against the label database.
#'
#' The table must have one column per procedure item (matched by name),
#' plus \code{longitude}, \code{latitude}, and \code{timestamp} columns.
#'
#' If the procedure has a \code{label} data_type item, the table should carry
#' taxonomic rank columns - \code{species}, \code{genus}, \code{family},
#' \code{order}, \code{class}, \code{phylum}, \code{kingdom} - looked up by
#' those fixed names, not by matching the label item's own item_name. Not
#' every organism is identified all the way to species: the most specific
#' non-blank rank is used (e.g. one only identified to genus still gets
#' checked/uploaded using that genus). Every unique resulting value is
#' checked against the wider IUCN species database (via
#' \link{getIUCNLabels}) once; matching is an exact (case-insensitive) match
#' against a returned row's name.
#'
#' An observation is only valid if \strong{every} field on it validates:
#' longitude/latitude parse as decimals, the timestamp parses (via
#' \code{lubridate::as_datetime()}), every referenced media file exists on
#' disk, every label value matches the label database, and every other value
#' matches its declared data_type. A single bad field rejects the whole
#' observation, mirroring \code{check_edna_labels()}/\code{upload_edna_records()}'s
#' status-gated pattern.
#'
#' @param procedure Named list returned by \code{get_procedure()}.
#' @param observation_data Data frame of observation rows - one row per
#'   feature, with procedure item names as column headers.
#' @param hdr A base URL and API key returned by \link{auth_headers} or
#'   \link{auth_headers_dev}. Required (not optional): it is used to check
#'   any \code{label} data_type values against the label database via
#'   \link{getIUCNLabels}, and this check cannot be silently skipped.
#' @param lon_col Longitude column. Default \code{"longitude"}.
#' @param lat_col Latitude column. Default \code{"latitude"}.
#' @param timestamp_col Timestamp column. Default \code{"timestamp"}.
#'
#' @return (Invisibly) \code{observation_data} as a tibble, with two columns added:
#'   \describe{
#'     \item{status}{\code{"success"} or \code{"rejected"} for that row/observation.}
#'     \item{message}{Why it was rejected (blank on success) - bad timestamp,
#'       non-decimal lon/lat, missing media file, unmatched label, or a
#'       wrong-typed value.}
#'   }
#'   Two further columns, \code{.payload} and \code{.media}, carry the
#'   already-built upload payload (including the label database check) for
#'   each row - pass this whole data frame as \code{validated} to
#'   \link{upload_observations_from_csv} to upload it directly, with no
#'   rebuilding and no re-checking any labels.
#'
#' @examples
#' \dontrun{
#'   hdr <- auth_headers("your_api_key")
#'   schema <- get_project_systems(hdr)
#'   procedure <- get_procedure(schema,
#'     system_name = "Plante Ivindo", procedure_name = "Arbre")
#'
#'   observation_data <- readr::read_csv("tutorials/example_wide.csv")
#'   validated <- validate_csv_against_procedure(procedure,
#'     observation_data = observation_data, hdr = hdr)
#' }
#'
#' @author Adam Varley
#' @export
validate_csv_against_procedure <- function(procedure,
                                           observation_data,
                                           hdr,
                                           lon_col           = "longitude",
                                           lat_col           = "latitude",
                                           timestamp_col     = "timestamp") {

  if (!is.list(procedure) || is.null(procedure$items) || nrow(procedure$items) == 0) {
    stop("procedure must be a non-empty list returned by get_procedure()")
  }

  # Extract metadata from the named list
  proc_system_id    <- procedure$system_id    %||% NA_integer_
  proc_procedure_id <- procedure$procedure_id %||% NA_integer_
  proc_system_name  <- procedure$system_name  %||% ""
  proc_proc_name    <- procedure$procedure_name %||% ""
  procedure_items <- procedure$items

  # ---- Column-mapping report (pure, no network) ---------------------------
  # Label items are resolved via the fixed taxonomic rank columns (see
  # below), not by matching their own item_name, so they're reported
  # separately rather than through the normal column/item-name matching.
  label_items     <- procedure_items[procedure_items$data_type == "label", , drop = FALSE]
  matchable_items <- procedure_items[procedure_items$data_type != "label", , drop = FALSE]
  matchable_norm  <- normalize_lookup_value(matchable_items$item_name)

  meta_cols <- c(lon_col, lat_col, timestamp_col, intersect(.taxonomy_ranks, names(observation_data)))
  item_cols <- setdiff(names(observation_data), meta_cols)
  resolved  <- .resolve_item_matches(matchable_items, matchable_norm, normalize_lookup_value(item_cols), item_cols,
                                     "column", "CSV column(s)")

  # ---- Mandatory validation: build against the procedure using the exact
  # same logic upload_observations_from_csv() uses, so a CSV that
  # validates here is guaranteed to build identically at upload time. -------
  built <- tryCatch(
    build_upload_observations_from_table(
      data          = observation_data,
      procedure     = procedure,
      lon_col       = lon_col,
      lat_col       = lat_col,
      timestamp_col = timestamp_col
    ),
    error = function(e) {
      list(resolved_rows = 0, rejected = list(), build_error = conditionMessage(e))
    }
  )

  # ---- Check every label value used against the label database - the only
  # network call this function makes (build_upload_observations_from_table()
  # is fully offline). ------------------------------------------------------
  label_uuid <- procedure_items$item_uuid[procedure_items$data_type == "label"]
  label_uuid <- if (length(label_uuid) > 0) label_uuid[[1]] else NA_character_

  label_bad <- character()  # source_ids (row number) whose label failed
  if (!is.na(label_uuid) && length(built$observations) > 0) {
    label_vals <- vapply(built$observations, function(o) as.character(o$values[[label_uuid]] %||% NA_character_), character(1))
    checked    <- which(!is.na(label_vals))
    if (length(checked) > 0) {
      label_ok <- .check_label_values(hdr, label_vals[checked])
      for (i in checked) {
        if (!isTRUE(label_ok[[label_vals[i]]])) label_bad <- c(label_bad, built$source_ids[i])
      }
    }
  }

  # ---- Build the returned data frame: the original CSV, plus status,
  # message, and the already-built payload ready for
  # upload_observations_from_csv() - no rebuilding, no rechecking labels. ---
  n       <- nrow(observation_data)
  status  <- rep("success", n)
  msg     <- rep("", n)
  payload <- vector("list", n)
  media   <- replicate(n, list(), simplify = FALSE)

  if (!is.null(built$build_error)) {
    status[] <- "rejected"
    msg[]    <- built$build_error
  } else {
    for (rj in built$rejected) {
      status[rj$row] <- "rejected"
      msg[rj$row]    <- paste(rj$reasons, collapse = "; ")
    }
    accounted <- vapply(built$rejected, function(rj) rj$row, integer(1))
    for (i in seq_along(built$observations)) {
      r <- as.integer(built$source_ids[i])
      accounted <- c(accounted, r)
      if (built$source_ids[i] %in% label_bad) {
        status[r] <- "rejected"
        msg[r]    <- paste0("'", built$observations[[i]]$values[[label_uuid]], "' not found in label database")
      } else {
        payload[[r]] <- built$observations[[i]]
      }
    }
    for (m in built$media_uploads) {
      r <- as.integer(built$source_ids[m$obs_index])
      if (!is.null(payload[[r]])) {
        media[[r]] <- c(media[[r]], list(list(
          item_uuid = m$item_uuid, filepath = m$filepath,
          data_type = m$data_type, content_type = m$content_type
        )))
      }
    }
    unaccounted <- setdiff(seq_len(n), accounted)
    status[unaccounted] <- "rejected"
    msg[unaccounted]     <- "no values provided for this row"
  }

  valid <- all(status == "success")

  # ---- Print summary -------------------------------------------------------
  message("\n--- CSV Validation Report ---")
  message("System: ", proc_system_name, " (id: ", proc_system_id, ")")
  message("Procedure: ", proc_proc_name, " (id: ", proc_procedure_id, ")")

  message("Matched items (", nrow(resolved$matched_items), "/", nrow(matchable_items), "):")
  if (nrow(resolved$matched_items) > 0) {
    print(resolved$matched_items[, c("item_name", "data_type"), drop = FALSE])
  }
  if (nrow(label_items) > 0) {
    message("\nTaxonomic label(s) checked:")
    print(label_items[, c("item_name", "data_type"), drop = FALSE])
  }
  if (nrow(resolved$missing_items) > 0) {
    required_miss <- resolved$missing_items[is.na(resolved$missing_items$nullable) | !resolved$missing_items$nullable, , drop = FALSE]
    optional_miss <- resolved$missing_items[!is.na(resolved$missing_items$nullable) & resolved$missing_items$nullable, , drop = FALSE]
    if (nrow(required_miss) > 0) {
      message("\nMissing required items (", nrow(required_miss), "):")
      print(required_miss[, c("item_name", "data_type", "nullable"), drop = FALSE])
    }
    if (nrow(optional_miss) > 0) {
      message("\nMissing nullable/optional items (", nrow(optional_miss), ") \u2014 allowed:")
      print(optional_miss[, c("item_name", "data_type", "nullable"), drop = FALSE])
    }
  }
  if (length(resolved$unrecognised_names) > 0) {
    message("\nUnrecognised item names in CSV:")
    print(resolved$unrecognised_names)
  }

  n_rejected <- sum(status == "rejected")
  if (n_rejected > 0) {
    message("\nRejected row(s) (", n_rejected, "), not eligible for upload:")
    for (i in which(status == "rejected")) {
      message("  [", i, "] ", msg[i])
    }
  }

  message("\n", sum(status == "success"), " of ", n, " row(s) would be uploaded.")
  message("\nValid: ", valid)

  result <- tibble::as_tibble(observation_data)
  result$status   <- status
  result$message  <- msg
  result$.payload <- payload
  result$.media   <- media

  return(invisible(result))
}


# Internal: resolves which procedure items are present/absent given the
# normalized column headers found in a CSV, and builds the associated issue
# messages.
.resolve_item_matches <- function(procedure, proc_norm, found_norm, found_names,
                                  missing_label, unrecognised_label) {
  matched_mask       <- proc_norm %in% found_norm
  matched_items      <- procedure[matched_mask,  , drop = FALSE]
  missing_items      <- procedure[!matched_mask, , drop = FALSE]
  unrecognised_names <- found_names[!found_norm %in% proc_norm]

  required_missing <- missing_items[is.na(missing_items$nullable) | !missing_items$nullable, , drop = FALSE]
  optional_missing <- missing_items[!is.na(missing_items$nullable) & missing_items$nullable, , drop = FALSE]

  issues <- character()
  if (nrow(required_missing) > 0) {
    issues <- c(issues, paste(nrow(required_missing), "required item(s) have no", missing_label,
                              "in the CSV:", paste(required_missing$item_name, collapse = ", ")))
  }
  if (nrow(optional_missing) > 0) {
    issues <- c(issues, paste(nrow(optional_missing), "nullable/optional item(s) have no", missing_label,
                              "in the CSV:", paste(optional_missing$item_name, collapse = ", ")))
  }
  if (length(unrecognised_names) > 0) {
    issues <- c(issues, paste(length(unrecognised_names), unrecognised_label,
                              "not found in procedure:", paste(unrecognised_names, collapse = ", ")))
  }

  list(
    matched_items      = matched_items,
    missing_items      = missing_items,
    unrecognised_names = unrecognised_names,
    issues             = issues
  )
}



#' @title Upload Observations
#'
#' @description
#' Uploads one or more observations to the `uploadObservations` endpoint.
#'
#' @param hdr A base URL and API key returned by \link{auth_headers} or
#'   \link{auth_headers_dev}.
#' @param observations List of observations, keyed by item UUID, such as
#'   those built by \code{build_upload_observations_from_table()}.
#' @param dry_run Logical. If \code{TRUE}, returns the request payload
#'   without sending it to the API. Default \code{FALSE}.
#'
#' @return Parsed API response as a list.
#'
#' @examples
#' \dontrun{
#'   hdr       <- auth_headers("your_api_key")
#'   schema    <- get_project_systems(hdr)
#'   procedure <- get_procedure(schema,
#'     system_name = "Plante Ivindo", procedure_name = "Arbre")
#'   built <- build_upload_observations_from_table(df, procedure = procedure)
#'   resp  <- upload_observations(hdr, built$observations)
#' }
#'
#' @author Adam Varley
#' @export
upload_observations <- function(hdr, observations, dry_run = FALSE) {

  if (missing(observations) || is.null(observations) || length(observations) == 0) {
    stop("observations must be a non-empty list")
  }

  # Allows caller to inspect the exact JSON before sending
  if (isTRUE(dry_run)) {
    cat(jsonlite::toJSON(list(observations = observations), auto_unbox = TRUE, pretty = TRUE))
    return(invisible(list(observations = observations)))
  }

  # uploadObservations rejects a batch of more than 500 observations, so
  # larger uploads are split into sequential requests and the per-observation
  # results concatenated back into one list, in order.
  batch_size <- 500L
  n <- length(observations)
  responses <- list()

  pb <- cli::cli_progress_bar(
    format = "Uploading {cli::pb_current}/{cli::pb_total} observations to NatureCube | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    total  = n,
    clear  = FALSE
  )
  on.exit(cli::cli_progress_done(id = pb), add = TRUE)

  for (start in seq(1L, n, by = batch_size)) {
    end   <- min(start + batch_size - 1L, n)
    urlreq <- httr2::req_url_path_append(hdr$root, "uploadObservations", hdr$key) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(list(observations = observations[start:end]), auto_unbox = TRUE)
    responses <- c(responses, httr2::resp_body_json(.perform_or_stop(urlreq)))
    cli::cli_progress_update(id = pb, set = end)
  }
  responses
}


# Internal helper to normalize lookup strings.
normalize_lookup_value <- function(x) {
  out <- tolower(trimws(as.character(x)))
  out <- iconv(out, from = "", to = "ASCII//TRANSLIT")
  out[is.na(out)] <- ""
  return(out)
}




# Internal helper to coerce a value to the JSON type its item's data_type
# calls for, on the final object sent to NatureCube: numeric items become
# real numbers (the API rejects e.g. "20" as a string for a numeric item).
# Everything else - text, choice, label, or any data_type not recognised as
# numeric - is always forced to character, so a numeric-looking value in a
# non-numeric field (e.g. a plot number "007") can never slip through as a
# JSON number just because it happened to arrive as a non-character value.
.NUMERIC_DATA_TYPE_RE <- "num|int|float|double|decimal|real"

coerce_value_by_data_type <- function(value, data_type) {
  dt <- data_type %||% ""
  if (grepl(.NUMERIC_DATA_TYPE_RE, dt)) {
    numeric_candidate <- suppressWarnings(as.numeric(value))
    if (!is.na(numeric_candidate)) {
      return(numeric_candidate)
    }
  }
  value <- as.character(value)
  # data_type isn't numeric, but this particular value's content still looks
  # numeric (e.g. a plot number like "30") - the dashboard re-parses
  # numeric-looking text back into a number regardless of JSON type, so a
  # leading quote forces it to stay text. Non-numeric-looking values (e.g.
  # "M5") are left untouched.
  if (!is.na(suppressWarnings(as.numeric(value)))) {
    value <- paste0("'", value)
  }
  value
}


# data_type values that reference a local media file rather than a plain
# text/numeric value. The cell holds a local file path; uploading it to cloud
# storage (via a signed URL) and substituting the returned blob_path happens
# later in upload_observations_from_csv(), since build_upload_observations_from_table()
# has no network access by design.
.MEDIA_DATA_TYPES <- c("phone-photo", "phone-video", "phone-audio")

.media_content_type <- function(data_type) {
  switch(data_type,
    "phone-photo" = "image/jpeg",
    "phone-video" = "video/mp4",
    "phone-audio" = "audio/mpeg",
    "application/octet-stream"
  )
}


# Internal: checks each unique species value against the wider IUCN species
# database via getIUCNLabels(). getIUCNLabels() takes one search_term per
# call (it's a search endpoint, not a batch lookup), so unique values are
# deduplicated to avoid one call per row. A species counts as valid only if
# a returned row's `label` or `genus`+`species` is an exact (case-insensitive,
# trimmed) match - a plain text search can otherwise return unrelated
# partial matches. Returns a named logical vector keyed by the original
# (trimmed) species value.
.check_label_values <- function(hdr, values) {
  unique_values <- unique(trimws(values))
  unique_values <- unique_values[nzchar(unique_values)]

  if (length(unique_values) == 0) {
    return(stats::setNames(logical(0), character(0)))
  }

  pb <- cli::cli_progress_bar(
    format = "Checking {cli::pb_current}/{cli::pb_total} label(s) against the database | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    total  = length(unique_values),
    clear  = FALSE
  )
  # on.exit (not tryCatch) so the bar is always closed - including on a user
  # interrupt (e.g. Escape/Ctrl+C), which tryCatch's `error` handler does not
  # catch and would otherwise leave it animating for the rest of the session.
  on.exit(cli::cli_progress_done(id = pb), add = TRUE)

  found <- logical(length(unique_values))
  for (i in seq_along(unique_values)) {
    val      <- unique_values[i]
    rows     <- getIUCNLabels(hdr, offset = 0, search_term = val)$data
    found[i] <- nrow(rows) > 0 && any(
      normalize_lookup_value(rows$label) == normalize_lookup_value(val) |
      normalize_lookup_value(paste(rows$genus, rows$species)) == normalize_lookup_value(val)
    )
    cli::cli_progress_update(id = pb, inc = 1)
    if (i < length(unique_values)) Sys.sleep(0.2)  # throttle to avoid tripping the API's rate limit
  }

  stats::setNames(found, unique_values)
}


# Internal: TRUE if a non-blank cell value matches what its item's declared
# data_type requires (numeric/boolean types only - text/choice/instruction
# accept anything, since coerce_value_by_data_type() forces text items to
# character on the final payload regardless of what the raw value looks
# like). Media (phone-photo/video/audio) and label types are validated
# separately (file existence, label database).
.value_matches_data_type <- function(value, data_type) {
  dt <- data_type %||% ""
  if (grepl(.NUMERIC_DATA_TYPE_RE, dt)) {
    return(!is.na(suppressWarnings(as.numeric(value))))
  }
  if (grepl("bool", dt)) {
    return(tolower(value) %in% c("true", "false", "yes", "no", "1", "0"))
  }
  TRUE
}


# Internal helper to recursively collect all item-like nodes from a procedure.
collect_item_nodes <- function(x, out = list()) {
  if (!is.list(x)) {
    return(out)
  }

  if (!is.null(x$item_uuid)) {
    out[[length(out) + 1]] <- x
  }

  if (is.null(names(x))) {
    for (i in seq_along(x)) {
      out <- collect_item_nodes(x[[i]], out)
    }
  } else {
    for (nm in names(x)) {
      out <- collect_item_nodes(x[[nm]], out)
    }
  }

  return(out)
}


# Internal helper to resolve system/procedure indices from schema and optional names.
resolve_schema_indices <- function(schema,
                                   system_index = NULL,
                                   procedure_index = NULL,
                                   system_name = NULL,
                                   procedure_name = NULL) {
  if (is.null(schema$systems) || length(schema$systems) == 0) {
    stop("schema does not contain any systems")
  }

  if (is.null(system_index)) {
    if (!is.null(system_name) && nzchar(system_name)) {
      system_names <- vapply(
        schema$systems,
        function(x) as.character(x$system_name %||% ""),
        character(1)
      )
      match_idx <- which(normalize_lookup_value(system_names) == normalize_lookup_value(system_name))[1]
      if (is.na(match_idx)) {
        stop("system_name was provided but not found in schema")
      }
      system_index <- match_idx
    } else {
      system_index <- 1
    }
  }

  if (length(schema$systems) < system_index) {
    stop("system_index is out of bounds for schema$systems")
  }

  procedures <- schema$systems[[system_index]]$procedures
  if (is.null(procedures) || length(procedures) == 0) {
    stop("selected system does not contain any procedures")
  }

  if (is.null(procedure_index)) {
    if (!is.null(procedure_name) && nzchar(procedure_name)) {
      procedure_names <- vapply(
        procedures,
        function(x) as.character(x$procedure_name %||% ""),
        character(1)
      )
      match_idx <- which(normalize_lookup_value(procedure_names) == normalize_lookup_value(procedure_name))[1]
      if (is.na(match_idx)) {
        stop("procedure_name was provided but not found in schema for selected system")
      }
      procedure_index <- match_idx
    } else {
      procedure_index <- 1
    }
  }

  if (length(procedures) < procedure_index) {
    stop("procedure_index is out of bounds for selected system$procedures")
  }

  return(list(system_index = system_index, procedure_index = procedure_index))
}


#' @title Build Upload Observations From Table
#'
#' @description
#' Converts a wide-format data frame - one row per feature, procedure item
#' names spread as column headers - into a list of \code{RObservationRecord}
#' payloads for \code{upload_observations()}, resolving item UUIDs from the
#' \code{procedure} list returned by \code{get_procedure()}.
#'
#' Every observation is validated as a whole: longitude/latitude must parse
#' as decimals, the timestamp must parse (via \code{lubridate::as_datetime()}
#' - any format it recognises), every referenced media file must exist on
#' disk, and every other value must match its declared data_type. A single
#' bad field rejects the whole observation, which is then reported in
#' \code{rejected} instead of being built/uploaded. This function is fully
#' offline - it does not connect to NatureCube.
#'
#' If the procedure has a \code{label} data_type item, its value is read
#' from fixed taxonomic rank columns - \code{species}, \code{genus},
#' \code{family}, \code{order}, \code{class}, \code{phylum}, \code{kingdom} -
#' not from a column matching the item's own name. The most specific
#' non-blank rank is used (an organism only identified to genus still gets
#' uploaded, using that genus). This function does not check that value
#' against the label database - only \link{validate_csv_against_procedure}
#' does, since that's the only step here that needs a NatureCube connection.
#'
#' @param data Data frame of observation rows - one row per feature, with
#'   procedure item names as column headers.
#' @param procedure Named list returned by \code{get_procedure()}.
#' @param lon_col Character name of longitude column. Default \code{"longitude"}.
#' @param lat_col Character name of latitude column. Default \code{"latitude"}.
#' @param timestamp_col Character name of the timestamp column. Default
#'   \code{"timestamp"}.
#'
#' @return A list with \code{observations} (only those that passed every
#'   check), \code{source_ids} (the original CSV row number, parallel to
#'   \code{observations} - use this to trace an uploaded observation back to
#'   its source row), \code{unresolved_rows},
#'   \code{resolved_rows}, \code{media_uploads} (local files pending
#'   signed-URL upload for surviving observations), and \code{rejected}
#'   (observations dropped for any failed check, each with an identifier and
#'   the reasons).
#'
#' @examples
#' \dontrun{
#'   hdr       <- auth_headers("your_api_key")
#'   schema    <- get_project_systems(hdr)
#'   procedure <- get_procedure(schema,
#'     system_name = "Plante Ivindo", procedure_name = "Arbre")
#'   df <- readr::read_csv("tutorials/data/example_observation_data_wide.csv")
#'   built <- build_upload_observations_from_table(
#'     data      = df,
#'     procedure = procedure
#'   )
#' }
#'
#' @author Adam Varley
#' @export
build_upload_observations_from_table <- function(data,
                                                 procedure,
                                                 lon_col = "longitude",
                                                 lat_col = "latitude",
                                                 timestamp_col = "timestamp") {

  required_cols <- c(lon_col, lat_col, timestamp_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in data: ", paste(missing_cols, collapse = ", "))
  }

  if (!is.list(procedure) || is.null(procedure$system_id)) {
    stop("procedure must be a named list returned by get_procedure()")
  }

  .build_wide_observations(
    data       = data,
    dictionary = procedure$items[, c("item_uuid", "item_name", "data_type"), drop = FALSE],
    sys_id     = as.integer(procedure$system_id),
    proc_id    = as.integer(procedure$procedure_id),
    lon_col = lon_col, lat_col = lat_col, timestamp_col = timestamp_col
  )
}


# Internal: build RObservationRecord payloads from a wide-format table
# (one row per feature, procedure item names as column headers).
.build_wide_observations <- function(data, dictionary, sys_id, proc_id,
                                     lon_col, lat_col, timestamp_col) {

  meta_cols <- c(lon_col, lat_col, timestamp_col, intersect(.taxonomy_ranks, names(data)))
  item_cols <- setdiff(names(data), meta_cols)

  name_lookup <- stats::setNames(
    dictionary$item_uuid,
    normalize_lookup_value(dictionary$item_name)
  )
  dt_lookup <- stats::setNames(
    dictionary$data_type,
    normalize_lookup_value(dictionary$item_name)
  )

  label_uuid <- dictionary$item_uuid[dictionary$data_type == "label"]
  label_uuid <- if (length(label_uuid) > 0) label_uuid[[1]] else NA_character_

  col_uuid        <- unname(name_lookup[normalize_lookup_value(item_cols)])
  col_dt          <- unname(dt_lookup[normalize_lookup_value(item_cols)])
  resolved_cols   <- item_cols[!is.na(col_uuid)]
  resolved_uuids  <- col_uuid[!is.na(col_uuid)]
  resolved_dt     <- col_dt[!is.na(col_uuid)]
  unresolved_cols <- item_cols[is.na(col_uuid)]

  if (length(resolved_cols) == 0 && is.na(label_uuid)) {
    stop("No columns could be matched to procedure items. Check column names against item_name in the procedure.")
  }

  # The API's "recorded_at" field requires strict ISO-8601 UTC
  # (yyyy-mm-ddThh:mm:ssZ) - a bare POSIXct serialises to JSON in a format
  # the API rejects, so it's reformatted to that exact string here.
  parsed_timestamp <- suppressWarnings(lubridate::as_datetime(data[[timestamp_col]], tz = "UTC"))
  iso_timestamp <- ifelse(is.na(parsed_timestamp), NA_character_,
                          strftime(parsed_timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  lon_vals <- suppressWarnings(as.numeric(data[[lon_col]]))
  lat_vals <- suppressWarnings(as.numeric(data[[lat_col]]))

  # Phase 1: assess every row independently (no network yet).
  rows <- lapply(seq_len(nrow(data)), function(r) {
    reasons    <- character()
    row_values <- list()
    row_media  <- list()

    if (is.na(lon_vals[r]))      reasons <- c(reasons, paste0(lon_col, " '", data[[lon_col]][[r]], "' is not a valid decimal"))
    if (is.na(lat_vals[r]))      reasons <- c(reasons, paste0(lat_col, " '", data[[lat_col]][[r]], "' is not a valid decimal"))
    if (is.na(iso_timestamp[r])) reasons <- c(reasons, paste0(timestamp_col, " '", data[[timestamp_col]][[r]], "' could not be parsed"))

    for (i in seq_along(resolved_cols)) {
      val_chr <- trimws(as.character(data[[resolved_cols[i]]][[r]]))
      if (is.na(val_chr) || !nzchar(val_chr) || identical(val_chr, "NA")) next

      if (resolved_dt[i] %in% .MEDIA_DATA_TYPES) {
        if (file.exists(val_chr)) {
          row_media[[resolved_uuids[i]]] <- list(
            filepath     = val_chr,
            data_type    = resolved_dt[i],
            content_type = .media_content_type(resolved_dt[i])
          )
        } else {
          reasons <- c(reasons, paste0(resolved_cols[i], ": file not found: ", val_chr))
        }
        next
      }

      if (!.value_matches_data_type(val_chr, resolved_dt[i])) {
        reasons <- c(reasons, paste0(resolved_cols[i], ": '", val_chr, "' does not match data_type '", resolved_dt[i], "'"))
        next
      }

      row_values[[resolved_uuids[i]]] <- coerce_value_by_data_type(val_chr, resolved_dt[i])
    }

    species <- if (!is.na(label_uuid)) .coalesce_taxonomic_label(data, r) else NA_character_
    if (!is.na(species)) row_values[[label_uuid]] <- species

    list(
      row = r, reasons = reasons, values = row_values, media = row_media,
      empty = length(row_values) == 0 && length(row_media) == 0 && length(reasons) == 0,
      timestamp = iso_timestamp[r], lon = lon_vals[r], lat = lat_vals[r]
    )
  })

  rows <- Filter(function(x) !x$empty, rows)

  if (length(rows) == 0) {
    stop("No rows could be converted into observations. Check item column mapping and values.")
  }

  # Phase 2: only rows with zero reasons become observations; every other
  # field failure is checked, so one bad field rejects the whole row. Label
  # values are checked against the database separately, by
  # validate_csv_against_procedure() - not here, since that's the only step
  # that needs a NatureCube connection.
  observations  <- list()
  source_ids    <- character()  # original CSV row number, parallel to observations
  media_uploads <- list()
  rejected      <- list()

  for (x in rows) {
    if (length(x$reasons) > 0) {
      rejected[[length(rejected) + 1]] <- list(row = x$row, reasons = x$reasons)
      next
    }
    obs_index <- length(observations) + 1
    observations[[obs_index]] <- list(
      survey_uuid       = uuid::UUIDgenerate(),
      project_system_id = sys_id,
      procedure_id      = proc_id,
      recorded_at       = x$timestamp,
      lon               = x$lon,
      lat               = x$lat,
      values            = x$values
    )
    source_ids[obs_index] <- as.character(x$row)
    for (item_uuid in names(x$media)) {
      media_uploads[[length(media_uploads) + 1]] <- c(list(obs_index = obs_index, item_uuid = item_uuid), x$media[[item_uuid]])
    }
  }

  unresolved_rows <- tibble::tibble(
    item_name = unresolved_cols,
    item_uuid = rep(NA_character_, length(unresolved_cols))
  )

  list(
    observations    = unname(observations),  # must be unnamed so JSON serialises as array not object
    source_ids      = source_ids,            # original CSV row number, parallel to observations
    resolved_rows   = length(observations),
    unresolved_rows = unresolved_rows,
    media_uploads   = media_uploads,
    rejected        = rejected
  )
}


# Internal: uploads each local file referenced in `media_uploads` via the
# signed-URL flow, and returns a named character vector mapping basename ->
# blob_path.
.upload_pending_media <- function(hdr, media_uploads) {
  filenames <- vapply(media_uploads, function(m) basename(m$filepath), character(1))

  media_files <- stats::setNames(
    lapply(media_uploads, function(m) {
      list(filepath = m$filepath, data_type = m$data_type, content_type = m$content_type)
    }),
    filenames
  )

  signed <- upload_field_media_files(hdr, media_files)
  stats::setNames(
    vapply(signed, function(s) s$blob_path, character(1)),
    vapply(signed, function(s) s$filename, character(1))
  )
}


#' @title Upload Observations From CSV
#'
#' @description
#' Uploads the result of \link{validate_csv_against_procedure} to
#' \code{uploadObservations} - mirroring how \link{upload_edna_records} takes
#' the output of \link{check_edna_labels}. Only rows with \code{status ==
#' "success"} are sent; nothing is rebuilt and no label is checked against
#' the database again, since \code{validated} already did that.
#'
#' If any of those rows reference a local media file (\code{phone-photo}/
#' \code{phone-video}/\code{phone-audio}), it's uploaded via a signed URL to
#' cloud storage first, and the resulting \code{blob_path} - not the local
#' path - is what gets stored on the observation.
#'
#' @param hdr A base URL and API key returned by \link{auth_headers} or
#'   \link{auth_headers_dev}. Used only for the actual upload calls (media
#'   signed URLs and \code{uploadObservations} itself).
#' @param validated The tibble returned by
#'   \link{validate_csv_against_procedure}.
#' @param dry_run Logical; if \code{TRUE}, returns the built observations
#'   without uploading anything (media files are not uploaded either).
#'   Default \code{FALSE}.
#'
#' @return A list with \code{uploaded}, \code{response} (per-observation API
#'   results), \code{n_success}, \code{n_failed}, \code{observations} (the
#'   payloads sent), and \code{result}: \code{validated}'s successful rows
#'   with \code{api_status}/\code{api_message} columns added (or
#'   \code{"pending"} on a \code{dry_run}) - the simplest way to see what
#'   happened to each row. The API validates each observation independently
#'   in a single request, so one rejected-by-the-API observation does not
#'   prevent the others from being saved; a final success/failure summary is
#'   printed once the upload completes.
#'
#' @examples
#' \dontrun{
#'   hdr       <- auth_headers("your_api_key")
#'   schema    <- get_project_systems(hdr)
#'   procedure <- get_procedure(schema,
#'     system_name = "Plante Ivindo", procedure_name = "Arbre")
#'
#'   observation_data <- readr::read_csv("tutorials/data/example_observation_data_wide.csv")
#'   validated <- validate_csv_against_procedure(
#'     procedure        = procedure,
#'     observation_data = observation_data,
#'     hdr              = hdr
#'   )
#'
#'   result <- upload_observations_from_csv(hdr = hdr, validated = validated)
#' }
#'
#' @author Adam Varley
#' @export
upload_observations_from_csv <- function(hdr, validated, dry_run = FALSE) {
  if (!is.data.frame(validated) || !"status" %in% names(validated)) {
    stop("validated must be the tibble returned by validate_csv_against_procedure().")
  }

  successful <- validated[validated$status == "success", , drop = FALSE]

  if (nrow(successful) == 0) {
    stop("No successful rows to upload. All rows failed validation.")
  }

  observations <- successful$.payload

  media_uploads <- list()
  for (i in seq_len(nrow(successful))) {
    for (m in successful$.media[[i]]) {
      media_uploads[[length(media_uploads) + 1]] <- c(list(obs_index = i), m)
    }
  }

  keep_cols <- setdiff(names(successful), c(".payload", ".media"))

  if (isTRUE(dry_run)) {
    successful$api_status  <- "pending"
    successful$api_message <- ""
    return(list(
      uploaded      = FALSE,
      observations  = observations,
      resolved_rows = length(observations),
      result        = successful[, c(keep_cols, "api_status", "api_message"), drop = FALSE]
    ))
  }

  if (length(media_uploads) > 0) {
    n_media <- length(media_uploads)
    cli::cli_progress_bar(
      "Uploading {cli::pb_current}/{cli::pb_total} media files to cloud storage | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
      total = n_media,
      .auto_close = FALSE
    )
    # on.exit (not tryCatch) so the spinner always closes - including on a
    # user interrupt (e.g. Escape/Ctrl+C), which tryCatch's `error` handler
    # does not catch and would otherwise leave it animating indefinitely.
    media_step_ok <- FALSE
    on.exit(cli::cli_progress_done(result = if (media_step_ok) "done" else "failed"), add = TRUE)

    blob_by_filename <- .upload_pending_media(hdr, media_uploads)
    media_step_ok <- TRUE

    for (m in media_uploads) {
      observations[[m$obs_index]]$values[[m$item_uuid]] <- unname(blob_by_filename[basename(m$filepath)])
    }
  }

  # upload_observations() shows its own progress bar - it's the one doing the
  # (possibly chunked, >500 observations) network calls.
  response <- upload_observations(hdr = hdr, observations = observations)

  n_success <- sum(vapply(response, function(x) identical(x$status, "success"), logical(1)))
  n_failed  <- length(response) - n_success

  if (n_failed > 0) {
    cli::cli_alert_danger("Upload complete: {n_success} of {length(response)} observation{?s} succeeded, {n_failed} failed.")
    for (e in Filter(function(x) identical(x$status, "error"), response)) {
      cli::cli_alert_warning("  [{e$survey_uuid %||% 'unknown'}] {e$message %||% 'no message'}")
    }
  } else {
    cli::cli_alert_success("Upload complete: all {n_success} observations succeeded.")
  }

  # response is parallel to `observations`/`successful` (one row = one
  # observation), so results map back positionally.
  successful$api_status  <- vapply(response, function(x) as.character(x$status %||% "unknown"), character(1))
  successful$api_message <- vapply(response, function(x) as.character(x$message %||% ""), character(1))

  list(
    uploaded      = TRUE,
    response      = response,
    n_success     = n_success,
    n_failed      = n_failed,
    observations  = observations,
    resolved_rows = length(observations),
    result        = successful[, c(keep_cols, "api_status", "api_message"), drop = FALSE]
  )
}
