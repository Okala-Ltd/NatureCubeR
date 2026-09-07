.PHONE_OBS_DATA_TYPES <- c(
  "phone-photo",
  "phone-video",
  "phone-audio",
  "choice",
  "text",
  "numeric",
  "label"
)

.PHONE_OBS_MEDIA_TYPES <- c("phone-photo", "phone-video", "phone-audio")

# Perform getPhoneObservations. A 404 with "No observations found..." is an
# empty result, not a hard failure — return empty = TRUE and a clear message
# instead of dumping the raw URL (which includes the API key).
.perform_phone_obs_request <- function(req, data_type, project_id, procedure_id) {
  resp   <- httr2::req_perform(req %>% httr2::req_error(is_error = \(r) FALSE))
  status <- httr2::resp_status(resp)
  if (status < 400) {
    return(list(empty = FALSE, resp = resp))
  }

  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  detail <- tryCatch({
    parsed <- jsonlite::fromJSON(body)
    as.character(parsed$detail %||% "")
  }, error = function(e) "")

  no_data <- status == 404L && grepl(
    "no observations found",
    detail,
    ignore.case = TRUE
  )
  if (no_data) {
    return(list(
      empty = TRUE,
      detail = if (nzchar(detail)) {
        detail
      } else {
        "No observations found for the selected criteria"
      },
      data_type = data_type,
      project_id = project_id,
      procedure_id = procedure_id
    ))
  }

  stop(
    sprintf(
      "HTTP %d downloading phone observations (data_type=%s, project_id=%s, procedure_id=%s)\n%s",
      status,
      data_type,
      project_id,
      procedure_id,
      if (nzchar(body)) body else "<no response body>"
    ),
    call. = FALSE
  )
}

.message_phone_obs_empty <- function(info) {
  message(
    "No phone observations for data_type='", info$data_type,
    "' (project_id=", info$project_id,
    ", procedure_id=", info$procedure_id, "). ",
    info$detail, "."
  )
}

.sanitize_dir_token <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "-", as.character(x))
  x <- gsub("^-+|-+$", "", x)
  if (!nzchar(x)) "export" else x
}

.unique_export_dir <- function(dest_dir, procedure_id, data_type) {
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  base <- file.path(
    dest_dir,
    paste(
      "phone-obs",
      .sanitize_dir_token(procedure_id),
      .sanitize_dir_token(data_type),
      stamp,
      sep = "-"
    )
  )
  if (!dir.exists(base)) {
    return(base)
  }
  paste0(base, "-", as.integer(Sys.time()))
}

.phone_obs_ids <- function(procedure) {
  if (!is.list(procedure) || is.null(procedure$procedure_id)) {
    stop("`procedure` must be the named list returned by get_procedure().")
  }
  procedure_id <- as.integer(procedure$procedure_id)
  project_id <- as.integer(procedure$project_id %||% NA_integer_)
  if (length(procedure_id) != 1L || is.na(procedure_id)) {
    stop("`procedure$procedure_id` is missing.")
  }
  if (length(project_id) != 1L || is.na(project_id)) {
    stop(
      "`procedure$project_id` is missing. Call get_procedure() on a schema ",
      "from get_project_systems() so project_id is included."
    )
  }
  list(project_id = project_id, procedure_id = procedure_id)
}

.phone_obs_token_from_url <- function(download_url) {
  path <- httr2::url_parse(download_url)$path %||% ""
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  idx <- match("downloadPhoneObservationExport", parts)
  if (!is.na(idx) && length(parts) >= idx + 3L) {
    return(parts[[length(parts)]])
  }
  NULL
}

.PHONE_OBS_LABEL_COLS <- c(
  "class",
  "order",
  "family",
  "genus",
  "species",
  "common_name",
  "label",
  "number_of_individuals",
  "prediction_accuracy"
)

.PHONE_OBS_DROP_COLS <- c(
  "project_id",
  "project_system_id",
  "procedure_id",
  "phone_model",
  "phone_operating_system",
  "item_uuid",
  "observation_id"
)

.has_phone_obs_label_data <- function(labels) {
  if (is.null(labels) || length(labels) == 0L) {
    return(FALSE)
  }
  any(vapply(labels, function(x) {
    if (is.null(x) || (length(x) == 1L && is.na(x))) {
      return(FALSE)
    }
    nzchar(as.character(x))
  }, logical(1)))
}

# Expand the labels JSON array into flat taxonomy / prediction columns.
# Uses the first label object when several are present.
.expand_phone_obs_labels <- function(labels) {
  n <- length(labels)
  out <- tibble::tibble(
    class = rep(NA_character_, n),
    order = rep(NA_character_, n),
    family = rep(NA_character_, n),
    genus = rep(NA_character_, n),
    species = rep(NA_character_, n),
    common_name = rep(NA_character_, n),
    label = rep(NA_character_, n),
    number_of_individuals = rep(NA_real_, n),
    prediction_accuracy = rep(NA_real_, n)
  )
  if (n == 0L) {
    return(out)
  }

  for (i in seq_len(n)) {
    raw <- labels[[i]]
    if (is.null(raw) || (length(raw) == 1L && is.na(raw))) {
      next
    }
    raw <- as.character(raw)
    if (!nzchar(raw)) {
      next
    }
    parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyDataFrame = TRUE), error = function(e) NULL)
    if (is.null(parsed)) {
      next
    }
    if (is.data.frame(parsed)) {
      if (nrow(parsed) == 0L) {
        next
      }
      row <- parsed[1, , drop = FALSE]
    } else if (is.list(parsed)) {
      row <- parsed
    } else {
      next
    }

    pick_chr <- function(x) {
      if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) {
        return(NA_character_)
      }
      as.character(x[[1]])
    }
    pick_num <- function(x) {
      if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) {
        return(NA_real_)
      }
      suppressWarnings(as.numeric(x[[1]]))
    }

    out$class[[i]] <- pick_chr(row[["class_"]] %||% row[["class"]])
    out$order[[i]] <- pick_chr(row[["order"]])
    out$family[[i]] <- pick_chr(row[["family"]])
    out$genus[[i]] <- pick_chr(row[["genus"]])
    out$species[[i]] <- pick_chr(row[["species"]])
    out$common_name[[i]] <- pick_chr(row[["common_name"]])
    out$label[[i]] <- pick_chr(row[["label"]])
    out$number_of_individuals[[i]] <- pick_num(
      row[["number_of_individuals"]] %||% row[["n_individuals"]]
    )
    out$prediction_accuracy[[i]] <- pick_num(
      row[["prediction_accuracy"]] %||% row[["predictions_accuracy"]]
    )
  }
  out
}

# Drop noise columns, expand labels when present, rename client-facing fields,
# keep a single recording timestamp, and put surveyor / *_uuid columns last.
# Coordinate columns are left as returned by the API.
.tidy_phone_observations <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0L) {
    return(tibble::as_tibble(df))
  }
  df <- tibble::as_tibble(df)

  drop <- intersect(.PHONE_OBS_DROP_COLS, names(df))
  if (length(drop)) {
    df <- df[, setdiff(names(df), drop), drop = FALSE]
  }

  label_src <- if ("labels" %in% names(df)) df$labels else NULL
  add_tax <- .has_phone_obs_label_data(label_src)
  if ("labels" %in% names(df)) {
    df$labels <- NULL
  }
  for (nm in c(.PHONE_OBS_LABEL_COLS, "class_")) {
    if (nm %in% names(df)) {
      df[[nm]] <- NULL
    }
  }
  if (add_tax) {
    df <- dplyr::bind_cols(df, .expand_phone_obs_labels(label_src))
  }

  rename_map <- c(
    procedure_name = "survey_name",
    recorded_at = "observation_recording_timestamp",
    data = "survey_observation",
    observation = "survey_observation",
    username = "surveyor_name"
  )
  for (from in names(rename_map)) {
    if (!(from %in% names(df))) {
      next
    }
    target <- rename_map[[from]]
    if (identical(from, target)) {
      next
    }
    if (target %in% names(df)) {
      df[[from]] <- NULL
    } else {
      names(df)[names(df) == from] <- target
    }
  }

  ts_drop <- names(df)[
    (grepl("timestamp", names(df), ignore.case = TRUE) |
      grepl("_at$", names(df), ignore.case = TRUE)) &
      names(df) != "observation_recording_timestamp"
  ]
  if (length(ts_drop)) {
    df <- df[, setdiff(names(df), ts_drop), drop = FALSE]
  }

  nm <- names(df)
  lead_cols <- intersect(c("system_name", "survey_name"), nm)
  ts_cols <- intersect("observation_recording_timestamp", nm)
  uuid_cols <- nm[grepl("_uuid$", nm, ignore.case = TRUE)]
  tax_cols <- if (add_tax) intersect(.PHONE_OBS_LABEL_COLS, nm) else character()
  surveyor_cols <- intersect("surveyor_name", nm)
  special <- unique(c(
    lead_cols,
    ts_cols,
    "survey_observation",
    tax_cols,
    uuid_cols,
    surveyor_cols
  ))
  other_cols <- setdiff(nm, special)

  ordered <- unique(c(
    lead_cols,
    other_cols,
    ts_cols,
    intersect("survey_observation", nm),
    tax_cols,
    uuid_cols,
    surveyor_cols,
    nm
  ))
  df[, ordered, drop = FALSE]
}

.parse_phone_obs_csv <- function(text) {
  if (!nzchar(text)) {
    return(tibble::tibble())
  }
  readr::read_csv(
    I(text),
    show_col_types = FALSE,
    progress = FALSE
  ) %>%
    tibble::as_tibble() %>%
    .tidy_phone_observations()
}

#' @title Resolve a phone-observation export token
#'
#' @description
#' Calls \code{GET /downloadPhoneObservationExport/{api_key}/{project_id}/{token}}
#' and follows the 307 redirect to a short-lived GCS signed URL. The path-based
#' token avoids query-string \code{&} corruption that breaks raw GCS V4 URLs.
#'
#' @param hdr Auth headers from \link{auth_headers} or \link{auth_headers_dev}.
#' @param project_id Integer project ID.
#' @param token Opaque export token from \code{getPhoneObservations}.
#' @param path Optional local file path. When supplied, the ZIP is written
#'   there instead of being returned as an in-memory response.
#'
#' @return An httr2 response (or the on-disk path when \code{path} is set).
#'
#' @author Cristobal Salamé
#' @export
download_phone_observation_export <- function(hdr, project_id, token, path = NULL) {
  req <- httr2::req_url_path_append(
    hdr$root,
    "downloadPhoneObservationExport",
    hdr$key,
    as.integer(project_id),
    as.character(token)
  ) %>%
    httr2::req_timeout(MEDIA_PAGE_TIMEOUT) %>%
    httr2::req_retry(
      max_tries = MEDIA_MAX_RETRIES,
      is_transient = \(resp) {
        httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
      }
    )

  if (!is.null(path)) {
    httr2::req_perform(req, path = path)
    return(invisible(path))
  }
  .perform_or_stop(req)
}

.fetch_phone_obs_csv <- function(hdr, project_id, procedure_id, data_type) {
  req <- httr2::req_url_path_append(
    hdr$root,
    "getPhoneObservations",
    hdr$key,
    as.integer(project_id),
    as.integer(procedure_id),
    data_type
  ) %>%
    httr2::req_timeout(MEDIA_PAGE_TIMEOUT) %>%
    httr2::req_retry(
      max_tries = MEDIA_MAX_RETRIES,
      is_transient = \(resp) {
        httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
      }
    )
  result <- .perform_phone_obs_request(req, data_type, project_id, procedure_id)
  if (isTRUE(result$empty)) {
    .message_phone_obs_empty(result)
    return(tibble::tibble())
  }
  .parse_phone_obs_csv(httr2::resp_body_string(result$resp))
}

.fetch_phone_obs_media <- function(hdr, project_id, procedure_id, data_type, dest_dir) {
  req <- httr2::req_url_path_append(
    hdr$root,
    "getPhoneObservations",
    hdr$key,
    as.integer(project_id),
    as.integer(procedure_id),
    data_type
  ) %>%
    httr2::req_timeout(MEDIA_PAGE_TIMEOUT) %>%
    httr2::req_retry(
      max_tries = MEDIA_MAX_RETRIES,
      is_transient = \(resp) {
        httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
      }
    )

  result <- .perform_phone_obs_request(req, data_type, project_id, procedure_id)
  if (isTRUE(result$empty)) {
    .message_phone_obs_empty(result)
    return(list(
      data_type    = data_type,
      observations = tibble::tibble(),
      dest_dir     = NULL,
      zip_path     = NULL,
      files        = character(),
      row_count    = 0L,
      media_count  = 0L,
      expires_at   = NULL,
      size_bytes   = 0L,
      filename     = NULL
    ))
  }

  meta <- httr2::resp_body_json(result$resp)
  download_url <- as.character(meta$download_url %||% "")
  if (!nzchar(download_url)) {
    stop("getPhoneObservations did not return a download_url for ", data_type, ".")
  }

  out_dir <- .unique_export_dir(dest_dir, procedure_id, data_type)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  zip_name <- meta$filename %||% paste0(data_type, ".zip")
  zip_path <- file.path(out_dir, zip_name)

  token <- .phone_obs_token_from_url(download_url)
  if (!is.null(token)) {
    download_phone_observation_export(
      hdr = hdr,
      project_id = project_id,
      token = token,
      path = zip_path
    )
  } else {
    httr2::request(download_url) %>%
      httr2::req_timeout(MEDIA_PAGE_TIMEOUT) %>%
      httr2::req_perform(path = zip_path)
  }

  utils::unzip(zip_path, exdir = out_dir)
  files <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
  csv_path <- files[grepl("observations\\.csv$", files, ignore.case = TRUE)]
  observations <- if (length(csv_path)) {
    readr::read_csv(csv_path[[1]], show_col_types = FALSE, progress = FALSE) %>%
      tibble::as_tibble() %>%
      .tidy_phone_observations()
  } else {
    tibble::tibble()
  }
  if (length(csv_path)) {
    readr::write_csv(observations, csv_path[[1]])
  }

  list(
    data_type     = data_type,
    observations  = observations,
    dest_dir      = out_dir,
    zip_path      = zip_path,
    files         = files,
    row_count     = meta$row_count,
    media_count   = meta$media_count,
    expires_at    = meta$expires_at,
    size_bytes    = meta$size_bytes,
    filename      = meta$filename
  )
}

.fetch_one_phone_obs_type <- function(hdr, project_id, procedure_id, data_type, dest_dir) {
  if (data_type %in% .PHONE_OBS_MEDIA_TYPES) {
    .fetch_phone_obs_media(hdr, project_id, procedure_id, data_type, dest_dir)
  } else {
    list(
      data_type    = data_type,
      observations = .fetch_phone_obs_csv(hdr, project_id, procedure_id, data_type)
    )
  }
}

#' @title Download phone observations for a procedure
#'
#' @description
#' Downloads observations for a procedure from
#' \code{GET /getPhoneObservations/{api_key}/{project_id}/{procedure_id}/{data_type}}.
#'
#' Non-media types (\code{choice}, \code{text}, \code{numeric}, \code{label})
#' stream a CSV that is returned as a tibble. Media types
#' (\code{phone-photo}, \code{phone-video}, \code{phone-audio}) return a JSON
#' export descriptor; this function then calls
#' \link{download_phone_observation_export}, follows the redirect to GCS,
#' extracts the ZIP into a uniquely named directory under \code{dest_dir},
#' and reads \code{observations.csv}.
#'
#' Returned observations (and the rewritten on-disk \code{observations.csv})
#' drop identifiers/device noise (\code{project_id}, \code{project_system_id},
#' \code{procedure_id}, \code{observation_id}, \code{item_uuid},
#' \code{phone_model}, \code{phone_operating_system}); rename
#' \code{system_name}/\code{procedure_name} lead columns
#' (\code{procedure_name} becomes \code{survey_name}),
#' \code{recorded_at} to \code{observation_recording_timestamp} (other
#' timestamp columns dropped), \code{data} to \code{survey_observation}, and
#' \code{username} to \code{surveyor_name} (last column). Coordinate columns
#' are left unchanged. When any \code{labels} JSON is present it is expanded
#' into taxonomy/prediction columns (\code{class}, \code{order}, ...) after
#' \code{survey_observation}; otherwise those columns are omitted. All
#' \code{*_uuid} columns are grouped before \code{surveyor_name}.
#'
#' \code{data_type} may be omitted (download every type), one type, or several.
#' Multiple types are fetched sequentially (the media ZIP build is server-side
#' and slow; parallel requests tend to 429). Pass several types in one call
#' when you want a single workflow; pass one type at a time to inspect timing.
#'
#' @param hdr Auth headers from \link{auth_headers} or \link{auth_headers_dev}.
#'   SIT must be used until this endpoint is on production
#'   (\link{auth_headers_dev}).
#' @param procedure Named list returned by \link{get_procedure}. Must include
#'   \code{project_id} and \code{procedure_id}.
#' @param data_type Optional character. One or more of \code{phone-photo},
#'   \code{phone-video}, \code{phone-audio}, \code{choice}, \code{text},
#'   \code{numeric}, \code{label}. When \code{NULL} (default), every type is
#'   downloaded.
#' @param dest_dir Optional directory for extracted media ZIP contents. When
#'   \code{NULL} (default), uses the current working directory. Existing names
#'   are never overwritten; a timestamped directory is created instead.
#'
#' @return If a single \code{data_type} is requested, a tibble (non-media)
#'   or a named list with \code{observations}, \code{dest_dir}, \code{files},
#'   and export metadata (media). If several types are requested (including
#'   the all-types default), a named list of those results.
#'
#' @examples
#' \dontrun{
#'   hdr <- auth_headers_dev("your_api_key")
#'   schema <- get_project_systems(hdr)
#'   procedure <- get_procedure(schema,
#'     system_name = "Plante Ivindo",
#'     procedure_name = "Arbre")
#'
#'   all_obs <- get_phone_observations(hdr, procedure)
#'   text_obs <- get_phone_observations(hdr, procedure, data_type = "text")
#'   photos <- get_phone_observations(hdr, procedure, data_type = "phone-photo")
#'   mixed <- get_phone_observations(
#'     hdr, procedure,
#'     data_type = c("text", "phone-photo"),
#'     dest_dir = tempdir()
#'   )
#' }
#'
#' @author Cristobal Salamé
#' @export
get_phone_observations <- function(hdr,
                                   procedure,
                                   data_type = NULL,
                                   dest_dir = NULL) {
  ids <- .phone_obs_ids(procedure)
  if (is.null(dest_dir)) {
    dest_dir <- getwd()
  }
  if (is.null(data_type)) {
    data_type <- .PHONE_OBS_DATA_TYPES
  } else {
    data_type <- unique(as.character(data_type))
  }
  unknown <- setdiff(data_type, .PHONE_OBS_DATA_TYPES)
  if (length(unknown) > 0L) {
    stop(
      "Please use one of the following data_types: ", paste(.PHONE_OBS_DATA_TYPES, collapse = ", "), "."
    )
  }

  pb <- cli::cli_progress_bar(
    format = "Downloading phone observations {cli::pb_current}/{cli::pb_total} type(s) | {cli::pb_bar} {cli::pb_percent} | elapsed: {cli::pb_elapsed} | ETA: {cli::pb_eta}",
    total  = length(data_type),
    clear  = FALSE
  )
  on.exit(cli::cli_progress_done(id = pb), add = TRUE)

  results <- vector("list", length(data_type))
  names(results) <- data_type
  for (i in seq_along(data_type)) {
    results[[i]] <- .fetch_one_phone_obs_type(
      hdr = hdr,
      project_id = ids$project_id,
      procedure_id = ids$procedure_id,
      data_type = data_type[[i]],
      dest_dir = dest_dir
    )
    cli::cli_progress_update(id = pb, inc = 1)
  }

  if (length(results) == 1L) {
    one <- results[[1]]
    if (identical(one$data_type, data_type) && !("dest_dir" %in% names(one))) {
      return(one$observations)
    }
    return(one)
  }
  results
}
