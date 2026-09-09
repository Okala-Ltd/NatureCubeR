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
  x[!nzchar(x) | is.na(x)] <- "export"
  x
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
  "observation_id",
  "observation_uuid"
)

.PHONE_OBS_COORD_COLS <- c("longitude", "latitude")

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

# Parse the feature-level WKT geometry. Raw longitude/latitude fields describe
# individual observations and are intentionally replaced by these coordinates.
.phone_obs_feature_coordinates <- function(feature_geometry) {
  n <- length(feature_geometry)
  coordinates <- tibble::tibble(
    longitude = rep(NA_real_, n),
    latitude = rep(NA_real_, n)
  )
  if (n == 0L) {
    return(coordinates)
  }

  geometry <- as.character(feature_geometry)
  pattern <- paste0(
    "(?i)^\\s*POINT\\s*\\(\\s*",
    "([+-]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?)",
    "\\s+",
    "([+-]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?)",
    "\\s*\\)\\s*$"
  )
  for (i in seq_len(n)) {
    if (is.na(geometry[[i]]) || !nzchar(geometry[[i]])) {
      next
    }
    matched <- regmatches(
      geometry[[i]],
      regexec(pattern, geometry[[i]], perl = TRUE)
    )[[1]]
    if (length(matched) == 3L) {
      coordinates$longitude[[i]] <- as.numeric(matched[[2]])
      coordinates$latitude[[i]] <- as.numeric(matched[[3]])
    }
  }
  coordinates
}

.phone_obs_feature_ids <- function(feature_uuid) {
  feature_uuid <- as.character(feature_uuid)
  unique_uuid <- unique(feature_uuid[!is.na(feature_uuid) & nzchar(feature_uuid)])
  missing <- is.na(feature_uuid) | !nzchar(feature_uuid)
  feature_count <- length(unique_uuid) + sum(missing)
  width <- max(3L, nchar(as.character(feature_count)))
  ids <- stats::setNames(
    sprintf(paste0("f%0", width, "d"), seq_along(unique_uuid)),
    unique_uuid
  )
  result <- unname(ids[feature_uuid])
  if (any(missing)) {
    missing_ids <- seq.int(length(unique_uuid) + 1L, feature_count)
    result[missing] <- sprintf(paste0("f%0", width, "d"), missing_ids)
  }
  result
}

# Drop noise columns, expand labels when present, rename client-facing fields,
# derive feature coordinates/IDs, and put surveyor / observation UUIDs last.
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
  existing_tax <- intersect(c(.PHONE_OBS_LABEL_COLS, "class_"), names(df))
  add_tax <- .has_phone_obs_label_data(label_src) ||
    (length(existing_tax) > 0L && any(vapply(
      df[existing_tax],
      function(x) any(!is.na(x) & nzchar(as.character(x))),
      logical(1)
    )))
  if ("labels" %in% names(df)) {
    df$labels <- NULL
  }
  if (.has_phone_obs_label_data(label_src)) {
    for (nm in c(.PHONE_OBS_LABEL_COLS, "class_")) {
      if (nm %in% names(df)) {
        df[[nm]] <- NULL
      }
    }
    df <- dplyr::bind_cols(df, .expand_phone_obs_labels(label_src))
  } else if ("class_" %in% names(df)) {
    names(df)[names(df) == "class_"] <- "class"
  }

  rename_map <- c(
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

  # system_name -> survey_name; previous survey_name (old procedure label) ->
  # procedure_name. Raw exports already use procedure_name.
  if ("survey_name" %in% names(df) && !("procedure_name" %in% names(df))) {
    names(df)[names(df) == "survey_name"] <- "procedure_name"
  }
  if ("system_name" %in% names(df)) {
    if ("survey_name" %in% names(df)) {
      df$system_name <- NULL
    } else {
      names(df)[names(df) == "system_name"] <- "survey_name"
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

  if ("feature_geometry" %in% names(df)) {
    coordinates <- .phone_obs_feature_coordinates(df$feature_geometry)
    df$feature_geometry <- NULL
    for (nm in .PHONE_OBS_COORD_COLS) {
      if (nm %in% names(df)) {
        df[[nm]] <- NULL
      }
    }
    df <- dplyr::bind_cols(df, coordinates)
  }

  if ("feature_uuid" %in% names(df)) {
    df$feature_id <- .phone_obs_feature_ids(df$feature_uuid)
    df$feature_uuid <- NULL
  }

  if (all(c("data_type", "survey_observation") %in% names(df))) {
    media <- df$data_type %in% .PHONE_OBS_MEDIA_TYPES
    has_file <- media & !is.na(df$survey_observation) &
      nzchar(as.character(df$survey_observation))
    if (any(has_file)) {
      file_name <- basename(sub(
        "[?#].*$",
        "",
        as.character(df$survey_observation[has_file])
      ))
      survey <- if ("survey_name" %in% names(df)) {
        df$survey_name[has_file]
      } else {
        "survey"
      }
      procedure <- if ("procedure_name" %in% names(df)) {
        df$procedure_name[has_file]
      } else {
        "procedure"
      }
      feature <- if ("feature_id" %in% names(df)) {
        df$feature_id[has_file]
      } else {
        "f000"
      }
      df$survey_observation[has_file] <- .phone_obs_media_relpath(
        survey,
        procedure,
        feature,
        file_name
      )
    }
  }

  nm <- names(df)
  lead_cols <- intersect(c("feature_id", "survey_name", "procedure_name"), nm)
  ts_cols <- intersect("observation_recording_timestamp", nm)
  uuid_cols <- character()
  coord_cols <- intersect(.PHONE_OBS_COORD_COLS, nm)
  tax_cols <- if (add_tax) intersect(.PHONE_OBS_LABEL_COLS, nm) else character()
  surveyor_cols <- intersect("surveyor_name", nm)
  special <- unique(c(
    lead_cols,
    ts_cols,
    coord_cols,
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
    coord_cols,
    intersect("survey_observation", nm),
    tax_cols,
    uuid_cols,
    surveyor_cols,
    nm
  ))
  df[, ordered, drop = FALSE]
}

.phone_obs_wide_format <- function(long_data) {
  if (!is.data.frame(long_data) || nrow(long_data) == 0L ||
      !("feature_id" %in% names(long_data))) {
    return(tibble::as_tibble(long_data))
  }
  long_data <- tibble::as_tibble(long_data)
  feature_ids <- unique(long_data$feature_id)
  feature_ids <- feature_ids[!is.na(feature_ids)]
  base_cols <- intersect(
    c(
      "feature_id",
      "survey_name",
      "procedure_name",
      "observation_recording_timestamp",
      "longitude",
      "latitude"
    ),
    names(long_data)
  )

  observations <- lapply(feature_ids, function(feature_id) {
    feature <- long_data[long_data$feature_id == feature_id, , drop = FALSE]
    item <- if ("item_name" %in% names(feature)) {
      as.character(feature$item_name)
    } else {
      rep("observation", nrow(feature))
    }
    item[is.na(item) | !nzchar(item)] <- "Observation"

    value <- if ("survey_observation" %in% names(feature)) {
      as.character(feature$survey_observation)
    } else {
      rep(NA_character_, nrow(feature))
    }
    if ("label" %in% names(feature)) {
      use_label <- is.na(value) | !nzchar(value)
      value[use_label] <- as.character(feature$label[use_label])
    }

    item_names <- unique(item)
    values <- lapply(item_names, function(item_name) {
      item_values <- value[item == item_name]
      if (length(item_values) == 1L) {
        return(item_values[[1]])
      }
      jsonlite::toJSON(item_values, auto_unbox = FALSE, na = "null")
    })
    names(values) <- item_names
    values
  })

  first_rows <- match(feature_ids, long_data$feature_id)
  wide <- long_data[first_rows, base_cols, drop = FALSE]
  observation_cols <- unique(unlist(lapply(observations, names), use.names = FALSE))
  for (column in observation_cols) {
    wide[[column]] <- NA_character_
  }
  for (i in seq_along(observations)) {
    for (column in names(observations[[i]])) {
      wide[[column]][[i]] <- observations[[i]][[column]]
    }
  }
  if ("surveyor_name" %in% names(long_data)) {
    wide$surveyor_name <- long_data$surveyor_name[first_rows]
  }
  wide
}

.phone_obs_column_guide <- function(wide_data, long_data) {
  descriptions <- c(
    feature_id = "Unique identifier shared by observations from the same feature.",
    survey_name = "NatureCube survey kit name.",
    procedure_name = "NatureCube survey procedure name.",
    observation_recording_timestamp = "Date and time the feature observations were recorded.",
    longitude = "Feature longitude.",
    latitude = "Feature latitude.",
    item_name = "Procedure item associated with this observation.",
    data_type = "Observation data type, such as phone-photo, numeric, or label.",
    survey_observation = "Recorded value, or media path survey_name/procedure_name/feature_id/file_name.",
    observation_notes = "User-provided notes for the observation.",
    class = "Taxonomic class from procedure labels.",
    order = "Taxonomic order from procedure labels.",
    family = "Taxonomic family from procedure labels.",
    genus = "Taxonomic genus from procedure labels.",
    species = "Taxonomic species from procedure labels.",
    common_name = "Common name from procedure labels.",
    label = "Taxonomic label applied to the procedure item.",
    number_of_individuals = "Number of individuals associated with the procedure item.",
    prediction_accuracy = "Automated-label prediction score returned by NatureCube.",
    surveyor_name = "Name or username of the surveyor who recorded the observation."
  )
  guide_for <- function(data, sheet) {
    columns <- names(data)
    description <- unname(descriptions[columns])
    generated <- sheet == "Wide format" & is.na(description)
    description[generated] <- paste0(
      "Value for survey item '",
      columns[generated],
      "'. Repeated observations are stored as a JSON array."
    )
    description[is.na(description)] <- "Field returned by the NatureCube observation export."
    tibble::tibble(`sheet name` = sheet, column = columns, description = description)
  }
  dplyr::bind_rows(
    guide_for(wide_data, "Wide format"),
    guide_for(long_data, "Long format")
  )
}

.phone_obs_media_relpath <- function(survey, procedure, feature_id, file_name) {
  clean <- function(x, fallback) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(x)] <- fallback
    .sanitize_dir_token(x)
  }
  paste(
    clean(survey, "survey"),
    clean(procedure, "procedure"),
    clean(feature_id, "f000"),
    file_name,
    sep = "/"
  )
}

.relocate_phone_obs_media <- function(observations, extract_dir, dest_dir) {
  if (!is.data.frame(observations) || nrow(observations) == 0L) {
    return(invisible(NULL))
  }
  if (!all(c("data_type", "survey_observation") %in% names(observations))) {
    return(invisible(NULL))
  }

  extracted <- list.files(extract_dir, recursive = TRUE, full.names = TRUE)
  extracted <- extracted[file.info(extracted)$isdir %in% FALSE]
  extracted <- extracted[!grepl(
    "\\.(csv|xlsx|gpkg|zip)$",
    extracted,
    ignore.case = TRUE
  )]
  if (length(extracted) == 0L) {
    return(invisible(NULL))
  }
  by_name <- split(extracted, basename(extracted))

  media <- observations$data_type %in% .PHONE_OBS_MEDIA_TYPES
  paths <- as.character(observations$survey_observation)
  media <- media & !is.na(paths) & nzchar(paths)

  for (i in which(media)) {
    rel <- paths[[i]]
    file_name <- basename(rel)
    candidates <- by_name[[file_name]]
    if (is.null(candidates) || length(candidates) == 0L) {
      next
    }
    src <- candidates[[1]]
    by_name[[file_name]] <- candidates[-1]
    dest <- file.path(dest_dir, rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    same <- file.exists(dest) &&
      identical(
        normalizePath(src, winslash = "/", mustWork = FALSE),
        normalizePath(dest, winslash = "/", mustWork = FALSE)
      )
    if (!same) {
      ok <- file.rename(src, dest)
      if (!isTRUE(ok)) {
        file.copy(src, dest, overwrite = TRUE)
        unlink(src)
      }
    }
  }
  invisible(NULL)
}

.write_phone_obs_workbook <- function(long_data, path) {
  wide_data <- .phone_obs_wide_format(long_data)
  guide <- .phone_obs_column_guide(wide_data, long_data)
  writexl::write_xlsx(
    list(
      "Column description" = guide,
      "Wide format" = wide_data,
      "Long format" = long_data
    ),
    path = path
  )
  invisible(list(path = path, wide = wide_data, long = long_data))
}

.write_phone_obs_geopackage <- function(long_data, path) {
  if (!is.data.frame(long_data) || nrow(long_data) == 0L ||
      !all(c("feature_id", "longitude", "latitude") %in% names(long_data))) {
    return(invisible(NULL))
  }
  long_data <- tibble::as_tibble(long_data)
  first_rows <- match(unique(long_data$feature_id), long_data$feature_id)
  parent_cols <- intersect(
    c(
      "feature_id",
      "survey_name",
      "procedure_name",
      "observation_recording_timestamp",
      "longitude",
      "latitude",
      "surveyor_name"
    ),
    names(long_data)
  )
  parent_features <- long_data[first_rows, parent_cols, drop = FALSE]
  parent_features <- sf::st_as_sf(
    parent_features,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE,
    na.fail = FALSE
  )

  data <- long_data[
    ,
    setdiff(names(long_data), c("longitude", "latitude")),
    drop = FALSE
  ]
  if (file.exists(path)) {
    unlink(path)
  }
  sf::st_write(
    parent_features,
    dsn = path,
    layer = "parent_features",
    quiet = TRUE
  )
  sf::st_write(
    data,
    dsn = path,
    layer = "data",
    append = FALSE,
    layer_options = "ASPATIAL_VARIANT=GPKG_ATTRIBUTES",
    quiet = TRUE
  )
  invisible(path)
}

.parse_phone_obs_csv_raw <- function(text) {
  if (!nzchar(text)) {
    return(tibble::tibble())
  }
  readr::read_csv(
    I(text),
    show_col_types = FALSE,
    progress = FALSE
  ) %>%
    tibble::as_tibble()
}

.parse_phone_obs_csv <- function(text) {
  .parse_phone_obs_csv_raw(text) %>%
    .tidy_phone_observations()
}

.phone_obs_page_meta <- function(resp) {
  hdr <- function(name) {
    value <- httr2::resp_header(resp, name)
    if (is.null(value) || !nzchar(value)) {
      return(NA_integer_)
    }
    suppressWarnings(as.integer(value))
  }
  list(
    total = hdr("x-total-count"),
    limit = hdr("x-limit"),
    offset = hdr("x-offset"),
    next_observation_id = hdr("x-next-observation-id")
  )
}

.phone_obs_request <- function(hdr,
                               project_id,
                               procedure_id,
                               data_type,
                               limit,
                               offset = 0L,
                               after_observation_id = NULL) {
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

  page_limit <- min(as.integer(limit), API_MAX_LIMIT)
  if (!is.null(after_observation_id)) {
    req <- httr2::req_url_query(
      req,
      limit = page_limit,
      after_observation_id = as.integer(after_observation_id)
    )
  } else {
    req <- httr2::req_url_query(
      req,
      limit = page_limit,
      offset = as.integer(offset)
    )
  }
  req
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

.fetch_phone_obs_csv_page <- function(hdr,
                                      project_id,
                                      procedure_id,
                                      data_type,
                                      limit,
                                      offset = 0L,
                                      after_observation_id = NULL) {
  req <- .phone_obs_request(
    hdr = hdr,
    project_id = project_id,
    procedure_id = procedure_id,
    data_type = data_type,
    limit = limit,
    offset = offset,
    after_observation_id = after_observation_id
  )
  result <- .perform_phone_obs_request(req, data_type, project_id, procedure_id)
  if (isTRUE(result$empty)) {
    return(list(
      empty = TRUE,
      rows = tibble::tibble(),
      total = 0L,
      next_observation_id = NA_integer_,
      info = result
    ))
  }
  meta <- .phone_obs_page_meta(result$resp)
  rows <- .parse_phone_obs_csv_raw(httr2::resp_body_string(result$resp))
  list(
    empty = FALSE,
    rows = rows,
    total = meta$total,
    next_observation_id = meta$next_observation_id,
    info = NULL
  )
}

.fetch_phone_obs_csv <- function(hdr, project_id, procedure_id, data_type) {
  limit <- API_MAX_LIMIT
  batches <- list()
  offset <- 0L
  after_observation_id <- NULL
  use_keyset <- TRUE
  total <- NA_integer_
  first <- TRUE

  repeat {
    page <- .fetch_phone_obs_csv_page(
      hdr = hdr,
      project_id = project_id,
      procedure_id = procedure_id,
      data_type = data_type,
      limit = limit,
      offset = offset,
      after_observation_id = if (use_keyset) after_observation_id else NULL
    )

    if (isTRUE(page$empty)) {
      if (first) {
        .message_phone_obs_empty(page$info)
      }
      break
    }
    first <- FALSE

    batch <- page$rows
    if (!is.na(page$total)) {
      total <- page$total
    }
    if (nrow(batch) == 0L) {
      break
    }

    batches[[length(batches) + 1L]] <- batch

    if (use_keyset && !is.na(page$next_observation_id)) {
      after_observation_id <- page$next_observation_id
      if (nrow(batch) < limit) {
        break
      }
      next
    }

    use_keyset <- FALSE
    after_observation_id <- NULL
    offset <- offset + nrow(batch)

    if (!is.na(total)) {
      if (offset >= total) {
        break
      }
    } else if (nrow(batch) < limit) {
      break
    }
  }

  if (length(batches) == 0L) {
    return(tibble::tibble())
  }
  dplyr::bind_rows(batches) %>%
    .tidy_phone_observations()
}

.download_phone_obs_media_page <- function(hdr,
                                           project_id,
                                           procedure_id,
                                           data_type,
                                           limit,
                                           offset = 0L,
                                           after_observation_id = NULL,
                                           staging_dir,
                                           page_index = 1L) {
  req <- .phone_obs_request(
    hdr = hdr,
    project_id = project_id,
    procedure_id = procedure_id,
    data_type = data_type,
    limit = limit,
    offset = offset,
    after_observation_id = after_observation_id
  )
  result <- .perform_phone_obs_request(req, data_type, project_id, procedure_id)
  if (isTRUE(result$empty)) {
    return(list(
      empty = TRUE,
      rows = tibble::tibble(),
      total = 0L,
      next_observation_id = NA_integer_,
      media_count = 0L,
      info = result
    ))
  }

  meta_headers <- .phone_obs_page_meta(result$resp)
  meta <- httr2::resp_body_json(result$resp)
  row_count <- as.integer(meta$row_count %||% 0L)
  if (row_count <= 0L || !nzchar(as.character(meta$download_url %||% ""))) {
    return(list(
      empty = FALSE,
      rows = tibble::tibble(),
      total = meta_headers$total,
      next_observation_id = meta_headers$next_observation_id,
      media_count = 0L,
      info = NULL
    ))
  }

  extract_dir <- tempfile("phone-obs-page-")
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)

  zip_name <- meta$filename %||% paste0(data_type, ".zip")
  zip_path <- file.path(extract_dir, zip_name)
  download_url <- as.character(meta$download_url)
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

  utils::unzip(zip_path, exdir = extract_dir)
  files <- list.files(extract_dir, recursive = TRUE, full.names = TRUE)
  csv_path <- files[grepl("observations\\.csv$", files, ignore.case = TRUE)]
  rows <- if (length(csv_path)) {
    readr::read_csv(csv_path[[1]], show_col_types = FALSE, progress = FALSE) %>%
      tibble::as_tibble()
  } else {
    tibble::tibble()
  }

  media_files <- files[!grepl(
    "\\.(csv|xlsx|gpkg|zip)$",
    files,
    ignore.case = TRUE
  )]
  media_files <- media_files[file.info(media_files)$isdir %in% FALSE]
  page_stage <- file.path(staging_dir, sprintf("p%04d", as.integer(page_index)))
  dir.create(page_stage, recursive = TRUE, showWarnings = FALSE)
  for (src in media_files) {
    file.copy(src, file.path(page_stage, basename(src)), overwrite = FALSE)
  }

  list(
    empty = FALSE,
    rows = rows,
    total = meta_headers$total,
    next_observation_id = meta_headers$next_observation_id,
    media_count = as.integer(meta$media_count %||% length(media_files)),
    size_bytes = as.integer(meta$size_bytes %||% 0L),
    expires_at = meta$expires_at,
    filename = meta$filename,
    info = NULL
  )
}

.fetch_phone_obs_media <- function(hdr, project_id, procedure_id, data_type, dest_dir) {
  limit <- API_MAX_LIMIT
  out_dir <- .unique_export_dir(dest_dir, procedure_id, data_type)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  staging_dir <- file.path(out_dir, ".media-staging")
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging_dir, recursive = TRUE), add = TRUE)

  batches <- list()
  offset <- 0L
  after_observation_id <- NULL
  use_keyset <- TRUE
  total <- NA_integer_
  media_count <- 0L
  size_bytes <- 0L
  expires_at <- NULL
  filename <- NULL
  first <- TRUE
  page_index <- 0L

  repeat {
    page_index <- page_index + 1L
    page <- .download_phone_obs_media_page(
      hdr = hdr,
      project_id = project_id,
      procedure_id = procedure_id,
      data_type = data_type,
      limit = limit,
      offset = offset,
      after_observation_id = if (use_keyset) after_observation_id else NULL,
      staging_dir = staging_dir,
      page_index = page_index
    )

    if (isTRUE(page$empty)) {
      if (first) {
        .message_phone_obs_empty(page$info)
        unlink(out_dir, recursive = TRUE)
        return(list(
          data_type = data_type,
          observations = tibble::tibble(),
          dest_dir = NULL,
          zip_path = NULL,
          files = character(),
          excel_path = NULL,
          geopackage_path = NULL,
          row_count = 0L,
          media_count = 0L,
          expires_at = NULL,
          size_bytes = 0L,
          filename = NULL
        ))
      }
      break
    }
    first <- FALSE

    batch <- page$rows
    if (!is.na(page$total)) {
      total <- page$total
    }
    media_count <- media_count + as.integer(page$media_count %||% 0L)
    size_bytes <- size_bytes + as.integer(page$size_bytes %||% 0L)
    if (!is.null(page$expires_at)) {
      expires_at <- page$expires_at
    }
    if (!is.null(page$filename)) {
      filename <- page$filename
    }

    if (nrow(batch) == 0L) {
      break
    }
    batches[[length(batches) + 1L]] <- batch

    if (use_keyset && !is.na(page$next_observation_id)) {
      after_observation_id <- page$next_observation_id
      if (nrow(batch) < limit) {
        break
      }
      next
    }

    use_keyset <- FALSE
    after_observation_id <- NULL
    offset <- offset + nrow(batch)
    if (!is.na(total)) {
      if (offset >= total) {
        break
      }
    } else if (nrow(batch) < limit) {
      break
    }
  }

  observations <- if (length(batches) == 0L) {
    tibble::tibble()
  } else {
    dplyr::bind_rows(batches) %>%
      .tidy_phone_observations()
  }

  .relocate_phone_obs_media(observations, staging_dir, out_dir)

  excel_path <- NULL
  geopackage_path <- NULL
  if (nrow(observations) > 0L) {
    excel_path <- file.path(out_dir, "observations.xlsx")
    .write_phone_obs_workbook(observations, excel_path)
    geopackage_path <- file.path(out_dir, "observations.gpkg")
    .write_phone_obs_geopackage(observations, geopackage_path)
  }
  files <- list.files(out_dir, recursive = TRUE, full.names = TRUE)

  list(
    data_type = data_type,
    observations = observations,
    dest_dir = out_dir,
    zip_path = NULL,
    files = files,
    excel_path = excel_path,
    geopackage_path = geopackage_path,
    row_count = nrow(observations),
    media_count = media_count,
    expires_at = expires_at,
    size_bytes = size_bytes,
    filename = filename
  )
}

.fetch_one_phone_obs_type <- function(hdr, project_id, procedure_id, data_type, dest_dir) {
  if (data_type %in% .PHONE_OBS_MEDIA_TYPES) {
    .fetch_phone_obs_media(hdr, project_id, procedure_id, data_type, dest_dir)
  } else {
    list(
      data_type = data_type,
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
#' Large exports are fetched in pages of up to 1000 rows (\code{limit}/
#' \code{offset}, preferring \code{after_observation_id} keyset cursors when the
#' API returns \code{X-Next-Observation-Id}). Pages are combined automatically
#' so callers still receive one tibble / export folder.
#'
#' Non-media types (\code{choice}, \code{text}, \code{numeric}, \code{label})
#' stream a CSV that is returned as a tibble. Media types
#' (\code{phone-photo}, \code{phone-video}, \code{phone-audio}) return a JSON
#' export descriptor per page; this function then calls
#' \link{download_phone_observation_export}, follows the redirect to GCS,
#' extracts each ZIP, merges rows and media into a timestamped folder under
#' \code{dest_dir}, writes \code{observations.xlsx} and
#' \code{observations.gpkg} at that folder root, and rearranges media into
#' \code{survey_name/procedure_name/feature_id/} beside those files.
#'
#' The workbook has three sheets: a column guide, one wide-format row per
#' feature, and long-format observations linked by a client-friendly
#' \code{feature_id} such as \code{f001}. Wide-format survey-item
#' columns use the exact \code{item_name}; repeated observations for the same
#' feature and item are stored as a JSON array. The GeoPackage contains a
#' spatial \code{parent_features} layer and a related non-spatial \code{data}
#' table linked by \code{feature_id}.
#'
#' Returned observations (and the workbook data)
#' drop identifiers/device noise (\code{project_id}, \code{project_system_id},
#' \code{procedure_id}, \code{observation_id}, \code{item_uuid},
#' \code{observation_uuid}, \code{phone_model},
#' \code{phone_operating_system}); rename \code{system_name} to
#' \code{survey_name} and keep \code{procedure_name};
#' \code{recorded_at} to \code{observation_recording_timestamp} (other
#' timestamp columns dropped), \code{data} to \code{survey_observation}, and
#' \code{username} to \code{surveyor_name} (last column). Raw
#' \code{longitude}/\code{latitude} are replaced by coordinates parsed from
#' the feature-level \code{feature_geometry}. When any \code{labels} JSON is
#' present it is expanded into taxonomy/prediction columns (\code{class},
#' \code{order}, ...) after \code{survey_observation}; otherwise those columns
#' are omitted. The internal \code{feature_uuid} is replaced by
#' \code{feature_id}. Media observations use the relative path
#' \code{survey_name/procedure_name/feature_id/file_name}.
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
#' @param dest_dir Optional parent directory for the timestamped export folder.
#'   When \code{NULL} (default), uses the current working directory. Each media
#'   download creates a new \code{phone-obs-...} folder that is never
#'   overwritten.
#'
#' @return If a single \code{data_type} is requested, a tibble (non-media)
#'   or a named list with \code{observations}, \code{dest_dir},
#'   \code{excel_path}, \code{geopackage_path}, \code{files}, and export
#'   metadata (media). If several types are requested (including the all-types
#'   default), a named list of those results.
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
