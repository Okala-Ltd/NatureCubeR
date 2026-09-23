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
  invisible(info)
}

.sanitize_dir_token <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "-", as.character(x))
  x <- gsub("^-+|-+$", "", x)
  x[!nzchar(x) | is.na(x)] <- "export"
  x
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

# Export layout:
#   {system_id}_{system_name}_{timestamp}/
#     {procedure_id}_{procedure_name}/
#       {item_name}/
#         observations.xlsx
#         observations.gpkg
#         {data_type}/   (media files only, when present)
.phone_obs_export_root <- function(dest_dir, procedure) {
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  sys_id <- procedure$system_id %||% "NA"
  sys_name <- .sanitize_dir_token(procedure$system_name %||% "system")
  base <- file.path(
    dest_dir,
    paste(sys_id, sys_name, stamp, sep = "_")
  )
  if (!dir.exists(base)) {
    return(base)
  }
  paste0(base, "-", as.integer(Sys.time()))
}

.phone_obs_procedure_dir <- function(export_root, procedure) {
  proc_id <- procedure$procedure_id %||% "NA"
  proc_name <- .sanitize_dir_token(procedure$procedure_name %||% "procedure")
  file.path(export_root, paste(proc_id, proc_name, sep = "_"))
}

.phone_obs_item_dir <- function(procedure_dir, item_name) {
  file.path(procedure_dir, .sanitize_dir_token(item_name))
}

.phone_obs_item_names_for_type <- function(procedure, data_type) {
  items <- procedure$items
  if (!is.data.frame(items) || nrow(items) == 0L ||
      !all(c("item_name", "data_type") %in% names(items))) {
    return(.sanitize_dir_token(data_type %||% "Observation"))
  }
  match <- !is.na(items$data_type) & items$data_type == data_type
  names <- .normalize_phone_obs_item_name(items$item_name[match])
  names <- unique(names[!is.na(names) & nzchar(names)])
  if (length(names) == 0L) {
    return(.sanitize_dir_token(data_type %||% "Observation"))
  }
  names
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
  "observation_uuid",
  "feature_id"
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

# Parse JSON that may be plain, double-encoded, or contain literal \" escapes.
.phone_obs_parse_json_value <- function(raw) {
  if (is.null(raw) || length(raw) == 0L || (length(raw) == 1L && is.na(raw))) {
    return(NULL)
  }
  text <- trimws(as.character(raw[[1]]))
  if (!nzchar(text)) {
    return(NULL)
  }

  for (attempt in seq_len(5L)) {
    parsed <- tryCatch(
      jsonlite::fromJSON(text, simplifyVector = TRUE, simplifyDataFrame = TRUE),
      error = function(e) NULL
    )
    if (is.null(parsed)) {
      unescaped <- gsub("\\\\\"", "\"", text)
      unescaped <- gsub("\\\\\\\\", "\\\\", unescaped)
      if (identical(unescaped, text)) {
        break
      }
      text <- unescaped
      next
    }
    # JSON string that itself contains JSON (double-encoded payload).
    if (is.character(parsed) && length(parsed) == 1L) {
      inner <- trimws(parsed)
      if (nzchar(inner) && grepl("^\\s*[\\[{]", inner)) {
        text <- inner
        next
      }
    }
    return(parsed)
  }
  NULL
}

.phone_obs_label_names_from_parsed <- function(parsed) {
  if (is.null(parsed)) {
    return(character())
  }
  if (is.data.frame(parsed)) {
    if (!("label" %in% names(parsed))) {
      return(character())
    }
    labs <- as.character(parsed$label)
    return(labs[!is.na(labs) & nzchar(labs)])
  }
  if (is.list(parsed)) {
    if (!is.null(names(parsed)) && "label" %in% names(parsed)) {
      lab <- parsed[["label"]]
      if (is.null(lab) || length(lab) == 0L || (length(lab) == 1L && is.na(lab))) {
        return(character())
      }
      return(as.character(lab[[1]]))
    }
    labs <- vapply(parsed, function(x) {
      if (is.list(x) && !is.null(x$label) && length(x$label) > 0L) {
        return(as.character(x$label[[1]]))
      }
      if (is.character(x) && length(x) > 0L && nzchar(x[[1]])) {
        return(as.character(x[[1]]))
      }
      NA_character_
    }, character(1))
    return(labs[!is.na(labs) & nzchar(labs)])
  }
  if (is.character(parsed)) {
    labs <- parsed[!is.na(parsed) & nzchar(parsed)]
    return(as.character(labs))
  }
  character()
}

# Return only the taxonomic label string(s). Never returns label_id or raw JSON.
.phone_obs_label_text <- function(raw) {
  if (is.null(raw) || length(raw) == 0L || (length(raw) == 1L && is.na(raw))) {
    return(NA_character_)
  }
  text <- as.character(raw[[1]])
  if (!nzchar(text)) {
    return(NA_character_)
  }
  parsed <- .phone_obs_parse_json_value(text)
  labs <- .phone_obs_label_names_from_parsed(parsed)
  if (length(labs) == 0L) {
    # Already a plain label value (not JSON-looking).
    if (!grepl("^\\s*[\\[{]", text)) {
      return(text)
    }
    return(NA_character_)
  }
  if (length(labs) == 1L) {
    return(labs[[1]])
  }
  paste(labs, collapse = "; ")
}

# Expand the labels JSON array into flat taxonomy / prediction columns.
# Uses the first label object when several are present. Never keeps label_id.
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
    parsed <- .phone_obs_parse_json_value(raw)
    if (is.null(parsed)) {
      next
    }
    if (is.data.frame(parsed)) {
      if (nrow(parsed) == 0L) {
        next
      }
      row <- parsed[1, , drop = FALSE]
    } else if (is.list(parsed)) {
      if (is.null(names(parsed)) && length(parsed) > 0L && is.list(parsed[[1]])) {
        row <- parsed[[1]]
      } else {
        row <- parsed
      }
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

.normalize_phone_obs_item_name <- function(item) {
  item <- as.character(item)
  missing <- is.na(item) | !nzchar(item)
  item[missing] <- "Observation"
  # API schemas sometimes expose both "Taxonomic label" and "taxonomic label".
  tax <- tolower(trimws(item)) == "taxonomic label"
  item[tax] <- "Taxonomic label"
  item
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

# Drop noise columns, expand labels when present, rename client-facing fields,
# derive feature coordinates, and put surveyor last.
.tidy_phone_observations <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0L) {
    return(tibble::as_tibble(df))
  }
  df <- tibble::as_tibble(df)

  drop <- intersect(.PHONE_OBS_DROP_COLS, names(df))
  if (length(drop)) {
    df <- df[, setdiff(names(df), drop), drop = FALSE]
  }

  # Prefer the dedicated labels column; fall back to data/observation for label rows.
  label_src <- if ("labels" %in% names(df)) df$labels else NULL
  if (!.has_phone_obs_label_data(label_src) &&
      "data_type" %in% names(df) &&
      any(!is.na(df$data_type) & df$data_type == "label")) {
    value_col <- if ("data" %in% names(df)) {
      "data"
    } else if ("observation" %in% names(df)) {
      "observation"
    } else if ("survey_observation" %in% names(df)) {
      "survey_observation"
    } else {
      NULL
    }
    if (!is.null(value_col)) {
      label_src <- df[[value_col]]
      label_src[is.na(df$data_type) | df$data_type != "label"] <- NA_character_
    }
  }

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
    for (nm in c(.PHONE_OBS_LABEL_COLS, "class_", "label_id")) {
      if (nm %in% names(df)) {
        df[[nm]] <- NULL
      }
    }
    df <- dplyr::bind_cols(df, .expand_phone_obs_labels(label_src))
  } else if ("class_" %in% names(df)) {
    names(df)[names(df) == "class_"] <- "class"
  }
  if ("label_id" %in% names(df)) {
    df$label_id <- NULL
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

  if ("item_name" %in% names(df)) {
    df$item_name <- .normalize_phone_obs_item_name(df$item_name)
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

  # feature_uuid is kept as the stable feature key (feature_id is dropped above).

  if (all(c("data_type", "survey_observation") %in% names(df))) {
    is_label <- !is.na(df$data_type) & df$data_type == "label"
    if (any(is_label)) {
      cleaned <- as.character(df$survey_observation)
      if ("label" %in% names(df)) {
        use_expanded <- is_label &
          !is.na(df$label) &
          nzchar(as.character(df$label))
        cleaned[use_expanded] <- as.character(df$label[use_expanded])
        still <- which(is_label & !use_expanded)
      } else {
        still <- which(is_label)
      }
      if (length(still) > 0L) {
        cleaned[still] <- vapply(
          df$survey_observation[still],
          .phone_obs_label_text,
          character(1),
          USE.NAMES = FALSE
        )
      }
      df$survey_observation <- cleaned
    }

    media <- df$data_type %in% .PHONE_OBS_MEDIA_TYPES
    has_file <- media & !is.na(df$survey_observation) &
      nzchar(as.character(df$survey_observation))
    if (any(has_file)) {
      file_name <- basename(sub(
        "[?#].*$",
        "",
        as.character(df$survey_observation[has_file])
      ))
      type <- as.character(df$data_type[has_file])
      df$survey_observation[has_file] <- .phone_obs_media_relpath(
        type,
        file_name
      )
    }
  }

  nm <- names(df)
  lead_cols <- intersect(c("feature_uuid", "survey_name", "procedure_name"), nm)
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
      !("feature_uuid" %in% names(long_data))) {
    return(tibble::as_tibble(long_data))
  }
  long_data <- tibble::as_tibble(long_data)
  if ("item_name" %in% names(long_data)) {
    long_data$item_name <- .normalize_phone_obs_item_name(long_data$item_name)
  }
  feature_uuids <- unique(long_data$feature_uuid)
  feature_uuids <- feature_uuids[!is.na(feature_uuids) & nzchar(as.character(feature_uuids))]
  base_cols <- intersect(
    c(
      "feature_uuid",
      "survey_name",
      "procedure_name",
      "observation_recording_timestamp",
      "longitude",
      "latitude"
    ),
    names(long_data)
  )

  observations <- lapply(feature_uuids, function(feature_uuid) {
    feature <- long_data[long_data$feature_uuid == feature_uuid, , drop = FALSE]
    item <- if ("item_name" %in% names(feature)) {
      as.character(feature$item_name)
    } else {
      rep("Observation", nrow(feature))
    }
    item[is.na(item) | !nzchar(item)] <- "Observation"

    value <- if ("survey_observation" %in% names(feature)) {
      as.character(feature$survey_observation)
    } else {
      rep(NA_character_, nrow(feature))
    }
    # Label rows should already be plain text; fall back to expanded label col.
    if ("data_type" %in% names(feature)) {
      is_label <- !is.na(feature$data_type) & feature$data_type == "label"
      if (any(is_label)) {
        value[is_label] <- vapply(
          value[is_label],
          .phone_obs_label_text,
          character(1),
          USE.NAMES = FALSE
        )
      }
    }
    if ("label" %in% names(feature)) {
      use_label <- is.na(value) | !nzchar(value)
      value[use_label] <- as.character(feature$label[use_label])
    }

    item_names <- unique(item)
    values <- lapply(item_names, function(item_name) {
      item_values <- value[item == item_name]
      item_values <- item_values[!is.na(item_values) & nzchar(item_values)]
      if (length(item_values) == 0L) {
        return(NA_character_)
      }
      if (length(item_values) == 1L) {
        return(item_values[[1]])
      }
      jsonlite::toJSON(as.character(item_values), auto_unbox = FALSE, na = "null")
    })
    names(values) <- item_names
    values
  })

  first_rows <- match(feature_uuids, long_data$feature_uuid)
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
    feature_uuid = paste(
      "Stable NatureCube feature UUID shared by observations from the same",
      "feature. Prefer this over generated short IDs so records stay traceable."
    ),
    survey_name = "NatureCube survey kit name.",
    procedure_name = "NatureCube survey procedure name.",
    observation_recording_timestamp = "Date and time the feature observations were recorded.",
    longitude = "Feature longitude.",
    latitude = "Feature latitude.",
    item_name = "Procedure item associated with this observation.",
    data_type = "Observation data type, such as phone-photo, numeric, or label.",
    survey_observation = paste(
      "Recorded value (plain taxonomic label text for data_type='label'),",
      "or media path data_type/file_name relative to the item folder."
    ),
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

.phone_obs_media_relpath <- function(data_type, file_name) {
  clean <- function(x, fallback) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(x)] <- fallback
    .sanitize_dir_token(x)
  }
  paste(clean(data_type, "media"), file_name, sep = "/")
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

# Write one item folder with workbook (+ gpkg when spatial) and optional media.
.export_phone_obs_by_item <- function(observations,
                                      procedure_dir,
                                      staging_dir = NULL,
                                      procedure = NULL,
                                      data_type = NULL) {
  dir.create(procedure_dir, recursive = TRUE, showWarnings = FALSE)
  observations <- if (is.data.frame(observations)) {
    tibble::as_tibble(observations)
  } else {
    tibble::tibble()
  }

  has_item_col <- nrow(observations) > 0L && "item_name" %in% names(observations)
  if (has_item_col) {
    observations$item_name <- .normalize_phone_obs_item_name(observations$item_name)
    item_names <- unique(observations$item_name)
  } else if (nrow(observations) > 0L) {
    item_names <- "Observation"
  } else {
    item_names <- .phone_obs_item_names_for_type(procedure, data_type)
  }

  excel_paths <- character()
  gpkg_paths <- character()
  item_dirs <- character()

  for (item in item_names) {
    item_dir <- .phone_obs_item_dir(procedure_dir, item)
    dir.create(item_dir, recursive = TRUE, showWarnings = FALSE)
    item_dirs <- c(item_dirs, item_dir)

    item_rows <- if (has_item_col) {
      observations[observations$item_name == item, , drop = FALSE]
    } else if (nrow(observations) > 0L) {
      observations
    } else {
      observations[0L, , drop = FALSE]
    }

    if (!is.null(staging_dir) && nrow(item_rows) > 0L) {
      .relocate_phone_obs_media(item_rows, staging_dir, item_dir)
    }

    excel_path <- file.path(item_dir, "observations.xlsx")
    .write_phone_obs_workbook(item_rows, excel_path)
    excel_paths <- c(excel_paths, excel_path)

    if (nrow(item_rows) > 0L) {
      gpkg_path <- file.path(item_dir, "observations.gpkg")
      written <- .write_phone_obs_geopackage(item_rows, gpkg_path)
      if (!is.null(written)) {
        gpkg_paths <- c(gpkg_paths, as.character(written))
      }
    }
  }

  list(
    procedure_dir = procedure_dir,
    item_dirs = item_dirs,
    excel_paths = excel_paths,
    geopackage_paths = gpkg_paths
  )
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
      !all(c("feature_uuid", "longitude", "latitude") %in% names(long_data))) {
    return(invisible(NULL))
  }
  long_data <- tibble::as_tibble(long_data)
  first_rows <- match(unique(long_data$feature_uuid), long_data$feature_uuid)
  parent_cols <- intersect(
    c(
      "feature_uuid",
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
  # Guess types per page, then force value columns to character so pages with
  # mixed `data` types (chr vs dbl for numeric exports) can still bind.
  .normalize_phone_obs_value_cols(
    readr::read_csv(
      I(text),
      show_col_types = FALSE,
      progress = FALSE
    ) %>%
      tibble::as_tibble()
  )
}

.PHONE_OBS_VALUE_COLS <- c("data", "observation", "numbers")

.normalize_phone_obs_value_cols <- function(page) {
  if (!is.data.frame(page) || ncol(page) == 0L) {
    return(page)
  }
  for (col in intersect(.PHONE_OBS_VALUE_COLS, names(page))) {
    page[[col]] <- as.character(page[[col]])
  }
  page
}

# Pages are parsed independently, so readr can guess different types for the
# same column across pages. Normalize value cols and coerce any remaining
# conflicts to character before bind_rows().
.bind_phone_obs_pages <- function(batches) {
  batches <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, batches)
  if (length(batches) == 0L) {
    return(tibble::tibble())
  }
  batches <- lapply(batches, .normalize_phone_obs_value_cols)
  if (length(batches) == 1L) {
    return(batches[[1]])
  }

  cols <- unique(unlist(lapply(batches, names), use.names = FALSE))
  for (col in cols) {
    types <- unique(vapply(batches, function(page) {
      if (!(col %in% names(page))) {
        return(NA_character_)
      }
      paste(class(page[[col]]), collapse = "/")
    }, character(1)))
    types <- types[!is.na(types)]
    if (length(types) <= 1L) {
      next
    }
    for (i in seq_along(batches)) {
      if (col %in% names(batches[[i]])) {
        batches[[i]][[col]] <- as.character(batches[[i]][[col]])
      }
    }
  }
  dplyr::bind_rows(batches)
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

.fetch_phone_obs_csv <- function(hdr,
                                 project_id,
                                 procedure_id,
                                 data_type,
                                 on_page = NULL) {
  limit <- API_MAX_LIMIT
  batches <- list()
  offset <- 0L
  after_observation_id <- NULL
  use_keyset <- TRUE
  total <- NA_integer_
  first <- TRUE
  page_num <- 0L
  row_count <- 0L

  repeat {
    page_num <- page_num + 1L
    if (is.function(on_page)) {
      on_page(page = page_num, rows = row_count, data_type = data_type)
    }
    page <- .fetch_phone_obs_csv_page(
      hdr = hdr,
      project_id = project_id,
      procedure_id = procedure_id,
      data_type = data_type,
      limit = limit,
      offset = offset,
      after_observation_id = if (use_keyset) after_observation_id else NULL
    )

    if (is.list(page) && isTRUE(page$empty)) {
      if (first) {
        .message_phone_obs_empty(page$info)
      }
      break
    }
    if (!is.list(page) || is.null(page$rows)) {
      stop(
        "Unexpected response while paging phone observations for ",
        data_type,
        ".",
        call. = FALSE
      )
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
    row_count <- row_count + nrow(batch)
    if (is.function(on_page)) {
      on_page(page = page_num, rows = row_count, data_type = data_type)
    }

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
  .bind_phone_obs_pages(batches) %>%
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
  if (row_count <= 0L) {
    return(list(
      empty = FALSE,
      rows = tibble::tibble(),
      total = meta_headers$total,
      next_observation_id = meta_headers$next_observation_id,
      media_count = 0L,
      info = NULL
    ))
  }
  download_url <- as.character(meta$download_url %||% "")
  if (!nzchar(download_url)) {
    stop(
      "getPhoneObservations did not return a download_url for ",
      data_type,
      "."
    )
  }

  extract_dir <- tempfile("phone-obs-page-")
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)

  zip_name <- meta$filename %||% paste0(data_type, ".zip")
  zip_path <- file.path(extract_dir, zip_name)
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
    .normalize_phone_obs_value_cols(
      readr::read_csv(csv_path[[1]], show_col_types = FALSE, progress = FALSE) %>%
        tibble::as_tibble()
    )
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
  extract_root <- normalizePath(extract_dir, winslash = "/", mustWork = TRUE)
  for (src in media_files) {
    # Keep relative paths so duplicate basenames in different folders are kept;
    # .relocate_phone_obs_media() matches by basename and consumes candidates in order.
    src_norm <- normalizePath(src, winslash = "/", mustWork = TRUE)
    rel <- substring(src_norm, nchar(extract_root) + 2L)
    if (!nzchar(rel)) {
      rel <- basename(src)
    }
    dest <- file.path(page_stage, rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(src, dest, overwrite = TRUE)
    if (!isTRUE(ok)) {
      warning("Failed to stage media file: ", src, call. = FALSE)
    }
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

.fetch_phone_obs_media <- function(hdr,
                                   project_id,
                                   procedure_id,
                                   data_type,
                                   procedure_dir,
                                   procedure,
                                   on_page = NULL) {
  limit <- API_MAX_LIMIT
  dir.create(procedure_dir, recursive = TRUE, showWarnings = FALSE)
  staging_dir <- tempfile("phone-obs-media-")
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
  row_count <- 0L

  repeat {
    page_index <- page_index + 1L
    if (is.function(on_page)) {
      on_page(page = page_index, rows = row_count, data_type = data_type)
    }
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

    if (is.list(page) && isTRUE(page$empty)) {
      if (first) {
        .message_phone_obs_empty(page$info)
        break
      }
      break
    }
    if (!is.list(page) || is.null(page$rows)) {
      stop(
        "Unexpected response while paging phone observations for ",
        data_type,
        ".",
        call. = FALSE
      )
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
    row_count <- row_count + nrow(batch)
    if (is.function(on_page)) {
      on_page(page = page_index, rows = row_count, data_type = data_type)
    }

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
    .bind_phone_obs_pages(batches) %>%
      .tidy_phone_observations()
  }

  exported <- .export_phone_obs_by_item(
    observations = observations,
    procedure_dir = procedure_dir,
    staging_dir = if (nrow(observations) > 0L) staging_dir else NULL,
    procedure = procedure,
    data_type = data_type
  )
  files <- list.files(procedure_dir, recursive = TRUE, full.names = TRUE)

  list(
    data_type = data_type,
    observations = observations,
    dest_dir = procedure_dir,
    zip_path = NULL,
    files = files,
    excel_path = exported$excel_paths,
    geopackage_path = exported$geopackage_paths,
    item_dirs = exported$item_dirs,
    row_count = nrow(observations),
    media_count = media_count,
    expires_at = expires_at,
    size_bytes = size_bytes,
    filename = filename
  )
}

.fetch_one_phone_obs_type <- function(hdr,
                                      project_id,
                                      procedure_id,
                                      data_type,
                                      procedure_dir,
                                      procedure,
                                      on_page = NULL) {
  if (data_type %in% .PHONE_OBS_MEDIA_TYPES) {
    .fetch_phone_obs_media(
      hdr = hdr,
      project_id = project_id,
      procedure_id = procedure_id,
      data_type = data_type,
      procedure_dir = procedure_dir,
      procedure = procedure,
      on_page = on_page
    )
  } else {
    observations <- .fetch_phone_obs_csv(
      hdr = hdr,
      project_id = project_id,
      procedure_id = procedure_id,
      data_type = data_type,
      on_page = on_page
    )
    exported <- .export_phone_obs_by_item(
      observations = observations,
      procedure_dir = procedure_dir,
      staging_dir = NULL,
      procedure = procedure,
      data_type = data_type
    )
    list(
      data_type = data_type,
      observations = observations,
      dest_dir = procedure_dir,
      excel_path = exported$excel_paths,
      geopackage_path = exported$geopackage_paths,
      item_dirs = exported$item_dirs,
      row_count = nrow(observations)
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
#' stream a CSV; media types (\code{phone-photo}, \code{phone-video},
#' \code{phone-audio}) return a JSON export descriptor per page, then this
#' function calls \link{download_phone_observation_export}, follows the
#' redirect to GCS, and extracts each ZIP. All requested types share one
#' timestamped export tree under \code{dest_dir}:
#' \preformatted{
#' {system_id}_{system_name}_{timestamp}/
#'   {procedure_id}_{procedure_name}/
#'     {item_name}/
#'       observations.xlsx
#'       observations.gpkg
#'       {data_type}/          # media files only, when present
#' }
#' Empty types (including media with no files) still create the matching
#' item folder(s) and an empty \code{observations.xlsx}, using procedure item
#' names when no rows are returned.
#'
#' Large exports are paginated with \code{limit = 1000}. Pages are fetched
#' with keyset cursors via \code{after_observation_id} when available,
#' otherwise with \code{offset}, until an empty or short page is returned.
#'
#' The workbook has three sheets: a column guide, one wide-format row per
#' feature, and long-format observations linked by \code{feature_uuid}.
#' Wide-format survey-item columns use the exact \code{item_name} (case
#' variants of taxonomic label items are collapsed to
#' \code{"Taxonomic label"}); repeated observations for the same feature and
#' item are stored as a JSON array of plain values. The GeoPackage contains a
#' spatial \code{parent_features} layer and a related non-spatial \code{data}
#' table linked by \code{feature_uuid}.
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
#' are omitted. \code{label_id} is never kept. For \code{data_type = "label"},
#' \code{survey_observation} is reduced to the plain taxonomic label text
#' (JSON / escaped payloads are parsed and discarded). The API
#' \code{feature_uuid} is kept as the stable feature key. Media observations
#' use the relative path \code{data_type/file_name} inside the item folder.
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
#'   \code{project_id}, \code{procedure_id}, \code{system_id},
#'   \code{system_name}, and \code{procedure_name}.
#' @param data_type Optional character. One or more of \code{phone-photo},
#'   \code{phone-video}, \code{phone-audio}, \code{choice}, \code{text},
#'   \code{numeric}, \code{label}. When \code{NULL} (default), every type is
#'   downloaded.
#' @param dest_dir Optional parent directory for the timestamped export folder.
#'   When \code{NULL} (default), uses the current working directory. Each call
#'   creates a new \code{{system_id}_{system_name}_{timestamp}/} tree that is
#'   never overwritten.
#'
#' @return A named list per requested \code{data_type} with
#'   \code{observations}, \code{dest_dir} (procedure folder),
#'   \code{excel_path}, \code{geopackage_path}, \code{item_dirs}, and related
#'   export metadata. If a single type is requested, that list is returned
#'   directly (not wrapped in an outer list).
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

  export_root <- .phone_obs_export_root(dest_dir, procedure)
  procedure_dir <- .phone_obs_procedure_dir(export_root, procedure)
  dir.create(procedure_dir, recursive = TRUE, showWarnings = FALSE)

  # One progress line per data type (like devtools::check()): show Downloading
  # immediately, update in place, then print a permanent aligned summary line.
  type_width <- max(nchar(data_type), nchar("phone-video"))
  verb_width <- nchar("Downloaded")
  # Non-breaking spaces so cli/terminals keep column alignment.
  .pad <- function(x, width) {
    x <- as.character(x)
    n <- nchar(x, type = "chars")
    if (is.na(n) || n >= width) {
      return(x)
    }
    paste0(x, strrep("\u00a0", width - n))
  }
  .elapsed_label <- function(secs) {
    secs <- as.numeric(secs)
    if (!is.finite(secs) || secs < 0) {
      secs <- 0
    }
    if (secs < 60) {
      return(sprintf("%.1fs", secs))
    }
    sprintf("%dm %.0fs", as.integer(secs %/% 60), secs %% 60)
  }
  .print_summary <- function(ok, padded_type, page_num, row_count, elapsed) {
    if (ok) {
      line <- paste0(
        cli::col_green(cli::symbol$tick), " ",
        .pad("Downloaded", verb_width), " ",
        padded_type,
        " | page ", page_num,
        " | ", row_count, " row(s)",
        " | elapsed: ", elapsed
      )
    } else {
      line <- paste0(
        cli::col_cyan(cli::symbol$info), " ",
        .pad("No data", verb_width), " ",
        padded_type,
        " | page 0",
        " | 0 row(s)",
        " | elapsed: ", elapsed
      )
    }
    # cat keeps padding; cli_text collapses trailing spaces.
    cat(line, "\n", sep = "")
  }

  results <- vector("list", length(data_type))
  names(results) <- data_type
  for (i in seq_along(data_type)) {
    current_type <- data_type[[i]]
    # Show page 1 / 0 rows immediately so long first-page waits are visible.
    page_num <- 1L
    row_count <- 0L
    started <- Sys.time()
    padded_type <- .pad(current_type, type_width)
    pb <- cli::cli_progress_bar(
      name = padded_type,
      format = paste0(
        "{cli::pb_spin} ",
        .pad("Downloading", verb_width),
        " {cli::pb_name} | ",
        "page {page_num} | {row_count} row(s) | elapsed: {cli::pb_elapsed}"
      ),
      total = NA,
      clear = TRUE
    )
    # Force the Downloading line onto the console before any network work.
    cli::cli_progress_update(id = pb, force = TRUE)
    try(cli::cli_flush(), silent = TRUE)

    on_page <- function(page, rows, data_type) {
      page_num <<- as.integer(page)
      row_count <<- as.integer(rows)
      cli::cli_progress_update(id = pb, force = TRUE)
      try(cli::cli_flush(), silent = TRUE)
    }
    results[[i]] <- tryCatch(
      .fetch_one_phone_obs_type(
        hdr = hdr,
        project_id = ids$project_id,
        procedure_id = ids$procedure_id,
        data_type = current_type,
        procedure_dir = procedure_dir,
        procedure = procedure,
        on_page = on_page
      ),
      error = function(e) {
        try(cli::cli_progress_done(id = pb, result = "failed"), silent = TRUE)
        stop(e)
      }
    )
    final_rows <- if (is.data.frame(results[[i]])) {
      nrow(results[[i]])
    } else if (is.list(results[[i]]) && is.data.frame(results[[i]]$observations)) {
      nrow(results[[i]]$observations)
    } else {
      0L
    }
    if (page_num < 1L && final_rows > 0L) {
      page_num <- max(1L, as.integer(ceiling(final_rows / API_MAX_LIMIT)))
    }
    row_count <- as.integer(final_rows)
    elapsed <- .elapsed_label(difftime(Sys.time(), started, units = "secs"))
    try(cli::cli_progress_done(id = pb, clear = TRUE), silent = TRUE)

    .print_summary(
      ok = final_rows > 0L,
      padded_type = padded_type,
      page_num = page_num,
      row_count = row_count,
      elapsed = elapsed
    )
  }

  if (length(results) == 1L) {
    return(results[[1]])
  }
  results
}
