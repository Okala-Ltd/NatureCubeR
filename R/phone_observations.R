#' @title Valid Phone Observation Types
#'
#' @description
#' Character vector of valid item types for phone observations.
#'
#' @keywords internal
phone_types <- c(

"phone-photo",
"phone-video",
"phone-audio",
"choice",
"text",
"numeric",
"label",
"instruction"
)

# Internal null-coalescing helper.
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' @title Build Device Settings
#'
#' @description
#' Constructs a validated device settings list matching the DeviceSettings schema
#' required by the NatureCube API.
#'
#' @param device_id Character. Unique identifier for the device.
#' @param phone_model Character. Model name of the phone (e.g., "iPhone 14 Pro").
#' @param phone_os Character. Operating system of the phone (e.g., "iOS 17.2").
#' @param carrier Character. Network carrier (e.g., "Vodafone").
#' @param build_number Character. App build number.
#' @param build_id Character. App build identifier.
#' @param battery_level Numeric. Battery level percentage (0-100). Default is 100.
#' @param device_last_used POSIXct or NULL. Timestamp of last device use. Default is current time.
#'
#' @return A named list with device settings ready for API submission.
#'
#' @examples
#' \dontrun{
#'   device <- build_device_settings(
#'     device_id = "abc123-unique-id",
#'     phone_model = "iPhone 14 Pro",
#'     phone_os = "iOS 17.2",
#'     carrier = "Vodafone",
#'     build_number = "1.2.3",
#'     build_id = "build-456"
#'   )
#' }
#'
#' @author Adam Varley
#' @export
build_device_settings <- function(device_id,
                                   phone_model,
                                   phone_os,
                                   carrier,
                                   build_number,
                                   build_id,
                                   battery_level = 100,
                                   device_last_used = NULL) {

# Validate required fields
if (missing(device_id) || is.null(device_id) || device_id == "") {
  stop("device_id is required")
}
if (missing(phone_model) || is.null(phone_model) || phone_model == "") {
  stop("phone_model is required")
}
if (missing(phone_os) || is.null(phone_os) || phone_os == "") {
  stop("phone_os is required")
}
if (missing(carrier) || is.null(carrier) || carrier == "") {
 stop("carrier is required")
}
if (missing(build_number) || is.null(build_number) || build_number == "") {
  stop("build_number is required")
}
if (missing(build_id) || is.null(build_id) || build_id == "") {
  stop("build_id is required")
}

# Validate battery level
if (!is.numeric(battery_level) || battery_level < 0 || battery_level > 100) {
  stop("battery_level must be a number between 0 and 100")
}

# Set default for device_last_used
if (is.null(device_last_used)) {
  device_last_used <- Sys.time()
}

# Build the device settings list
device_settings <- list(
  device_id = as.character(device_id),
  phone_model = as.character(phone_model),
  phone_operating_system = as.character(phone_os),
  carrier = as.character(carrier),
  build_number = as.character(build_number),
  build_id = as.character(build_id),
  battery_level = as.numeric(battery_level),
  device_created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
  device_last_used = format(device_last_used, "%Y-%m-%dT%H:%M:%SZ")
)

return(device_settings)
}


#' @title Build Observation
#'
#' @description
#' Creates a single observation record (NestedObservationRecord) for inclusion
#' in a feature record.
#'
#' @param item_uuid Character. UUID of the item/field this observation is for.
#' @param item_type Character. Type of observation. Must be one of: "phone-photo",
#'   "phone-video", "phone-audio", "choice", "text", "numeric", "label", "instruction".
#' @param data List or vector. The observation data. For media types, this should be
#'   a character vector of filenames. For other types, the appropriate data values.
#' @param geometry List. GeoJSON geometry object (Point, Polygon, or LineString).
#' @param observation_uuid Character or NULL. UUID for this observation. If NULL,
#'   a new UUID will be generated.
#' @param observation_created_at POSIXct or NULL. Timestamp when observation was created.
#'   If NULL, current time is used.
#'
#' @return A named list representing a NestedObservationRecord.
#'
#' @examples
#' \dontrun{
#'   obs <- build_observation(
#'     item_uuid = "f47ac10b-58cc-4372-a567-0e02b2c3d479",
#'     item_type = "phone-photo",
#'     data = c("photo1.jpg", "photo2.jpg"),
#'     geometry = list(type = "Point", coordinates = c(-1.5, 53.4))
#'   )
#' }
#'
#' @author Adam Varley
#' @export
build_observation <- function(item_uuid,
                               item_type,
                               data,
                               geometry,
                               observation_uuid = NULL,
                               observation_created_at = NULL) {

# Validate item_type
if (!item_type %in% phone_types) {
  stop("item_type must be one of: ", paste(phone_types, collapse = ", "))
}

# Validate item_uuid
if (missing(item_uuid) || is.null(item_uuid) || item_uuid == "") {
  stop("item_uuid is required")
}

# Validate geometry
if (missing(geometry) || is.null(geometry)) {
  stop("geometry is required")
}
if (!is.list(geometry) || !"type" %in% names(geometry)) {
  stop("geometry must be a GeoJSON object with 'type' property")
}
valid_geom_types <- c("Point", "Polygon", "LineString")
if (!geometry$type %in% valid_geom_types) {
  stop("geometry type must be one of: ", paste(valid_geom_types, collapse = ", "))
}

# Generate UUID if not provided
if (is.null(observation_uuid)) {
  observation_uuid <- uuid::UUIDgenerate()
}

# Set timestamp if not provided
if (is.null(observation_created_at)) {
  observation_created_at <- Sys.time()
}

# Build the observation properties
properties <- list(
  item_uuid = as.character(item_uuid),
  item_type = as.character(item_type),
  observation_uuid = as.character(observation_uuid),
  observation_created_at = format(observation_created_at, "%Y-%m-%dT%H:%M:%SZ"),
  data = as.list(data)
)

# Build the full observation record (GeoJSON Feature structure)
observation <- list(
  type = "Feature",
  geometry = geometry,
  properties = properties
)

return(observation)
}


#' @title Build Feature Record
#'
#' @description
#' Constructs a feature record (FieldRecord) containing a geometry and its
#' associated observations.
#'
#' @param feature_uuid Character. UUID for this feature record.
#' @param project_system_id Integer. ID of the project system.
#' @param procedure_id Integer. ID of the procedure being followed.
#' @param start_time POSIXct. Timestamp when the procedure started.
#' @param end_time POSIXct. Timestamp when the procedure ended.
#' @param created_by_method Character. How the feature was created: "drawn" or "traced".
#' @param geometry List. GeoJSON geometry object (Point, Polygon, or LineString).
#' @param observations List. List of observation records created with \code{build_observation()}.
#'
#' @return A named list representing a FieldRecord ready for API submission.
#'
#' @examples
#' \dontrun{
#'   obs1 <- build_observation(
#'     item_uuid = "abc-123",
#'     item_type = "text",
#'     data = list("Sample observation"),
#'     geometry = list(type = "Point", coordinates = c(-1.5, 53.4))
#'   )
#'
#'   feature <- build_feature_record(
#'     feature_uuid = "feature-uuid-123",
#'     project_system_id = 42,
#'     procedure_id = 7,
#'     start_time = Sys.time() - 3600,
#'     end_time = Sys.time(),
#'     created_by_method = "drawn",
#'     geometry = list(
#'       type = "Polygon",
#'       coordinates = list(list(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))
#'     ),
#'     observations = list(obs1)
#'   )
#' }
#'
#' @author Adam Varley
#' @export
build_feature_record <- function(feature_uuid,
                                  project_system_id,
                                  procedure_id,
                                  start_time,
                                  end_time,
                                  created_by_method,
                                  geometry,
                                  observations) {

# Validate required fields
if (missing(feature_uuid) || is.null(feature_uuid) || feature_uuid == "") {
  stop("feature_uuid is required")
}
if (missing(project_system_id) || is.null(project_system_id)) {
  stop("project_system_id is required")
}
if (missing(procedure_id) || is.null(procedure_id)) {
  stop("procedure_id is required")
}
if (missing(start_time) || is.null(start_time)) {
  stop("start_time is required")
}
if (missing(end_time) || is.null(end_time)) {
  stop("end_time is required")
}
if (missing(created_by_method) || is.null(created_by_method)) {
  stop("created_by_method is required")
}
if (!created_by_method %in% c("drawn", "traced")) {
  stop("created_by_method must be 'drawn' or 'traced'")
}
if (missing(geometry) || is.null(geometry)) {
  stop("geometry is required")
}
if (missing(observations) || is.null(observations)) {
  stop("observations is required")
}

# Validate geometry
if (!is.list(geometry) || !"type" %in% names(geometry)) {
  stop("geometry must be a GeoJSON object with 'type' property")
}
valid_geom_types <- c("Point", "Polygon", "LineString")
if (!geometry$type %in% valid_geom_types) {
  stop("geometry type must be one of: ", paste(valid_geom_types, collapse = ", "))
}

# Build the feature record
feature_record <- list(
  feature_uuid = as.character(feature_uuid),
  project_system_id = as.integer(project_system_id),
  procedure_id = as.integer(procedure_id),
  procedure_start_timestamp = format(start_time, "%Y-%m-%dT%H:%M:%SZ"),
  procedure_end_timestamp = format(end_time, "%Y-%m-%dT%H:%M:%SZ"),
  created_by_method = as.character(created_by_method),
  geometry = geometry,
  observations = observations
)

return(feature_record)
}


#' @title Collect Media Files from Observations
#'
#' @description
#' Extracts media filenames and local paths from observations with media types,
#' for use with the signed-URL upload flow.
#'
#' @param observations List. List of observation records.
#' @param media_dir Character. Path to the directory containing media files.
#'
#' @return A named list keyed by filename. Each element is a list with
#'   \code{filepath}, \code{data_type}, and \code{content_type}.
#'   Returns an empty list if no media files are found.
#'
#' @keywords internal
collect_media_files <- function(observations, media_dir) {

media_types <- c("phone-photo", "phone-video", "phone-audio")
media_files <- list()

for (obs in observations) {
  item_type <- obs$properties$item_type

  if (item_type %in% media_types) {
    # Get the filenames from data
    filenames <- obs$properties$data

    for (filename in filenames) {
      filepath <- file.path(media_dir, filename)

      if (file.exists(filepath)) {
        content_type <- switch(
          item_type,
          "phone-photo" = "image/jpeg",
          "phone-video" = "video/mp4",
          "phone-audio" = "audio/mpeg"
        )

        media_files[[filename]] <- list(
          filepath = filepath,
          data_type = item_type,
          content_type = content_type
        )
      }
    }
  }
}

return(media_files)
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
#' @param media_files Named list from \link{collect_media_files}.
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


#' @title Validate Observation Payload
#'
#' @description
#' Validates the device settings and feature payload before submission to the API.
#' Checks for required fields, valid item types, and verifies media files exist.
#'
#' @param feature_payload List. List of feature records created with \code{build_feature_record()}.
#' @param device_settings List. Device settings created with \code{build_device_settings()}.
#' @param media_dir Character or NULL. Path to directory containing media files.
#'   Required if any observations have media types.
#'
#' @return A list with \code{$valid} (logical) and \code{$errors} (character vector).
#'
#' @examples
#' \dontrun{
#'   validation <- validate_observation_payload(
#'     feature_payload = my_features,
#'     device_settings = my_device,
#'     media_dir = "/path/to/media"
#'   )
#'
#'   if (!validation$valid) {
#'     stop(paste(validation$errors, collapse = "\n"))
#'   }
#' }
#'
#' @author Adam Varley
#' @export
validate_observation_payload <- function(feature_payload, device_settings, media_dir = NULL) {

errors <- character()
media_types <- c("phone-photo", "phone-video", "phone-audio")

# Validate device_settings required fields
device_required <- c("device_id", "phone_model", "phone_operating_system",
                     "carrier", "build_number", "build_id")
missing_device <- setdiff(device_required, names(device_settings))
if (length(missing_device) > 0) {
  errors <- c(errors, paste("Missing device settings fields:",
                            paste(missing_device, collapse = ", ")))
}

# Validate feature_payload is a list
if (!is.list(feature_payload) || length(feature_payload) == 0) {
  errors <- c(errors, "feature_payload must be a non-empty list of feature records")
  return(list(valid = FALSE, errors = errors))
}

# Validate each feature
for (i in seq_along(feature_payload)) {
  feature <- feature_payload[[i]]
  feature_id <- feature$feature_uuid %||% paste("Feature", i)

  # Check required feature fields
  feature_required <- c("feature_uuid", "project_system_id", "procedure_id",
                        "procedure_start_timestamp", "procedure_end_timestamp",
                        "created_by_method", "geometry", "observations")
  missing_feature <- setdiff(feature_required, names(feature))
  if (length(missing_feature) > 0) {
    errors <- c(errors, paste0("[", feature_id, "] Missing fields: ",
                               paste(missing_feature, collapse = ", ")))
  }

  # Validate created_by_method
  if (!is.null(feature$created_by_method) &&
      !feature$created_by_method %in% c("drawn", "traced")) {
    errors <- c(errors, paste0("[", feature_id, "] created_by_method must be 'drawn' or 'traced'"))
  }

  # Validate geometry
  if (!is.null(feature$geometry)) {
    if (!is.list(feature$geometry) || !"type" %in% names(feature$geometry)) {
      errors <- c(errors, paste0("[", feature_id, "] geometry must be a valid GeoJSON object"))
    } else if (!feature$geometry$type %in% c("Point", "Polygon", "LineString")) {
      errors <- c(errors, paste0("[", feature_id, "] geometry type must be Point, Polygon, or LineString"))
    }
  }

  # Validate observations
  if (!is.null(feature$observations) && is.list(feature$observations)) {
    for (j in seq_along(feature$observations)) {
      obs <- feature$observations[[j]]
      obs_id <- obs$properties$observation_uuid %||% paste("Observation", j)

      # Check item_type
      item_type <- obs$properties$item_type
      if (is.null(item_type)) {
        errors <- c(errors, paste0("[", feature_id, "/", obs_id, "] item_type is required"))
      } else if (!item_type %in% phone_types) {
        errors <- c(errors, paste0("[", feature_id, "/", obs_id, "] Invalid item_type '",
                                   item_type, "'. Must be one of: ",
                                   paste(phone_types, collapse = ", ")))
      }

      # Validate observation geometry
      if (!is.null(obs$geometry)) {
        if (!is.list(obs$geometry) || !"type" %in% names(obs$geometry)) {
          errors <- c(errors, paste0("[", feature_id, "/", obs_id,
                                     "] observation geometry must be a valid GeoJSON object"))
        }
      }

      # Check media files exist
      if (!is.null(item_type) && item_type %in% media_types) {
        if (is.null(media_dir)) {
          errors <- c(errors, paste0("[", feature_id, "/", obs_id,
                                     "] media_dir is required for media type observations"))
        } else {
          filenames <- obs$properties$data
          for (filename in filenames) {
            filepath <- file.path(media_dir, filename)
            if (!file.exists(filepath)) {
              errors <- c(errors, paste0("[", feature_id, "/", obs_id,
                                         "] Media file not found: ", filepath))
            }
          }
        }
      }
    }
  }
}

return(list(
  valid = length(errors) == 0,
  errors = errors
))
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
  response <- tryCatch(
    httr2::req_perform(urlreq),
    error = function(e) {
      req_url <- urlreq$url
      stop(
        paste0(
          "Failed to fetch project schema from ", req_url, ". ",
          "If you are running locally, confirm the endpoint exists: ",
          "GET /api/getProjectSchema/{api_key}. Original error: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  return(httr2::resp_body_json(response))
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
#' @return A data frame (invisibly) with columns \code{system_index},
#'   \code{system_name}, \code{system_id}, \code{procedure_index},
#'   \code{procedure_name}, and \code{procedure_id}. The data frame is also
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
    return(invisible(data.frame()))
  }

  rows <- list()
  for (si in seq_along(schema$systems)) {
    sys      <- schema$systems[[si]]
    sys_name <- as.character(sys$system_name %||% "")
    sys_id   <- if (!is.null(sys$project_system_id)) as.integer(sys$project_system_id) else NA_integer_

    if (is.null(sys$procedures) || length(sys$procedures) == 0) {
      rows[[length(rows) + 1]] <- data.frame(
        system_index    = si,
        system_name     = sys_name,
        system_id       = sys_id,
        procedure_index = NA_integer_,
        procedure_name  = NA_character_,
        procedure_id    = NA_integer_,
        form            = NA,
        stringsAsFactors = FALSE
      )
      next
    }

    for (pi in seq_along(sys$procedures)) {
      proc      <- sys$procedures[[pi]]
      proc_name <- as.character(proc$procedure_name %||% "")
      proc_id   <- if (!is.null(proc$procedure_id)) as.integer(proc$procedure_id) else NA_integer_
      proc_form <- if (!is.null(proc$form)) as.logical(proc$form) else NA

      rows[[length(rows) + 1]] <- data.frame(
        system_index    = si,
        system_name     = sys_name,
        system_id       = sys_id,
        procedure_index = pi,
        procedure_name  = proc_name,
        procedure_id    = proc_id,
        form            = proc_form,
        stringsAsFactors = FALSE
      )
    }
  }

  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  print(result)
  return(invisible(result))
}


#' @title Describe Procedure Items
#'
#' @description
#' Returns a detailed table of all items (fields) in a selected procedure,
#' including item names, UUIDs, types, and valid choices where applicable.
#' Use this to understand exactly what data to submit and how to structure it
#' before calling \code{build_upload_observation()} or
#' \code{upload_observations_from_csv()}.
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
#'     \item{items}{Data frame with one row per item: \code{item_id}, \code{item_uuid},
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

    # Resolve display name
    item_name <- node$item_name
    if (is.null(item_name) || identical(item_name, "")) item_name <- node$name
    if (is.null(item_name) || identical(item_name, "")) item_name <- node$label
    if (is.null(item_name) || identical(item_name, "")) item_name <- node$title

    # Resolve type
    item_type <- as.character(node$item_type %||% node$type %||% "")

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

    # Resolve required flag
    req_val <- node$required %||% node$is_required
    if (is.null(req_val)) {
      req_str <- ""
    } else if (is.logical(req_val)) {
      req_str <- ifelse(isTRUE(req_val), "yes", "no")
    } else {
      req_str <- as.character(req_val)
    }

    tibble::tibble(
      item_id          = if (!is.null(node$item_id)) as.integer(node$item_id) else NA_integer_,
      item_uuid        = as.character(node$item_uuid),
      item_name        = as.character(item_name %||% ""),
      item_description = as.character(node$item_description %||% NA_character_),
      data_type        = as.character(node$data_type %||% ""),
      nullable         = if (!is.null(node$nullable)) as.logical(node$nullable) else NA,
      choices          = choices_str,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, rows)
  rownames(result) <- NULL

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
#' @return (Invisibly) \code{observation_data}, with two columns added:
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

  valid <- length(resolved$issues) == 0 && all(status == "success")

  # ---- Print summary -------------------------------------------------------
  message("\n--- CSV Validation Report ---")
  message("System: ", proc_system_name, " (id: ", proc_system_id, ")")
  message("Procedure: ", proc_proc_name, " (id: ", proc_procedure_id, ")")

  message("Matched items (", nrow(resolved$matched_items), "/", nrow(matchable_items), "):")
  if (nrow(resolved$matched_items) > 0) {
    print(resolved$matched_items[, c("item_name", "data_type"), drop = FALSE])
  }
  if (nrow(label_items) > 0) {
    message("\nLabel item(s) (checked via taxonomic rank columns - species/genus/family/order/class/phylum/kingdom - not by name):")
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

  result <- observation_data
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
    message("Note: ", nrow(optional_missing), " nullable item(s) absent from CSV (allowed): ",
            paste(optional_missing$item_name, collapse = ", "))
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



#' @title Build Observation Record
#'
#' @description
#' Builds a single \code{RObservationRecord} payload ready for
#' \code{upload_observations()}, using a procedure list returned by
#' \code{get_procedure()} rather than a raw schema. This is the preferred
#' builder when you have already called \code{get_procedure()}, as it
#' carries \code{system_id} and \code{procedure_id} directly.
#'
#' @param procedure Named list returned by \code{get_procedure()}.
#' @param values Named list of values keyed by item UUID
#'   (\code{item_uuid -> value}).
#' @param recorded_at Character or POSIXct. ISO-8601 timestamp when the
#'   observation was made.
#' @param lon Numeric WGS-84 longitude (-180 to 180).
#' @param lat Numeric WGS-84 latitude (-90 to 90).
#' @param survey_uuid Character or NULL. Optional client-generated UUID used
#'   as an idempotency key. A new UUID is generated automatically when NULL.
#'
#' @return A named list conforming to \code{RObservationRecord}:
#'   \code{survey_uuid}, \code{project_system_id}, \code{procedure_id},
#'   \code{recorded_at}, \code{lon}, \code{lat}, \code{values}.
#'
#' @examples
#' \dontrun{
#'   hdr       <- auth_headers("your_api_key")
#'   schema    <- get_project_systems(hdr)
#'   procedure <- get_procedure(schema,
#'     system_name = "Plante Ivindo", procedure_name = "Arbre")
#'
#'   rec <- build_observation_record(
#'     procedure   = procedure,
#'     values      = list("item-uuid-here" = "Roe Deer"),
#'     recorded_at = "2024-06-01T09:00:00Z",
#'     lon         = 13.703612,
#'     lat         = 0.931838
#'   )
#'   resp <- upload_observations(hdr, list(rec))
#' }
#'
#' @author Adam Varley
#' @export
build_observation_record <- function(procedure,
                                     values,
                                     recorded_at,
                                     lon,
                                     lat,
                                     survey_uuid = NULL) {

  if (!is.list(procedure) || is.null(procedure$system_id) || is.null(procedure$procedure_id)) {
    stop("procedure must be a named list returned by get_procedure()")
  }

  if (missing(values) || is.null(values) || length(values) == 0) {
    stop("values must be a non-empty named list keyed by item UUID")
  }
  if (is.null(names(values)) || any(names(values) == "")) {
    stop("values must be named with item UUID keys")
  }

  if (missing(recorded_at) || is.null(recorded_at)) {
    stop("recorded_at is required")
  }
  if (inherits(recorded_at, "POSIXt")) {
    recorded_at <- format(recorded_at, "%Y-%m-%dT%H:%M:%SZ")
  } else {
    recorded_at <- as.character(recorded_at)
  }

  if (missing(lon) || !is.numeric(lon) || length(lon) != 1 || lon < -180 || lon > 180) {
    stop("lon must be a single numeric value between -180 and 180")
  }
  if (missing(lat) || !is.numeric(lat) || length(lat) != 1 || lat < -90 || lat > 90) {
    stop("lat must be a single numeric value between -90 and 90")
  }

  if (is.null(survey_uuid)) {
    survey_uuid <- uuid::UUIDgenerate()
  }

  list(
    survey_uuid       = as.character(survey_uuid),
    project_system_id = as.integer(procedure$system_id),
    procedure_id      = as.integer(procedure$procedure_id),
    recorded_at       = recorded_at,
    lon               = as.numeric(lon),
    lat               = as.numeric(lat),
    values            = as.list(values)
  )
}


#' @title Build Upload Observation
#'
#' @description
#' Builds a single observation payload for the `uploadObservations` endpoint,
#' where `values` is keyed by item UUID.
#'
#' @param schema List. Project schema returned by \code{get_project_systems()}.
#' @param values Named list/vector of values keyed by item UUID.
#' @param recorded_at Character or POSIXct. Timestamp in ISO-8601 format,
#'   e.g. `"2024-06-01T09:00:00Z"`.
#' @param lon Numeric longitude.
#' @param lat Numeric latitude.
#' @param system_index Integer index of the system in schema$systems. Default `1`.
#' @param procedure_index Integer index of the procedure in selected system.
#'   Default `1`.
#'
#' @return A named list representing one observation row for upload.
#'
#' @examples
#' \dontrun{
#'   schema <- get_project_systems(hdr)
#'   obs <- build_upload_observation(
#'     schema = schema,
#'     values = list("item-uuid-here" = "Roe Deer"),
#'     recorded_at = "2024-06-01T09:00:00Z",
#'     lon = -1.543,
#'     lat = 51.761
#'   )
#' }
#'
#' @author Adam Varley
#' @export
build_upload_observation <- function(schema,
                                     values,
                                     recorded_at,
                                     lon,
                                     lat,
                                     system_index = 1,
                                     procedure_index = 1) {

  if (missing(schema) || is.null(schema)) {
    stop("schema is required")
  }

  if (missing(values) || is.null(values) || length(values) == 0) {
    stop("values must be a non-empty named list or vector keyed by item UUID")
  }

  if (is.null(names(values)) || any(names(values) == "")) {
    stop("values must be named with item UUID keys")
  }

  if (missing(recorded_at) || is.null(recorded_at)) {
    stop("recorded_at is required")
  }

  if (inherits(recorded_at, "POSIXt")) {
    recorded_at <- format(recorded_at, "%Y-%m-%dT%H:%M:%SZ")
  } else {
    recorded_at <- as.character(recorded_at)
  }

  if (missing(lon) || !is.numeric(lon) || length(lon) != 1) {
    stop("lon must be a single numeric value")
  }

  if (missing(lat) || !is.numeric(lat) || length(lat) != 1) {
    stop("lat must be a single numeric value")
  }

  if (is.null(schema$systems) || length(schema$systems) < system_index) {
    stop("system_index is out of bounds for schema$systems")
  }

  system <- schema$systems[[system_index]]
  if (is.null(system$procedures) || length(system$procedures) < procedure_index) {
    stop("procedure_index is out of bounds for selected system$procedures")
  }

  procedure <- system$procedures[[procedure_index]]

  return(list(
    project_system_id = system$project_system_id,
    procedure_id = procedure$procedure_id,
    recorded_at = recorded_at,
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    values = as.list(values)
  ))
}


#' @title Upload Observations
#'
#' @description
#' Uploads one or more observations to the `uploadObservations` endpoint.
#'
#' @param hdr A base URL and API key returned by \link{auth_headers} or
#'   \link{auth_headers_dev}.
#' @param observations List of observations created with
#'   \code{build_upload_observation()}.
#' @param dry_run_payload Logical. If \code{TRUE}, returns the request payload
#'   without sending it to the API. Default \code{FALSE}.
#'
#' @return Parsed API response as a list.
#'
#' @examples
#' \dontrun{
#'   hdr <- auth_headers("your_api_key")
#'   schema <- get_project_systems(hdr)
#'   obs <- build_upload_observation(
#'     schema = schema,
#'     values = list("item-uuid-here" = "Roe Deer"),
#'     recorded_at = "2024-06-01T09:00:00Z",
#'     lon = -1.543,
#'     lat = 51.761
#'   )
#'   resp <- upload_observations(hdr, list(obs))
#' }
#'
#' @author Adam Varley
#' @export
upload_observations <- function(hdr, observations, dry_run_payload = FALSE) {

  if (missing(observations) || is.null(observations) || length(observations) == 0) {
    stop("observations must be a non-empty list")
  }

  body <- list(observations = observations)

  # Allows caller to inspect the exact JSON before sending
  if (isTRUE(dry_run_payload)) {
    cat(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
    return(invisible(body))
  }

  urlreq <- httr2::req_url_path_append(hdr$root, "uploadObservations", hdr$key) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_error(is_error = \(r) FALSE)  # never throw; we inspect the body ourselves

  response <- httr2::req_perform(urlreq)
  status   <- httr2::resp_status(response)

  if (status >= 400) {
    body_text <- tryCatch(
      httr2::resp_body_string(response),
      error = function(e) "<could not read response body>"
    )
    stop(sprintf(
      "HTTP %d from uploadObservations.\nResponse body:\n%s",
      status, body_text
    ))
  }

  return(httr2::resp_body_json(response))
}


# Internal helper to normalize lookup strings.
normalize_lookup_value <- function(x) {
  out <- tolower(trimws(as.character(x)))
  out <- iconv(out, from = "", to = "ASCII//TRANSLIT")
  out[is.na(out)] <- ""
  return(out)
}




# Internal helper to coerce a text value to a numeric when the target item's
# data_type calls for one, so numeric fields are sent as JSON numbers rather
# than strings (the API rejects e.g. "20" for a numeric item).
coerce_value_by_data_type <- function(value, data_type) {
  if (is.null(data_type) || is.na(data_type) || !nzchar(data_type)) {
    return(value)
  }
  if (grepl("num|int|float|double|decimal|real", data_type)) {
    numeric_candidate <- suppressWarnings(as.numeric(value))
    if (!is.na(numeric_candidate)) {
      return(numeric_candidate)
    }
  }
  return(value)
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
      tolower(trimws(as.character(rows$label))) == tolower(val) |
      tolower(trimws(paste(rows$genus, rows$species))) == tolower(val)
    )
    cli::cli_progress_update(id = pb, inc = 1)
    if (i < length(unique_values)) Sys.sleep(0.2)  # throttle to avoid tripping the API's rate limit
  }

  stats::setNames(found, unique_values)
}


# Internal: TRUE if a non-blank cell value matches what its item's declared
# data_type requires (numeric/boolean types only - text/choice/instruction
# accept anything). Media (phone-photo/video/audio) and label types are
# validated separately (file existence, label database).
.value_matches_data_type <- function(value, data_type) {
  dt <- data_type %||% ""
  if (grepl("num|int|float|double|decimal|real", dt)) {
    return(!is.na(suppressWarnings(as.numeric(value))))
  }
  if (grepl("bool", dt)) {
    return(tolower(value) %in% c("true", "false", "yes", "no", "1", "0"))
  }
  TRUE
}


# The API's uploadObservations endpoint requires the outgoing "recorded_at"
# field (a fixed name in the API contract - not to be renamed) to be strict
# ISO-8601 UTC: yyyy-mm-ddThh:mm:ssZ. Incoming timestamps are parsed via
# lubridate::as_datetime() (any format it recognises) and reformatted to
# that strict string; values that don't parse become NA_character_ so the
# caller can reject just that row/observation.
.parse_timestamps <- function(raw_values) {
  parsed <- suppressWarnings(lubridate::as_datetime(as.character(raw_values), tz = "UTC"))
  ifelse(is.na(parsed), NA_character_, strftime(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
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


# Internal helper to create an item dictionary from schema for a selected procedure.
get_schema_item_dictionary <- function(schema, system_index = 1, procedure_index = 1) {
  if (is.null(schema$systems) || length(schema$systems) < system_index) {
    stop("system_index is out of bounds for schema$systems")
  }

  system <- schema$systems[[system_index]]
  if (is.null(system$procedures) || length(system$procedures) < procedure_index) {
    stop("procedure_index is out of bounds for selected system$procedures")
  }

  procedure <- system$procedures[[procedure_index]]
  item_nodes <- collect_item_nodes(procedure)

  if (length(item_nodes) == 0) {
    return(data.frame(
      item_uuid = character(),
      item_name = character(),
      data_type = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(item_nodes, function(node) {
    item_name <- node$item_name
    if (is.null(item_name) || identical(item_name, "")) item_name <- node$name
    if (is.null(item_name) || identical(item_name, "")) item_name <- node$label
    if (is.null(item_name) || identical(item_name, "")) item_name <- node$title

    data.frame(
      item_uuid = as.character(node$item_uuid),
      item_name = as.character(item_name %||% ""),
      data_type = as.character(node$data_type %||% ""),
      stringsAsFactors = FALSE
    )
  })

  dictionary <- do.call(rbind, rows)
  dictionary <- dictionary[dictionary$item_uuid != "", , drop = FALSE]
  dictionary <- dictionary[!duplicated(dictionary$item_uuid), , drop = FALSE]
  rownames(dictionary) <- NULL
  return(dictionary)
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
#' payloads for \code{upload_observations()}, automatically resolving item
#' UUIDs from project schema when needed.
#'
#' Supply either a \code{procedure} list (returned by \code{get_procedure()})
#' or a raw \code{schema} with \code{system_name}/\code{procedure_name}.
#' When \code{procedure} is provided it takes precedence.
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
#' @param procedure Named list returned by \code{get_procedure()}. Takes
#'   precedence over \code{schema} when provided.
#' @param schema Project schema returned by \code{get_project_systems()}.
#'   Used only when \code{procedure} is \code{NULL}.
#' @param system_index Integer index of target system in schema. Optional.
#' @param procedure_index Integer index of target procedure in selected system.
#'   Optional.
#' @param system_name Character system name used to resolve system index.
#'   Optional.
#' @param procedure_name Character procedure name used to resolve procedure
#'   index. Optional.
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
#'   df <- read_csv("tutorials/example_observation_data.csv",
#'                  stringsAsFactors = FALSE)
#'   built <- build_upload_observations_from_table(
#'     data      = df,
#'     procedure = procedure
#'   )
#' }
#'
#' @author Adam Varley
#' @export
build_upload_observations_from_table <- function(data,
                                                 procedure = NULL,
                                                 schema = NULL,
                                                 system_index = NULL,
                                                 procedure_index = NULL,
                                                 system_name = NULL,
                                                 procedure_name = NULL,
                                                 lon_col = "longitude",
                                                 lat_col = "latitude",
                                                 timestamp_col = "timestamp") {

  required_cols <- c(lon_col, lat_col, timestamp_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in data: ", paste(missing_cols, collapse = ", "))
  }

  # Resolve IDs and item dictionary from procedure list or schema
  use_procedure <- !is.null(procedure) && is.list(procedure) && !is.null(procedure$system_id)

  if (use_procedure) {
    sys_id  <- as.integer(procedure$system_id)
    proc_id <- as.integer(procedure$procedure_id)
    dictionary <- procedure$items[, c("item_uuid", "item_name", "data_type"), drop = FALSE]
  } else {
    if (is.null(schema)) stop("Either procedure or schema must be provided")
    idx <- resolve_schema_indices(
      schema = schema,
      system_index = system_index,
      procedure_index = procedure_index,
      system_name = system_name,
      procedure_name = procedure_name
    )
    dictionary <- get_schema_item_dictionary(
      schema = schema,
      system_index = idx$system_index,
      procedure_index = idx$procedure_index
    )
    sys_obj  <- schema$systems[[idx$system_index]]
    proc_obj <- sys_obj$procedures[[idx$procedure_index]]
    sys_id   <- as.integer(sys_obj$project_system_id)
    proc_id  <- as.integer(proc_obj$procedure_id)
  }

  .build_wide_observations(
    data = data, dictionary = dictionary, sys_id = sys_id, proc_id = proc_id,
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

  iso_timestamp <- .parse_timestamps(data[[timestamp_col]])
  lon_vals <- suppressWarnings(as.numeric(data[[lon_col]]))
  lat_vals <- suppressWarnings(as.numeric(data[[lat_col]]))

  # Phase 1: assess every row independently (no network yet).
  rows <- lapply(seq_len(nrow(data)), function(r) {
    reasons    <- character()
    row_values <- list()
    row_media  <- list()

    if (is.na(lon_vals[r]))       reasons <- c(reasons, paste0("longitude '", data[[lon_col]][[r]], "' is not a valid decimal"))
    if (is.na(lat_vals[r]))       reasons <- c(reasons, paste0("latitude '", data[[lat_col]][[r]], "' is not a valid decimal"))
    if (is.na(iso_timestamp[r]))  reasons <- c(reasons, paste0("timestamp '", data[[timestamp_col]][[r]], "' could not be parsed"))

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

  unresolved_rows <- data.frame(
    item_name = unresolved_cols,
    item_uuid = rep(NA_character_, length(unresolved_cols)),
    stringsAsFactors = FALSE
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
  filepaths    <- vapply(media_uploads, function(m) m$filepath, character(1))
  filenames    <- basename(filepaths)

  media_files <- stats::setNames(
    lapply(filepaths, function(p) {
      m <- media_uploads[[match(p, filepaths)]]
      list(filepath = p, data_type = m$data_type, content_type = m$content_type)
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
#' @param validated The data frame returned by
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
#'   validated <- validate_csv_against_procedure(
#'     procedure = procedure,
#'     csv_path  = "tutorials/example_observation_data.csv",
#'     hdr       = hdr
#'   )
#'
#'   result <- upload_observations_from_csv(hdr = hdr, validated = validated)
#' }
#'
#' @author Adam Varley
#' @export
upload_observations_from_csv <- function(hdr, validated, dry_run = FALSE) {
  if (!is.data.frame(validated) || !".payload" %in% names(validated)) {
    stop("validated must be the data frame returned by validate_csv_against_procedure().")
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

  n_obs <- length(observations)
  cli::cli_progress_bar(
    "Uploading {cli::pb_current}/{cli::pb_total} observations to NatureCube | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    total = n_obs,
    .auto_close = FALSE
  )
  upload_step_ok <- FALSE
  on.exit(cli::cli_progress_done(result = if (upload_step_ok) "done" else "failed"), add = TRUE)

  response <- upload_observations(hdr = hdr, observations = observations)
  upload_step_ok <- TRUE

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


#' @title Upload Phone Observations
#'
#' @description
#' Uploads phone observation records to the NatureCube platform. This function
#' processes one feature at a time. When observations include media
#' (photos, videos, audio), files are uploaded via signed URLs to cloud storage
#' first, then observation metadata is submitted (matching the mobile app flow).
#'
#' The function loops through all features in the payload, providing progress messages
#' and collecting any errors that occur. Partial failures do not stop the upload process;
#' instead, errors are collected and returned in the summary.
#'
#' @param hdr A base URL and API key returned by \link{auth_headers} or \link{auth_headers_dev}.
#' @param feature_payload List. A list of feature records created with \code{build_feature_record()}.
#' @param device_settings List. Device settings created with \code{build_device_settings()}.
#' @param media_dir Character or NULL. Path to the directory containing media files.
#'   Required if any observations include media types (photo, video, audio).
#' @param validate Logical. Whether to validate the payload before uploading. Default is TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{successes}{List of successfully uploaded feature UUIDs with their responses}
#'     \item{failures}{List of failed feature UUIDs with their error messages}
#'     \item{summary}{Character string summarizing the upload results}
#'   }
#'
#' @examples
#' \dontrun{
#'   # Set up authentication
#'   hdr <- auth_headers("your_api_key")
#'
#'   # Build device settings
#'   device <- build_device_settings(
#'     device_id = "device-123",
#'     phone_model = "iPhone 14",
#'     phone_os = "iOS 17",
#'     carrier = "Vodafone",
#'     build_number = "1.0.0",
#'     build_id = "build-001"
#'   )
#'
#'   # Build an observation
#'   obs1 <- build_observation(
#'     item_uuid = "item-uuid-1",
#'     item_type = "text",
#'     data = list("My observation text"),
#'     geometry = list(type = "Point", coordinates = c(-1.5, 53.4))
#'   )
#'
#'   # Build a feature with observations
#'   feature1 <- build_feature_record(
#'     feature_uuid = "feature-uuid-1",
#'     project_system_id = 10,
#'     procedure_id = 5,
#'     start_time = Sys.time() - 3600,
#'     end_time = Sys.time(),
#'     created_by_method = "drawn",
#'     geometry = list(type = "Point", coordinates = c(-1.5, 53.4)),
#'     observations = list(obs1)
#'   )
#'
#'   # Upload
#'   result <- upload_phone_observations(
#'     hdr = hdr,
#'     feature_payload = list(feature1),
#'     device_settings = device
#'   )
#'
#'   print(result$summary)
#' }
#'
#' @author Adam Varley
#' @export
upload_phone_observations <- function(hdr,
                                        feature_payload,
                                        device_settings,
                                        media_dir = NULL,
                                        validate = TRUE) {

# Validate inputs if requested
if (validate) {
  validation <- validate_observation_payload(feature_payload, device_settings, media_dir)
  if (!validation$valid) {
    stop("Validation failed:\n", paste(validation$errors, collapse = "\n"))
  }
}

# Initialize result containers
successes <- list()
failures <- list()

n_features <- length(feature_payload)
message("Starting upload of ", n_features, " feature(s)...")

# Process each feature one at a time
for (i in seq_along(feature_payload)) {
  feature <- feature_payload[[i]]
  feature_uuid <- feature$feature_uuid

  message("Uploading feature ", i, " of ", n_features, " (", feature_uuid, ")...")

  tryCatch({
    # Build the payload for this single feature
    # Wrap in a list as the API expects feature_payload to be an array
    single_feature_payload <- list(feature)

    device_upload <- list(
      feature_payload = single_feature_payload,
      device_settings = device_settings
    )

    # Upload media via signed URLs before metadata push
    if (!is.null(media_dir) && !is.null(feature$observations)) {
      media_files <- collect_media_files(feature$observations, media_dir)
      if (length(media_files) > 0) {
        message("  Uploading ", length(media_files), " media file(s) via signed URLs...")
        upload_field_media_files(hdr, media_files)
      }
    }

    # Metadata-only push — form field "data" (SchemaChecker)
    json_payload <- jsonlite::toJSON(device_upload, auto_unbox = TRUE)
    urlreq <- httr2::req_url_path_append(
      hdr$root,
      "pushPhoneObservations",
      hdr$key
    )
    urlreq <- urlreq |>
      httr2::req_method("POST") |>
      httr2::req_body_multipart(data = json_payload)

    # Perform the request
    response <- httr2::req_perform(urlreq)
    resp_body <- httr2::resp_body_json(response)

    # Record success
    successes[[feature_uuid]] <- list(
      feature_uuid = feature_uuid,
      response = resp_body
    )

    message("  \u2713 Feature ", feature_uuid, " uploaded successfully")

  }, error = function(e) {
    # Record failure
    error_msg <- conditionMessage(e)
    failures[[feature_uuid]] <<- list(
      feature_uuid = feature_uuid,
      error = error_msg
    )

    message("  \u2717 Feature ", feature_uuid, " failed: ", error_msg)
  })
}

# Build summary
n_success <- length(successes)
n_failed <- length(failures)
summary_msg <- sprintf(
  "Upload complete: %d of %d features uploaded successfully, %d failed",
  n_success, n_features, n_failed
)

message(summary_msg)

# Return results
return(list(
  successes = successes,
  failures = failures,
  summary = summary_msg
))
}
