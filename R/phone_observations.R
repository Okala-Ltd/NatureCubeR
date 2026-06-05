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
#' required by the Okala API.
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
#'     geometry = list(type = "Polygon", coordinates = list(list(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
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
#' Extracts media file paths from observations with media types and prepares them
#' for multipart upload.
#'
#' @param observations List. List of observation records.
#' @param media_dir Character. Path to the directory containing media files.
#'
#' @return A named list of curl::form_file objects ready for multipart upload,
#'   or an empty list if no media files are found.
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
        # Determine MIME type based on item_type
        mime_type <- switch(
          item_type,
          "phone-photo" = "image/jpeg",
          "phone-video" = "video/mp4",
          "phone-audio" = "audio/mpeg"
        )

        media_files[[filename]] <- curl::form_file(filepath, type = mime_type)
      }
    }
  }
}

return(media_files)
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
#'   schema <- get_project_schema(hdr)
#' }
#'
#' @author Adam Varley
#' @export
get_project_schema <- function(hdr) {
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
#' @param schema List. Project schema returned by \code{get_project_schema()}.
#'
#' @return A data frame (invisibly) with columns \code{system_index},
#'   \code{system_name}, \code{system_id}, \code{procedure_index},
#'   \code{procedure_name}, and \code{procedure_id}. The data frame is also
#'   printed to the console.
#'
#' @examples
#' \dontrun{
#'   hdr <- auth_headers("your_api_key")
#'   schema <- get_project_schema(hdr)
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
        stringsAsFactors = FALSE
      )
      next
    }

    for (pi in seq_along(sys$procedures)) {
      proc      <- sys$procedures[[pi]]
      proc_name <- as.character(proc$procedure_name %||% "")
      proc_id   <- if (!is.null(proc$procedure_id)) as.integer(proc$procedure_id) else NA_integer_

      rows[[length(rows) + 1]] <- data.frame(
        system_index    = si,
        system_name     = sys_name,
        system_id       = sys_id,
        procedure_index = pi,
        procedure_name  = proc_name,
        procedure_id    = proc_id,
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
#' @param schema List. Project schema returned by \code{get_project_schema()}.
#' @param system_name Character. Name of the system. Optional; takes precedence
#'   over \code{system_index} when provided.
#' @param system_index Integer. Index of the system. Default \code{NULL}
#'   (resolves to 1 if \code{system_name} is also absent).
#' @param procedure_name Character. Name of the procedure. Optional; takes
#'   precedence over \code{procedure_index} when provided.
#' @param procedure_index Integer. Index of the procedure. Default \code{NULL}
#'   (resolves to 1 if \code{procedure_name} is also absent).
#'
#' @return A data frame (invisibly) with one row per item and columns
#'   \code{order}, \code{item_name}, \code{item_type}, \code{item_uuid},
#'   \code{required}, and \code{choices}. The data frame is also printed to
#'   the console.
#'
#' @examples
#' \dontrun{
#'   hdr <- auth_headers("your_api_key")
#'   schema <- get_project_schema(hdr)
#'
#'   # Explore using names
#'   describe_procedure(schema,
#'     system_name = "Plante Ivindo",
#'     procedure_name = "Arbre")
#'
#'   # Or by index
#'   describe_procedure(schema, system_index = 1, procedure_index = 2)
#' }
#'
#' @author Adam Varley
#' @export
describe_procedure <- function(schema,
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
    return(invisible(data.frame()))
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

    data.frame(
      order     = i,
      item_name = as.character(item_name %||% ""),
      item_type = item_type,
      item_uuid = as.character(node$item_uuid),
      required  = req_str,
      choices   = choices_str,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, rows)
  rownames(result) <- NULL

  sys_name  <- as.character(system$system_name %||% paste("System", idx$system_index))
  proc_name <- as.character(procedure$procedure_name %||% paste("Procedure", idx$procedure_index))

  message("System: ", sys_name)
  message("Procedure: ", proc_name)
  message("Items (", nrow(result), "):")
  print(result)
  return(invisible(result))
}


#' @title Build Upload Observation
#'
#' @description
#' Builds a single observation payload for the `uploadObservations` endpoint,
#' where `values` is keyed by item UUID.
#'
#' @param schema List. Project schema returned by \code{get_project_schema()}.
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
#'   schema <- get_project_schema(hdr)
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
#'
#' @return Parsed API response as a list.
#'
#' @examples
#' \dontrun{
#'   hdr <- auth_headers("your_api_key")
#'   schema <- get_project_schema(hdr)
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
upload_observations <- function(hdr, observations) {

  if (missing(observations) || is.null(observations) || length(observations) == 0) {
    stop("observations must be a non-empty list")
  }

  urlreq <- httr2::req_url_path_append(hdr$root, "uploadObservations", hdr$key) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(list(observations = observations))

  response <- httr2::req_perform(urlreq)
  return(httr2::resp_body_json(response))
}


# Internal helper to normalize lookup strings.
normalize_lookup_value <- function(x) {
  out <- tolower(trimws(as.character(x)))
  out <- iconv(out, from = "", to = "ASCII//TRANSLIT")
  out[is.na(out)] <- ""
  return(out)
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
#' Converts a flat data frame into a list of observations for
#' \code{upload_observations()}, automatically resolving item UUIDs from
#' project schema when needed.
#'
#' @param data Data frame of observation rows.
#' @param schema Project schema returned by \code{get_project_schema()}.
#' @param system_index Integer index of target system in schema. Optional.
#' @param procedure_index Integer index of target procedure in selected system.
#'   Optional.
#' @param system_name Character system name used to resolve system index.
#'   Optional.
#' @param procedure_name Character procedure name used to resolve procedure
#'   index. Optional.
#' @param lon_col Character name of longitude column. Default `"longitude"`.
#' @param lat_col Character name of latitude column. Default `"latitude"`.
#' @param recorded_at_col Character name of recorded timestamp column.
#'   Default `"recorded_at"`.
#' @param item_uuid_col Character name of item UUID column.
#'   Default `"item_uuid"`.
#' @param item_name_col Character name of item name column.
#'   Default `"item_name"`.
#' @param value_col Character name of primary value column. Default `"data"`.
#' @param numeric_value_col Character name of numeric fallback value column.
#'   Default `"numbers"`.
#' @param observation_id_col Character name of observation ID column.
#'   Default `"observation_id"`.
#' @param recorded_at_format Optional timestamp parsing format.
#'   Default `"%d/%m/%Y %H:%M"`.
#' @param timezone Timezone used when parsing non-ISO timestamps. Default `"UTC"`.
#'
#' @return A list with `observations`, `unresolved_rows`, `resolved_rows`, and
#'   schema selection metadata.
#'
#' @examples
#' \dontrun{
#'   hdr <- auth_headers("your_api_key")
#'   schema <- get_project_schema(hdr)
#'   df <- read.csv("tutorials/example_observation_data .csv", stringsAsFactors = FALSE)
#'   built <- build_upload_observations_from_table(
#'     data = df,
#'     schema = schema,
#'     system_name = "Plante Ivindo",
#'     procedure_name = "Arbre"
#'   )
#' }
#'
#' @author Adam Varley
#' @export
build_upload_observations_from_table <- function(data,
                                                 schema,
                                                 system_index = NULL,
                                                 procedure_index = NULL,
                                                 system_name = NULL,
                                                 procedure_name = NULL,
                                                 lon_col = "longitude",
                                                 lat_col = "latitude",
                                                 recorded_at_col = "recorded_at",
                                                 item_uuid_col = "item_uuid",
                                                 item_name_col = "item_name",
                                                 value_col = "data",
                                                 numeric_value_col = "numbers",
                                                 observation_id_col = "observation_id",
                                                 recorded_at_format = "%d/%m/%Y %H:%M",
                                                 timezone = "UTC") {

  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("data must be a non-empty data frame")
  }

  required_cols <- c(lon_col, lat_col, recorded_at_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in data: ", paste(missing_cols, collapse = ", "))
  }

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

  data <- data
  data[[item_uuid_col]] <- as.character(data[[item_uuid_col]] %||% "")
  data[[item_name_col]] <- as.character(data[[item_name_col]] %||% "")
  data[[value_col]] <- as.character(data[[value_col]] %||% "")

  if (!numeric_value_col %in% names(data)) {
    data[[numeric_value_col]] <- NA
  }

  if (!observation_id_col %in% names(data)) {
    data[[observation_id_col]] <- ""
  }

  name_lookup <- stats::setNames(
    dictionary$item_uuid,
    normalize_lookup_value(dictionary$item_name)
  )

  resolved_from_name <- name_lookup[normalize_lookup_value(data[[item_name_col]])]
  resolved_from_name[is.na(resolved_from_name)] <- ""

  explicit_uuid <- trimws(data[[item_uuid_col]])
  explicit_uuid[is.na(explicit_uuid)] <- ""

  data$.resolved_item_uuid <- ifelse(explicit_uuid != "", explicit_uuid, resolved_from_name)

  has_value <- trimws(data[[value_col]]) != ""
  numeric_values <- data[[numeric_value_col]]
  has_numeric <- !is.na(numeric_values) & as.character(numeric_values) != ""

  unresolved_mask <- trimws(data$.resolved_item_uuid) == ""
  empty_value_mask <- !(has_value | has_numeric)

  valid_rows <- data[!(unresolved_mask | empty_value_mask), , drop = FALSE]

  if (nrow(valid_rows) == 0) {
    stop("No rows could be converted into observations. Check item UUID/name mapping and values.")
  }

  timestamp_raw <- as.character(valid_rows[[recorded_at_col]])
  timestamp_parsed <- as.POSIXct(timestamp_raw, format = recorded_at_format, tz = timezone)
  iso_timestamp <- ifelse(
    !is.na(timestamp_parsed),
    format(timestamp_parsed, "%Y-%m-%dT%H:%M:%SZ"),
    timestamp_raw
  )

  lon_vals <- as.numeric(valid_rows[[lon_col]])
  lat_vals <- as.numeric(valid_rows[[lat_col]])

  obs_ids <- as.character(valid_rows[[observation_id_col]])
  obs_ids[is.na(obs_ids)] <- ""

  grouping_key <- ifelse(
    trimws(obs_ids) != "",
    paste0("obs-id::", trimws(obs_ids)),
    paste0("coord-time::", round(lon_vals, 7), "::", round(lat_vals, 7), "::", iso_timestamp)
  )

  split_rows <- split(valid_rows, grouping_key)

  observations <- lapply(split_rows, function(chunk) {
    chunk_values <- list()

    for (r in seq_len(nrow(chunk))) {
      row <- chunk[r, , drop = FALSE]
      item_uuid <- as.character(row$.resolved_item_uuid[[1]])
      value_text <- trimws(as.character(row[[value_col]][[1]] %||% ""))
      value_num <- row[[numeric_value_col]][[1]]

      value_to_use <- value_text
      if (identical(value_to_use, "") && !is.na(value_num) && as.character(value_num) != "") {
        value_to_use <- as.numeric(value_num)
      }

      chunk_values[[item_uuid]] <- value_to_use
    }

    recorded_at_value <- as.character(chunk[[recorded_at_col]][[1]])
    recorded_at_parsed <- as.POSIXct(recorded_at_value, format = recorded_at_format, tz = timezone)
    if (!is.na(recorded_at_parsed)) {
      recorded_at_value <- format(recorded_at_parsed, "%Y-%m-%dT%H:%M:%SZ")
    }

    build_upload_observation(
      schema = schema,
      values = chunk_values,
      recorded_at = recorded_at_value,
      lon = as.numeric(chunk[[lon_col]][[1]]),
      lat = as.numeric(chunk[[lat_col]][[1]]),
      system_index = idx$system_index,
      procedure_index = idx$procedure_index
    )
  })

  unresolved_rows <- data[unresolved_mask, c(item_name_col, item_uuid_col), drop = FALSE]

  return(list(
    observations = observations,
    resolved_rows = nrow(valid_rows),
    unresolved_rows = unresolved_rows,
    system_index = idx$system_index,
    procedure_index = idx$procedure_index
  ))
}


#' @title Upload Observations From CSV
#'
#' @description
#' One-call workflow to read a CSV file, fetch schema, build observations, and
#' upload to `uploadObservations`.
#'
#' @param hdr A base URL and API key returned by \link{auth_headers} or
#'   \link{auth_headers_dev}.
#' @param csv_path Path to CSV containing observation rows.
#' @param system_name Character system name used to resolve schema system.
#'   Optional.
#' @param procedure_name Character procedure name used to resolve procedure.
#'   Optional.
#' @param system_index Integer system index fallback. Default `NULL`.
#' @param procedure_index Integer procedure index fallback. Default `NULL`.
#' @param dry_run Logical; if `TRUE`, returns built observations without
#'   uploading. Default `FALSE`.
#' @param ... Additional arguments passed to
#'   \code{build_upload_observations_from_table()}.
#'
#' @return A list with built observations summary and API response when uploaded.
#'
#' @examples
#' \dontrun{
#'   hdr <- auth_headers("your_api_key")
#'   result <- upload_observations_from_csv(
#'     hdr = hdr,
#'     csv_path = "tutorials/example_observation_data .csv",
#'     system_name = "Plante Ivindo",
#'     procedure_name = "Arbre",
#'     dry_run = TRUE
#'   )
#' }
#'
#' @author Adam Varley
#' @export
upload_observations_from_csv <- function(hdr,
                                         csv_path,
                                         system_name = NULL,
                                         procedure_name = NULL,
                                         system_index = NULL,
                                         procedure_index = NULL,
                                         dry_run = FALSE,
                                         ...) {
  if (missing(csv_path) || is.null(csv_path) || !file.exists(csv_path)) {
    stop("csv_path must exist")
  }

  observation_data <- utils::read.csv(
    csv_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )

  schema <- get_project_schema(hdr)

  built <- build_upload_observations_from_table(
    data = observation_data,
    schema = schema,
    system_name = system_name,
    procedure_name = procedure_name,
    system_index = system_index,
    procedure_index = procedure_index,
    ...
  )

  if (isTRUE(dry_run)) {
    return(list(
      uploaded = FALSE,
      observations = built$observations,
      resolved_rows = built$resolved_rows,
      unresolved_rows = built$unresolved_rows,
      system_index = built$system_index,
      procedure_index = built$procedure_index
    ))
  }

  response <- upload_observations(hdr = hdr, observations = built$observations)

  return(list(
    uploaded = TRUE,
    response = response,
    observations = built$observations,
    resolved_rows = built$resolved_rows,
    unresolved_rows = built$unresolved_rows,
    system_index = built$system_index,
    procedure_index = built$procedure_index
  ))
}


#' @title Upload Phone Observations
#'
#' @description
#' Uploads phone observation records to the Okala platform. This function processes
#' one feature at a time, uploading the feature geometry, its child observations,
#' and any associated media files (photos, videos, audio) in a single request per feature.
#'
#' The function loops through all features in the payload, providing progress messages
#' and collecting any errors that occur. Partial failures do not stop the upload process;
#' instead, errors are collected and returned in the summary.
#'
#' @param hdr A base URL and API key returned by \link{auth_headers} or \link{auth_headers_dev}.
#' @param project_id Integer. The ID of the project to upload observations to.
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
#'     project_id = 42,
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
                                        project_id,
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

    # Collect media files for this feature's observations
    media_files <- list()
    if (!is.null(media_dir) && !is.null(feature$observations)) {
      media_files <- collect_media_files(feature$observations, media_dir)
    }

    # Build the request URL
    urlreq <- httr2::req_url_path_append(
      hdr$root,
      "pushObservation",
      hdr$key
    )

    # Add project_id as path parameter
    urlreq <- httr2::req_url_path_append(urlreq, as.character(project_id))

    # Set method
    urlreq <- urlreq |> httr2::req_method("POST")

    # Build request body based on whether we have media files
    if (length(media_files) > 0) {
      # Multipart request with JSON data and files
      json_payload <- jsonlite::toJSON(device_upload, auto_unbox = TRUE)

      # Combine JSON payload with media files
      body_parts <- c(
        list(device_upload = json_payload),
        media_files
      )

      urlreq <- urlreq |> httr2::req_body_multipart(!!!body_parts)
    } else
    {
      # Simple JSON request
      urlreq <- urlreq |> httr2::req_body_json(data = device_upload)
    }

    # Perform the request
    response <- httr2::req_perform(urlreq)
    resp_body <- httr2::resp_body_json(response)

    # Record success
    successes[[feature_uuid]] <- list(
      feature_uuid = feature_uuid,
      response = resp_body
    )

    message("  ✓ Feature ", feature_uuid, " uploaded successfully")

  }, error = function(e) {
    # Record failure
    error_msg <- conditionMessage(e)
    failures[[feature_uuid]] <<- list(
      feature_uuid = feature_uuid,
      error = error_msg
    )

    message("  ✗ Feature ", feature_uuid, " failed: ", error_msg)
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
