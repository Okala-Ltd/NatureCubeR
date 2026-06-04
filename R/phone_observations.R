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
