# Internal function used to chunk media metadata
send_media_chunks <- function(hdr, datachunk) {
  datachunk <- jsonlite::toJSON(datachunk, pretty = TRUE)

  urlreq_ap <- httr2::req_url_path_append(hdr$root, "updateTimestamps", hdr$key)
  urlreq_ap <- urlreq_ap |> httr2::req_method("PUT") |> httr2::req_body_json(jsonlite::fromJSON(datachunk))
  preq <- httr2::req_perform(urlreq_ap, verbosity = 3)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp))
}


#' @title Update timestamps for multiple media records
#'
#' @description
#' Updates timestamps for one or more media records in a single API call.
#' For large datasets (>1000 records), use \link{push_new_timestamps} instead.
#'
#' @param hdr A base URL provided and valid API key returned by the
#'   function \link{auth_headers}
#' @param media_records A data frame or tibble containing the media records to update.
#'   Must contain the following columns:
#'   \itemize{
#'     \item \strong{media_file_record_id} (required): Numeric. The unique identifier
#'           of the media file record to update.
#'     \item \strong{new_timestamp} (required): Character. The new timestamp in
#'           ISO 8601 format (e.g., "2024-01-15T10:30:00" or "2024-01-15 10:30:00").
#'   }
#'   Additional columns will be ignored.
#'
#' @return A list containing the API response with update status
#'
#' @examples
#' \dontrun{
#'   # Create a data frame with media records to update
#'   updates <- data.frame(
#'     media_file_record_id = c(123, 456, 789),
#'     new_timestamp = c(
#'       "2024-01-15T10:30:00",
#'       "2024-01-15T14:20:00",
#'       "2024-01-15T18:45:00"
#'     )
#'   )
#'   
#'   # Update the timestamps
#'   result <- update_media_timestamps(headers, updates)
#'   
#'   # Using tibble format
#'   library(tibble)
#'   updates <- tibble(
#'     media_file_record_id = c(123, 456),
#'     new_timestamp = c("2024-01-15T10:30:00", "2024-01-15T14:20:00")
#'   )
#'   result <- update_media_timestamps(headers, updates)
#' }
#'
#' @seealso
#' \link{push_new_timestamps} for automatic chunking of large datasets
#'
#' @author
#' Adam Varley
#' @export
update_media_timestamps <- function(hdr, media_records) {
  # Validate required columns
  required_cols <- c("media_file_record_id", "new_timestamp")
  missing_cols <- setdiff(required_cols, names(media_records))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Validate data types
  if (!is.numeric(media_records$media_file_record_id)) {
    stop("media_file_record_id must be numeric")
  }
  if (any(is.na(media_records$media_file_record_id))) {
    stop("media_file_record_id cannot contain NA values")
  }
  if (any(media_records$media_file_record_id <= 0)) {
    stop("media_file_record_id must be positive")
  }
  if (any(media_records$media_file_record_id %% 1 != 0)) {
    stop("media_file_record_id must be an integer")
  }

  if (!is.character(media_records$new_timestamp)) {
    stop("new_timestamp must be character string in ISO 8601 format")
  }

  # Basic ISO 8601 format validation for new_timestamp
  # Accepts patterns like: 2024-01-31T23:59:59Z or 2024-01-31T23:59:59+01:00
  iso8601_pattern <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:?\\d{2})?$"
  invalid_idx <- which(!is.na(media_records$new_timestamp) &
                         !grepl(iso8601_pattern, media_records$new_timestamp))
  if (length(invalid_idx) > 0) {
    stop(
      "new_timestamp must be in ISO 8601 format, e.g. '2024-01-31T23:59:59Z'. ",
      "Invalid values at row(s): ",
      paste(invalid_idx, collapse = ", ")
    )
  }

  # Select only required columns
  media_records <- media_records[, required_cols, drop = FALSE]

  # Make API request
  urlreq_ap <- httr2::req_url_path_append(hdr$root, "updateTimestamps", hdr$key) %>%
    httr2::req_method("PUT") %>%
    httr2::req_body_json(media_records)

  preq <- tryCatch(
    httr2::req_perform(urlreq_ap),
    error = function(e) {
      stop(
        "Failed to perform request to update media timestamps: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  resp <- httr2::resp_body_string(preq)

  result <- jsonlite::fromJSON(resp)

  message("Successfully updated ", nrow(media_records), " media timestamp(s)")

  return(result)
}


#' @title Push new timestamps to the platform in chunks
#'
#' @description
#' Updates timestamps for many media records by splitting them into chunks,
#' avoiding timeouts on large datasets. For small datasets, use
#' \link{update_media_timestamps} directly.
#'
#' @param hdr A base URL provided and valid API key returned by the
#'   function \link{auth_headers}
#' @param media_metadata A data frame or tibble containing the media records to update.
#'   Must contain the following columns:
#'   \itemize{
#'     \item \strong{media_file_record_id} (required): Numeric. The unique identifier
#'           of the media file record to update.
#'     \item \strong{new_timestamp} (required): Character. The new timestamp in
#'           ISO 8601 format (e.g., "2024-01-15T10:30:00" or "2024-01-15 10:30:00").
#'   }
#'   Additional columns will be ignored during submission.
#' @param chunksize An integer specifying the number of records to submit per
#'   chunk. Recommended values: 50-200 depending on network conditions. If
#'   chunksize exceeds the number of rows, it will be automatically adjusted.
#'
#' @return No explicit return value. Progress messages are displayed for each
#'   chunk submitted.
#'
#' @examples
#' \dontrun{
#'   # Create a large dataset with timestamp updates
#'   updates <- data.frame(
#'     media_file_record_id = 1:1000,
#'     new_timestamp = seq(
#'       as.POSIXct("2024-01-15 08:00:00"),
#'       by = "30 sec",
#'       length.out = 1000
#'     ) %>% format("%Y-%m-%dT%H:%M:%S")
#'   )
#'   
#'   # Push updates in chunks of 100
#'   push_new_timestamps(headers, updates, chunksize = 100)
#'   
#'   # Using tibble format
#'   library(tibble)
#'   updates <- tibble(
#'     media_file_record_id = c(123, 456, 789),
#'     new_timestamp = c(
#'       "2024-01-15T10:30:00",
#'       "2024-01-15T14:20:00",
#'       "2024-01-15T18:45:00"
#'     )
#'   )
#'   push_new_timestamps(headers, updates, chunksize = 50)
#' }
#'
#' @seealso
#' \link{update_media_timestamps} for direct submission without chunking
#'
#' @author
#' Adam Varley
#' @export
push_new_timestamps <- function(hdr, media_metadata, chunksize) {
  if (chunksize > nrow(media_metadata)) {
    message('chunksize is bigger than length of data altering chunksize to ', nrow(media_metadata))
    chunksize <- nrow(media_metadata)
  }

  spl.dt <- split(media_metadata, cut(seq_len(nrow(media_metadata)), round(nrow(media_metadata) / chunksize)))
  for (i in seq_along(spl.dt)) {

    send_media_chunks(hdr, spl.dt[[i]])
    message('submitted ', i * chunksize, ' timestamps of ', nrow(media_metadata))
  }
}


#' @title Sequentially correct timestamps for a single device
#'
#' @description
#' Internal helper called by \link{correct_timestamps}. Chains corrected
#' timestamps row-by-row for one device (pre-sorted by \code{file_path}, with
#' \code{is_outlier} already computed): good files keep their timestamp, a bad
#' first file is anchored to \code{installation_timestamp}, and each
#' subsequent bad run is anchored to the last good time (+1 minute) then
#' offset by the original inter-file gaps.
#'
#' @param media_metadata A data frame for a single device, sorted by
#'   \code{file_path}, containing at minimum the columns \code{timestamp}
#'   (POSIXct), \code{installation_timestamp} (POSIXct or Date), and
#'   \code{is_outlier} (logical).
#'
#' @return The input data frame with two additional columns:
#'   \itemize{
#'     \item \strong{corrected_timestamp}: POSIXct. The corrected timestamp.
#'     \item \strong{correction_type}: Character. One of \code{"none"},
#'       \code{"initial_run"}, \code{"mid_deployment_first"}, or
#'       \code{"mid_deployment_chain"}.
#'   }
#'
#' @seealso \link{correct_timestamps}
#'
#' @author
#' Cristobal Salame
#' @keywords internal
.correct_device_timestamps <- function(media_metadata) {
  n          <- nrow(media_metadata)
  ts         <- media_metadata$timestamp
  is_outlier <- media_metadata$is_outlier

  tz_orig <- attr(ts, "tzone")
  if (is.null(tz_orig) || nchar(tz_orig) == 0) tz_orig <- "UTC"

  corrected       <- lubridate::as_datetime(rep(NA_real_, n), tz = tz_orig)
  correction_type <- character(n)
  install_ts      <- lubridate::as_datetime(media_metadata$installation_timestamp[1])
  gap_secs        <- c(NA_real_, as.numeric(diff(ts), units = "secs"))

  for (i in seq_len(n)) {
    if (!is_outlier[i]) {
      # Good timestamp: preserve as-is
      corrected[i]       <- ts[i]
      correction_type[i] <- "none"
    } else if (i == 1) {
      # is_outlier[i] is TRUE here (the preceding `if` already handled the
      # FALSE case), so this only fires when the first file on the device
      # is genuinely bad: anchor to installation timestamp
      corrected[i]       <- install_ts
      correction_type[i] <- "initial_run"
    } else if (!is_outlier[i - 1]) {
      # First bad file after a run of good files (mid-deployment reset):
      # use the last good corrected timestamp + 1 minute
      corrected[i]       <- corrected[i - 1] + 60
      correction_type[i] <- "mid_deployment_first"
    } else {
      # Continuation of a bad run: preserve the original inter-file gap
      corrected[i]       <- corrected[i - 1] + gap_secs[i]
      correction_type[i] <- "mid_deployment_chain"
    }
  }

  media_metadata$corrected_timestamp <- corrected
  media_metadata$correction_type     <- correction_type
  media_metadata
}


#' @title Correct camera trap timestamps
#'
#' @description
#' Identifies and corrects media timestamps corrupted by a camera clock reset
#' (e.g. the clock reverts to a fixed "bad year" after a battery failure).
#' Handles both a reset at the start of a deployment (first file anchored to
#' \code{installation_timestamp}) and a reset partway through (anchored to the
#' last known-good timestamp), preserving the camera's original inter-file
#' gaps in both cases.
#'
#' Outlier detection uses a 48-hour buffer on both deployment bounds: a file
#' is flagged if its \code{timestamp} falls more than 48 hours before
#' \code{installation_timestamp} or more than 48 hours after
#' \code{removal_timestamp}.
#'
#' @param file_path Character vector. Full file paths of the media files, used
#'   to sort files in chronological order within each device.
#' @param device_id Character vector. Unique camera identifier for each file.
#' @param timestamp POSIXct vector. Timestamps read from the files' EXIF
#'   metadata. Non-POSIXct input is coerced via \link[lubridate]{as_datetime}.
#' @param installation_timestamp Date, POSIXct, or character vector giving the
#'   date/time the camera was deployed in the field (typically one repeated
#'   value per device). Always parsed via
#'   \code{lubridate::ymd_hms(truncated = 3)}, so a date with no time
#'   component (e.g. \code{"2024-01-15"} or a bare \code{Date}) defaults to
#'   midnight.
#' @param removal_timestamp Date, POSIXct, or character vector giving the
#'   date/time the camera was removed from the field. Parsed the same way as
#'   \code{installation_timestamp}.
#'
#' @return A tibble with the input columns (\code{file_path}, \code{device_id},
#'   \code{timestamp}, \code{installation_timestamp}, \code{removal_timestamp})
#'   and three additional columns produced by the function:
#'   \itemize{
#'     \item \strong{is_outlier}: Logical. \code{TRUE} for files whose
#'           timestamp was identified as incorrect.
#'     \item \strong{corrected_timestamp}: POSIXct. The corrected timestamp.
#'           Equals \code{timestamp} for non-outlier files.
#'     \item \strong{correction_type}: Character. One of:
#'       \describe{
#'         \item{\code{"none"}}{Timestamp was valid; no correction applied.}
#'         \item{\code{"initial_run"}}{First file on device was bad; anchored
#'           to \code{installation_timestamp}.}
#'         \item{\code{"mid_deployment_first"}}{First bad file after a run of
#'           good files; anchored to last good timestamp + 1 minute.}
#'         \item{\code{"mid_deployment_chain"}}{Subsequent bad file in a run;
#'           offset from the previous corrected timestamp by the original
#'           inter-file time gap.}
#'       }
#'   }
#'
#' @examples
#' \dontrun{
#'   library(dplyr)
#'   library(readr)
#'
#'   cam_data <- read_csv("cam_timestamp.csv") %>%
#'     left_join(deployments, by = "device_id")
#'
#'   corrected <- correct_timestamps(
#'     file_path              = cam_data$file_path,
#'     device_id              = cam_data$device_id,
#'     timestamp              = cam_data$timestamp,
#'     installation_timestamp = cam_data$installation_timestamp,
#'     removal_timestamp      = cam_data$removal_timestamp
#'   )
#'
#'   # Inspect corrections
#'   corrected %>%
#'     filter(is_outlier) %>%
#'     select(device_id, file_path, corrected_timestamp, correction_type)
#'
#'   # Prepare for platform upload
#'   to_upload <- corrected %>%
#'     filter(is_outlier) %>%
#'     mutate(new_timestamp = format(corrected_timestamp, "%Y-%m-%dT%H:%M:%S")) %>%
#'     select(media_file_record_id, new_timestamp)
#'
#'   push_new_timestamps(headers, to_upload, chunksize = 100)
#' }
#'
#' @seealso
#' \link{push_new_timestamps} to upload corrected timestamps to the platform
#'
#' @author
#' Cristobal Salame
#' @export
correct_timestamps <- function(file_path, device_id, timestamp, installation_timestamp, removal_timestamp) {

  # --- Input validation: all arguments must be supplied ---
  if (missing(file_path))              stop("Missing required argument: file_path")
  if (missing(device_id))              stop("Missing required argument: device_id")
  if (missing(timestamp))              stop("Missing required argument: timestamp")
  if (missing(installation_timestamp)) stop("Missing required argument: installation_timestamp")
  if (missing(removal_timestamp))      stop("Missing required argument: removal_timestamp")

  # --- timestamp: keep as-is if already POSIXct, otherwise coerce ---
  if (!inherits(timestamp, c("POSIXct", "POSIXt"))) {
    timestamp <- lubridate::as_datetime(timestamp)
  }

  # --- Deployment dates: force to full POSIXct via ymd_hms. `truncated = 3`
  # accepts Date/POSIXct/character input missing time components and
  # defaults them to midnight, instead of erroring or returning NA. ---
  force_ymd_hms <- function(x, name) {
    tz <- if (inherits(x, "POSIXct")) attr(x, "tzone") else NULL
    if (is.null(tz) || !nzchar(tz)) tz <- "UTC"
    out <- lubridate::ymd_hms(x, truncated = 3, tz = tz)
    if (any(is.na(out) & !is.na(x))) {
      stop(name, " could not be parsed as a date/time (expected Date, POSIXct, or a ymd/ymd_hms-style string).")
    }
    out
  }
  installation_timestamp <- force_ymd_hms(installation_timestamp, "installation_timestamp")
  removal_timestamp      <- force_ymd_hms(removal_timestamp, "removal_timestamp")

  # Assemble individual vectors into a single data frame for grouped processing
  data <- tibble::tibble(
    file_path              = file_path,
    device_id              = device_id,
    timestamp              = timestamp,
    installation_timestamp = installation_timestamp,
    removal_timestamp      = removal_timestamp
  )

  # --- Check for missing values ---
  cols_with_na <- names(data)[sapply(data, anyNA)]
  if (length(cols_with_na) > 0) {
    stop(
      "The following column(s) contain missing values, please check the data: ",
      paste(cols_with_na, collapse = ", ")
    )
  }

  # --- Outlier detection: 48-hour buffer around deployment bounds ---
  data$is_outlier <- (data$timestamp + lubridate::hours(48) < data$installation_timestamp) |
    (data$timestamp - lubridate::hours(48) > data$removal_timestamp)

  # --- Sequential correction: sort by device/file, then chain per device ---
  data  %>% 
    dplyr::arrange(device_id, file_path)  %>% 
    dplyr::group_by(device_id)  %>% 
    dplyr::group_modify(~ .correct_device_timestamps(.x))  %>% 
    dplyr::ungroup()
}
