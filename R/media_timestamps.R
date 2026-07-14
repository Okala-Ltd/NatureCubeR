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
#' Updates timestamps for one or more media file records in a single API call.
#' This function provides direct access to the updateTimestamps endpoint without
#' automatic chunking. For large datasets (>1000 records), consider using
#' \link{push_new_timestamps} which handles chunking automatically.
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
#' Updates timestamps for multiple media file records by automatically splitting
#' the data into manageable chunks. This function is recommended for large
#' datasets (>1000 records) as it prevents timeout issues and provides progress
#' tracking. For smaller datasets, consider using \link{update_media_timestamps}
#' for direct submission without chunking.
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
#' Internal helper called by \link{correct_timestamps}. Iterates row-by-row
#' over one device's media records (pre-sorted by \code{file_path}, with
#' \code{is_outlier} already computed) and assigns a corrected timestamp using
#' a sequential chaining strategy:
#'
#' \itemize{
#'   \item Non-outlier files keep their original \code{timestamp}.
#'   \item The first file on the device, if bad, is anchored to
#'     \code{installation_timestamp}.
#'   \item The first bad file after a run of good files is set to the previous
#'     corrected timestamp plus one minute.
#'   \item Subsequent bad files in the same run are offset from the previous
#'     corrected timestamp by the original inter-file time gap, preserving the
#'     camera's internal clock rhythm.
#' }
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
  n       <- nrow(media_metadata)
  tz_orig <- attr(media_metadata$timestamp, "tzone")
  if (is.null(tz_orig) || nchar(tz_orig) == 0) tz_orig <- "UTC"

  corrected       <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = tz_orig)
  correction_type <- character(n)
  install_ts      <- as.POSIXct(media_metadata$installation_timestamp[1])

  for (i in seq_len(n)) {
    if (!media_metadata$is_outlier[i]) {
      # Good timestamp: preserve as-is
      corrected[i]       <- media_metadata$timestamp[i]
      correction_type[i] <- "none"
    } else if (i == 1) {
      # First file on the device is already bad: anchor to installation timestamp
      corrected[i]       <- install_ts
      correction_type[i] <- "initial_run"
    } else if (!media_metadata$is_outlier[i - 1]) {
      # First bad file after a run of good files (mid-deployment reset):
      # use the last good corrected timestamp + 1 minute
      corrected[i]       <- corrected[i - 1] + 60
      correction_type[i] <- "mid_deployment_first"
    } else {
      # Continuation of a bad run: preserve the inter-file time gap
      gap_secs           <- as.numeric(difftime(
        media_metadata$timestamp[i],
        media_metadata$timestamp[i - 1],
        units = "secs"
      ))
      corrected[i]       <- corrected[i - 1] + gap_secs
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
#' Identifies and corrects media file timestamps that have been corrupted by
#' a camera reset (e.g., the clock reverts to a fixed "bad year" such as 2020
#' after a battery failure or after the field team accidentally resets the
#' camera settings). Two failure modes are handled:
#'
#' \itemize{
#'   \item \strong{Start-of-deployment reset}: All files from the device have
#'     a bad timestamp from the first photo. The first file is anchored to
#'     \code{installation_timestamp} and subsequent files in that run preserve
#'     their original inter-file time gaps.
#'   \item \strong{Mid-deployment reset}: Early files have correct timestamps;
#'     the camera resets partway through. The first bad file after a run of
#'     good files is set to the last good corrected timestamp plus one minute.
#'     Subsequent bad files in that run preserve inter-file time gaps relative
#'     to that anchor.
#' }
#'
#' Outlier detection uses a 10-day buffer: a file is flagged if
#' \code{timestamp + 10 days < installation_timestamp}. An optional
#' \code{reset_year} extends detection to any file whose year matches the
#' camera's default reset year.
#'
#' @param file_path Character vector. Full file paths of the media files, used
#'   to sort files in chronological order within each device.
#' @param device_id Character vector. Unique camera identifier for each file.
#' @param timestamp POSIXct vector. Timestamps read from the files' EXIF
#'   metadata.
#' @param installation_timestamp POSIXct or Date vector. The date/time the
#'   camera was deployed in the field. Typically one repeated value per device.
#' @param reset_year An optional integer specifying the year cameras revert to
#'   after a reset (e.g., \code{2020}). Any file whose \code{timestamp}
#'   falls in this year is additionally flagged as an outlier even if the
#'   10-day buffer alone would not catch it. When \code{NULL} (default), the
#'   function attempts to auto-detect a reset year by finding the most common
#'   year among buffer-flagged outliers, accepting it only if it predates all
#'   deployment years (conservative check to avoid flagging valid deployments
#'   that happened in that year).
#'
#' @return A tibble with columns \code{file_path}, \code{device_id},
#'   \code{timestamp}, \code{installation_timestamp}, and three additional
#'   columns produced by the function:
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
#'   # Auto-detect reset year
#'   corrected <- correct_timestamps(
#'     file_path              = cam_data$file_path,
#'     device_id              = cam_data$device_id,
#'     timestamp              = cam_data$timestamp,
#'     installation_timestamp = cam_data$installation_timestamp
#'   )
#'
#'   # Provide reset year explicitly
#'   corrected <- correct_timestamps(
#'     file_path              = cam_data$file_path,
#'     device_id              = cam_data$device_id,
#'     timestamp              = cam_data$timestamp,
#'     installation_timestamp = cam_data$installation_timestamp,
#'     reset_year             = 2020
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
correct_timestamps <- function(file_path, device_id, timestamp, installation_timestamp, reset_year = NULL) {

  # --- Input validation ---
  if (!inherits(timestamp, c("POSIXct", "POSIXt"))) {
    warning("timestamp is not POSIXct; coercing via as.POSIXct().")
    timestamp <- as.POSIXct(timestamp)
  }
  if (!inherits(installation_timestamp, c("POSIXct", "POSIXt", "Date"))) {
    warning("installation_timestamp is not POSIXct or Date; coercing via as.POSIXct().")
    installation_timestamp <- as.POSIXct(installation_timestamp)
  }

  # Assemble individual vectors into a single data frame for grouped processing
  data <- tibble::tibble(
    file_path              = file_path,
    device_id              = device_id,
    timestamp              = timestamp,
    installation_timestamp = installation_timestamp
  )

  # --- Outlier detection: 10-day buffer ---
  is_outlier_buf <- data$timestamp + lubridate::hours(240) < data$installation_timestamp

  # --- Reset year: validate supplied value or attempt auto-detection ---
  if (!is.null(reset_year)) {
    if (!is.numeric(reset_year) || length(reset_year) != 1) {
      stop("reset_year must be a single integer (e.g., 2020).")
    }
    reset_year <- as.integer(reset_year)
  } else {
    outlier_years <- lubridate::year(data$timestamp[is_outlier_buf])
    if (length(outlier_years) > 0) {
      candidate_year <- as.integer(names(sort(table(outlier_years), decreasing = TRUE))[1])
      install_years  <- unique(lubridate::year(data$installation_timestamp))
      # Only adopt the candidate if it clearly predates all deployments
      if (candidate_year < min(install_years) && !(candidate_year %in% install_years)) {
        reset_year <- candidate_year
        message("Auto-detected reset year: ", reset_year)
      }
    }
  }

  # --- Combined outlier flag ---
  if (!is.null(reset_year)) {
    data$is_outlier <- is_outlier_buf | lubridate::year(data$timestamp) == reset_year
  } else {
    data$is_outlier <- is_outlier_buf
  }

  # --- Sequential correction: split by device, sort by file_path, then chain ---
  data <- data[order(data$device_id, data$file_path), ]
  data <- do.call(
    rbind,
    lapply(split(data, data$device_id), .correct_device_timestamps)
  )
  rownames(data) <- NULL

  data
}
