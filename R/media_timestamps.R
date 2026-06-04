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
