# =============================================================================
# Media Timestamp Correction Tutorial
# =============================================================================
# Use this workflow when media files were recorded with an incorrect clock and
# you need to correct their timestamps in bulk on the NatureCube platform.
#
# Two situations are covered:
#   A. The clock was off by a known, constant amount (e.g. 60 seconds fast).
#      Shift every timestamp by that offset.
#   B. The camera clock RESET (e.g. it reverted to the year 2000 after a
#      battery failure). correct_timestamps() finds the bad files and rebuilds
#      their timestamps, and handles several deployments of the same device.
#
# Both end the same way: a table with `media_file_record_id` and a
# `new_timestamp` text column ("2024-01-15T10:30:00"), pushed with
# push_new_timestamps(). Pushing WRITES to the platform: test on a few rows.
#
# Prerequisites: set NATURECUBE_API_KEY (and optionally NATURECUBE_URL) in
# ~/.Renviron, then restart R. See tutorials/auth_tutorial.R for details.
# =============================================================================

library(NatureCubeR)
library(lubridate)

# -----------------------------------------------------------------------------
# Step 1: Authenticate
# -----------------------------------------------------------------------------
api_key <- get_key()
hdr <- auth_headers(api_key)

# Confirm the active project before making any changes.
get_project(hdr = hdr)

# -----------------------------------------------------------------------------
# Step 2: Retrieve station metadata
# -----------------------------------------------------------------------------
stations <- get_station_info(hdr = hdr, datatype = "video")

# Optional: inspect station locations on an interactive map.
plot_stations(stations)

# -----------------------------------------------------------------------------
# Step 3: Fetch media assets for the stations
# -----------------------------------------------------------------------------
# One row per media file, including media_file_record_id and
# media_file_created_at (the timestamp currently stored). Passing
# record_count skips stations without media; use max_workers = 1L to download
# one station at a time.
media_assets <- get_media_assets(
  hdr          = hdr,
  datatype     = "video",
  psrID        = stations$project_system_record_id,
  record_count = stations$record_count
)
names(media_assets)

# =============================================================================
# A. Shift every timestamp by a fixed offset
# =============================================================================
# Work on a subset while testing; remove the slice() call to process everything.
to_correct <- media_assets %>%
  dplyr::slice(1:100)

# In this example the device clock was 60 seconds fast, so we subtract 60 s.
# Adjust the offset to match the actual clock error for your deployment.
offset_seconds <- -60

# new_timestamp must be text in ISO 8601 form with a "T" between date and time:
# "2024-01-15T10:30:00", optionally followed by "Z" or an offset like "+01:00".
to_correct <- to_correct %>%
  dplyr::mutate(
    new_timestamp = lubridate::as_datetime(media_file_created_at) + offset_seconds,
    new_timestamp = format(new_timestamp, "%Y-%m-%dT%H:%M:%S")
  )

# Inspect the before/after values before pushing.
to_correct %>%
  dplyr::select(media_file_record_id, media_file_created_at, new_timestamp) %>%
  head()

# push_new_timestamps() needs the columns media_file_record_id and
# new_timestamp (any other column is ignored) and splits the submission into
# chunks to avoid timeouts. chunksize must not exceed the number of rows;
# 50-200 is a good range.
push_new_timestamps(
  hdr            = hdr,
  media_metadata = to_correct,
  chunksize      = 50
)

# For a handful of files you can call update_media_timestamps() instead: same
# two columns, one request, no chunking.
# update_media_timestamps(hdr, to_correct[, c("media_file_record_id", "new_timestamp")])

# =============================================================================
# B. Repair a camera clock reset with correct_timestamps()
# =============================================================================
# correct_timestamps() is offline: it works on a table you give it. It needs
# one row per media file with these columns:
#   file_name               file name; sorts the files in order within a device
#   device_id               camera identifier
#   timestamp               the time read from the file (e.g. EXIF
#                           DateTimeOriginal, via exifr::read_exif())
#   installation_timestamp  when the camera was put out
#   removal_timestamp       when the camera was taken in
# (the stations table holds each deployment's start and end timestamps in
# project_system_record_start_timestamp / project_system_record_end_timestamp)
#
# Optional column:
#   subfolder               deployment folder under device_id
#
# A file is flagged as an outlier when its timestamp is more than 48 hours
# before installation_timestamp or more than 48 hours after removal_timestamp.
# Flagged files are rebuilt in order, keeping the camera's own gaps between
# files: a bad first file is anchored to installation_timestamp, and a bad run
# that starts mid-deployment continues one minute after the last good file.
#
# Any other column you add (such as media_file_record_id below) comes back
# unchanged, so keep the platform ID in the table and you can push straight
# from the result. Join your files to media_assets to get it; check
# names(media_assets) for the file-name column to join on.

# --- A small self-contained example -----------------------------------------
# cam_A was deployed twice, and both deployments have a file called
# IMG_0001.JPG. The 2024-01 deployment lost its clock for the first two files.
cam_data <- tibble::tibble(
  media_file_record_id   = 1001:1006,
  device_id              = "cam_A",
  subfolder              = rep(c("2024-01", "2024-06"), each = 3),
  file_name              = rep(c("IMG_0001.JPG", "IMG_0002.JPG", "IMG_0003.JPG"), times = 2),
  timestamp              = as.POSIXct(c(
    "2000-01-01 00:00:00", "2000-01-01 00:05:00", "2024-01-10 08:00:00",
    "2024-06-10 08:00:00", "2024-06-10 08:10:00", "2024-06-10 08:20:00"
  ), tz = "UTC"),
  installation_timestamp = as.POSIXct(
    rep(c("2024-01-01 00:00:00", "2024-06-01 00:00:00"), each = 3), tz = "UTC"
  ),
  removal_timestamp      = as.POSIXct(
    rep(c("2024-01-31 00:00:00", "2024-06-30 00:00:00"), each = 3), tz = "UTC"
  )
)

# Without `subfolder`, the two IMG_0001.JPG files would be a duplicate
# file_name within cam_A and correct_timestamps() would stop with an error.
# With it, each device_id + subfolder is checked and corrected on its own.
# Use NA or "" for files that sit directly under the device folder.
corrected <- correct_timestamps(cam_data)

corrected %>%
  dplyr::select(device_id, subfolder, file_name, is_outlier, correction_type, new_timestamp)
# correction_type is one of:
#   "none"                  the timestamp was fine
#   "initial_run"           first file was bad: set to installation_timestamp
#   "mid_deployment_first"  first bad file after good ones: last good + 1 minute
#   "mid_deployment_chain"  later bad file: previous corrected time + original gap

# Push only the files that changed.
to_upload <- corrected %>%
  dplyr::filter(is_outlier) %>%
  dplyr::mutate(new_timestamp = format(new_timestamp, "%Y-%m-%dT%H:%M:%S")) %>%
  dplyr::select(media_file_record_id, new_timestamp)

to_upload

# With real record IDs:
# push_new_timestamps(hdr = hdr, media_metadata = to_upload, chunksize = 100)
