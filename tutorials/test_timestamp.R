# =============================================================================
# Media Timestamp Correction Tutorial
# =============================================================================
# Use this workflow when media files have been recorded with an incorrect clock
# and you need to correct timestamps in bulk on the NatureCube platform.
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
# Step 3: Fetch media assets for all stations
# -----------------------------------------------------------------------------
# Returns a tibble with one row per media file, including media_file_record_id
# and media_file_created_at (the current stored timestamp).
media_assets <- get_media_assets(
  hdr      = hdr,
  datatype = "video",
  psrID    = stations$project_system_record_id
)

# -----------------------------------------------------------------------------
# Step 4: Calculate corrected timestamps
# -----------------------------------------------------------------------------
# Work on a subset while testing; remove the slice() call to process everything.
to_correct <- media_assets |>
  dplyr::slice(1:100)

# Parse the stored timestamp, then apply your correction.
# In this example the device clock was 60 seconds fast, so we subtract 60 s.
# Adjust the offset to match the actual clock error for your deployment.
offset_seconds <- -60

to_correct <- to_correct |>
  dplyr::mutate(
    new_timestamp = lubridate::as_datetime(media_file_created_at) + offset_seconds,
    new_timestamp = format(new_timestamp, "%Y-%m-%dT%H:%M:%S")
  )

# Inspect the before/after values before pushing.
to_correct |>
  dplyr::select(media_file_record_id, media_file_created_at, new_timestamp) |>
  head()

# -----------------------------------------------------------------------------
# Step 5: Push corrected timestamps to the platform
# -----------------------------------------------------------------------------
# push_new_timestamps() splits the submission into chunks to avoid timeouts.
# Recommended chunksize: 50–200 depending on network conditions.
push_new_timestamps(
  hdr            = hdr,
  media_metadata = to_correct,
  chunksize      = 50
)
