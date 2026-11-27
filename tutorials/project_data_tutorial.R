# =============================================================================
# Project Data Tutorial
# =============================================================================
# This tutorial walks through the core project-data workflow in NatureCubeR:
#   1. Authenticate
#   2. Confirm the active project
#   3. Retrieve station metadata and plot locations
#   4. Fetch media assets
#   5. Browse and manage species labels
#   6. Push labels back to the platform
#   7. Mark segments blank / publish them
#
# Prerequisites: set NATURECUBE_API_KEY (and optionally NATURECUBE_URL) in
# ~/.Renviron, then restart R. See tutorials/auth_tutorial.R for details.
#
# Steps 6 and 7 WRITE to the platform. Try them on one or two segments first.
# =============================================================================

library(NatureCubeR)

# -----------------------------------------------------------------------------
# Step 1: Authenticate
# -----------------------------------------------------------------------------
api_key <- get_key()
hdr <- auth_headers(api_key)

# -----------------------------------------------------------------------------
# Step 2: Confirm the active project
# -----------------------------------------------------------------------------
# Prints the project name for your key; there is no return value.
get_project(hdr = hdr)

# -----------------------------------------------------------------------------
# Step 3: Retrieve station metadata and plot locations
# -----------------------------------------------------------------------------
# One row per sensor deployment, as an sf object. `datatype` is one of
# "video", "audio", "image" or "eDNA".
stations <- get_station_info(hdr = hdr, datatype = "image")

# Columns used below: project_system_record_id (the station ID the media
# functions need), record_count (number of media files), device_id and the
# deployment start/end timestamps.
names(stations)

# Interactive leaflet map, circles sized by record_count.
plot_stations(stations)

# -----------------------------------------------------------------------------
# Step 4: Fetch media assets
# -----------------------------------------------------------------------------
# get_media_assets() takes the station IDs in `psrID`. Pass `record_count` too
# so stations with no media are skipped without an API call.
#
# Start with a single station to see what comes back.
first_station <- which(stations$record_count > 0)[1]

media_one <- get_media_assets(
  hdr          = hdr,
  datatype     = "image",
  psrID        = stations$project_system_record_id[first_station],
  record_count = stations$record_count[first_station]
)
names(media_one)   # includes segment_record_id and media_file_record_id

# Every station in the project. Each station is paged until the API returns an
# empty page, and up to `max_workers` stations are downloaded at once.
media_assets <- get_media_assets(
  hdr          = hdr,
  datatype     = "image",
  psrID        = stations$project_system_record_id,
  record_count = stations$record_count,
  max_workers  = 4L
)

# Notes on max_workers:
# * The API answers HTTP 429 when it is asked for too much. The function waits
#   and retries on its own; if you still see 429s, lower max_workers.
# * max_workers = 1L downloads one station at a time in this R session.
#   Above 1, the stations are fetched by background R processes that load the
#   INSTALLED NatureCubeR, not code loaded with devtools::load_all(). Use
#   max_workers = 1L while developing the package.

# -----------------------------------------------------------------------------
# Step 5: Browse and manage species labels
# -----------------------------------------------------------------------------
# --- 5a. Project labels (labels available to labellers on the dashboard) ----
# labeltype is "Camera" or "Bioacoustic" (pass exactly one).
project_labels <- get_project_labels(hdr = hdr, labeltype = "Camera")
names(project_labels)

# --- 5b. Search the IUCN database for a species -----------------------------
# getIUCNLabels() returns ONE page of up to 1000 rows. The page size is fixed,
# so there is no `limit` argument; use `offset` to move through the results.
horse <- getIUCNLabels(hdr = hdr, offset = 0, search_term = "Domestic horse")

horse$data    # the rows on this page
horse$total   # rows matching the search across all pages

# --- 5c. Pull the whole database, or one taxonomic group --------------------
# Omit search_term and keep asking for the next page until you have `total`
# rows. There is no server-side filter on class/order/family, so filter the
# result afterwards.
pages <- list()
offset <- 0L
repeat {
  page <- getIUCNLabels(hdr = hdr, offset = offset)
  pages[[length(pages) + 1L]] <- page$data
  offset <- offset + nrow(page$data)
  if (nrow(page$data) == 0L || offset >= page$total) break
}
iucn_labels <- dplyr::bind_rows(pages)

# All birds. The rank column may be called `class` or `class_`; check with
# names(iucn_labels) and use whichever your data has.
class_col <- intersect(c("class", "class_"), names(iucn_labels))[1]
birds <- iucn_labels[iucn_labels[[class_col]] %in% "Aves", ]

# --- 5d. Add new IUCN species to the database -------------------------------
# This writes to the shared label database. Prepare a data frame with the
# required IUCN columns, then upload in chunks. `chunksize` must be smaller
# than the number of rows.
# example_data <- readLines("path/to/species.json") %>% jsonlite::fromJSON()
# example_data$extant_country_list <- NA
# add_IUCN_labels(hdr = hdr, labels = example_data, chunksize = 500)

# -----------------------------------------------------------------------------
# Step 6: Push labels back to the platform
# -----------------------------------------------------------------------------
# One row per media segment to label. push_new_labels() renames the columns
# `segment_record_id` -> `segment_record_id_fk` and `label_id` ->
# `label_id_fk` for you, so you can use the names exactly as they appear in
# media_assets and project_labels.
label_common_name <- "Uneven-toothed Rat"   # a common_name from project_labels
new_label_id <- project_labels$label_id[
  which(project_labels$common_name == label_common_name)
]
stopifnot(length(new_label_id) == 1L)

submission_frame <- data.frame(
  segment_record_id     = media_assets$segment_record_id[1],
  label_id              = new_label_id,
  number_of_individuals = 1L
)

# Optional columns: label_record_uuid, prediction_accuracy, label_created_at,
# label_record_id. Any other column is rejected before anything is sent.
push_new_labels(hdr = hdr, submission_records = submission_frame, chunksize = 30)

# -----------------------------------------------------------------------------
# Step 7: Mark segments blank / publish them
# -----------------------------------------------------------------------------
segment_ids <- media_assets$segment_record_id[1:2]

# Mark the labels on these segments as blank (TRUE) or undo it (FALSE).
set_segment_blank_status(hdr, blank_status = TRUE, segment_record_ids = segment_ids)

# Publish (TRUE) or unpublish (FALSE) segment records. Sent in chunks of
# `chunksize` IDs per request (default 500).
publish_segments(hdr, publish_status = TRUE, segment_record_ids = segment_ids)
