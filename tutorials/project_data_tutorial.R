# =============================================================================
# Project Data Tutorial
# =============================================================================
# This tutorial walks through the core project-data workflow in NatureCubeR:
#   1. Authenticate
#   2. Inspect your project
#   3. Retrieve station metadata and plot locations
#   4. Fetch media assets for a station
#   5. Browse and manage species labels
#   6. Push updated labels back to the platform
#
# Prerequisites: set NATURECUBE_API_KEY (and optionally NATURECUBE_URL) in
# ~/.Renviron, then restart R. See tutorials/auth_tutorial.R for details.
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
get_project(hdr = hdr)

# -----------------------------------------------------------------------------
# Step 3: Retrieve station metadata and plot locations
# -----------------------------------------------------------------------------
# Returns an sf object — one row per sensor deployment.
stations <- get_station_info(hdr = hdr, datatype = "image")

# Interactive leaflet map sized by record count.
plot_stations(stations)

# -----------------------------------------------------------------------------
# Step 4: Fetch media assets for a station
# -----------------------------------------------------------------------------
# Pass the project_system_record_id values from the stations object.
psr_ids <- stations$project_system_record_id

media_assets <- get_media_assets(
  hdr      = hdr,
  datatype = "image",
  psrID    = psr_ids
)

# -----------------------------------------------------------------------------
# Step 5: Browse and manage species labels
# -----------------------------------------------------------------------------
# --- 5a. Project labels (labels available to labellers on the dashboard) ----
project_labels <- get_project_labels(hdr = hdr, labeltype = "Camera")

# --- 5b. Search the IUCN database for a species -----------------------------
iucn_results <- getIUCNLabels(
  hdr         = hdr,
  limit       = 2000,
  offset      = 0,
  search_term = "Domestic horse"
)

# Results are in iucn_results$data; pagination info in $total, $offset, $limit.

# --- 5c. Add new IUCN species to the project --------------------------------
# Prepare a data frame with the required IUCN columns, then upload in chunks.
# example_data <- readLines("path/to/species.json") |> jsonlite::fromJSON()
# example_data$extant_country_list <- NA
# add_IUCN_labels(hdr = hdr, labels = example_data, chunksize = 500)

# -----------------------------------------------------------------------------
# Step 6: Push updated labels back to the platform
# -----------------------------------------------------------------------------
# Build a submission frame with one row per media segment to label.
new_label_id <- project_labels[
  project_labels$common_name == "Uneven-toothed Rat",
  "label_id"
]

submission_frame <- data.frame(
  segment_record_id_fk = media_assets$segment_record_id[1],
  label_id_fk          = new_label_id,
  number_of_individuals = 1
)

push_new_labels(hdr = hdr, submission_records = submission_frame, chunksize = 30)

