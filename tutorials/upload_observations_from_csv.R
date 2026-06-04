# Tutorial: Upload observations from CSV with automatic schema mapping
#
# This script demonstrates the low-friction workflow:
# 1) Read CSV rows of observations
# 2) Fetch project schema once
# 3) Resolve item UUIDs from item names
# 4) Build observations for uploadObservations
# 5) Dry-run or upload

library(okalaR)
source("R/phone_observations.R") # for internal helper functions
source("R/auth.R") # for get_key() and auth_headers()
# ----------------------------------------------------------------------------
# 1. Authentication
# ----------------------------------------------------------------------------

api_key <- get_key()

# Use production:
hdr <- auth_headers(api_key)

# Or use development:
# hdr <- auth_headers_dev(api_key)

# ----------------------------------------------------------------------------
# 2. Input CSV
# ----------------------------------------------------------------------------

csv_path <- "tutorials/example_observation_data.csv"

if (!file.exists(csv_path)) {
  stop("CSV file not found at: ", csv_path)
}

# ----------------------------------------------------------------------------
# 3. Dry run (recommended first)
# ----------------------------------------------------------------------------

# This will:
# - fetch schema once
# - resolve system/procedure from names
# - map item_name -> item_uuid where item_uuid is missing
# - build observations grouped by observation_id or lon/lat/time
# - return a payload preview without uploading

dry_run_result <- upload_observations_from_csv(
  hdr = hdr,
  csv_path = csv_path,
  system_name = "Plante Ivindo",
  procedure_name = "Arbre",
  dry_run = TRUE,
  recorded_at_format = "%d/%m/%Y %H:%M"
)

cat("Built observations:", length(dry_run_result$observations), "\n")
cat("Resolved rows:", dry_run_result$resolved_rows, "\n")
cat("Unresolved rows:", nrow(dry_run_result$unresolved_rows), "\n")

if (nrow(dry_run_result$unresolved_rows) > 0) {
  cat("Rows with unresolved item mapping (first 10):\n")
  print(utils::head(dry_run_result$unresolved_rows, 10))
}

# ----------------------------------------------------------------------------
# 4. Upload for real
# ----------------------------------------------------------------------------

# Uncomment when the dry run looks correct.

# upload_result <- upload_observations_from_csv(
#   hdr = hdr,
#   csv_path = csv_path,
#   system_name = "Plante Ivindo",
#   procedure_name = "Arbre",
#   dry_run = FALSE,
#   recorded_at_format = "%d/%m/%Y %H:%M"
# )
#
# print(upload_result$response)
# cat("Uploaded observations:", length(upload_result$observations), "\n")
