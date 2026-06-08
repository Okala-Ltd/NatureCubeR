# Tutorial: Upload observations from CSV with automatic schema mapping
#
# This script demonstrates the low-friction workflow:
# 1) Read CSV rows of observations
# 2) Fetch project schema once
# 3) Resolve item UUIDs from item names
# 4) Build observations for uploadObservations
# 5) Dry-run or upload

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Package 'devtools' is required for local testing. Install with install.packages('devtools').")
}

# Load package code from this local repository so changes on your current branch are used.
repo_root <- if (file.exists("DESCRIPTION") && dir.exists("R")) "." else ".."
if (!file.exists(file.path(repo_root, "DESCRIPTION")) || !dir.exists(file.path(repo_root, "R"))) {
  stop("Could not find package root. Run this script from the repo root or the tutorials/ folder.")
}
devtools::load_all(repo_root, quiet = TRUE)
# ----------------------------------------------------------------------------
# 1. Authentication
# ----------------------------------------------------------------------------

api_key <- get_key()


# Optional for local one-off testing (avoid committing real keys):
# api_key <- get_key(api_key = "your_api_key_here")

# Reads NATURECUBE_URL from .Renviron by default.
hdr <- auth_headers(api_key)

# get_project(hdr)

# ----------------------------------------------------------------------------
# 2. Fetch project systems first
# ----------------------------------------------------------------------------

project_systems <- get_project_systems(hdr)

# List systems and procedures to find the names you want to use in the CSV. You can also get this info from the web app or ask your administrator.
list_systems(project_systems)

# Get a specific procedure by name. This is needed to understand the expected schema and map item names to UUIDs.
procedure <- get_procedure(project_systems,
                   system_name    = "Plante Ivindo",
                   procedure_name = "Arbre")

csv_path <- file.path(repo_root, "tutorials", "example_observation_data.csv")

# Validate the CSV against the procedure before attempting upload. This checks for missing required columns and other common issues.
validate_csv_against_procedure(
  procedure = procedure,
  csv_path  = csv_path
)


# ----------------------------------------------------------------------------
# 4. Dry run (recommended first)
# ----------------------------------------------------------------------------

# This will:
# - fetch schema once
# - resolve system/procedure from names
# - map item_name -> item_uuid where item_uuid is missing
# - build observations grouped by observation_id or lon/lat/time
# - return a payload preview without uploading
# - Currently the user can upload simple observations with item_name and value, but not complex observations with nested sub-observations. Support for this is coming soon. 
# - Also this only supports simple point data. Not polygons or lines. Support for this is also coming soon.

dry_run_result <- upload_observations_from_csv(
  hdr       = hdr,
  csv_path  = csv_path,
  procedure = procedure,
  dry_run   = FALSE,
  recorded_at_format = "%d/%m/%Y %H:%M"
)



