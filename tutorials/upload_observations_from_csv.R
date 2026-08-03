# Tutorial: Upload observations from CSV with automatic schema mapping
#
# This script demonstrates the low-friction workflow:
# 1) Read a wide-format CSV (one row per feature, procedure item names as
#    column headers)
# 2) Fetch project schema once
# 3) Validate against the procedure (checks labels against the database)
# 4) Upload the validated result

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

csv_path <- file.path(repo_root, "tutorials", "data", "example_observation_data.csv")

# Read the CSV yourself - validate_csv_against_procedure() takes a data
# frame, not a path, so you're free to filter/mutate it first if needed.
observation_data <- readr::read_csv(csv_path, show_col_types = FALSE)

# ----------------------------------------------------------------------------
# 3. Validate the CSV against the procedure
# ----------------------------------------------------------------------------

# Checks for missing required columns, bad values, and unrecognised labels
# before upload. Every label value is checked against the label database
# here - reuse this result in step 4 instead of validating twice.
validated <- validate_csv_against_procedure(
  procedure        = procedure,
  observation_data = observation_data,
  hdr              = hdr
)

# ----------------------------------------------------------------------------
# 4. Upload the validated result
# ----------------------------------------------------------------------------

# This will:
# - reuse the observations/labels already built and checked in step 3
#   (nothing is rebuilt and no label is checked against the database again)
# - upload only the rows that passed validation
# - Currently only simple point observations are supported - one row per
#   feature, no nested sub-observations, no polygons or lines.

upload_result <- upload_observations_from_csv(
  hdr       = hdr,
  validated = validated
)

upload_result$result



