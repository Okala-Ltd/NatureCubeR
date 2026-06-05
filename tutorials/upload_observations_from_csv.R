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
# 2. Fetch reference schema first
# ----------------------------------------------------------------------------

reference_schema <- get_project_schema(hdr)

list_systems(reference_schema)

procedure <- get_procedure(reference_schema,
                   system_name    = "Plante Ivindo",
                   procedure_name = "Arbre")

validate_csv_against_procedure(
  procedure = procedure,
  csv_path  = file.path(repo_root, "tutorials", "example_observation_data.csv")
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
# 5. Upload for real
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
