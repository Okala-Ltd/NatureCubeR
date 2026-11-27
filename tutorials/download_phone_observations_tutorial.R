# =============================================================================
# Download Phone Observations Tutorial
# =============================================================================
# Export the observations collected on the NatureCube phone app, together with
# their photos, videos and audio, to files on disk:
#   1. Authenticate
#   2. Find the system and procedure you want
#   3. Download one procedure
#   4. Download every procedure in the project
#   5. Read the results
#
# Prerequisites: set NATURECUBE_API_KEY (and optionally NATURECUBE_URL) in
# ~/.Renviron, then restart R. See tutorials/auth_tutorial.R for details.
#
# get_phone_observations() talks to the production service, so use
# auth_headers(), not auth_headers_dev().
# =============================================================================

library(NatureCubeR)

# -----------------------------------------------------------------------------
# Step 1: Authenticate
# -----------------------------------------------------------------------------
api_key <- get_key()
hdr <- auth_headers(api_key)
get_project(hdr = hdr)

# -----------------------------------------------------------------------------
# Step 2: Find the system and procedure
# -----------------------------------------------------------------------------
# A project has systems (surveys), each with procedures (forms).
schema <- get_project_systems(hdr)

# Prints a table and invisibly returns it as a tibble with system_index,
# system_name, system_id, procedure_index, procedure_name, procedure_id, form.
systems <- list_systems(schema)

# get_procedure() returns a list with project_id, system_id, procedure_id,
# system_name, procedure_name, form and items. That list is what
# get_phone_observations() takes.
procedure <- get_procedure(
  schema,
  system_name    = "Plante Ivindo",
  procedure_name = "Arbre"
)

# You can also select by position, using the indices from list_systems():
# procedure <- get_procedure(schema, system_index = 1, procedure_index = 1)

# -----------------------------------------------------------------------------
# Step 3: Download one procedure
# -----------------------------------------------------------------------------
# Every data type is downloaded by default. Files go to
#   dest_dir/{system_id}_{system_name}/{procedure_id}_{procedure_name}/
# which contains
#   observations.xlsx   every data type in one workbook
#   observations.gpkg   the same rows as spatial features (when there are coordinates)
#   media/              the photos, videos and audio (when there are any)
export_dir <- file.path(getwd(), "naturecube_export")

result <- get_phone_observations(
  hdr       = hdr,
  procedure = procedure,
  dest_dir  = export_dir
)

result$row_count          # observation rows
result$media_count        # media files downloaded
result$by_type            # rows and media files per data type
result$excel_path
result$geopackage_path    # NULL when there are no coordinates
result$media_dir          # NULL when there is no media
head(result$observations)

# Media files that came in the export without a matching observation row are
# kept in media/ and listed here. This should normally be empty.
result$media_unreferenced

# Download only some data types. Valid values are phone-photo, phone-video,
# phone-audio, choice, text, numeric and label.
# photos <- get_phone_observations(hdr, procedure, data_type = "phone-photo",
#                                  dest_dir = export_dir)
# mixed  <- get_phone_observations(hdr, procedure,
#                                  data_type = c("text", "phone-photo"),
#                                  dest_dir = export_dir)
#
# The media export is built on the server and can take a while. Types are
# requested one after the other because parallel requests tend to get HTTP 429.

# -----------------------------------------------------------------------------
# Step 4: Download every procedure in the project
# -----------------------------------------------------------------------------
# The folder layout is the same on every call, so looping over procedures fills
# one tree. Running a procedure again overwrites its workbook and tops up media.
results <- vector("list", nrow(systems))

for (i in seq_len(nrow(systems))) {
  proc_i <- get_procedure(
    schema,
    system_index    = systems$system_index[i],
    procedure_index = systems$procedure_index[i]
  )
  results[[i]] <- tryCatch(
    get_phone_observations(hdr, proc_i, dest_dir = export_dir),
    error = function(e) {
      message("Failed: ", systems$system_name[i], " / ", systems$procedure_name[i],
              "\n  ", conditionMessage(e))
      NULL
    }
  )
}

# One summary row per procedure that downloaded.
ok <- !vapply(results, is.null, logical(1))
data.frame(
  system     = systems$system_name[ok],
  procedure  = systems$procedure_name[ok],
  rows       = vapply(results[ok], function(r) r$row_count, numeric(1)),
  media      = vapply(results[ok], function(r) r$media_count, numeric(1))
)

# -----------------------------------------------------------------------------
# Step 5: Read the results
# -----------------------------------------------------------------------------
# The same data is already in result$observations. To read a file you
# downloaded earlier, use the readxl package (install.packages("readxl")):
# readxl::excel_sheets(result$excel_path)
# obs <- readxl::read_excel(result$excel_path, sheet = 1)
#
# Spatial layer:
# obs_sf <- sf::st_read(result$geopackage_path)
#
# Media rows hold the relative path media/<file_name> inside the procedure
# folder. A row that listed several files is split into one row per file.
