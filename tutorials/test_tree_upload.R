# Test Script: Upload Tree Data using upload_phone_observations()
# This script demonstrates transforming CSV tree survey data into the
# phone observations format required by the Okala API.

library(okalaR)
library(dplyr)
library(tidyr)

# ============================================================================
# 1. LOAD AND EXPLORE THE DATA
# ============================================================================

# Load the tree data
tree_data <- read.csv("/Users/adamvarley/Downloads/Tree_Data_Adam.csv")

# Preview the data
head(tree_data)
str(tree_data)

# Check unique plots (these will become features)
unique_plots <- unique(tree_data$plotID)
cat("Number of unique plots:", length(unique_plots), "\n")
cat("Plots:", paste(unique_plots, collapse = ", "), "\n")

# ============================================================================
# 2. SET UP AUTHENTICATION
# ============================================================================

# Set up your API credentials
# Replace with your actual API key
api_key <- Sys.getenv("OKALA_API_KEY")  # Or paste your key directly

if (api_key == "") {
  stop("Please set your OKALA_API_KEY environment variable or paste your API key")
}

# Create auth headers (use auth_headers_dev() for development environment)
hdr <- auth_headers(api_key)

# Set your project ID
project_id <- 123  # Replace with your actual project ID

# ============================================================================
# 3. BUILD DEVICE SETTINGS
# ============================================================================

# Create device settings for this upload session
device <- build_device_settings(
  device_id = "r-upload-session-001",
  phone_model = "R Studio Upload",
  phone_os = "R 4.3",
  carrier = "Desktop",
  build_number = "1.0.0",

  build_id = "okalaR-test"
)

print("Device settings created:")
print(device)

# ============================================================================
# 4. TRANSFORM TREE DATA INTO FEATURE/OBSERVATION FORMAT
# ============================================================================

# Function to create a bounding box polygon from plot points
create_plot_bbox <- function(plot_data) {
  lon_min <- min(plot_data$longitude) - 0.00001
  lon_max <- max(plot_data$longitude) + 0.00001
  lat_min <- min(plot_data$latitude) - 0.00001
  lat_max <- max(plot_data$latitude) + 0.00001
  
  # Create polygon coordinates (closed ring)
  list(
    type = "Polygon",
    coordinates = list(list(
      c(lon_min, lat_min),
      c(lon_max, lat_min),
      c(lon_max, lat_max),
      c(lon_min, lat_max),
      c(lon_min, lat_min)  # Close the ring
    ))
  )
}

# Function to convert a single tree record to observations
# Each tree will have multiple observations: label, diameter, height
create_tree_observations <- function(tree_row, item_uuids) {
  observations <- list()
  
  # Point geometry for this tree
  tree_point <- list(
    type = "Point",
    coordinates = c(tree_row$longitude, tree_row$latitude)
  )
  
  # Observation 1: Taxonomic label (as text since we're testing)
  obs_label <- build_observation(
    item_uuid = item_uuids$label,
    item_type = "text",
    data = list(tree_row$label),
    geometry = tree_point,
    observation_uuid = paste0("obs-label-", tree_row$plantID),
    observation_created_at = as.POSIXct(tree_row$timestamp, format = "%Y-%m-%dT%H:%MZ")
  )
  observations <- c(observations, list(obs_label))
  
  # Observation 2: Diameter (numeric)
  obs_diameter <- build_observation(
    item_uuid = item_uuids$diameter,
    item_type = "numeric",
    data = list(tree_row$diameter),
    geometry = tree_point,
    observation_uuid = paste0("obs-diameter-", tree_row$plantID),
    observation_created_at = as.POSIXct(tree_row$timestamp, format = "%Y-%m-%dT%H:%MZ")
  )
  observations <- c(observations, list(obs_diameter))
  
  # Observation 3: Height (numeric)
  obs_height <- build_observation(
    item_uuid = item_uuids$height,
    item_type = "numeric",
    data = list(tree_row$height),
    geometry = tree_point,
    observation_uuid = paste0("obs-height-", tree_row$plantID),
    observation_created_at = as.POSIXct(tree_row$timestamp, format = "%Y-%m-%dT%H:%MZ")
  )
  observations <- c(observations, list(obs_height))
  
  return(observations)
}

# Define item UUIDs for your procedure
# IMPORTANT: Replace these with actual item UUIDs from your Okala project procedure
item_uuids <- list(
  label = "00000000-0000-0000-0000-000000000001",     # UUID for label item
  diameter = "00000000-0000-0000-0000-000000000002",  # UUID for diameter item
  height = "00000000-0000-0000-0000-000000000003"     # UUID for height item
)

# Define project system and procedure IDs
# IMPORTANT: Replace with your actual IDs from Okala
project_system_id <- 1  # Your project system ID
procedure_id <- 1       # Your procedure ID

# Build feature records grouped by plot
feature_payload <- list()

for (plot in unique_plots) {
  cat("Processing plot:", plot, "\n")
  
  # Filter trees for this plot
  plot_trees <- tree_data[tree_data$plotID == plot, ]
  
  # Get timestamp range for this plot
  timestamps <- as.POSIXct(plot_trees$timestamp, format = "%Y-%m-%dT%H:%MZ")
  start_time <- min(timestamps, na.rm = TRUE)
  end_time <- max(timestamps, na.rm = TRUE)
  
  # If same timestamp, add 1 hour to end
  if (start_time == end_time) {
    end_time <- end_time + 3600
  }
  
  # Create plot bounding box geometry
  plot_geometry <- create_plot_bbox(plot_trees)
  
  # Create observations for all trees in this plot
  all_observations <- list()
  for (i in 1:nrow(plot_trees)) {
    tree_obs <- create_tree_observations(plot_trees[i, ], item_uuids)
    all_observations <- c(all_observations, tree_obs)
  }
  
  cat("  - Trees in plot:", nrow(plot_trees), "\n")
  cat("  - Total observations:", length(all_observations), "\n")
  
  # Build the feature record for this plot
  feature <- build_feature_record(
    feature_uuid = paste0("plot-", gsub(" ", "-", plot)),
    project_system_id = project_system_id,
    procedure_id = procedure_id,
    start_time = start_time,
    end_time = end_time,
    created_by_method = "traced",
    geometry = plot_geometry,
    observations = all_observations
  )
  
  feature_payload <- c(feature_payload, list(feature))
}

cat("\nTotal features (plots) to upload:", length(feature_payload), "\n")

# ============================================================================
# 5. VALIDATE THE PAYLOAD (DRY RUN)
# ============================================================================

cat("\n--- Validating payload before upload ---\n")

validation <- validate_observation_payload(
  feature_payload = feature_payload,
  device_settings = device,
  media_dir = NULL  # No media files for this upload
)

if (validation$valid) {
  cat("✓ Payload validation passed!\n")
} else {
  cat("✗ Validation errors:\n")
  for (err in validation$errors) {
    cat("  -", err, "\n")
  }
  stop("Please fix validation errors before uploading")
}

# ============================================================================
# 6. PREVIEW A SINGLE FEATURE (OPTIONAL)
# ============================================================================

cat("\n--- Preview of first feature ---\n")
cat("Feature UUID:", feature_payload[[1]]$feature_uuid, "\n")
cat("Number of observations:", length(feature_payload[[1]]$observations), "\n")
cat("Geometry type:", feature_payload[[1]]$geometry$type, "\n")

# Preview the JSON structure
json_preview <- jsonlite::toJSON(feature_payload[[1]], auto_unbox = TRUE, pretty = TRUE)
cat("\nJSON preview (truncated):\n")
cat(substr(as.character(json_preview), 1, 1000), "...\n")

# ============================================================================
# 7. UPLOAD (UNCOMMENT WHEN READY)
# ============================================================================

# WARNING: This will make actual API calls to upload data
# Make sure you have:
# 1. Set the correct API key
# 2. Set the correct project_id
# 3. Set the correct project_system_id and procedure_id
# 4. Set the correct item_uuids for your procedure

# Uncomment the following lines to perform the actual upload:

# result <- upload_phone_observations(
#   hdr = hdr,
#   project_id = project_id,
#   feature_payload = feature_payload,
#   device_settings = device,
#   media_dir = NULL,
#   validate = TRUE
# )
#
# # Print results
# cat("\n--- Upload Results ---\n")
# print(result$summary)
#
# # Check for failures
# if (length(result$failures) > 0) {
#   cat("\nFailed features:\n")
#   for (f in names(result$failures)) {
#     cat("  -", f, ":", result$failures[[f]]$error, "\n")
#   }
# }

cat("\n--- Test script complete ---\n")
cat("To perform the actual upload, uncomment section 7 and run again.\n")
