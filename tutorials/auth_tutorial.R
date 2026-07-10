# =============================================================================
# Authentication Tutorial
# =============================================================================
# This tutorial covers how to authenticate with the NatureCube API using NatureCubeR.
#
# Before running this script:
#   1. Obtain your API key from the NatureCube dashboard: https://naturecube.io
#   2. Add your credentials to your ~/.Renviron file (recommended):
#
#        NATURECUBE_API_KEY=your_api_key_here
#        NATURECUBE_URL=https://naturecube.io/api/
#
#      Then restart R so the new environment variables are picked up.
#      You can open ~/.Renviron quickly with: usethis::edit_r_environ()
# =============================================================================

library(NatureCubeR)

# -----------------------------------------------------------------------------
# Step 1: Retrieve your API key
# -----------------------------------------------------------------------------
# get_key() reads NATURECUBE_API_KEY from your environment. If the variable is
# not set, it will raise an informative error.
#
# You can also pass the key directly (useful for quick testing only —
# never commit a hard-coded key to version control):
#
#   api_key <- get_key(api_key = "your_api_key_here")

api_key <- get_key()
cat("API key retrieved successfully.\n")

# -----------------------------------------------------------------------------
# Step 2: Create auth headers (production)
# -----------------------------------------------------------------------------
# auth_headers() combines your API key with the base URL and returns a list
# that is passed to all other NatureCubeR functions as `hdr`.
#
# The base URL defaults to the NATURECUBE_URL environment variable, falling
# back to "https://naturecube.io/api/" if the variable is not set.

hdr <- auth_headers(api_key)

# You can inspect what was created:
cat("Base URL:", hdr$root$url, "\n")

# -----------------------------------------------------------------------------
# Step 3: Verify the connection
# -----------------------------------------------------------------------------
# get_project() is a lightweight call that returns basic project metadata.
# A successful response confirms your key and URL are correct.

project <- get_project(hdr = hdr)
print(project)

# -----------------------------------------------------------------------------
# Step 4 (optional): Connect to the development / SIT environment
# -----------------------------------------------------------------------------
# Use auth_headers_dev() when testing against the staging environment.
# It reads NATURECUBE_SIT_URL, defaulting to https://sit.api.naturecube.io/api/
#
# Add to ~/.Renviron if you need a non-default SIT URL:
#   NATURECUBE_SIT_URL=https://your-sit-url/api/

# hdr_dev <- auth_headers_dev(api_key)
# project_dev <- get_project(hdr = hdr_dev)
# print(project_dev)
