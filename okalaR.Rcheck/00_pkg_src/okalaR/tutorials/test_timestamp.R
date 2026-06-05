source('R/api.R')

# Your api key is found in your project settings on the Okala dashboard.
# You can find the Okala dashboard here: https://dashboard.okala.io/

api_key <- Sys.getenv("OKALA_API_KEY")
if (api_key == "") stop("OKALA_API_KEY environment variable not set.")
# Set auth headers appropriately
headers <- auth_headers(api_key,okala_url="https://api.dashboard.okala.io/api/")

# Way to see the project you are pulling and confirm api key is correct
get_project(hdr=headers)

# Get station information for videos in the project and there corresponding IDs
stations <- get_station_info(hdr=headers,datatype="video")

plot_stations(stations)

# Get media lables for a list of sensors
media_labels <- get_media_assets(hdr=headers,
                                 datatype="video",
                                 psrID=stations$project_system_record_id)

subset <- media_labels[1:100,]

# Get media labels for a specific sensor

subset$media_file_created_at <- lubridate::as_datetime(subset$media_file_created_at)

subset$media_file_created_at <- subset$media_file_created_at + 60 # Add 60 seconds (1 minute) to the timestamps


push_new_timestamps(hdr=headers,media_metadata = subset,chunksize = 50)
