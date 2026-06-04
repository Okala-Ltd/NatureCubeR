library(tidyverse)
library(jsonlite)
library(jsonify)
library(leaflet)


#' @title Get API key from environment variable
#'
#' @description
#' Retrieves the API key from the environment variable OKALA_API_KEY.
#' If the variable is not set, an error is raised. # nolint
#'
#' @return The API key as a character string
#'
#' @examples
#' \dontrun{
#'   api_key <- get_key()
#' }
#'
#' @author
#' Adam Varley
#' @export
get_key <- function() {
  api_key <- Sys.getenv("OKALA_API_KEY")
  if (api_key == "") stop("OKALA_API_KEY environment variable not set.")
  return(api_key)
}

#' @title Initiate root URL with API key
#'
#' @description
#' Creates a base URL object that can be used as a root to call endpoints.
#' This requires a project API key, which can be obtained directly from the
#' Okala dashboard.
#'
#' @param api_key A valid API key
#' @param okala_url The base URL for the Okala API (default: production)
#'
#' @return A list containing the root URL and the API key
#'
#' @examples
#' \dontrun{
#'   headers <- auth_headers("your_api_key")
#' }
#'
#' @author
#' Adam Varley
#' @export
auth_headers <- function(api_key,
                         okala_url="https://api.dashboard.okala.io/api/"){
  root <- httr2::request(okala_url)
  d = list(key=api_key,
           root=root)
  return(d)
}

#' @title Initiate root URL with API key (Development)
#'
#' @description
#' Creates a base URL object for the development Okala API.
#' Requires a project API key.
#'
#' @param api_key A valid API key
#' @param okala_url The base URL for the Okala dev API (default: dev)
#'
#' @return A list containing the root URL and the API key
#'
#' @examples
#' \dontrun{
#'   headers <- auth_headers_dev("your_api_key")
#' }
#'
#' @author
#' Adam Varley
#' @export
auth_headers_dev <- function(
    api_key,
    okala_url="https://dev.api.dashboard.okala.io/api/"){
  root <- httr2::request(okala_url)
  d = list(key=api_key,
           root=root)
  return(d)
}


#' @title Get Project Information
#'
#' @description
#' Retrieves information about the active project associated with the
#' provided API key and sets it as the active project.
#'
#' @param hdr A list containing the root URL and API key, as returned by
#'   \link{auth_headers}.
#'
#' @return
#' No return value. Displays a message indicating the active project name.
#'
#' @examples
#' \dontrun{
#'   headers <- auth_headers("your_api_key")
#'   get_project(headers)
#' }
#'
#' @author
#' Adam Varley
#' @export
get_project <- function(hdr){
  urlreq_ap <- httr2::req_url_path_append(hdr$root, "getProject", hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp_str <- httr2::resp_body_json(preq)
  project_name <- resp_str$boundary$features[[1]]$properties$project_name
  message('Setting your active project as - ', project_name)
}


#' @title Get project station metadata
#'
#' @description
#' Retrieve all of the station data associated with your project, including
#' video, audio, image, and eDNA data types.
#'
#' @param hdr A base URL provided and valid API key returned by the
#'   function \link{auth_headers}
#' @param datatype A character vector of data types
#'   c("video","audio","image","eDNA")
#'
#' @return An sf object containing station metadata and geometry
#'
#' @examples
#' \dontrun{
#'   stations <- get_station_info(headers, datatype="video")
#' }
#'
#' @author
#' Adam Varley
#' @export
get_station_info <- function(hdr,
                             datatype=c("video","audio","image","eDNA")){
  urlreq_ap <- httr2::req_url_path_append(
    hdr$root, "getStations", datatype, hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)
  geojson_response = geojsonsf::geojson_sf(resp)

  return(geojson_response)
}

#' @title Plot stations on a leaflet map
#'
#' @description
#' Plots station locations using leaflet, with circle markers sized by
#' record count.
#'
#' @param geojson_response An sf object containing station metadata and
#'   geometry
#'
#' @return A leaflet map widget
#'
#' @examples
#' \dontrun{
#'   plot_stations(stations)
#' }
#'
#' @author
#' Adam Varley
plot_stations <- function(geojson_response){
    message('Plotting stations')
    leaflet::leaflet(data = geojson_response) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(
        lat=sf::st_coordinates(geojson_response)[,2],
        lng=sf::st_coordinates(geojson_response)[,1],
        label = ~paste(device_id),
        popup = ~paste("QR code: ", device_id, "<br>",
        "Start time: ",
        project_system_record_start_timestamp, "<br>",
        "End time: ",
        project_system_record_end_timestamp, "<br>",
        "No. media files: ", record_count, "<br>"
        ),
        color = "red",
        opacity = 0.2,
        stroke = T,
        fillOpacity = 0.6,
        radius = ~ scales::rescale(record_count, c(5,15))
      )

}



#' @title Retrieve media assets for a given project system record ID
#'
#' @description
#' Get all of the station data associated with your project.
#' For data types c("video","audio","image")
#'
#' @param hdr A base URL provided and valid API key returned by the
#'   function \link{auth_headers}
#' @param datatype A character vector of data types
#'   c("video","audio","image","eDNA")
#' @param psrID Unique project system ID for which the media assets
#'   will be retrieved
#'
#' @return A tibble of media assets for the specified project system record
#'
#' @examples
#' \dontrun{
#'   assets <- get_media_assets(headers, datatype="video", psrID=123)
#' }
#'
#' @author
#' Adam Varley
#' @export
get_media_assets <- function(hdr,
                             datatype=c("video","audio","image","eDNA"),
                             psrID){

  urlreq_ap <- httr2::req_url_path_append(
    hdr$root, "getMediaAssets", datatype, hdr$key) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(data=psrID)

  preq <- httr2::req_perform(urlreq_ap,verbosity=3)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp) %>% tibble::as_tibble())

}



#' @title Get project labels for either bioacoustics or camera
#'
#' @description
#' Labels are derived by using either suggested labels on the platform or
#' by manually adding labels from the wider database.
#'
#' @param hdr A base URL provided and valid API key returned by the
#'   function \link{auth_headers}
#' @param labeltype A character vector specifying the label type
#'   ('Bioacoustic' or 'Camera')
#'
#' @return A tibble containing project labels
#'
#' @examples
#' \dontrun{
#'   labels <- get_project_labels(headers, labeltype='Camera')
#' }
#'
#' @author
#' Adam Varley
#' @export
get_project_labels <- function(hdr,
                               labeltype = c('Bioacoustic','Camera')){
  urlreq_ap <- httr2::req_url_path_append(
    hdr$root, "getProjectLabels", labeltype, hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp) %>% tibble::as_tibble())
}

#' @title Add project labels for either bioacoustics or camera
#'
#' @description
#' Add labels so labellers have access to them in the Dashboard. Labels are
#' derived by using either suggested labels on the platform or by manually
#' adding labels from the wider database.
#'
#' @param hdr A base URL provided and valid API key returned by the
#'   function \link{auth_headers}
#' @param labeltype A character vector specifying the label type
#'   ('Bioacoustic' or 'Camera')
#' @param labels A label object list specifying the labels to be added
#'
#' @return A success message as a list
#'
#' @examples
#' \dontrun{
#'   add_project_labels(headers, labeltype='Camera', labels=my_labels)
#' }
#'
#' @author
#' Adam Varley
#' @export
add_project_labels <- function(hdr,
                               labeltype = c('Bioacoustic','Camera'),labels){
  urlreq_ap <- httr2::req_url_path_append(hdr$root,"addProjectLabels",labeltype,hdr$key)
  urlreq_ap <- urlreq_ap |>  httr2::req_method("POST") |> httr2::req_body_json(data=labels)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq)

  return(resp)
}

# Utility function to replace NULLs with NAs in a data frame
replace_nas <- function(df){
  df[sapply(df,function(x) is.null(x))] = NA
  return(df)
}

#' @title Get labels from the wider IUCN database (all species)
#'
#' @description
#' Retrieve labels from the wider IUCN database, optionally filtered by a search term.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param offset An integer specifying the offset for the query
#' @param limit An integer specifying the limit for the query
#' @param search_term A character vector specifying the search term to be used (optional)
#'
#' @return A list containing tabular data and pagination information for iterative calls
#'
#' @examples
#' \dontrun{
#'   getIUCNLabels(headers, offset=0, limit=100, search_term="horse")
#' }
#'
#' @author
#' Adam Varley
#' @export
getIUCNLabels <- function(hdr, offset, limit,search_term=NULL){
  if (is.null(search_term)){
    search_term = ""
  }
  if (limit > 20000){
    stop("Limit cannot be greater than 20000")
  }
  urlreq_ap <- httr2::req_url_path_append(hdr$root,"getIUCNLabels",hdr$key)
  urlreq_ap <- urlreq_ap |>  httr2::req_method("GET") |> httr2::req_url_query("offset" = offset, "limit" = limit,'search_term'= search_term)

  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq)
  resp_table <- lapply(resp$table, function(x) x %>% replace_nas() %>%  tibble::as_tibble()) %>% dplyr::bind_rows()

  return (list(data=resp_table, total=resp$pagination_state$total, offset=resp$pagination_state$offset, limit=resp$pagination_state$limit))
}


#' @title Add labels from the wider IUCN database (all species)
#'
#' @description
#' Add labels from the wider IUCN database in chunks.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param labels A data frame of labels to add
#' @param chunksize An integer specifying the chunk size for the submission
#'
#' @return A success message as a list
#'
#' @examples
#' \dontrun{
#'   add_IUCN_labels(headers, labels=my_labels, chunksize=200)
#' }
#'
#' @author
#' Adam Varley
#' @export
add_IUCN_labels <- function(hdr,labels,chunksize){

  if(nrow(labels) < 100){
    message('Data is too small to chunk, submitting all data')
    chunksize = nrow(labels)
    spl.dt = list(labels)
  } else {

    if(chunksize > nrow(labels)){
      message('chunksize is bigger than length of data altering chunkszie to ', nrow(labels))
      chunksize = nrow(labels)/2
    } else {
      spl.dt <- split( labels , cut(seq_len(nrow(labels)), round(nrow(labels)/chunksize)))

    }
  }


  i = 31
  for (i in seq_along(spl.dt)){

    urlreq_ap <- httr2::req_url_path_append(hdr$root,"addIUCNLabels",hdr$key)
    urlreq_ap <- urlreq_ap |>  httr2::req_method("POST") |> httr2::req_body_json(data=spl.dt[[i]])

    preq <- httr2::req_perform(urlreq_ap,verbosity=3)
    resp <- httr2::resp_body_json(preq)

    message('submitted ',i*chunksize,' labels of ', nrow(nrow(labels)))
  }

  return(resp)
}


# Internally used function to send updated labels in chunks

sendupatedlabels <- function(hdr,datachunk) {


  datachunk = jsonlite::toJSON(datachunk,pretty=TRUE)

  urlreq_ap <- httr2::req_url_path_append(hdr$root,"updateSegmentLabels", hdr$key)
  urlreq_ap <- urlreq_ap |>  httr2::req_method("PUT")  |> httr2::req_body_json(jsonlite::fromJSON(datachunk))
  #
  preq <- httr2::req_perform(urlreq_ap,verbosity=3)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp))
}

#' @title Push new labels using a chunked process
#'
#' @description
#' Push new labels to the platform in chunks.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}
#' @param submission_records A tibble containing the records to be submitted
#' @param chunksize An integer specifying the chunk size for the submission
#'
#' @return A list containing tabular data and pagination information for iterative calls
#'
#' @examples
#' \dontrun{
#'   push_new_labels(headers, submission_records, chunksize=30)
#' }
#'
#' @author
#' Adam Varley
#' @export
push_new_labels <- function(hdr,submission_records,chunksize){

  if(chunksize > nrow(submission_records)){
    message('chunksize is bigger than length of data altering chunkszie to ', nrow(labels))
    chunksize = nrow(submission_records)
  }

  spl.dt <- split( submission_records , cut(seq_len(nrow(submission_records)), round(nrow(submission_records)/chunksize)))
  spl.dt[1]
  i=1
  for (i in seq_along(spl.dt)){

    sendupatedlabels(hdr,datachunk=spl.dt[[i]])
    message('submitted ',i*chunksize,' labels of ', nrow(submission_records))
  }
}



# Internal function used to chunk media metadata
send_media_chunks <- function(hdr, datachunk) {
  datachunk = jsonlite::toJSON(datachunk,pretty=TRUE)

  urlreq_ap <- httr2::req_url_path_append(hdr$root,"updateTimestamps", hdr$key)
  urlreq_ap <- urlreq_ap |>  httr2::req_method("PUT")  |> httr2::req_body_json(jsonlite::fromJSON(datachunk))
  #
  preq <- httr2::req_perform(urlreq_ap,verbosity=3)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp))
}


#' @title Update timestamps for multiple media records
#'
#' @description
#' Updates timestamps for one or more media file records in a single API call.
#' This function provides direct access to the updateTimestamps endpoint without
#' automatic chunking. For large datasets (>1000 records), consider using
#' \link{push_new_timestamps} which handles chunking automatically.
#'
#' @param hdr A base URL provided and valid API key returned by the
#'   function \link{auth_headers}
#' @param media_records A data frame or tibble containing the media records to update.
#'   Must contain the following columns:
#'   \itemize{
#'     \item \strong{media_file_record_id} (required): Numeric. The unique identifier
#'           of the media file record to update.
#'     \item \strong{new_timestamp} (required): Character. The new timestamp in
#'           ISO 8601 format (e.g., "2024-01-15T10:30:00" or "2024-01-15 10:30:00").
#'   }
#'   Additional columns will be ignored.
#'
#' @return A list containing the API response with update status
#'
#' @examples
#' \dontrun{
#'   # Create a data frame with media records to update
#'   updates <- data.frame(
#'     media_file_record_id = c(123, 456, 789),
#'     new_timestamp = c(
#'       "2024-01-15T10:30:00",
#'       "2024-01-15T14:20:00",
#'       "2024-01-15T18:45:00"
#'     )
#'   )
#'   
#'   # Update the timestamps
#'   result <- update_media_timestamps(headers, updates)
#'   
#'   # Using tibble format
#'   library(tibble)
#'   updates <- tibble(
#'     media_file_record_id = c(123, 456),
#'     new_timestamp = c("2024-01-15T10:30:00", "2024-01-15T14:20:00")
#'   )
#'   result <- update_media_timestamps(headers, updates)
#' }
#'
#' @seealso
#' \link{push_new_timestamps} for automatic chunking of large datasets
#'
#' @author
#' Adam Varley
#' @export
update_media_timestamps <- function(hdr, media_records) {
  # Validate required columns
  required_cols <- c("media_file_record_id", "new_timestamp")
  missing_cols <- setdiff(required_cols, names(media_records))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Validate data types
  if (!is.numeric(media_records$media_file_record_id)) {
    stop("media_file_record_id must be numeric")
  }
  if (any(is.na(media_records$media_file_record_id))) {
    stop("media_file_record_id cannot contain NA values")
  }
  if (any(media_records$media_file_record_id <= 0)) {
    stop("media_file_record_id must be positive")
  }
  if (any(media_records$media_file_record_id %% 1 != 0)) {
    stop("media_file_record_id must be an integer")
  }
  
  if (!is.character(media_records$new_timestamp)) {
    stop("new_timestamp must be character string in ISO 8601 format")
  }
  
  # Basic ISO 8601 format validation for new_timestamp
  # Accepts patterns like: 2024-01-31T23:59:59Z or 2024-01-31T23:59:59+01:00
  iso8601_pattern <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:?\\d{2})?$"
  invalid_idx <- which(!is.na(media_records$new_timestamp) &
                         !grepl(iso8601_pattern, media_records$new_timestamp))
  if (length(invalid_idx) > 0) {
    stop(
      "new_timestamp must be in ISO 8601 format, e.g. '2024-01-31T23:59:59Z'. ",
      "Invalid values at row(s): ",
      paste(invalid_idx, collapse = ", ")
    )
  }
  
  # Select only required columns
  media_records <- media_records[, required_cols, drop = FALSE]
  
  # Make API request
  urlreq_ap <- httr2::req_url_path_append(hdr$root, "updateTimestamps", hdr$key) %>%
    httr2::req_method("PUT") %>%
    httr2::req_body_json(media_records)
  
  preq <- tryCatch(
    httr2::req_perform(urlreq_ap),
    error = function(e) {
      stop(
        "Failed to perform request to update media timestamps: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  resp <- httr2::resp_body_string(preq)
  
  result <- jsonlite::fromJSON(resp)
  
  message("Successfully updated ", nrow(media_records), " media timestamp(s)")
  
  return(result)
}


#' @title Push new timestamps to the platform in chunks
#'
#' @description
#' Updates timestamps for multiple media file records by automatically splitting
#' the data into manageable chunks. This function is recommended for large
#' datasets (>1000 records) as it prevents timeout issues and provides progress
#' tracking. For smaller datasets, consider using \link{update_media_timestamps}
#' for direct submission without chunking.
#'
#' @param hdr A base URL provided and valid API key returned by the
#'   function \link{auth_headers}
#' @param media_metadata A data frame or tibble containing the media records to update.
#'   Must contain the following columns:
#'   \itemize{
#'     \item \strong{media_file_record_id} (required): Numeric. The unique identifier
#'           of the media file record to update.
#'     \item \strong{new_timestamp} (required): Character. The new timestamp in
#'           ISO 8601 format (e.g., "2024-01-15T10:30:00" or "2024-01-15 10:30:00").
#'   }
#'   Additional columns will be ignored during submission.
#' @param chunksize An integer specifying the number of records to submit per
#'   chunk. Recommended values: 50-200 depending on network conditions. If
#'   chunksize exceeds the number of rows, it will be automatically adjusted.
#'
#' @return No explicit return value. Progress messages are displayed for each
#'   chunk submitted.
#'
#' @examples
#' \dontrun{
#'   # Create a large dataset with timestamp updates
#'   updates <- data.frame(
#'     media_file_record_id = 1:1000,
#'     new_timestamp = seq(
#'       as.POSIXct("2024-01-15 08:00:00"),
#'       by = "30 sec",
#'       length.out = 1000
#'     ) %>% format("%Y-%m-%dT%H:%M:%S")
#'   )
#'   
#'   # Push updates in chunks of 100
#'   push_new_timestamps(headers, updates, chunksize = 100)
#'   
#'   # Using tibble format
#'   library(tibble)
#'   updates <- tibble(
#'     media_file_record_id = c(123, 456, 789),
#'     new_timestamp = c(
#'       "2024-01-15T10:30:00",
#'       "2024-01-15T14:20:00",
#'       "2024-01-15T18:45:00"
#'     )
#'   )
#'   push_new_timestamps(headers, updates, chunksize = 50)
#' }
#'
#' @seealso
#' \link{update_media_timestamps} for direct submission without chunking
#'
#' @author
#' Adam Varley
#' @export
push_new_timestamps <- function(hdr, media_metadata, chunksize) {
    if(chunksize > nrow(media_metadata)){
        message('chunksize is bigger than length of data altering chunkszie to ', nrow(media_metadata))
        chunksize = nrow(media_metadata)
    }

    spl.dt <- split( media_metadata , cut(seq_len(nrow(media_metadata)), round(nrow(media_metadata)/chunksize)))
    i=1
    for (i in seq_along(spl.dt)){

        send_media_chunks(hdr,spl.dt[[i]])
        message('submitted ',i*chunksize,' timestamps of ', nrow(media_metadata))
    }
}

#' @title Set blank status for segment labels
#'
#' @description
#' Marks or unmarks segment labels as blank for a given list of segment record IDs.
#'
#' @param hdr A base URL provided and valid API key returned by the function \link{auth_headers}.
#' @param blank_status A boolean value indicating whether to mark as blank (TRUE) or unblank (FALSE).
#' @param segment_record_ids A numeric vector of segment record IDs to update.
#'
#' @return A list containing the API response message.
#'
#' @examples
#' \dontrun{
#'   # Mark segments as blank
#'   set_segment_blank_status(headers, blank_status = TRUE, segment_record_ids = c(101, 102, 103))
#'   # Unmark segments as blank
#'   set_segment_blank_status(headers, blank_status = FALSE, segment_record_ids = c(101, 102, 103))
#' }
#'
#' @author
#' Adam Varley
#' @export
set_segment_blank_status <- function(hdr, blank_status, segment_record_ids) {
  status_str <- tolower(as.character(blank_status))

  urlreq_ap <- httr2::req_url_path_append(hdr$root, "segmentLabelsBlankStatus", hdr$key, status_str) %>%
    httr2::req_method("PUT") %>%
    httr2::req_body_json(data = segment_record_ids)

  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq)

  message(resp$message)
  return(resp)
}

#' @title Check eDNA Labels
#'
#' @description
#' Validates eDNA records against the Okala database and returns matching labels.
#' Uses a hierarchical taxonomy approach: species -> genus -> family -> order -> class -> phylum -> kingdom.
#' Returns the most specific taxonomic level that matches the database.
#'
#' @param hdr A list containing the root URL and API key for authentication (from auth_headers()).
#' @param edna_data A data frame or tibble with eDNA records containing the following columns:
#'   \itemize{
#'     \item marker_name: The genetic marker used (required)
#'     \item sequence: DNA sequence (required)
#'     \item primer: Primer used for amplification (required)
#'     \item timestamp: Timestamp for the record (required)
#'     \item kingdom: Kingdom taxonomic rank (optional)
#'     \item phylum: Phylum taxonomic rank (optional)
#'     \item class: Class taxonomic rank (optional, note: 'class_' can also be used)
#'     \item order: Order taxonomic rank (optional)
#'     \item family: Family taxonomic rank (optional)
#'     \item genus: Genus taxonomic rank (optional)
#'     \item species: Species taxonomic rank (optional)
#'     \item confidence: Confidence score 0-100 (optional, defaults to 100)
#'   }
#'
#' @return A tibble with the original data plus additional columns:
#'   \itemize{
#'     \item label: The matched label name from the database
#'     \item label_id: The database ID of the matched label
#'     \item status: "success" or "error"
#'     \item message: Status message describing the result
#'   }
#'
#' @examples
#' \dontrun{
#'   headers <- auth_headers()
#'   
#'   edna_records <- data.frame(
#'     marker_name = "COI",
#'     sequence = "ACGTACGT",
#'     primer = "mlCOIintF",
#'     timestamp = "2024-01-15 10:30:00",
#'     species = "Panthera leo",
#'     genus = "Panthera",
#'     family = "Felidae",
#'     confidence = 95
#'   )
#'   
#'   validated <- check_edna_labels(headers, edna_records)
#' }
#'
#' @author
#' Adam Varley
#' @export
check_edna_labels <- function(hdr, edna_data) {
  # Validate required columns
  required_cols <- c("marker_name", "sequence", "primer", "timestamp")
  missing_cols <- setdiff(required_cols, names(edna_data))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Ensure confidence column exists with default value of 100
  if (!"confidence" %in% names(edna_data)) {
    edna_data$confidence <- 100
  }
  
  # Handle class_ vs class column naming
  if ("class_" %in% names(edna_data) && !"class" %in% names(edna_data)) {
    edna_data$class <- edna_data$class_
  }
  
  # Convert to list format for JSON
  edna_list <- lapply(seq_len(nrow(edna_data)), function(i) {
    row <- as.list(edna_data[i, ])
    # Remove NA values to send cleaner JSON
    row[!is.na(row)]
  })
  
  # Make API request
  urlreq_ap <- httr2::req_url_path_append(hdr$root, "checkeDNALabels", hdr$key) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(data = edna_list)
  
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq, simplifyVector = TRUE)
  
  # Convert to tibble
  result <- tibble::as_tibble(resp)
  
  message("Validated ", nrow(result), " eDNA records")
  return(result)
}

#' @title Upload eDNA Records
#'
#' @description
#' Uploads validated eDNA records to a specific project system record.
#' Only records with status='success' from check_edna_labels will be uploaded.
#'
#' @param hdr A list containing the root URL and API key for authentication
#'   (from auth_headers()).
#' @param validated_data A data frame or tibble containing validated eDNA
#'   records from check_edna_labels. Must include all original columns plus
#'   label, label_id, status, and message fields.
#' @param project_system_record_id The project system record ID to which the
#'   eDNA records will be uploaded.
#'
#' @return A tibble with the upload response for each record
#'
#' @examples
#' \dontrun{
#'   headers <- auth_headers()
#'   
#'   # First validate the records
#'   validated <- check_edna_labels(headers, edna_records)
#'   
#'   # Then upload only successful validations
#'   upload_result <- upload_edna_records(
#'     headers,
#'     validated,
#'     project_system_record_id = 123
#'   )
#' }
#'
#' @author
#' Adam Varley
#' @export
upload_edna_records <- function(hdr, validated_data,
                                 project_system_record_id) {
  # Filter for only successful records
  if (!"status" %in% names(validated_data)) {
    stop("Data must be validated first using check_edna_labels()")
  }
  
  successful_records <- validated_data[validated_data$status == "success", ]
  
  if (nrow(successful_records) == 0) {
    stop("No successful records to upload. All records failed validation.")
  }
  
  message("Uploading ", nrow(successful_records), " validated eDNA records")
  
  # Convert to list format for JSON
  edna_list <- lapply(seq_len(nrow(successful_records)), function(i) {
    row <- as.list(successful_records[i, ])
    # Remove NA values to send cleaner JSON
    row[!is.na(row)]
  })
  
  # Make API request
  urlreq_ap <- httr2::req_url_path_append(
    hdr$root, "uploadeDNA", hdr$key,
    as.character(project_system_record_id)) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(data = edna_list)
  
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_json(preq, simplifyVector = TRUE)
  
  # Convert to tibble
  result <- tibble::as_tibble(resp)
  
  message("Upload complete: ", nrow(result), " records processed")
  return(result)
}






