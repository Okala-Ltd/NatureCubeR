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
get_project <- function(hdr) {
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
                             datatype = c("video", "audio", "image", "eDNA")) {
  urlreq_ap <- httr2::req_url_path_append(
    hdr$root, "getStations", datatype, hdr$key)
  preq <- httr2::req_perform(urlreq_ap)
  resp <- httr2::resp_body_string(preq)
  geojson_response <- geojsonsf::geojson_sf(resp)

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
plot_stations <- function(geojson_response) {
    message('Plotting stations')
    leaflet::leaflet(data = geojson_response) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(
        lat = sf::st_coordinates(geojson_response)[, 2],
        lng = sf::st_coordinates(geojson_response)[, 1],
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
        stroke = TRUE,
        fillOpacity = 0.6,
        radius = ~ scales::rescale(record_count, c(5, 15))
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
                             datatype = c("video", "audio", "image", "eDNA"),
                             psrID) {

  urlreq_ap <- httr2::req_url_path_append(
    hdr$root, "getMediaAssets", datatype, hdr$key) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(data = psrID)

  preq <- httr2::req_perform(urlreq_ap, verbosity = 3)
  resp <- httr2::resp_body_string(preq)

  return(jsonlite::fromJSON(resp) %>% tibble::as_tibble())

}
