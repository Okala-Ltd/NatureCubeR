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
                         okala_url = "https://api.dashboard.okala.io/api/") {
  root <- httr2::request(okala_url)
  d <- list(key = api_key,
            root = root)
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
    okala_url = "https://dev.api.dashboard.okala.io/api/") {
  root <- httr2::request(okala_url)
  d <- list(key = api_key,
            root = root)
  return(d)
}
