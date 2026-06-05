#' @title Get API key from environment variable
#'
#' @description
#' Retrieves the API key from the environment variable OKALA_API_KEY.
#' You may also pass `api_key` directly for local testing.
#' If neither is set, an error is raised. # nolint
#'
#' @param api_key Optional API key string. If provided and non-empty,
#'   this value is returned. Otherwise the function reads `OKALA_API_KEY`.
#'
#' @return The API key as a character string
#'
#' @examples
#' \dontrun{
#'   api_key <- get_key()
#'   api_key <- get_key(api_key = "your_api_key")
#' }
#'
#' @author
#' Adam Varley
#' @export
get_key <- function(api_key = NULL) {
  if (!is.null(api_key) && nzchar(api_key)) {
    return(api_key)
  }

  api_key_env <- Sys.getenv("OKALA_API_KEY")
  if (api_key_env == "") {
    stop(
      "No API key found. Set OKALA_API_KEY or pass api_key to get_key(api_key = ...)."
    )
  }
  return(api_key_env)
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
                         okala_url = "https://naturecube.io/api/") {
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
    okala_url = "https://sit.api.naturecube.io/api/") {
  root <- httr2::request(okala_url)
  d <- list(key = api_key,
            root = root)
  return(d)
}
