#' @title Get API key from environment variable
#'
#' @description
#' Retrieves the API key from the environment variable NATURECUBE_API_KEY.
#' You may also pass `api_key` directly for local testing.
#' If neither is set, an error is raised. # nolint
#'
#' @param api_key Optional API key string. If provided and non-empty,
#'   this value is returned. Otherwise the function reads `NATURECUBE_API_KEY`.
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

  api_key_env <- Sys.getenv("NATURECUBE_API_KEY")
  # Support quoted values in .Renviron, e.g. NATURECUBE_API_KEY='...'
  api_key_env <- gsub("^['\"]|['\"]$", "", api_key_env)
  if (api_key_env == "") {
    stop(
      "No API key found. Set NATURECUBE_API_KEY or pass api_key to get_key(api_key = ...)."
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
#' @param api_key A valid API key. If omitted, reads \code{NATURECUBE_API_KEY}
#'   via \code{get_key()}.
#' @param NATURECUBE_URL The base URL for the API. If omitted, reads
#'   `NATURECUBE_URL` from the environment and falls back to production.
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
auth_headers <- function(api_key = get_key(),
                         NATURECUBE_URL = Sys.getenv(
                           "NATURECUBE_URL",
                           unset = "https://naturecube.io/api/"
                         )) {
  NATURECUBE_URL <- gsub("^['\"]|['\"]$", "", NATURECUBE_URL)
  root <- httr2::request(NATURECUBE_URL)
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
#' @param api_key A valid API key. If omitted, reads \code{NATURECUBE_API_KEY}
#'   via \code{get_key()}.
#' @param NATURECUBE_URL The base URL for the SIT API. If omitted, reads
#'   `NATURECUBE_SIT_URL` and falls back to the SIT default URL.
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
    api_key = get_key(),
    NATURECUBE_URL = Sys.getenv(
      "NATURECUBE_SIT_URL",
      unset = "https://sit.api.naturecube.io/api/"
    )) {
  NATURECUBE_URL <- gsub("^['\"]|['\"]$", "", NATURECUBE_URL)
  root <- httr2::request(NATURECUBE_URL)
  d <- list(key = api_key,
            root = root)
  return(d)
}
