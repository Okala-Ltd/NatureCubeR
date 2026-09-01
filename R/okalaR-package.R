#' @keywords internal
"_PACKAGE"

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# Package-level constant for the maximum number of records per API request
API_MAX_LIMIT <- 1000L

# Media asset download tuning (aligned with NatureCubePy)
MEDIA_MAX_WORKERS <- 4L
MEDIA_MAX_RETRIES <- 6L
MEDIA_RETRY_BASE_SECONDS <- 2
MEDIA_PAGE_TIMEOUT <- 180

## usethis namespace: start
## usethis namespace: end
NULL
