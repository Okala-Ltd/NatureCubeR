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



#' Extract total row count from paginated media API payloads when present.
#' @noRd
.extract_media_total_count <- function(payload) {
  if (!is.list(payload) || is.data.frame(payload)) {
    return(NA_integer_)
  }
  for (key in c("total", "total_count", "totalCount", "count", "recordsTotal")) {
    if (is.null(payload[[key]])) next
    value <- suppressWarnings(as.integer(payload[[key]]))
    if (!is.na(value) && value >= 0L) return(value)
  }
  NA_integer_
}

#' Normalise list/dict media API payloads to a tibble.
#' @noRd
.normalize_media_assets_payload <- function(payload) {
  if (is.data.frame(payload)) {
    return(tibble::as_tibble(payload))
  }
  if (is.list(payload)) {
    for (key in c("table", "rows", "data", "items", "results")) {
      rows <- payload[[key]]
      if (is.data.frame(rows)) {
        return(tibble::as_tibble(rows))
      }
    }
    if (length(payload) > 0L) {
      is_row_list <- vapply(payload, is.list, logical(1L))
      if (all(is_row_list)) {
        return(tibble::as_tibble(payload))
      }
    }
  }
  tibble::tibble()
}

#' Parse a getMediaAssets response body into rows and optional total count.
#' @noRd
.parse_media_assets_response <- function(body) {
  payload <- jsonlite::fromJSON(body, simplifyDataFrame = TRUE)
  list(
    rows = .normalize_media_assets_payload(payload),
    total = .extract_media_total_count(payload)
  )
}

#' POST one paginated getMediaAssets page with retry/backoff on rate limits.
#' @noRd
.perform_media_assets_page <- function(hdr,
                                       datatype,
                                       psr_ids,
                                       limit,
                                       offset = 0L,
                                       after_segment_record_id = NULL,
                                       timeout = MEDIA_PAGE_TIMEOUT) {
  req <- httr2::req_url_path_append(
    hdr$root, "getMediaAssets", datatype, hdr$key
  ) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(data = as.list(as.integer(psr_ids))) |>
    httr2::req_timeout(timeout)

  page_limit <- min(as.integer(limit), API_MAX_LIMIT)
  if (!is.null(after_segment_record_id)) {
    req <- httr2::req_url_query(
      req,
      limit = page_limit,
      after_segment_record_id = as.integer(after_segment_record_id)
    )
  } else {
    req <- httr2::req_url_query(
      req,
      limit = page_limit,
      offset = as.integer(offset)
    )
  }

  max_tries <- MEDIA_MAX_RETRIES + 1L
  for (attempt in seq_len(max_tries)) {
    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)

    if (status == 429L && attempt < max_tries) {
      retry_after <- httr2::resp_header(resp, "Retry-After")
      wait <- suppressWarnings(as.numeric(retry_after))
      if (is.na(wait) || wait <= 0) {
        wait <- MEDIA_RETRY_BASE_SECONDS * (2 ^ (attempt - 1L))
      }
      Sys.sleep(max(wait, MEDIA_RETRY_BASE_SECONDS))
      next
    }

    if (status %in% c(500L, 502L, 503L, 504L) && attempt < max_tries) {
      Sys.sleep(MEDIA_RETRY_BASE_SECONDS * (2 ^ (attempt - 1L)))
      next
    }

    httr2::resp_check_status(resp)
    return(.parse_media_assets_response(httr2::resp_body_string(resp)))
  }

  stop("getMediaAssets request failed after retries", call. = FALSE)
}

#' Fetch all media assets for one project system record ID.
#' @noRd
.fetch_media_assets_for_psr <- function(hdr, datatype, psr_id, limit) {
  batches <- list()
  offset <- 0L
  after_segment_record_id <- NULL
  use_keyset <- TRUE
  total <- NA_integer_

  repeat {
    page <- .perform_media_assets_page(
      hdr = hdr,
      datatype = datatype,
      psr_ids = psr_id,
      limit = limit,
      offset = offset,
      after_segment_record_id = if (use_keyset) after_segment_record_id else NULL
    )

    batch <- page$rows
    if (!is.na(page$total)) {
      total <- page$total
    }
    if (nrow(batch) == 0L) break

    batches[[length(batches) + 1L]] <- batch

    segment_ids <- batch$segment_record_id
    segment_ids <- segment_ids[!is.na(segment_ids) & segment_ids > 0L]

    if (use_keyset && length(segment_ids) > 0L) {
      after_segment_record_id <- max(segment_ids)
      if (length(unique(segment_ids)) < limit) break
      next
    }

    use_keyset <- FALSE
    after_segment_record_id <- NULL
    offset <- offset + nrow(batch)

    if (!is.na(total)) {
      if (offset >= total) break
    } else if (nrow(batch) < limit) {
      break
    }
  }

  if (length(batches) == 0L) {
    return(tibble::tibble())
  }
  dplyr::bind_rows(batches)
}

#' Extract the API root URL string from auth headers.
#' @noRd
.api_root_url <- function(hdr) {
  url <- httr2::req_get_url(hdr$root)
  if (is.null(url) || !nzchar(url)) {
    stop("Could not determine API root URL from hdr$root", call. = FALSE)
  }
  as.character(url)
}

#' Fetch media assets in parallel using a PSOCK cluster (safe with httr2).
#' @noRd
.fetch_media_assets_cluster <- function(psrID, hdr, datatype, limit, workers, pb) {
  n <- length(psrID)
  results <- vector("list", n)
  api_key <- hdr$key
  api_root <- .api_root_url(hdr)

  cl <- parallel::makeCluster(min(workers, n))
  on.exit(parallel::stopCluster(cl), add = TRUE)

  parallel::clusterEvalQ(cl, suppressPackageStartupMessages(library(NatureCubeR)))
  parallel::clusterExport(
    cl,
    varlist = c("api_key", "api_root", "datatype", "limit", "psrID"),
    envir = environment()
  )

  i <- 1L
  while (i <= n) {
    wave_end <- min(i + workers - 1L, n)
    wave_idx <- i:wave_end
    wave_results <- parallel::parLapply(cl, wave_idx, function(j) {
      hdr_local <- list(key = api_key, root = httr2::request(api_root))
      getFromNamespace(".fetch_media_assets_for_psr", "NatureCubeR")(
        hdr = hdr_local,
        datatype = datatype,
        psr_id = as.integer(psrID[[j]]),
        limit = limit
      )
    })
    results[wave_idx] <- wave_results
    cli::cli_progress_update(id = pb, inc = length(wave_idx))
    i <- wave_end + 1L
  }

  results
}

#' @title Retrieve media assets for a given project system record ID
#'
#' @description
#' Get all media assets associated with your project for data types
#' c("video","audio","image").
#'
#' Each project system record ID is queried separately. Several stations are
#' fetched concurrently (default \code{4}); the API rate-limits higher
#' concurrency with HTTP 429, which is retried with backoff.
#'
#' Pagination prefers keyset cursors via \code{after_segment_record_id} when
#' the backend supports them, falling back to offset pagination. An empty page
#' ends pagination for that station.
#'
#' @param hdr A base URL provided and valid API key returned by the
#'   function \link{auth_headers}
#' @param datatype A character vector of data types
#'   c("video","audio","image","eDNA")
#' @param psrID A numeric vector of project system record IDs (as found in
#'   \code{stations$project_system_record_id}). Safe to pass every station in
#'   a project.
#' @param record_count Optional integer vector, same length as \code{psrID}.
#'   When provided, stations with \code{record_count == 0} are skipped (no API
#'   call). Pass \code{stations$record_count} from \link{get_station_info}.
#' @param max_workers Maximum number of stations to fetch concurrently.
#'   Defaults to \code{4}. Set to \code{1} for sequential downloads.
#'
#' @return A tibble of media assets for the specified project system record
#'
#' @examples
#' \dontrun{
#'   assets <- get_media_assets(
#'     headers,
#'     datatype = "video",
#'     psrID = stations$project_system_record_id,
#'     record_count = stations$record_count
#'   )
#' }
#'
#' @author
#' Adam Varley
#' @export
get_media_assets <- function(hdr,
                             datatype = c("video", "audio", "image", "eDNA"),
                             psrID,
                             record_count = NULL,
                             max_workers = MEDIA_MAX_WORKERS) {
  psrID <- as.integer(psrID)
  if (length(psrID) == 0L) {
    return(tibble::tibble())
  }

  if (!is.null(record_count)) {
    if (length(record_count) != length(psrID)) {
      stop("record_count must be the same length as psrID", call. = FALSE)
    }
    station_tbl <- tibble::tibble(
      psr_id = psrID,
      record_count = as.integer(record_count)
    ) |>
      dplyr::group_by(.data$psr_id) |>
      dplyr::summarise(
        record_count = max(.data$record_count, na.rm = TRUE),
        .groups = "drop"
      )
    n_skip <- sum(!is.na(station_tbl$record_count) & station_tbl$record_count <= 0L)
    if (n_skip > 0L) {
      message("Skipping ", n_skip, " station(s) with record_count = 0")
    }
    station_tbl <- station_tbl[
      is.na(station_tbl$record_count) | station_tbl$record_count > 0L,
    ]
    psrID <- station_tbl$psr_id
  } else {
    psrID <- unique(psrID)
  }

  if (length(psrID) == 0L) {
    return(tibble::tibble())
  }

  limit <- API_MAX_LIMIT
  workers <- max(1L, min(as.integer(max_workers), length(psrID)))

  pb <- cli::cli_progress_bar(
    format = "Fetching media assets {cli::pb_current}/{cli::pb_total} stations | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    total = length(psrID),
    clear = FALSE
  )

  if (workers > 1L) {
    all_results <- .fetch_media_assets_cluster(
      psrID = psrID,
      hdr = hdr,
      datatype = datatype,
      limit = limit,
      workers = workers,
      pb = pb
    )
  } else {
    all_results <- vector("list", length(psrID))
    for (i in seq_along(psrID)) {
      all_results[[i]] <- .fetch_media_assets_for_psr(
        hdr = hdr,
        datatype = datatype,
        psr_id = psrID[[i]],
        limit = limit
      )
      cli::cli_progress_update(id = pb, inc = 1L)
    }
  }

  cli::cli_progress_done(id = pb)
  dplyr::bind_rows(all_results)
}