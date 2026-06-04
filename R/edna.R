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
