# Observation Upload From CSV

This document describes the easiest route to upload observations to the
`uploadObservations` endpoint.

## What the workflow does

- Pulls project schema once using your API key
- Selects a system and procedure
- Resolves `item_uuid` from `item_name` when needed
- Groups rows into observations using:
  - `observation_id` when present
  - otherwise `(longitude, latitude, recorded_at)`
- Uploads all built observations

## Required CSV columns

Minimum required columns:

- `longitude`
- `latitude`
- `recorded_at`

Recommended columns:

- `item_uuid` (optional if `item_name` is present and schema has matching names)
- `item_name`
- `data` (text values)
- `numbers` (numeric fallback)
- `observation_id` (optional, improves grouping)

## Fast start

```r
library(okalaR)

hdr <- auth_headers(get_key())

dry <- upload_observations_from_csv(
  hdr = hdr,
  csv_path = "tutorials/example_observation_data .csv",
  system_name = "Plante Ivindo",
  procedure_name = "Arbre",
  dry_run = TRUE,
  recorded_at_format = "%d/%m/%Y %H:%M"
)

nrow(dry$unresolved_rows)
```

If unresolved rows are zero, run with `dry_run = FALSE` to upload.

## Full tutorial script

See:

- `tutorials/upload_observations_from_csv.R`
