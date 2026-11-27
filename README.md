# NatureCubeR
## Overview

**NatureCubeR** is an R package that provides a convenient wrapper around Okala's API, enabling seamless integration with your R workflows.

## Installation

You can install the development version of NatureCubeR directly from GitHub using the [`devtools`](https://cran.r-project.org/package=devtools) package:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install NatureCubeR from GitHub
devtools::install_github("Okala-Ltd/NatureCubeR")
```

## Usage

After installation, load the package and start using its functions:

```r
library(NatureCubeR)

# Set NATURECUBE_API_KEY in ~/.Renviron first, then:
hdr <- auth_headers(get_key())
get_project(hdr)          # prints the active project name

# Sensors in the project, as an sf object, and a map of them
stations <- get_station_info(hdr, datatype = "image")
plot_stations(stations)
```

For more detailed examples, see the [`tutorials/`](tutorials/) folder, which contains scripts demonstrating typical workflows.

### Tutorials

Each tutorial is available as a runnable `.R` script and as an `.Rmd` document.

| Tutorial | Covers |
|---|---|
| [`auth_tutorial`](tutorials/auth_tutorial.R) | API keys, production and SIT headers |
| [`project_data_tutorial`](tutorials/project_data_tutorial.R) | stations, media assets, species labels, blank/publish status |
| [`download_phone_observations_tutorial`](tutorials/download_phone_observations_tutorial.R) | export phone observations and media to Excel, GeoPackage and a media folder |
| [`upload_observations_from_csv`](tutorials/upload_observations_from_csv.R) | validate and upload observations from a CSV |
| [`test_timestamp`](tutorials/test_timestamp.R) | correct media timestamps in bulk |

## Observation Upload From CSV

The package includes a low-friction workflow for `uploadObservations`, for a
wide-format table (one row per feature, procedure item names as column
headers):

1. Fetches project schema once
2. Validates the table against a procedure - resolving item names to
   item UUIDs, checking any `label` values against the label database, and
   rejecting individual rows that fail (bad timestamp, wrong-typed value,
   missing media file, unmatched label) without stopping the rest
3. Uploads only the rows that passed validation

### Recommended tutorial script

- [`tutorials/upload_observations_from_csv.R`](tutorials/upload_observations_from_csv.R)

### Minimal usage

```r
library(NatureCubeR)

hdr <- auth_headers(get_key())
schema <- get_project_systems(hdr)
procedure <- get_procedure(schema, system_name = "Plante Ivindo", procedure_name = "Arbre")

observation_data <- readr::read_csv("tutorials/data/example_observation_data.csv")

validated <- validate_csv_against_procedure(
    procedure = procedure,
    observation_data = observation_data,
    hdr = hdr
)

result <- upload_observations_from_csv(hdr = hdr, validated = validated)

result$result
```

## Phone Observations With Media

**Upload.** `upload_observations_from_csv()` attaches photos, videos and audio
referenced in the CSV. Put the local file path in the column of a
`phone-photo`, `phone-video` or `phone-audio` item. `validate_csv_against_procedure()`
rejects rows whose file does not exist, and the upload then works like the
mobile app:

1. Request signed PUT URLs
2. Upload each file directly to cloud storage
3. Submit the observation with the stored file reference

You do not need to manage signed URLs yourself.

**Download.** `get_phone_observations(hdr, procedure, dest_dir = ...)` exports
a procedure's observations to `observations.xlsx` and `observations.gpkg`, with
its media in a `media/` folder. See
[`tutorials/download_phone_observations_tutorial.R`](tutorials/download_phone_observations_tutorial.R).

## Contributing

We welcome contributions! Please follow these best practices:

### Branching

- Always create a new branch for your feature or bugfix:
    ```sh
    git checkout -b feature/your-feature-name
    ```
- Use descriptive branch names (e.g., `feature/add-auth`, `bugfix/fix-typo`).

### Pull Requests

- Push your branch to GitHub and open a Pull Request (PR) against the `main` branch.
- Clearly describe your changes and reference any related issues.
- Ensure your code follows the project's style and passes all checks.
- PRs will be reviewed by maintainers before merging.

### Tutorials

- Example scripts are located in the [`tutorials/`](tutorials/) folder.
- Feel free to contribute new tutorials or improve existing ones.

## Support

For questions or issues, please open an [issue](https://github.com/Okala-Ltd/NatureCubeR/issues) on GitHub.

## Building the Package

Change the version number in the DESCRIPTION file

To build the package locally, use the following command in your R console:

```r
devtools::build(path = ".")
```

This will create a `.tar.gz` file that you can install or distribute.

for CRAN distribution checking 

R CMD check ~.tar.gz




