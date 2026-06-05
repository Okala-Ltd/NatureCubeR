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
# Example usage
# result <- okala_function(args)
```

For more detailed examples, see the [`tutorials/`](tutorials/) folder, which contains scripts demonstrating typical workflows.

## Observation Upload From CSV

The package now includes a low-friction workflow for `uploadObservations`:

1. Fetches project schema once
2. Resolves `system_name` and `procedure_name`
3. Maps `item_name` to `item_uuid` when UUIDs are missing
4. Builds grouped observations from flat CSV rows
5. Uploads to the `uploadObservations` endpoint

### Recommended tutorial script

- [`tutorials/upload_observations_from_csv.R`](tutorials/upload_observations_from_csv.R)

### Minimal usage

```r
library(NatureCubeR)

hdr <- auth_headers(get_key())

result <- upload_observations_from_csv(
    hdr = hdr,
    csv_path = "tutorials/example_observation_data .csv",
    system_name = "Plante Ivindo",
    procedure_name = "Arbre",
    dry_run = TRUE,
    recorded_at_format = "%d/%m/%Y %H:%M"
)

length(result$observations)
```

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




