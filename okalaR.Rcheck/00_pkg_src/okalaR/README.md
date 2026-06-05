# OkalaR
## Overview

**OkalaR** is an R package that provides a convenient wrapper around Okala's API, enabling seamless integration with your R workflows.

## Installation

You can install the development version of OkalaR directly from GitHub using the [`devtools`](https://cran.r-project.org/package=devtools) package:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install OkalaR from GitHub
devtools::install_github("your-org/okalaR")
```

## Usage

After installation, load the package and start using its functions:

```r
library(OkalaR)
# Example usage
# result <- okala_function(args)
```

For more detailed examples, see the [`tutorials/`](tutorials/) folder, which contains scripts demonstrating typical workflows.

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

For questions or issues, please open an [issue](https://github.com/your-org/okalaR/issues) on GitHub.

## Building the Package

Change the version number in the DESCRIPTION file

To build the package locally, use the following command in your R console:

```r
devtools::build()
```

This will create a `.tar.gz` file that you can install or distribute.

for CRAN distribution checking 

R CMD check ~.tar.gz




