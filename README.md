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

## API keys

API keys for Okala are available in the Okala dashboard under the Settings panel. Each API key is project-specific, meaning it can only be used to access and manipulate data within the associated project. Make sure to keep your API keys secure and do not share them publicly.

To this end, you need to set it as an environment variable in your console 

### Using OKALA_API_KEY

This section describes how to use the `OKALA_API_KEY` in your application.

- The `OKALA_API_KEY` is required to authenticate requests to the Okala API.
- Set the `OKALA_API_KEY` as an environment variable in your system or in your application's configuration file.
- The application will automatically read the `OKALA_API_KEY` from the environment and include it in the authorization header for all API requests.
- If the `OKALA_API_KEY` is missing or invalid, API requests will fail with an authentication error.

**Example:**

#### Setting the `OKALA_API_KEY` Environment Variable

**Windows (Command Prompt):**
```cmd
set OKALA_API_KEY=your_api_key_here
```

**Windows (PowerShell):**
```powershell
$env:OKALA_API_KEY="your_api_key_here"
```

**macOS/Linux (Bash):**
```bash
export OKALA_API_KEY=your_api_key_here
```

You can add the export command to your `.bash_profile`, `.bashrc`, or `.zshrc` file to set the variable automatically on each session.

After setting the environment variable, restart your R session or terminal to ensure the changes take effect.


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

devtools::load_all()

```

This will create a `.tar.gz` file that you can install or distribute.

for CRAN distribution checking 

R CMD check ~.tar.gz




