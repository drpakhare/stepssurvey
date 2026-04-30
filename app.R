# Shiny app entry point for Posit Connect Cloud deployment
# This file allows the app to be deployed directly from the GitHub repo.

# Install the package from the local source if not already installed
if (!requireNamespace("stepssurvey", quietly = TRUE)) {
  # When deployed on Posit Connect Cloud, the package is installed from
  # the repo via the manifest. Locally, install from source:
  if (file.exists("DESCRIPTION")) {
    remotes::install_local(".", dependencies = TRUE, upgrade = "never")
  }
}

library(stepssurvey)
stepssurvey::run_app()
