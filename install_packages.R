# ─────────────────────────────────────────────────────────────────────────────
# install_packages.R
# Run this once before launching the app to install all dependencies
# ─────────────────────────────────────────────────────────────────────────────

message(
  "Installing required packages for the Economic Intelligence Dashboard..."
)

cran_packages <- c(
  # Core Shiny
  "shiny",
  "shinydashboard",
  "shinydashboardPlus",
  "shinyWidgets",
  "shinycssloaders",
  "shinyjs",
  # Tables & Visualization
  "DT",
  "plotly",
  "leaflet",
  "leaflet.extras",
  # Data Wrangling
  "tidyverse",
  "lubridate",
  "scales",
  "glue",
  # Economic Data
  "fredr",
  "tidyquant",
  "tidycensus",
  # API / HTTP
  "httr2",
  "jsonlite",
  # Forecasting
  "prophet",
  # Spatial
  "sf",
  # UI Polish
  "waiter",
  "fresh",
  # Utilities
  "memoise",
  "cachem",
  "markdown"
)

# Install missing packages
new_pkgs <- cran_packages[
  !(cran_packages %in% installed.packages()[, "Package"])
]
if (length(new_pkgs) > 0) {
  message("Installing: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs, repos = "https://cloud.r-project.org/")
} else {
  message("All CRAN packages already installed.")
}

# blsAPI from CRAN (if available) – fallback to direct HTTP in the app
if (!"blsAPI" %in% installed.packages()[, "Package"]) {
  tryCatch(
    install.packages("blsAPI"),
    error = function(e) {
      message("blsAPI not available; using direct HTTP calls instead.")
    }
  )
}

message("\n✅ Package installation complete!")
message("Next steps:")
message("  1. Copy .Renviron.example to .Renviron and fill in your API keys")
message("  2. Run: shiny::runApp('app.R')")


remotes::install_github("trafficonese/leaflet.extras")

remotes::install_github("https://github.com/mikeasilva/blsAPI")
