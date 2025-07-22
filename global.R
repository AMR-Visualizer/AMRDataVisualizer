# ------------------------------------------------------------------------------
# Global initialization script
# Author: Kurtis Sobkowich
# Description: Initializes global environment (i.e., libraries, functions, etc.)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Install and Load Required Packages
# ------------------------------------------------------------------------------

required_packages <- c(
  "AMR", "arrow", "chromote", "colorspace", "data.table", "doParallel", "DT",
  "foreach", "fresh", "ggpattern", "gt", "HatchedPolygons", "jsonlite", "leaflet",
  "mapview", "nanoparquet", "plotly", "quarto", "readxl", "renv", "scales", "sf",
  "shiny", "shinyalert", "shinyBS", "shinycssloaders", "shinydashboard",
  "shinydashboardPlus", "shinyjs", "shinyWidgets", "spacyr", "stringdist",
  "tigris", "tidyverse", "tools", "typedjs", "vroom", "webshot2", "writexl", "zoo"
)

# Install any missing packages
installed_packages <- rownames(installed.packages())
missing_packages <- setdiff(required_packages, installed_packages)

if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages)
}

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))

# ------------------------------------------------------------------------------
# Install packages not available on CRAN               
# ------------------------------------------------------------------------------
# install.packages("devtools")
# devtools::install_github("statnmap/HatchedPolygons")
# devtools::install_github("JohnCoene/typedjs")

# ------------------------------------------------------------------------------
# RENV set-up and maintenance                 
# ------------------------------------------------------------------------------
# renv::init()
# Run this line and select option 2 for dockerizing.
# renv::snapshot()
# renv::deactivate()

# ------------------------------------------------------------------------------
# Source external modules            
# ------------------------------------------------------------------------------
source("Modules/homePage.R")
source("Modules/ovPage.R")
source("Modules/micPage.R")
source("Modules/abPage.R")
source("Modules/mapPage.R")
source("Modules/tsPage.R")
source("Modules/mdrPage.R")
source("Modules/pathogenPage.R")
source("Modules/explorePage.R")
source("Modules/importDataModule.R")
source("Modules/filterPanelModule.R")
source("Modules/changeLogModule.R")

# ------------------------------------------------------------------------------
# Source external functions                 
# ------------------------------------------------------------------------------
source("Functions/dataCleaningFunction.R")
source("Functions/columnDetectFunctions.R")
source("Functions/regionMatching.R")
source("Functions/parseWideColumns.R")
source("Functions/formatAntibiogram.R")
source("Functions/dataUtilityFunctions.R")
source("Functions/micTableFunction.R")

# ------------------------------------------------------------------------------
# Source external data
# ------------------------------------------------------------------------------
awareList <- read.csv("./Data/2023AwareClassifications.csv")

# ------------------------------------------------------------------------------
# Define initial variables                 
# ------------------------------------------------------------------------------
#' Possible metadata columns that may appear in data.
#' Want to ignore these when trying the find antimicrobial columns.
#' !! These need to be in lower case !!
g_metadataCols <- c(
  "organism",
  "year",
  "beta_lac",
  "host_species",
  "sex",
  "breed",
  "age",
  "specimen",
  "source_group",
  "source",
  "organism_long",
  "host_species_long",
  "comment",
  "species",
  "row_id"
)

# Test mapping values
g_sir_keywords <- c("sir", "value", "interpretation", "resistance", "status", "result", "interp")
g_mic_keywords <- c("mic")
g_mic_sign_keywords <- c("mic sign", "sign")
g_mic_value_keywords <- c("mic value", "value", "mic", "concentration", "val")

g_test_mapping <- list(
  "mic_sign" = g_mic_sign_keywords,
  "mic_value" = g_mic_value_keywords,
  "mic" = g_mic_keywords,
  "interp" = g_sir_keywords
)

# ------------------------------------------------------------------------------
# Increase maximum allowable file upload                 
# ------------------------------------------------------------------------------
options(shiny.maxRequestSize = 1000 * 1024^2)
