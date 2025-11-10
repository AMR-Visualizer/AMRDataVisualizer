# ------------------------------------------------------------------------------
# Global initialization script
# Author: Kurtis Sobkowich
# Description: Initializes global environment (i.e., libraries, functions, etc.)
# ------------------------------------------------------------------------------

# TODO: Move all packages to renv and call them via library()
library(zip)
library(openxlsx)

# ------------------------------------------------------------------------------
# Helper function to install and load a package (CRAN or GitHub)
# ------------------------------------------------------------------------------
install_and_load <- function(pkg, github = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing '", pkg, "'...")
    tryCatch(
      {
        if (is.null(github)) {
          install.packages(pkg)
        } else {
          if (!requireNamespace("devtools", quietly = TRUE)) {
            install.packages("devtools")
          }
          devtools::install_github(github)
        }
      },
      error = function(e) {
        warning("Failed to install ", pkg, ": ", e$message)
      }
    )
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# CRAN Packages
cran_packages <- c(
  "AMR",
  "arrow",
  "chromote",
  "colorspace",
  "data.table",
  "doParallel",
  "DT",
  "foreach",
  "fresh",
  "ggpattern",
  "gt",
  "jsonlite",
  "leaflet",
  "mapview",
  "nanoparquet",
  "plotly",
  "quarto",
  "readxl",
  "renv",
  "scales",
  "sf",
  "shiny",
  "shinyalert",
  "shinyBS",
  "shinycssloaders",
  "shinydashboard",
  "shinydashboardPlus",
  "shinyjs",
  "shinyWidgets",
  "spacyr",
  "stringdist",
  "tigris",
  "tidyverse",
  "tools",
  "vroom",
  "webshot2",
  "writexl",
  "zoo"
)

# GitHub Packages
github_packages <- list(
  HatchedPolygons = "statnmap/HatchedPolygons",
  typedjs = "JohnCoene/typedjs"
)

# ------------------------------------------------------------------------------
# Install and Load All Packages
# ------------------------------------------------------------------------------
# Load CRAN packages
invisible(lapply(cran_packages, install_and_load))

# Load GitHub packages
invisible(mapply(install_and_load, names(github_packages), github_packages))

# ------------------------------------------------------------------------------
# RENV set-up and maintenance
# ------------------------------------------------------------------------------
# renv::init()
# Run this line and select option 2 for dockerizing.
# renv::snapshot()
# renv::deactivate()

library(fs)
library(rprojroot)

APP_DIR <- tryCatch(
  rprojroot::find_root(
    rprojroot::has_file(".here") | rprojroot::has_file("app.R") | rprojroot::has_dir("www")
  ),
  error = function(e) normalizePath(getwd())
)

options(amrvis.paths = list(
  app_dir = APP_DIR,
  reports = fs::path(APP_DIR, "Reports")
))


# ------------------------------------------------------------------------------
# Source external modules
# ------------------------------------------------------------------------------
source("Modules/homePage.R")
source("Modules/ovPage.R")
source("Modules/micPage.R")
source("Modules/micDistPage.R")
source("Modules/abPage.R")
source("Modules/mapPage.R")
source("Modules/tsPage.R")
source("Modules/mdrPage.R")
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
source("Functions/uiUtilities.R")
source("Functions/generalUtilities.R")
source("Functions/micDistributionPlot.R")

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
  "row_id",
  "date",
  "day",
  "order",
  "country",
  "month",
  "state",
  "id",
  "county"
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


# Full df of AMR clinical breakpoints with ab and mo names added
g_fullClinicalBreakpoints <- getFullClinicalBps()

# ------------------------------------------------------------------------------
# Increase maximum allowable file upload
# ------------------------------------------------------------------------------
options(shiny.maxRequestSize = 1000 * 1024^2)
