# ------------------------------------------------------------------------------
# Global initialization script
# Author: Kurtis Sobkowich
# Description: Initializes global environment (i.e., libraries, functions, etc.)
# ------------------------------------------------------------------------------

# Shiny and related
library(shiny)
library(shinyalert)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(fresh)

# Data + data manipulation
library(AMR)
library(dplyr)
library(stringr)
library(stringdist)
library(tidyr)
library(purrr)
library(jsonlite)
library(sf)
library(tidyverse)
library(zoo)

# File reading/upload/download
library(zip)
library(openxlsx)
library(chromote)
library(nanoparquet)
library(quarto)
library(yaml)
library(readxl)
library(writexl)
library(webshot2)
library(vroom)
library(arrow)

# Visualization - Tables
library(data.table)
library(DT)
library(gt)

# Visualization - Mapping
library(leaflet)
library(mapview)
library(tigris)
library(HatchedPolygons)

# Visualization - Plots
library(plotly)
library(ggplot2)
library(ggpattern)

# Visualization - Other
library(scales)
library(colorspace)

# Performance
library(foreach)
library(doParallel)

# Other
library(renv)
library(lubridate)
library(spacyr)
library(typedjs)

# For renv setup see the "Managing Dependencies with `renv`" section in `Documentation/quick_start_guide.md`

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
