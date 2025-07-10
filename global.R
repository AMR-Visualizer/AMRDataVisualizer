# Load Libraries ----------------------------------------------------------
library(AMR)
library(arrow)
library(data.table)
library(DT)
library(fresh)
library(ggpattern)
library(plotly)
library(scales)
library(shiny)
library(shinyalert)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(tools)
library(tidyverse)
library(vroom)
library(typedjs)
library(readxl)
library(foreach)
library(doParallel)
library(spacyr)
library(shinyBS)
library(jsonlite)
library(leaflet)
library(sf)
library(tigris)
library(stringdist)
library(colorspace)
library(HatchedPolygons)
library(plotly)
library(zoo)
library(renv)
library(mapview)
library(nanoparquet)
library(shinycssloaders)
library(quarto)
library(webshot2)
library(chromote)
library(writexl)
library(gt)


# install.packages("devtools")
# devtools::install_github("statnmap/HatchedPolygons")
# devtools::install_github("JohnCoene/typedjs")

# renv::init()

# Run this line and select option 2 for dockerizing.
# renv::snapshot()

# renv::deactivate()

# Source Files ------------------------------------------------------------
#Modules
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


#Functions
source("Functions/dataCleaningFunction.R")
source("Functions/columnDetectFunctions.R")
source("Functions/regionMatching.R")
source("Functions/parseWideColumns.R")
source("Functions/formatAntibiogram.R")
source("Functions/dataUtilityFunctions.R")
source("Functions/micTableFunction.R")

#Data
awareList <- read.csv("./Data/2023AwareClassifications.csv")

# Increase maximum data size ----------------------------------------------
options(shiny.maxRequestSize = 1000 * 1024^2)

# Global Variables --------------------------------------------------------

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
