#' This file contains general utility functions used accross the app.
#'
#' @keywords internal
NULL

#' Get the name for custom guidelines used throughout the app.
#'
#' @return A string representing the custom guideline name with the current year.
getCustomGuidelineName <- function() {
  paste("CUSTOM BREAKPOINTS", year(Sys.Date()))
}
