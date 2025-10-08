#' General Utilities
#'
#' Functions:
#' - `getCustomGuidelineName`: Returns a string for custom guidelines with the current year.

#' Get the name for custom guidelines used throughout the app.
#'
#' @return A string representing the custom guideline name with the current year.
getCustomGuidelineName <- function() {
  paste("CUSTOM BREAKPOINTS", year(Sys.Date()))
}
