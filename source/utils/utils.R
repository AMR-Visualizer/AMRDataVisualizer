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


#' TODO: Documentation
#' [Summary]
#'
#' @param n [Description]
#' @return [Description]
get_gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
