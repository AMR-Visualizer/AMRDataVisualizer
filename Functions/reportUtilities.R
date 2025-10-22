#' Utilities used in the report generation process
#'

#' Helper to prepare summary values
#'
#' @param values  A vector of filter values
#' @return        A string summarizing the filter values
prepare_summary_value <- function(values) {
  if (length(values) == 0) {
    return("All")
  }
  return(paste(values, collapse = ", "))
}

#' Get the report summary based on the provided filters.
#' This summary is included at the top of each page of the Antibiogram report.
#'
#' @param filters A list containing filter values for the report.
#' @return        A `div` element summarizing the report filters.
get_report_summary <- function(filters) {
  return(div(
    id = "filter-grid",
    span(strong("Host species: "), prepare_summary_value(filters$Species)),
    span(strong("Bacteria: "), prepare_summary_value(filters$Microorganism)),
    span(strong("Sample source: "), prepare_summary_value(filters$Source)),
    span(
      strong("Date range: "),
      ifelse(length(filters$Date) > 0, paste(filters$Date, collapse = " - "), "Not specified")
    ),
    span(strong("Breakpoints applied: "), filters$Bp %||% "All")
  ))
}
