#' Utilities used in the report generation process
#'

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

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

#' Helper to generate footnote IDs (to match links)
#' @param num            The footnote number
#' @param include_hash   Boolean indicating whether to include '#' in the returned string
#' @return               A string representing the footnote ID or href
.get_footnote_id <- function(num, include_hash = FALSE) {
  return(sprintf("%sfn-%s", ifelse(include_hash, "#", ""), num))
}

#' Helper to generate footnote hrefs (to match links)
#' @param num            The footnote number
#' @param include_hash   Boolean indicating whether to include '#' in the returned string
#' @return               A string representing the footnote href or ID
.get_footnote_href <- function(num, include_hash = FALSE) {
  return(sprintf("%sfnref-%s", ifelse(include_hash, "#", ""), num))
}

#' Get the report footnote link.
#' 
#' @param num        The footnote number
#' @param page_num   The page number (optional)
#' @return           A `span` element containing the footnote link.
#' @seealso .get_footnote_id, .get_footnote_href
get_report_footnote_link <- function(num, page_num = NULL) {
  original_number <- num
  if (!is.null(page_num)) {
    num <- sprintf("%s_%s", page_num, num)
  }
  return(span(
    id = .get_footnote_href(num),
    tags$a(
      href = .get_footnote_id(num, include_hash = TRUE),
      class = "footnote-arrow",
      tags$sup(original_number)
    )
  ))
}

#' Get the footnote content.
#' 
#' @param num        The footnote number
#' @param content    The content of the footnote
#' @param page_num   The page number (optional)
#' @return           A `div` element containing the footnote content.
#' @seealso .get_footnote_id, .get_footnote_href
get_footnote_content <- function(num, content, page_num = NULL) {
  original_number <- num
  if (!is.null(page_num)) {
    num <- sprintf("%s_%s", page_num, num)
  }
  return(div(
    id = .get_footnote_id(num),
    class = "custom-footnote",
    tags$a(
      href = .get_footnote_href(num, include_hash = TRUE),
      class = "footnote-arrow",
      sprintf("%s.", original_number)
    ),
    content
  ))
}

#' Helper to get report footnote items.
#' @param page_num          The page number
#' @param value             The current filter value
#' @param all_values        All possible filter values
#' @param existing_footnotes The number of existing footnotes
#' @return                  A list containing the footnote UI elements and updated footnote count
#' @seealso get_report_footnote_link, get_footnote_content
.get_report_footnote_items <- function(page_num, value, all_values, existing_footnotes = 0) {
  if (length(value) != 0) {
    return(list(ui = NULL, footnote_ui = NULL, footnote_num = existing_footnotes))
  }
  num <- existing_footnotes + 1
  return(list(
    ui = get_report_footnote_link(num, page_num),
    footnote_ui = get_footnote_content(num, prepare_summary_value(all_values), page_num),
    footnote_num = num
  ))
}

#' Get the report summary based on the provided filters.
#' This summary is included at the top of each page of the Antibiogram report.
#'
#' @param filters           A list containing filter values for the report.
#' @param all_filter_values A list containing all possible filter values.
#' @param page_num          The current page number (default is 1).
#' @return                  A list containing the summary UI, footnotes UI, and number of footnotes.
get_report_summary <- function(filters, all_filter_values, page_num = 1) {
  species_footnote <- .get_report_footnote_items(
    page_num,
    filters$Species,
    all_filter_values$Species
  )
  mo_footnote <- .get_report_footnote_items(
    page_num,
    filters$Microorganism,
    all_filter_values$Microorganism,
    species_footnote$footnote_num
  )
  sources_footnote <- .get_report_footnote_items(
    page_num,
    filters$Source,
    all_filter_values$Source,
    mo_footnote$footnote_num
  )

  return(list(
    ui = div(
      id = "filter-grid",
      span(strong("Host species: "), prepare_summary_value(filters$Species), species_footnote$ui),
      span(strong("Bacteria: "), prepare_summary_value(filters$Microorganism), mo_footnote$ui),
      span(strong("Sample source: "), prepare_summary_value(filters$Source), sources_footnote$ui),
      span(
        strong("Date range: "),
        ifelse(length(filters$Date) > 0, paste(filters$Date, collapse = " - "), "Not specified")
      ),
      span(strong("Breakpoints applied: "), filters$guideline %||% "All") # Should never be empty
    ),
    footnotes_ui = div(
      species_footnote$footnote_ui,
      mo_footnote$footnote_ui,
      sources_footnote$footnote_ui
    ),
    num_footnotes = sources_footnote$footnote_num
  ))
}
