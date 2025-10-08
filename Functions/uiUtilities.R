#' General UI utility functions for the application.
#'
#' Functions:
#' - `getBpLabel`: Constructs a label for breakpoints.
#' - `getBreakpointsMessage`: Creates a message displaying the breakpoints.

#' Get the label for breakpoints used in the app.
#'
#' @param bp_s      The S breakpoint value.
#' @param bp_r      The R breakpoint value.
#' @param guideline The guideline name.
#' @return          A string containing the formatted label for breakpoints.
getBpLabel <- function(bp_s, bp_r, guideline) {
  miss <- function(x) is.null(x) || length(x) == 0 || all(is.na(x))

  gl <- if (miss(guideline)) "Custom" else as.character(guideline)[1]

  if (miss(bp_s) || miss(bp_r)) {
    return(paste0("No MIC breakpoints available (", gl, ")."))
  }

  paste0(
    "Breakpoints applied: S \u2264 ",
    as.numeric(bp_s)[1],
    ", R \u2265 ",
    as.numeric(bp_r)[1],
    " (",
    gl,
    ")"
  )
}


#' Get a message displaying the breakpoints.
#'
#' @param breakpoint The breakpoint data frame containing S and R values.
#' @param isCustom   Boolean indicating if the breakpoints are custom values.
#' @return            A `div` containing the message with breakpoint information.
getBreakpointsMessage <- function(bp_row, isCustom = FALSE) {
  miss <- function(x) is.null(x) || length(x) == 0 || all(is.na(x))

  if (is.null(bp_row) || (is.data.frame(bp_row) && nrow(bp_row) == 0)) {
    s <- NA_real_
    r <- NA_real_
    gl <- "User selected breakpoint"
  } else {
    s <- suppressWarnings(as.numeric(bp_row$breakpoint_S[1]))
    r <- suppressWarnings(as.numeric(bp_row$breakpoint_R[1]))
    gl <- bp_row$guideline
    if (miss(gl)) gl <- "User selected breakpoint" else gl <- as.character(gl)[1]
  }

  lbl <- getBpLabel(s, r, gl)

  htmltools::div(
    htmltools::strong(lbl),
    if (isTRUE(isCustom)) htmltools::tags$em(" Custom breakpoints in use.")
  )
}
