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
  if (is.na(bp_s) || is.na(bp_r)) {
    return("No breakpoints available.")
  }
  paste0(
    "Breakpoints applied: S ≤ ",
    bp_s,
    if (!is.na(bp_r)) paste0(", R ≥ ", bp_r) else "",
    " (",
    guideline,
    ")"
  )
}

#' Get a message displaying the breakpoints.
#'
#' @param breakpoint The breakpoint data frame containing S and R values.
#' @param isCustom   Boolean indicating if the breakpoints are custom values.
#' @return            A `div` containing the message with breakpoint information.
getBreakpointsMessage <- function(breakpoint, isCustom = FALSE) {
  div(
    class = "breakpoints-message",
    p(
      getBpLabel(
        breakpoint$breakpoint_S,
        breakpoint$breakpoint_R,
        breakpoint$guideline
      ),
      style = "font-weight: bold;"
    ),
    if (isCustom) {
      p("Applied breakpoints are custom values.", style = "font-weight: bold; color: red;")
    }
  )
}
