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

#' Items for the simplified antibiogram legend.
.simplified_legend_items <- list(
  size = list(
    list(
      style = "font-size: 10px;",
      label = "Low susceptibility (<70%)"
    ),
    list(
      style = "font-size: 20px;",
      label = "Moderate susceptibility (70 - 90%)"
    ),
    list(
      style = "font-size: 30px;",
      label = "High susceptibility (>90%)"
    )
  ),
  opacity = list(
    list(
      style = "opacity: 0.1;",
      label = "<30 Samples"
    ),
    list(
      style = "",
      label = "30+ Samples"
    )
  )
)

#' Helper to create a simplified legend item.
#'
#' @param class CSS class for the legend item.
#' @param label Label text for the legend item.
#' @param style CSS style for the legend circle.
#' @return      A `div` element representing the legend item.
.get_simplified_legend_item <- function(class, label, style) {
  return(div(
    class = class,
    div(
      style = "min-width: 30px; display: flex; justify-content: center; align-items: center;",
      tags$i(
        class = "fas fa-circle legend-circle",
        style = style
      )
    ),
    span(
      label,
      class = "legend-label",
      style = "margin-left: 10px;"
    )
  ))
}

#' Get the simplified antibiogram legend.
#'
#' @return A `div` element containing the simplified legend.
get_simplified_ab_legend <- function() {
  return(div(
    class = "legend-section simplified-legend",
    div(
      class = "legend-group",
      h5("Size"),
      div(
        class = "legend-section",
        lapply(.simplified_legend_items$size, function(item) {
          .get_simplified_legend_item("legend-item", item$label, item$style)
        })
      )
    ),
    div(
      class = "legend-group",
      h5("Opacity"),
      div(
        class = "opacity-container",
        .get_simplified_legend_item(
          "opacity-item",
          .simplified_legend_items$opacity[[1]]$label,
          .simplified_legend_items$opacity[[1]]$style
        ),
        div(class = "vertical-divider"),
        .get_simplified_legend_item(
          "opacity-item",
          .simplified_legend_items$opacity[[2]]$label,
          .simplified_legend_items$opacity[[2]]$style
        )
      )
    )
  ))
}

#' Items for the classic antibiogram legend.
.classic_legend_items <- list(
  list(
    icon_class = "far fa-square",
    colour = "grey",
    label = "Too few observations"
  ),
  list(
    icon_class = "fas fa-solid fa-square",
    colour = "var(--red)",
    label = "Low susceptibility (<70%)"
  ),
  list(
    icon_class = "fas fa-solid fa-square",
    colour = "var(--yellow)",
    label = "Moderate susceptibility (70 - 90%)"
  ),
  list(
    icon_class = "fas fa-solid fa-square",
    colour = "var(--primary-blue)",
    label = "High susceptibility (>90%)"
  )
)

#' Helper to create a classic legend item.
#'
#' @param icon_class  CSS class for the legend icon.
#' @param label       Label text for the legend item.
#' @param colour      Colour for the legend icon.
#' @return            A `div` element representing the legend item.
.get_classic_legend_item <- function(icon_class, label, colour) {
  return(div(
    class = "legend-item",
    tags$i(
      class = icon_class,
      style = sprintf("font-size: 20px; margin-left: 5px; color: %s;", colour)
    ),
    span(label, class = "legend-label", style = "margin-left: 15px;")
  ))
}

#' Get the classic antibiogram legend.
#'
#' @return A `div` element containing the classic legend.
get_classic_ab_legend <- function() {
  return(div(
    class = "legend-section classic-legend",
    lapply(.classic_legend_items, function(item) {
      .get_classic_legend_item(item$icon_class, item$label, item$colour)
    })
  ))
}
