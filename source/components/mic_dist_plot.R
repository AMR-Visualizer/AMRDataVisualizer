#' Plot MIC distributions with MIC50 and MIC90 markers
#'
#' @description
#' Given antimicrobial susceptibility test results containing MIC values with
#' optional inequality operators (e.g., `"<="`, `">"`, `"="`), this function:
#' 1) counts isolates per discrete MIC string by drug,
#' 2) imposes a correct ordinal ordering of the MIC strings using a tiny
#'    epsilon offset to respect inequality direction, and
#' 3) computes and overlays MIC50 and MIC90 positions per drug.
#'
#' @details
#' MIC strings are parsed into an operator and a numeric value. To obtain an
#' ordinal x-axis that respects inequalities, we build an `order_key = value + offset`,
#' where `offset` is a small multiple of `epsilon` determined by the operator:
#' \itemize{
#'   \item \code{"<"}  = \code{-2 * epsilon}
#'   \item \code{"<="} = \code{-1 * epsilon}
#'   \item \code{"="}  = \code{0}
#'   \item \code{">="} = \code{+1 * epsilon}
#'   \item \code{">"}  = \code{+2 * epsilon}
#' }
#' This preserves the intuitive ordering (e.g., \code{"<1"} \eqn{<} \code{"<=1"} \eqn{<} \code{"=1"} \eqn{<} \code{">=1"} \eqn{<} \code{">1"}).
#'
#' MIC50 and MIC90 are the first MIC bins at or above the cumulative proportions
#' 0.5 and 0.9, respectively, within each drug's distribution.
#'
#' @param data A data frame containing at least the columns \code{DRUG} and \code{MIC_VALUE}.
#' @param drug_col A string column name in \code{data} giving drug identifiers. Default \code{"DRUG"}.
#' @param mic_col A string column name in \code{data} giving MIC values (as character, possibly with \code{<, <=, =, >=, >}). Default \code{"MIC_VALUE"}.
#' @param epsilon Numeric scalar used for tiny ordering offsets; see Details. Default \code{1e-4}.
#'
#' @return A named list with:
#' \describe{
#'   \item{\code{plot}}{A \code{ggplot} object showing counts by MIC (x) and drug (fill), with MIC50 (dotted) and MIC90 (dashed) lines and labels.}
#'   \item{\code{mic_thresholds}}{A tibble with per-drug MIC50/MIC90 levels and their x-positions.}
#'   \item{\code{plot_data}}{The expanded plotting tibble including the ordered MIC factor and zero-filled combinations.}
#' }
#'
#' @section Required columns:
#' \itemize{
#'   \item \strong{DRUG} (or the column named by \code{drug_col})
#'   \item \strong{MIC_VALUE} (or the column named by \code{mic_col})
#' }
#'
#' @examples
#' \dontrun{
#' out <- plot_mic_distribution(data)
#' out$plot
#' out$mic_thresholds
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @import forcats
#' @import stringr
#' @importFrom tidyr complete
#' @export
plot_mic_distribution <- function(
  data,
  drug_col = "DRUG",
  mic_col = "MIC_VALUE",
  epsilon = 1e-4
) {
  req_cols <- c(drug_col, mic_col)
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("`data` is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  # Standardize local symbols
  df0 <- data %>%
    dplyr::rename(
      DRUG = !!drug_col,
      MIC_VALUE = !!mic_col
    ) %>%
    dplyr::filter(!is.na(MIC_VALUE), !is.na(DRUG)) %>%
    dplyr::select(DRUG, MIC_VALUE)

  # Count isolates per (DRUG, MIC_VALUE)
  df <- df0 %>%
    dplyr::group_by(DRUG, MIC_VALUE) %>%
    dplyr::summarize(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(!is.na(MIC_VALUE))

  # Factor MIC values with inequality operators
  df_plot <- df %>%
    dplyr::mutate(
      op = stringr::str_match(MIC_VALUE, "^(<=|>=|<|>|=)?\\s*([0-9.]+)$")[, 2],
      val = as.numeric(stringr::str_match(MIC_VALUE, "^(?:<=|>=|<|>|=)?\\s*([0-9.]+)$")[, 2]),
      op = dplyr::if_else(is.na(op), "=", op),
      offset = dplyr::case_when(
        op == "<" ~ -2 * epsilon,
        op == "<=" ~ -1 * epsilon,
        op == "=" ~ 0,
        op == ">=" ~ 1 * epsilon,
        op == ">" ~ 2 * epsilon,
        TRUE ~ 0
      ),
      order_key = val + offset
    ) %>%
    dplyr::mutate(
      MIC_value_ord = forcats::fct_reorder(MIC_VALUE, order_key, .desc = FALSE)
    ) %>%
    tidyr::complete(DRUG, MIC_value_ord, fill = list(n = 0))

  # Compute MIC50 and MIC90 values
  mic_thresh <- df_plot %>%
    dplyr::group_by(DRUG) %>%
    dplyr::arrange(MIC_value_ord, .by_group = TRUE) %>%
    dplyr::mutate(
      total = sum(n),
      cum_n = cumsum(n),
      cum_prop = cum_n / total
    ) %>%
    dplyr::summarize(
      MIC50 = MIC_value_ord[which(cum_prop >= 0.5)[1]],
      MIC90 = MIC_value_ord[which(cum_prop >= 0.9)[1]],
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      x50 = match(MIC50, levels(df_plot$MIC_value_ord)),
      x90 = match(MIC90, levels(df_plot$MIC_value_ord))
    )

  # Alternating background bands for readability
  mic_levels <- levels(df_plot$MIC_value_ord)
  bands <- data.frame(
    xmin = seq_along(mic_levels) - 0.5,
    xmax = seq_along(mic_levels) + 0.5,
    ymin = -Inf,
    ymax = Inf,
    shade = rep(c(TRUE, FALSE), length.out = length(mic_levels))
  )

  # Labels slightly above tallest bar
  y_top <- max(df_plot$n, na.rm = TRUE)
  mic_labels <- mic_thresh %>%
    dplyr::mutate(
      label50 = paste0("MIC50: ", MIC50),
      label90 = paste0("MIC90: ", MIC90),
      x50_lab = x50 - 0.18,
      x90_lab = x90 - 0.18
    )

  # Build plot
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = MIC_value_ord, y = n, fill = DRUG)) +
    ggplot2::geom_rect(
      data = bands,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = ymax, alpha = shade),
      inherit.aes = FALSE,
      fill = "grey90"
    ) +
    ggplot2::scale_alpha_manual(values = c(`TRUE` = 0.25, `FALSE` = 0), guide = "none") +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9), width = 0.8) +
    ggplot2::geom_vline(
      data = mic_thresh,
      ggplot2::aes(xintercept = x50, color = DRUG),
      linetype = "dotted",
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(
      data = mic_thresh,
      ggplot2::aes(xintercept = x90, color = DRUG),
      linetype = "dashed",
      linewidth = 1,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = mic_labels,
      ggplot2::aes(x = x50_lab, y = y_top * 1.3, label = label50, color = DRUG),
      angle = 90,
      vjust = 0,
      size = 5,
      fontface = "bold",
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = mic_labels,
      ggplot2::aes(x = x90_lab, y = y_top * 1.3, label = label90, color = DRUG),
      angle = 90,
      vjust = 0,
      size = 5,
      fontface = "bold",
      show.legend = FALSE
    ) +
    ggplot2::expand_limits(y = y_top * 1.7) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(
      x = "MIC",
      y = "Number of isolates (n)",
      fill = "Drug",
      title = NULL
    ) +
    ggplot2::theme_minimal(base_size = 14, base_family = "Carme") +
    ggplot2::theme(legend.position = "bottom")

  list(
    plot = p,
    mic_thresholds = mic_thresh[, 1:3],
    plot_data = df_plot
  )
}
