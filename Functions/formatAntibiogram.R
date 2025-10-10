classicAB <- function(
  data,
  obs_cols,
  drug_targets,
  combined_js,
  height,
  showColors = TRUE,
  includeN = TRUE,
  isIsolateTable = FALSE,
  drug_class_starts = NULL
) {
  fixedCols <- 1

  if (!includeN) {
    data <- data %>%
      dplyr::select(-dplyr::any_of("n ="))
  }

  if (includeN && "n =" %in% colnames(data)) {
    fixedCols <- 1:2
  }

  abs <- colnames(data)[unique(c(obs_cols, drug_targets + 1))]

  abs <- unique(gsub("^obs_", "", abs))
  abs <- abs[!abs %in% c(colnames(data)[fixedCols])]

  if (isIsolateTable) {
    # Remove percentages columns are rename observed columns
    oldData <- data
    data <- data %>%
      dplyr::select(-dplyr::any_of(abs))
    colnames(data) <- gsub("^obs_", "", colnames(data))
    which(colnames(data) %in% abs)
    obs_cols <- which(colnames(data) %in% abs)
    drug_targets <- obs_cols - 1
  }

  plt <- data %>%
    mutate(dplyr::across(dplyr::all_of(fixedCols), as.character)) %>%
    gt::gt()

  if (showColors) {
    maxCount <- data %>%
      dplyr::select(dplyr::all_of(drug_targets + 1)) %>%
      unlist() %>%
      max(na.rm = TRUE)

    for (ab in abs) {
      prop_col <- ab
      colour_col <- paste0("colour_", ab)
      obs_col <- paste0(ifelse(isIsolateTable, "", "obs_"), ab)

      if (!colour_col %in% colnames(data)) {
        # Add empty colour_col if it doesn't exist to stop errors being thrown below
        data[[colour_col]] <- NULL
      }

      plt <- plt %>%
        gt::data_color(columns = prop_col, fn = function(x, row) {
          # If there is a colour_col, use that instead
          color <- as.character(data[[colour_col]][row])

          if (is.null(color)) {
            # Get obs values for current column
            obs_vals <- data[[obs_col]][row]
            # Only color if obs > 30, otherwise transparent
            vals <- suppressWarnings(as.numeric(x))
            color <- ifelse(
              obs_vals > 30,
              as.character(cut(
                vals,
                breaks = c(0, 70, 90, maxCount),
                labels = c("#D73027", "#FEE08B", "#44CDC4"),
                include.lowest = TRUE
              )),
              "transparent"
            )
            color[is.na(color)] <- "transparent"
          }
          color
        })
    }
  }

  colour_cols <- grep("^colour_", colnames(data))
  hideColumns <- c(obs_cols, colour_cols)
  if (isIsolateTable) {
    hideColumns <- colour_cols
  }

  plt <- plt %>%
    gt::cols_hide(columns = hideColumns)

  if (!is.null(drug_class_starts)) {
    plt <- plt %>%
      #' This adds the `.gt_center` class to the fixed columns and
      #' `.gt_right` to the drug columns.
      #' Easier to style with CSS below with `opt_css`.
      gt::cols_align(
        align = "center",
        columns = fixedCols
      ) %>%
      gt::cols_align(
        align = "left",
        columns = drug_targets + 1
      ) %>%
      # Styling for all drug column labels
      gt::tab_style(
        style = gt::cell_text(v_align = "middle", weight = "bold"),
        locations = gt::cells_column_labels(columns = drug_targets + 1)
      ) %>%
      # Styling for all column labels
      gt::tab_style(
        style = gt::cell_text(weight = "bold", v_align = "bottom"),
        locations = gt::cells_column_labels(columns = fixedCols)
      ) %>%
      # Styling for all table cells
      gt::tab_style(
        style = list(
          gt::cell_borders(
            sides = c("left", "right"),
            color = "grey",
            weight = gt::px(1)
          ),
          gt::cell_text(align = "center")
        ),
        locations = gt::cells_body(columns = everything())
      ) %>%
      # Styling for drug class separator lines
      gt::tab_style(
        style = gt::cell_borders(
          sides = "left",
          style = "dashed",
          color = "black",
          weight = gt::px(5)
        ),
        locations = gt::cells_body(columns = unname(drug_class_starts + 1))
      ) %>%
      gt::cols_width(
        gt::matches("^n =") ~ gt::px(120),
        1 ~ gt::px(180),
        everything() ~ gt::px(40)
      ) %>%
      # Rotate drug column labels
      gt::opt_css(
        css = "
            .gt_col_heading.gt_left {
            writing-mode: vertical-rl;
            transform: scale(-1);
            }
            .gt_col_headings {
            border-top-color: white !important;
            }
            /** Make sure fixed columns don't move */
            .gt_center {
              position: sticky;
              background-color: white;
              z-index: 2;
            }
            
            .gt_table_body tr td:first-child.gt_center,
            .gt_col_headings th:first-child.gt_center {
              left: 0;
            }
            .gt_table_body tr td:nth-child(2).gt_center,
            .gt_col_headings th:nth-child(2).gt_center {
              left: 180px;
            }
            * {
              font-family: 'Carme', sans-serif;
            }
            "
      )
  }
  return(plt)
}
