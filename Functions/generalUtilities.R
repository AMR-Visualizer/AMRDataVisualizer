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


#' Get items for the antibiogram plot/table.
#'
#' Create the plot (for simplified antibiogram) and the table (for classic antibiogram).
#' If the `splitGram` control is TRUE and the yVar is "Microorganism", two tables are
#' returned: one for Gram-positive and one for Gram-negative microorganisms.
#'
#' @param plotData                Data frame containing the data for the plot/table.
#' @param controls                List of control parameters for the plot/table.
#' @param staticData              Optional static data frame for additional information.
#' @param table_type              Type of table to create: "percentage", "isolate", or "ci".
#' @param clopper_pearson_ci_data Optional data frame containing Clopper-Pearson confidence intervals.
#' @return                        A list containing the plot and/or table items for the antibiogram.
getAntibiogramPlotItems <- function(
  plotData,
  controls = list(
    yVar = "Microorganism",
    sortBy = "Frequency",
    abType = "Classic",
    lowCounts = "Include",
    maxRows = 15,
    showColors = TRUE,
    aggByGenus = FALSE,
    splitGram = FALSE
  ),
  staticData = NULL,
  table_type = "percentage",
  clopper_pearson_ci_data = NULL
) {
  if (is.null(plotData) || nrow(plotData) == 0) {
    return(NULL)
  }

  # If the CP CI data is provided, join it to the main plot data.
  if (!is.null(clopper_pearson_ci_data)) {
    plotData <- plotData %>%
      left_join(
        clopper_pearson_ci_data,
        by = c(controls$yVar, "Antimicrobial")
      )
  }

  # Only keep the top `maxRows` most frequent microorganisms/source if needed.
  if (controls$maxRows > length(unique(plotData[[controls$yVar]]))) {
    plotData <- plotData %>%
      group_by(!!sym(controls$yVar)) %>%
      mutate(Frequency = n()) %>%
      filter(., Frequency >= min(tail(sort(unique(.$Frequency)), controls$maxRows))) %>%
      ungroup() %>%
      select(-Frequency)
  }

  if (controls$abType == "Simplified") {
    if (controls$lowCounts == "Exclude") {
      plotData <- plotData %>%
        filter(obs >= 30) %>%
        mutate(alpha = 1)
    } else {
      plotData <- plotData %>%
        mutate(alpha = ifelse(obs > 30, 1, obs / 30))
    }
    plotData$text <- paste(
      controls$yVar,
      plotData[[controls$yVar]],
      "<br>Antimicrobial:",
      plotData$Antimicrobial,
      "<br>Class:",
      plotData$Class,
      "<br>% Susceptible:",
      round(plotData$prop * 100, 2),
      "<br>Isolates tested:",
      plotData$obs
    )

    g <- ggplot(
      plotData,
      aes(
        x = interaction(Antimicrobial, Class),
        y = short_form,
        size = size,
        colour = Class,
        fill = Class,
        text = text
      )
    ) +
      geom_point(shape = 21, stroke = 0.5, aes(alpha = alpha)) +
      scale_alpha_identity() +
      scale_x_discrete(
        label = ifelse(
          str_length(unique(staticData$Antimicrobial)) > 15,
          str_c(str_sub(unique(staticData$Antimicrobial), 1, 15), "..."),
          unique(staticData$Antimicrobial)
        )
      ) +
      scale_size_manual(values = c("s" = 2, "m" = 5, "l" = 7)) +
      labs(title = "", x = "", y = "") +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'white', color = NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = 'transparent'),
        axis.text.y = element_text(colour = "grey20"),
        axis.text.x = element_text(angle = 90, hjust = 0, color = "grey20")
      ) +
      guides(fill = "none")

    plotly_plot <- ggplotly(g, tooltip = c("text")) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = list(
          'sendDataToCloud',
          'autoScale2d',
          'resetScale2d',
          'hoverClosestCartesian',
          'hoverCompareCartesian',
          'zoom2d',
          'pan2d',
          'select2d',
          'lasso2d',
          'zoomIn2d',
          'zoomOut2d',
          'toggleSpikelines'
        )
      )

    plotData <- plotData %>%
      select(-text, -alpha, -colour, -short_form, -size)

    return(list(
      plot = plotly_plot,
      data = plotData
    ))
  } else {
    # If there is no colour column, add it based on prop and obs.
    if (!"colour" %in% colnames(plotData)) {
      plotData <- add_ab_cell_colours(plotData)
    }

    df_wide <- plotData %>%
      select(any_of(c(
        controls$yVar,
        "Antimicrobial",
        "prop",
        "obs",
        "colour",
        "num_susceptible",
        "ci"
      ))) %>%
      mutate(prop = round(prop * 100, 0)) %>%
      filter(if (controls$lowCounts == "Exclude") obs >= 30 else TRUE) %>%
      pivot_wider(
        id_cols = !!sym(controls$yVar),
        names_from = Antimicrobial,
        values_from = any_of(c("prop", "obs", "colour", "ci")),
        names_sep = "_"
      )

    if (table_type != "ci") {
      #' Want the "n =" column for both "isolate" and "percentage" table types.
      #' For "percentage" we want it to show in the app, but it needs to be removed
      #' later in the report download.
      #' For "isolate" we want it to show always.
      df_wide <- df_wide %>%
        rowwise() %>%
        mutate(
          `n =` = paste0(
            "(",
            min(c_across(starts_with("obs_")), na.rm = TRUE),
            " - ",
            max(c_across(starts_with("obs_")), na.rm = TRUE),
            ")"
          )
        ) %>%
        ungroup() %>%
        select(!!sym(controls$yVar), `n =`, everything())
    } else {
      df_wide <- df_wide %>%
        ungroup() %>%
        select(!!sym(controls$yVar), everything())
    }

    # "ci" tables do not have the "n =" column (1 fixed column), others do (2 fixed columns).
    drug_start <- ifelse(table_type != "ci", 2, 1)

    n_drug <- length(unique(plotData$Antimicrobial))

    colnames(df_wide) <- gsub("prop_", "", colnames(df_wide))

    drug_names <- names(df_wide)[(drug_start + 1):(n_drug + drug_start)]
    drug_classes <- AMR::ab_group(drug_names)
    drug_group_list <- split(seq_along(drug_names), drug_classes)
    drug_class_starts <- sapply(drug_group_list, function(x) min(x))

    if (drug_start == 2) {
      #' We are including the "n =" column.
      #' So we need to shift the indices by 1.
      drug_class_starts <- drug_class_starts + 1
    }

    obs_cols <- which(grepl("obs_", names(df_wide)))

    drug_targets <- drug_start:(n_drug + drug_start)

    df_wide <- switch(
      controls$sortBy,
      "Frequency" = df_wide %>%
        rowwise() %>%
        mutate(total_obs = sum(c_across(starts_with("obs_")), na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(desc(total_obs)) %>%
        select(-total_obs),
      "Alphabetical" = df_wide %>% arrange(!!sym(controls$yVar)),
      "GramStain" = df_wide %>%
        mutate(gram = AMR::mo_gramstain(controls$yVar)) %>%
        arrange(gram, !!sym(controls$yVar)),
      df_wide
    )

    if (controls$splitGram == TRUE && controls$yVar == "Microorganism") {
      df_wide_neg <- df_wide %>%
        mutate(gram = AMR::mo_gramstain(Microorganism)) %>%
        filter(gram == "Gram-negative") %>%
        select(-gram)

      df_wide_pos <- df_wide %>%
        mutate(gram = AMR::mo_gramstain(Microorganism)) %>%
        filter(gram == "Gram-positive") %>%
        select(-gram)

      negTable <- classicAB(
        data = df_wide_neg,
        obs_cols = obs_cols,
        drug_targets = drug_targets,
        showColors = controls$showColors,
        drug_class_starts = drug_class_starts,
        table_type = table_type
      )

      posTable <- classicAB(
        data = df_wide_pos,
        obs_cols = obs_cols,
        drug_targets = drug_targets,
        showColors = controls$showColors,
        drug_class_starts = drug_class_starts,
        table_type = table_type
      )

      return(list(
        negTable = negTable,
        posTable = posTable,
        df_wide_neg = df_wide_neg,
        df_wide_pos = df_wide_pos
      ))
    } else {
      plot <- classicAB(
        data = df_wide,
        obs_cols = obs_cols,
        drug_targets = drug_targets,
        showColors = controls$showColors,
        drug_class_starts = drug_class_starts,
        table_type = table_type
      )

      return(list(
        data = df_wide,
        table = plot
      ))
    }
  }
}

#' Add cell colours to antibiogram data based on proportions and observations.
#'
#' @param data  Data frame containing 'prop' and 'obs' columns.
#' @return      Data frame with an additional 'colour' column.
add_ab_cell_colours <- function(data) {
  if (is.null(data)) {
    return(data)
  }
  data <- data %>%
    mutate(
      colour = case_when(
        obs < 30 ~ "white",
        prop < 0.7 ~ "#D73027",
        prop >= 0.7 & prop <= 0.9 ~ "#FEE08B",
        prop > 0.9 ~ "#44CDC4",
        TRUE ~ "white"
      )
    )
  return(data)
}
