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

getAntibiogramPlotItems <- function(
  plotData,
  controls = list(
    yVar = "Microorganism",
    sortBy = "Frequency",
    abType = "Classic",
    lowCounts = "Include",
    numAb = 15,
    showColours = TRUE,
    aggByGenus = FALSE,
    splitGram = FALSE
  ),
  staticData = NULL,
  clopperPearsonCIData = NULL,
  includeNEquals = TRUE,
  isIsolateTable = FALSE
) {
  if (is.null(plotData) || nrow(plotData) == 0) {
    return(NULL)
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
    return(plotly_plot)
  } else {
    df_wide <- plotData %>%
      select(!!sym(controls$yVar), Antimicrobial, prop, obs, colour, num_susceptible) %>%
      mutate(prop = round(prop * 100, 0)) %>%
      filter(if (controls$lowCounts == "Exclude") obs >= 30 else TRUE) %>%
      pivot_wider(
        id_cols = !!sym(controls$yVar),
        names_from = Antimicrobial,
        values_from = c(prop, obs, colour),
        names_sep = "_"
      )

    if (includeNEquals) {
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

    drug_start <- ifelse(includeNEquals, 2, 1)

    n_total <- ncol(df_wide)
    n_drug <- (n_total - drug_start) / 2

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

    combined_js <- get_ab_colours_js(df_wide, drug_start, controls$showColors)

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
        combined_js = combined_js,
        height = "350px",
        showColors = controls$showColors,
        drug_class_starts = drug_class_starts,
        isIsolateTable = isIsolateTable
      )

      posTable <- classicAB(
        data = df_wide_pos,
        obs_cols = obs_cols,
        drug_targets = drug_targets,
        combined_js = combined_js,
        height = "350px",
        showColors = controls$showColors,
        drug_class_starts = drug_class_starts,
        isIsolateTable = isIsolateTable
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
        combined_js = combined_js,
        height = "750px",
        showColors = controls$showColors,
        drug_class_starts = drug_class_starts,
        isIsolateTable = isIsolateTable
      )
      return(list(
        data = df_wide,
        table = plot
      ))
    }
  }
}


get_ab_colours_js <- function(df_wide, drug_start, showColors = TRUE) {
  n_total <- ncol(df_wide)
  n_drug <- (n_total - drug_start) / 2

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

  combined_js <- JS(paste0(
    "function(td, cellData, rowData, row, col) {",
    sprintf("  var n_drug = (rowData.length - %s) / 2;", drug_start),
    "  var obsIndex = col + n_drug;",
    "  var obsValue = parseFloat(rowData[obsIndex]);",
    "  var cellValue = parseFloat(cellData);",
    "  var tooltipText = '';",
    "  var showColors = ",
    tolower(as.character(showColors)),
    ";",

    "  if (!isNaN(obsValue)) {",
    "    tooltipText = 'Number of tests: ' + obsValue;",
    "  }",

    "  $(td).attr('title', tooltipText);",

    "  if (!isNaN(obsValue) && obsValue >= 30 && showColors === true) {",
    "    if (!isNaN(cellValue)) {",
    "      if (cellValue < 70) {",
    "        $(td).css({'background-color': '#D73027', 'color': 'white'});",
    "      } else if (cellValue < 90) {",
    "        $(td).css({'background-color': '#FEE08B'});",
    "      } else if (cellValue >= 90) {",
    "        $(td).css({'background-color': '#44CDC4', 'color': 'white'});",
    "      }",
    "    }",
    "  }",

    "  var drug_class_starts = [",
    paste(drug_class_starts, collapse = ","),
    "];",
    "  if (drug_class_starts.includes(col)) {",
    "    $(td).css({'border-left': '3px dashed black'});",
    "  }",
    "}"
  ))
  return(combined_js)
}
