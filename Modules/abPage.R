abPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # Main Content ------------------------------------------------------------

      column(
        9,
        tagList(
          uiOutput(ns("has_data")),
          uiOutput(ns("has_no_data"))
        )
      ),

      # Side menus -----------------------------------------------------------------

      column(
        3,

        # Filters -----------------------------------------------------------------

        filterPanelUI(ns("filters")),

        # Plot controls -----------------------------------------------------------
        uiOutput(ns("controls")),

        # Legend ------------------------------------------------------------------

        uiOutput(ns("legend"))
      )
    )
  )
}

#' Antibiogram page server.
#'
#' @param id                Module id.
#' @param reactiveData      Reactive cleaned df (including any custom breakpoints).
#' @param customBreakpoints Reactive df containing any custom breakpoints.
#' @return                  None.
abPageServer <- function(id, reactiveData, customBreakpoints) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------------------
    # Sub-modules
    # ------------------------------------------------------------------------------

    filters <- filterPanelServer(
      "filters",
      reactiveData,
      default_filters = c("Microorganism", "Suppress Antimicrobials", "Species", "Source", "Date"),
      auto_populate = list()
    )

    # ------------------------------------------------------------------------------
    # Module variables
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Reactives
    # ------------------------------------------------------------------------------

    plotData <- reactive({
      filters$filteredData()
    })
    activeFilters <- reactive({
      filters$activeFilters()
    })

    showColors <- reactiveVal(TRUE)
    aggByGenus <- reactiveVal(FALSE)
    abType <- reactiveVal("Classic")
    lowCounts <- reactiveVal("Include")
    yVar <- reactiveVal("Microorganism")
    sortBy <- reactiveVal("Frequency")
    numAb <- reactiveVal(15)
    splitGram <- reactiveVal(FALSE)
    splitData <- reactiveVal()

    # Group all the inputs in the controls panel into a single reactive
    controls <- reactive({
      return(list(
        yVar = yVar(),
        sortBy = sortBy(),
        abType = abType(), # Antibiogram style
        lowCounts = lowCounts(), # Handle low counts
        numAb = numAb(), # Maximum rows
        showColors = showColors(),
        aggByGenus = aggByGenus(),
        splitGram = splitGram()
      ))
    })

    # First filter the data to include in the plot
    filteredData <- reactive({
      yVar <- yVar()

      data <- plotData() %>%
        filter(Interpretation %in% c("S", "R", "I")) %>%
        mutate(
          Interpretation = ifelse(Interpretation == "S", 1, 0),
          Microorganism = if (aggByGenus()) mo_genus(Microorganism) else Microorganism
        ) %>%
        group_by(!!sym(yVar)) %>%
        mutate(Frequency = n()) %>%
        ungroup() %>%
        {
          if (n_distinct(.[[yVar]]) > 1) {
            filter(., Frequency >= min(tail(sort(unique(.$Frequency)), numAb())))
          } else {
            .
          }
        } %>%
        group_by(!!sym(yVar), Antimicrobial, Class) %>%
        summarise(
          obs = n(),
          prop = round(mean(Interpretation == 1), 3),
          .groups = 'drop'
        ) %>%
        mutate(size = cut(prop, breaks = c(0, 0.7, 0.9, 1), labels = c("s", "m", "l"))) %>%
        {
          if (yVar == "Microorganism") {
            mutate(., short_form = shorten_bacteria_names(.[[yVar]]))
          } else {
            mutate(
              .,
              short_form = ifelse(
                str_length(.[[yVar]]) > 15,
                str_c(str_sub(.[[yVar]], 1, 15), "..."),
                .[[yVar]]
              )
            )
          }
        } %>%
        arrange(Class, Antimicrobial) %>%
        mutate(
          colour = case_when(
            obs < 30 ~ "white",
            prop < 0.7 ~ "#D73027",
            prop >= 0.7 & prop <= 0.9 ~ "#FEE08B",
            prop > 0.9 ~ "#44CDC4"
          )
        )
      return(data)
    })

    #' The type of output to generate.
    #'
    #' Similified: plotly plot
    #' Classic: gt table (if gram split then 2 tables)
    outputType <- reactive({
      if (abType() == "Simplified") {
        return("simplified")
      }
      if (splitGram() == TRUE && yVar() == "Microorganism") {
        return("classic_split")
      }
      return("classic")
    })

    #' A reactive to hold all the items needed to create the plot/tables.
    outputItems <- reactive({
      data <- filteredData()
      if (nrow(data) == 0) {
        return(NULL)
      }
      return(getAntibiogramPlotItems(data, controls(), reactiveData()))
    })

    # This plot only renders if the user selects Simplified AB
    simplifiedPlot <- reactive({
      req(outputType() == "simplified")
      return(outputItems())
    })

    #' Holds the first table for the classic AB output.
    #' This table will differ based on if the user selected gram split or not.
    classicAbTable <- reactive({
      req(outputType() %in% c("classic", "classic_split"))
      if (outputType() == "classic") {
        return(outputItems()$table)
      }
      return(outputItems()$negTable)
    })

    #' Holds the first table data for the classic AB output.
    #' This data will differ based on if the user selected gram split or not.
    classicAbTableData <- reactive({
      req(outputType() %in% c("classic", "classic_split"))
      if (outputType() == "classic") {
        return(outputItems()$data)
      }
      return(outputItems()$df_wide_neg)
    })

    #' Holds the second table for the classic AB output when the user
    #' has selected gram split = TRUE.
    classicAbTable2 <- reactive({
      req(outputType() == "classic_split")
      return(outputItems()$posTable)
    })

    #' Holds the second table data for the classic AB output when the user
    #' has selected gram split = TRUE.
    classicAbTableData2 <- reactive({
      req(outputType() == "classic_split")
      return(outputItems()$df_wide_pos)
    })

    # Is there data to plot?
    hasData <- reactive({
      if (is.null(filteredData())) {
        return(FALSE)
      }
      nrow(filteredData()) > 0
    })

    # ------------------------------------------------------------------------------
    # Render UI
    # ------------------------------------------------------------------------------

    # Render Legend
    output$legend <- renderUI({
      if (abType() == "Simplified") {
        # Visual Legend -----------------------------------------------------------
        wellPanel(
          h4("Legend", class = "legend-title"),
          h5("Size", class = "legend-section"),
          div(
            class = "legend-section",
            div(
              class = "legend-item",
              tags$i(
                class = "fas fa-circle legend-circle",
                style = "font-size: 10px; margin-left: 10px;"
              ),
              span(
                "Low susceptibility (<70%)",
                class = "legend-label",
                style = "margin-left: 20px;"
              )
            ),
            div(
              class = "legend-item",
              tags$i(
                class = "fas fa-circle legend-circle",
                style = "font-size: 20px; margin-left: 5px;"
              ),
              span(
                "Moderate susceptibility (70 - 90%)",
                class = "legend-label",
                style = "margin-left: 15px;"
              )
            ),
            div(
              class = "legend-item",
              tags$i(
                class = "fas fa-circle legend-circle",
                style = "font-size: 30px; margin-right: 10px;"
              ),
              span(
                "High susceptibility (>90%)",
                class = "legend-label",
                style = "margin-left: 0px;"
              )
            )
          ),
          h5("Opacity", class = "legend-section"),
          div(
            class = "opacity-container",
            div(
              class = "opacity-item",
              tags$i(class = "fas fa-circle legend-circle", style = "opacity: 0.1;"),
              span("<30 Samples", class = "legend-label")
            ),
            div(class = "vertical-divider"),
            div(
              class = "opacity-item",
              tags$i(class = "fas fa-circle legend-circle"),
              span("30+ Samples", class = "legend-label")
            )
          ),
          h6("Bubbles are colored by antimicrobial class."),
          h6("Hover over a bubble for more details."),
          class = "legendWell"
        )

        # Classic Legend ----------------------------------------------------------
      } else {
        wellPanel(
          h4("Legend", class = "legend-title"),
          h5("Color", class = "legend-section"),
          div(
            class = "legend-section",
            div(
              class = "legend-item",
              tags$i(icon("square"), style = "font-size: 20px; margin-left: 5px; color: grey;"),
              span("Too few observations", class = "legend-label", style = "margin-left: 15px;")
            ),
            div(
              class = "legend-item",
              tags$i(
                class = "fas fa-solid fa-square",
                style = "font-size: 20px; margin-left: 5px; color: #D73027;"
              ),
              span(
                "Low susceptibility (<70%)",
                class = "legend-label",
                style = "margin-left: 15px;"
              )
            ),
            div(
              class = "legend-item",
              tags$i(
                class = "fas fa-solid fa-square",
                style = "font-size: 20px; margin-left: 5px; color: #FEE08B;"
              ),
              span(
                "Moderate susceptibility (70 - 90%)",
                class = "legend-label",
                style = "margin-left: 15px;"
              )
            ),
            div(
              class = "legend-item",
              tags$i(
                class = "fas fa-solid fa-square",
                style = "font-size: 20px; margin-left: 5px; color: #44CDC4;"
              ),
              span(
                "High susceptibility (>90%)",
                class = "legend-label",
                style = "margin-left: 15px;"
              )
            )
          ),
          h6("Vertical divisions represent antimicrobial class."),
          h6("Horizontal divisions represent microorganism gram stain."),
          h6("Hover over a cell for more details."),
          class = "legendWell"
        )
      }
    })

    # Render Controls
    output$controls <- renderUI({
      tagList(
        bsCollapse(
          id = "collapsePanel",
          open = NULL,
          multiple = T,
          bsCollapsePanel(
            HTML(
              "Controls <span class='glyphicon glyphicon-chevron-down' data-toggle='collapse-icon' 
            style='float: right; color: #aaa;'></span>"
            ),
            selectizeInput(
              ns("yVar"),
              "Y-axis variable",
              selected = "Microorganism",
              choices = c("Microorganism", "Source")
            ),

            conditionalPanel(
              condition = sprintf(
                "input['%s'] == 'Microorganism' && input['%s'] == 'Classic'",
                ns("yVar"),
                ns("abType")
              ),
              selectizeInput(
                ns("sortBy"),
                "Sort by",
                selected = "Frequency",
                choices = c("Alphabetical", "Frequency", "Gram Stain")
              ),
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Source'", ns("yVar")),
              selectizeInput(
                ns("sortBy"),
                "Sort by",
                selected = "Frequency",
                choices = c("Alphabetical", "Frequency")
              ),
            ),

            radioGroupButtons(
              ns("abType"),
              label = "Antibiogram style:",
              selected = "Classic",
              choices = c("Classic", "Simplified")
            ),
            radioGroupButtons(
              ns("handleLowCount"),
              label = "Handle low-count (<30) results",
              selected = "Include",
              choices = c("Include", "Exclude")
            ),
            numericInput(ns("numAb"), "Maximum Rows", value = 15, step = 1, min = 1, max = 30),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Classic'", ns("abType")),
              materialSwitch(ns("abColors"), label = "Show colors", value = TRUE)
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Microorganism'", ns("yVar")),
              materialSwitch(ns("aggGenus"), label = "Aggregate by Genus", value = F)
            ),

            conditionalPanel(
              condition = sprintf(
                "input['%s'] == 'Microorganism' && input['%s'] == 'Classic'",
                ns("yVar"),
                ns("abType")
              ),
              materialSwitch(ns("splitGram"), label = "Split by Gram Stain", value = F)
            ),

            actionButton(ns("applyControl"), "Apply", class = "submitButton")
          )
        )
      )
    })

    # If there is data, show the plot
    output$has_data <- renderUI({
      req(hasData())

      tagList(
        wellPanel(
          style = "overflow-x: scroll; overflow-y: scroll; max-height: 80vh; min-width: 500px;",
          div(
            class = "ab-table-wrapper",
            uiOutput(ns("classicAbContainer")), # Classic with no Gram Split
            uiOutput(ns("classicGramSplit")), # Classic with Gram Split
            uiOutput(ns("simplifiedContainer")), # Simplified
          ),
          class = "contentWell"
        ),
        div(
          uiOutput(ns("bp_download")),
          downloadButton(ns("save_image"), "Save Report", class = "plotSaveButton"),
          downloadButton(ns("save_table"), "Save Data", class = "plotSaveButton")
        )
      )
    })

    output$classicGramSplit <- renderUI({
      req(outputType() == "classic_split")
      return(tagList(
        hr(),
        h4("Gram Positive"),
        fluidRow(
          column(
            align = "center",
            width = 12,
            withSpinner(gt::gt_output(ns("classicAB2")), type = 4, color = "#44CDC4")
          )
        )
      ))
    })

    output$classicAbContainer <- renderUI({
      req(outputType() %in% c("classic", "classic_split"))
      return(tagList(
        if (outputType() == "classic_split") {
          h4("Gram Negative")
        },
        fluidRow(
          column(
            align = "center",
            width = 12,
            withSpinner(gt::gt_output(ns("classicAB")), type = 4, color = "#44CDC4")
          )
        )
      ))
    })

    output$simplifiedContainer <- renderUI({
      req(outputType() == "simplified")
      return(withSpinner(
        plotlyOutput(ns("simplifiedPlot"), height = "750px"),
        type = 4,
        color = "#44CDC4"
      ))
    })

    # Show the custom breakpoint download only if custom breakpoints are present
    output$bp_download <- renderUI({
      req(customBreakpoints())
      req(nrow(customBreakpoints()) > 0)
      downloadButton(ns("save_breakpoints"), "Save Custom Breakpoints", class = "plotSaveButton")
    })

    # If there is no data, show message to user
    output$has_no_data <- renderUI({
      req(!hasData())

      wellPanel(
        style = "display: flex; align-items: center; justify-content: center; max-height: 80vh;",
        div(
          style = "min-width: 1150px; min-height: 750px; display: flex; align-items: center; justify-content: center;",
          div(
            style = "display: flex; align-items: center; justify-content: center; height: 100%; flex-direction: column; text-align: center;",
            icon("disease", style = "font-size:100px; color: #44CDC4"),
            h4("Oops... looks like there isn't enough data for this plot."),
            h6(
              "Try reducing the number of filters applied or adjust your data in the 'Import' tab."
            )
          )
        ),
        class = "contentWell"
      )
    })

    # Show a plotly plot for simplified AB
    output$simplifiedPlot <- renderPlotly({
      req(outputType() == "simplified", !is.null(simplifiedPlot()))
      simplifiedPlot()
    })

    # Show a gt table for classic AB (both split and not split)
    output$classicAB <- gt::render_gt({
      req(outputType() %in% c("classic", "classic_split"), !is.null(classicAbTable()))
      classicAbTable()
    })

    # Show a gt table for classic AB gram negative (only split)
    output$classicAB2 <- gt::render_gt({
      req(outputType() == "classic_split", !is.null(classicAbTable2()))
      classicAbTable2()
    })

    # ------------------------------------------------------------------------------
    # Utility functions
    # ------------------------------------------------------------------------------

    shorten_bacteria_names <- function(names) {
      str_replace(
        names,
        pattern = "\\b(\\w)\\w*\\s(\\w+)",
        replacement = "\\1. \\2"
      )
    }

    #' Get the number of visible columns in the table.
    #' This is used to calculate the width of the html table and the
    #' font size for the table text.
    #'
    #' @param tableData The data used to create the table.
    #' @return          The number of visible columns.
    getVisibleCols <- function(tableData) {
      tableData <- tableData %>%
        select(-which(grepl("^obs_", names(tableData))))
      return(ncol(tableData))
    }

    #' Get the width for the html table based on number of columns.
    #' This is used to set the width for webshot when saving the report.
    #'
    #' @param tableData The data used to create the table.
    #' @return          The width in pixels.
    #' @seealso {@link{getVisibleCols}}
    getHtmlTableWidth <- function(tableData) {
      numCols <- getVisibleCols(tableData)

      #' The fixed columns (first 2) are wider than the rest and have
      #' a fixed width. Calculate the total width based on this.
      fixedColsWidth <- ifelse("n = " %in% colnames(tableData), 300, 180)
      vwidth <- fixedColsWidth + (numCols - 2) * 45 + 100
      return(vwidth)
    }

    #' Add custom CSS styling to the html table.
    #' Includes the Carme font from Google Fonts.
    #'
    #' @param tableHtml The html of the table.
    #' @param tableData The data used to create the table.
    #' @return          The html with the CSS added.
    #' @seealso {@link{getVisibleCols}}
    addStylingToHtml <- function(tableHtml, tableData) {
      font_size <- max(12, 16 - 0.3 * getVisibleCols(tableData))

      css <- sprintf(
        '
      <link href="https://fonts.googleapis.com/css2?family=Carme&display=swap" rel="stylesheet">
      <style>
        body, table, td, th {
          font-family: "Carme", sans-serif !important;
          font-size: %dpx !important;
        }
        .dataTables_wrapper {
          overflow-x: visible !important;
        }
        table {
          table-layout: fixed;
        }
      </style>',
        round(font_size)
      )

      return(sub("</head>", paste0(css, "\n</head>"), tableHtml))
    }

    #' Save the visualisation as a png image.
    #'
    #' @param visualisation The visualisation to save (plotly or gt).
    #' @param table_data    The data used to create the visualisation.
    #' @param filename      The filename (without extension).
    #' @return              None.
    #' @seealso {@link{getHtmlTableWidth}}, {@link{addStylingToHtml}}
    saveVisualisationPng <- function(visualisation, table_data, filename) {
      htmlName <- paste0(filename, ".html")
      pngName <- paste0(filename, ".png")

      htmltools::save_html(visualisation, htmlName)
      html_lines <- readLines(htmlName)

      html_lines <- addStylingToHtml(html_lines, table_data)
      writeLines(html_lines, htmlName)

      pngWidth <- getHtmlTableWidth(table_data)

      webshot2::webshot(
        url = htmlName,
        file = pngName,
        vwidth = pngWidth
      )
    }

    # ------------------------------------------------------------------------------
    # Observes
    # ------------------------------------------------------------------------------

    observeEvent(input$applyControl, {
      showColors(input$abColors)
      abType(input$abType)
      aggByGenus(input$aggGenus)
      lowCounts(input$handleLowCount)
      yVar(input$yVar)
      sortBy(input$sortBy)
      numAb(input$numAb)
      splitGram(input$splitGram)
    })

    # ------------------------------------------------------------------------------
    # Download Handlers
    # ------------------------------------------------------------------------------

    # Save all custom breakpoints used in the app
    output$save_breakpoints <- downloadHandler(
      filename = function() {
        paste("Custom_Breakpoints.csv")
      },
      content = function(file) {
        write.csv(customBreakpoints(), file, row.names = FALSE)
      }
    )

    # Save Report -------------------------------------------------------------
    output$save_image <- downloadHandler(
      filename = "Antibiogram.html",

      content = function(file) {
        withProgress(message = 'Rendering, please wait!', {
          src <- normalizePath(".")
          tmp <- tempdir()
          unlink(list.files(tmp, full.names = TRUE), recursive = TRUE, force = TRUE)

          owd <- setwd(tmp)
          on.exit({
            setwd(owd)
            unlink(
              c(
                "filters.RDS",
                "antibiogram_table.html",
                "antibiogram_table2.html",
                "antibiogram_table.png",
                "antibiogram_table2.png",
                "Antibiogram.qmd"
              ),
              recursive = TRUE
            )
          })
          normalizedReport <- normalizePath(file.path(src, "Reports", "Antibiogram.qmd"))
          stylesPath <- normalizePath(file.path(src, "www", "css", "report.css"))
          logoPath <- normalizePath(file.path(src, "www", "img", "logoDark.png"))
          bpImgPath <- normalizePath(file.path(src, "www", "img", "report-bp-example.png"))
          file.copy(normalizedReport, "Antibiogram.qmd", overwrite = TRUE)
          file.copy(stylesPath, "report.css", overwrite = TRUE)
          file.copy(logoPath, "logo.png", overwrite = TRUE)
          file.copy(bpImgPath, "report-bp-example.png", overwrite = TRUE)

          currentVisualisation <- switch(
            outputType(),
            "simplified" = simplifiedPlot(),
            classicAbTable() %>% gt::cols_hide(columns = "n =")
          )

          table_data <- switch(
            outputType(),
            "simplified" = currentVisualisation$x$data,
            currentVisualisation$`_data`
          )

          if ("n =" %in% colnames(table_data)) {
            table_data <- table_data %>%
              select(-`n =`)
          }

          saveVisualisationPng(currentVisualisation, table_data, "antibiogram_table")

          # Number of Isolates Images
          numIsolates <- getAntibiogramPlotItems(
            filteredData(),
            controls(),
            reactiveData(),
            isIsolateTable = TRUE
          )

          if (splitGram()) {
            visualisation2 <- classicAbTable2() %>% gt::cols_hide(columns = "n =")
            saveVisualisationPng(visualisation2, table_data, "antibiogram_table2")

            # Number of Isolates Images
            saveVisualisationPng(numIsolates$negTable, table_data, "isolate_table")
            saveVisualisationPng(numIsolates$posTable, table_data, "isolate_table2")
          } else {
            saveVisualisationPng(numIsolates$table, table_data, "isolate_table")
          }

          filters <- lapply(activeFilters(), as.character)
          writeLines(lowCounts(), "low_counts_flag.txt")

          browser()
          quarto::quarto_render(
            input = "Antibiogram.qmd",
            output_format = "html",
            output_file = "Antibiogram.html",
            execute_params = list(vwidth = getHtmlTableWidth(table_data), filters = filters)
          )

          file.rename("Antibiogram.html", file)
        })
      }
    )

    # Save Data ---------------------------------------------------------------
    output$save_table <- downloadHandler(
      filename = function() {
        paste("Antibiogram.xlsx")
      },
      content = function(file) {
        sheets <- if (splitGram()) {
          list(
            "Gram Negative" = classicAbTableData(),
            "Gram Positive" = classicAbTableData2()
          )
        } else {
          list(
            "Antibiogram" = classicAbTableData()
          )
        }
        writexl::write_xlsx(sheets, path = file)
      }
    )
  })
}
