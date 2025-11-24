#' UI for the explore tab module.
#'
#' @param id  Module ID.
#' @return    Module UI.
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("content"))
  )
}

#' Server logic for the explore tab module.
#'
#' @param id            The ID of the module.
#' @param reactiveData  A reactive that returns the cleaned data to be explored.
#' @return              None.
server <- function(id, reactiveData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------------------
    # Sub-modules
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Module variables
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Reactives
    # ------------------------------------------------------------------------------

    # Has the user generated a table yet?
    dataGenerated <- reactiveVal(FALSE)

    inputs <- reactiveValues(
      MO = NULL,
      AB = NULL,
      Site = NULL,
      Species = NULL,
      Subregion = NULL,
      Region = NULL,
      startDate = Sys.Date(),
      endDate = Sys.Date(),
      groupMO = F,
      groupAB = F,
      groupSite = F,
      groupSpecies = F,
      groupSubregion = F,
      groupRegion = F,
      groupMonths = F
    )

    #' Data to display in the summary table.
    #' The `dataGenerated` reactiveVal needs to be TRUE (it is set to TRUE when the
    #' user clicks "Generate table" the first time) and there must be data in the
    #' `reactiveData` reactive.
    #'
    #' The bindEvent ensures that the table data is generated when the user clicks
    #' the generate button or when the `reactiveData` reactive changes due to updates
    #' to the custom breakpoints in the MIC tab.
    tableData <- reactive({
      req(reactiveData())
      req(dataGenerated())

      df <- reactiveData()

      filters <- list(
        Microorganism = input$selectedMO,
        Antimicrobial = input$selectedAB,
        Source = input$selectedSite,
        Species = input$selectedSpecies,
        Subregion = input$selectedSubregion,
        Region = input$selectedRegion
      )

      for (col in names(filters)) {
        if (!is.null(filters[[col]]) && col %in% names(df)) {
          df <- df %>% filter(.data[[col]] %in% filters[[col]])
        }
      }

      # Date filtering
      if (!is.null(input$selectedDates) && "Date" %in% names(df)) {
        df <- df %>%
          filter(Date >= input$selectedDates[1], Date <= input$selectedDates[2])
      }

      # Defensive selection logic
      cols_to_select <- c(
        if (!input$groupMonths && "Date" %in% names(df)) "Date",
        if (!input$groupMO && "Microorganism" %in% names(df)) "Microorganism",
        if (!input$groupAB && "Antimicrobial" %in% names(df)) "Antimicrobial",
        if (!input$groupSite && "Source" %in% names(df)) "Source",
        if (!input$groupSpecies && "Species" %in% names(df)) "Species",
        if (!input$groupSubregion && "Subregion" %in% names(df)) "Subregion",
        if (!input$groupRegion && "Region" %in% names(df)) "Region",
        if ("Class" %in% names(df)) "Class",
        if ("Interpretation" %in% names(df)) "Interpretation"
      )

      df <- df %>%
        select(any_of(cols_to_select))

      grouping_vars <- names(df)

      df <- df %>%
        group_by(across(all_of(grouping_vars))) %>%
        summarize(int_count = n(), .groups = "drop") %>%
        pivot_wider(
          names_from = Interpretation,
          values_from = int_count,
          values_fill = 0
        )

      for (col in c("S", "I", "R")) {
        if (!col %in% names(df)) df[[col]] <- 0
      }

      df %>%
        mutate(Total = S + I + R) %>%
        relocate(any_of(c("S", "I", "R")), .before = Total)
    }) %>%
      bindEvent(input$generate, reactiveData(), dataGenerated())

    # ------------------------------------------------------------------------------
    # Render UI
    # ------------------------------------------------------------------------------

    output$content <- renderUI({
      if (!dataGenerated()) {
        div(
          uiOutput(ns("filtersPanel")),
          style = "margin: auto;"
        )
      } else {
        tagList(
          fluidRow(
            column(4, uiOutput(ns("filtersPanel"))),
            column(
              8,
              wellPanel(
                DT::dataTableOutput(ns("table")),
                class = "dataPreviewWell"
              ),
              div(
                downloadButton(ns("downloadParams"), "Download inputs", class = "clearButton"),
                downloadButton(ns("downloadTable"), "Download table", class = "submitButton"),
                style = "float: right; margin-top: 15px;"
              )
            )
          )
        )
      }
    })

    output$filtersPanel <- renderUI({
      data <- reactiveData()
      wellPanel(
        h4("Summary table filters"),
        hr(),
        div(
          bsCollapse(
            id = "exFilters",
            open = "Microorganism",
            multiple = F,
            bsCollapsePanel(
              "Microorganism",
              selectizeInput(
                ns("selectedMO"),
                label = NULL,
                choices = c(sort(unique(data$Microorganism), na.last = TRUE)),
                selected = inputs$MO,
                multiple = T
              ),
              awesomeCheckbox(ns("groupMO"), "Aggregate microorganisms?", value = inputs$groupMO)
            ),
            bsCollapsePanel(
              "Antimicrobial",
              selectizeInput(
                ns("selectedAB"),
                label = NULL,
                choices = c(sort(unique(data$Antimicrobial), na.last = TRUE)),
                selected = inputs$AB,
                multiple = T
              ),
              awesomeCheckbox(ns("groupAB"), "Aggregate by class?", value = inputs$groupAB)
            ),
            bsCollapsePanel(
              "Sampling site",
              selectizeInput(
                ns("selectedSite"),
                label = NULL,
                choices = c(sort(unique(data$Source), na.last = TRUE)),
                selected = inputs$Site,
                multiple = T
              ),
              awesomeCheckbox(ns("groupSite"), "Aggregate sites?", value = inputs$groupSite)
            ),
            bsCollapsePanel(
              "Species",
              selectizeInput(
                ns("selectedSpecies"),
                label = NULL,
                choices = c(sort(unique(data$Species), na.last = TRUE)),
                selected = inputs$Species,
                multiple = T
              ),
              awesomeCheckbox(ns("groupSpecies"), "Aggregate species?", value = inputs$groupSpecies)
            ),
            bsCollapsePanel(
              "Subregion",
              selectizeInput(
                ns("selectedSubregion"),
                label = NULL,
                choices = c(sort(unique(data$Subregion), na.last = TRUE)),
                selected = inputs$Subregion,
                multiple = T
              ),
              awesomeCheckbox(
                ns("groupSubregion"),
                "Aggregate subregions?",
                value = inputs$groupSubregion
              )
            ),
            bsCollapsePanel(
              "Region",
              selectizeInput(
                ns("selectedRegion"),
                label = NULL,
                choices = c(sort(unique(data$Region), na.last = TRUE)),
                selected = inputs$Region,
                multiple = T
              ),
              awesomeCheckbox(ns("groupRegion"), "Aggregate Regions?", value = inputs$groupRegion)
            ),
            bsCollapsePanel(
              "Timeframe",
              dateRangeInput(
                ns("selectedDates"),
                label = NULL,
                min = min(data$Date),
                max = max(data$Date),
                # If dates are NA (e.g. no data), default to min/max of data
                start = ifelse(is.na(inputs$startDate), min(data$Date), inputs$startDate),
                end = ifelse(is.na(inputs$endDate), max(data$Date), inputs$endDate),
                width = "100%"
              ),
              awesomeCheckbox(ns("groupMonths"), "Aggregate months?", value = inputs$groupMonths)
            )
          ),
          style = "max-height: 50vh; overflow-y: auto;"
        ),
        hr(),
        h5("Upload parameter file"),
        fileInput(ns("paramsUpload"), label = NULL, accept = ".json"),
        actionButton(ns("generate"), "Generate table", class = "submitButton"),
        class = "exploreFiltersWell",
      )
    })

    output$table <- DT::renderDataTable({
      df <- tableData()
      req(!is.null(df))
      if (nrow(df) > 0) {
        datatable(
          df,
          class = "table",
          options = list(
            columnDefs = list(list(className = 'dt-center', targets = 5)),
            pageLength = 20,
            lengthMenu = c(10, 20, 50, 100)
          )
        )
      } else {
        datatable(data.frame(Message = "No data available with selected filters."))
      }
    })

    # ------------------------------------------------------------------------------
    # Utility functions
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Observes
    # ------------------------------------------------------------------------------

    observeEvent(input$generate, {
      data <- reactiveData()

      inputs$MO <- input$selectedMO
      inputs$AB <- input$selectedAB
      inputs$Site <- input$selectedSite
      inputs$Species <- input$selectedSpecies
      inputs$Subregion <- input$selectedSubregion
      inputs$Region <- input$selectedRegion
      inputs$startDate <- ifelse(
        is.na(input$selectedDates[1]),
        min(data$Date),
        input$selectedDates[1]
      )
      inputs$endDate <- ifelse(
        is.na(input$selectedDates[2]),
        max(data$Date),
        input$selectedDates[2]
      )
      inputs$groupMO <- input$groupMO
      inputs$groupAB <- input$groupAB
      inputs$groupSite <- input$groupSite
      inputs$groupSpecies <- input$groupSpecies
      inputs$groupSubregion <- input$groupSubregion
      inputs$groupRegion <- input$groupRegion
      inputs$groupMonths <- input$groupMonths
    })

    observeEvent(input$paramsUpload, {
      req(input$paramsUpload)

      params <- fromJSON(readLines(input$paramsUpload$datapath))

      updateSelectizeInput(session, "selectedMO", selected = params$Inputs$Microorganism$Selected)
      updateSelectizeInput(session, "selectedAB", selected = params$Inputs$Antimicrobial$Selected)
      updateSelectizeInput(session, "selectedSite", selected = params$Inputs$Source$Selected)
      updateSelectizeInput(session, "selectedSpecies", selected = params$Inputs$Species$Selected)
      updateSelectizeInput(
        session,
        "selectedSubregion",
        selected = params$Inputs$Subregion$Selected
      )
      updateSelectizeInput(session, "selectedRegion", selected = params$Inputs$Region$Selected)
      updateDateRangeInput(
        session,
        "selectedDates",
        start = params$Inputs$Dates$Selected[1],
        end = params$Inputs$Dates$Selected[2]
      )
      updateCheckboxInput(session, "groupMO", value = params$Inputs$Microorganism$Aggregated)
      updateCheckboxInput(session, "groupAB", value = params$Inputs$Antimicrobial$Aggregated)
      updateCheckboxInput(session, "groupSite", value = params$Inputs$Source$Aggregated)
      updateCheckboxInput(session, "groupSpecies", value = params$Inputs$Species$Aggregated)
      updateCheckboxInput(session, "groupSubregion", value = params$Inputs$Subregion$Aggregated)
      updateCheckboxInput(session, "groupRegion", value = params$Inputs$Region$Aggregated)
      updateCheckboxInput(session, "groupMonths", value = params$Inputs$Dates$Aggregated)
    })

    #' Set `dataGenerated` to TRUE when the user clicks "Generate table" the first time.
    observeEvent(input$generate, {
      req(!dataGenerated())
      dataGenerated(TRUE)
    })

    # ------------------------------------------------------------------------------
    # Download Handlers
    # ------------------------------------------------------------------------------

    output$downloadParams <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "-AMRDataVisualizer.json")
      },
      content = function(file) {
        paramsJSON <- list(
          AppInfo = list(
            Title = "AMR Data Visualizer - Summary Table",
            SystemDate = as.character(Sys.Date())
          ),
          Inputs = list(
            Microorganism = list(
              Selected = input$selectedMO,
              Aggregated = input$groupMO
            ),
            Antimicrobial = list(
              Selected = input$selectedAB,
              Aggregated = input$groupAB
            ),
            Source = list(
              Selected = input$selectedSite,
              Aggregated = input$groupSite
            ),
            Species = list(
              Selected = input$selectedSpecies,
              Aggregated = input$groupSpecies
            ),
            Subregion = list(
              Selected = input$selectedSubregion,
              Aggregated = input$groupSubregion
            ),
            Region = list(
              Selected = input$selectedRegion,
              Aggregated = input$groupRegion
            ),
            Dates = list(
              Selected = input$selectedDates,
              Aggregated = input$groupMonths
            )
          )
        )

        jsonData <- toJSON(paramsJSON, pretty = TRUE)

        writeLines(jsonData, file)
      }
    )

    output$downloadTable <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "-AMRDataVisualizerSummaryTable.csv")
      },
      content = function(file) {
        write.csv(tableData(), file)
      }
    )

    # ------------------------------------------------------------------------------
    # Module return
    # ------------------------------------------------------------------------------
  })
}

explore_tab <- list(
  ui = ui,
  server = server
)
