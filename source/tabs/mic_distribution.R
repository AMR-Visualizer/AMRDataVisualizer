#' UI for the MIC distribution tab module.
#'
#' @param id  Module ID.
#' @return    Module UI.
ui <- function(id, data) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Main Content ------------------------------------------------------------

      column(9, uiOutput(ns("content"))),

      # Filters -----------------------------------------------------------------

      column(3, filter_panel$ui(ns("filters")))
    )
  )
}

#' Server logic for the MIC distribution tab module.
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

    filters <- filter_panel$server(
      "filters",
      reactiveData,
      default_filters = c("Antimicrobial", "Microorganism", "Species", "Source", "Date"),
      auto_populate = list(Antimicrobial = TRUE, Microorganism = TRUE)
    )

    plotData <- reactive({
      filters$filteredData()
    })

    initialData <- reactive({
      reactiveData()
    })

    # ------------------------------------------------------------------------------
    # Render UI
    # ------------------------------------------------------------------------------

    output$content <- renderUI({
      req(plotData())
      if (!is.null(plotData()) && nrow(plotData()) > 0) {
        tagList(
          wellPanel(
            style = "overflow-x: scroll; overflow-y: scroll; max-height: 80vh;",
            div(style = "min-height: 750px", plotOutput(ns("plot"), height = "71vh")),
            class = "contentWell"
          ),
          downloadButton(ns("save_btn"), "Save", class = "plotSaveButton")
        )
      } else {
        wellPanel(
          style = "display: flex; align-items: center; justify-content: center; max-height: 80vh;",
          div(
            style = "min-width: 1150px; min-height: 750px; display: flex; align-items: center; justify-content: center;",
            uiOutput(ns("errorHandling"))
          ),
          class = "contentWell"
        )
      }
    })

    output$errorHandling <- renderUI({
      div(
        style = "display: flex; align-items: center; justify-content: center; height: 100%; flex-direction: column; text-align: center;",
        icon("disease", style = "font-size:100px; color: #44CDC4"),
        h4("Oops... looks like there isn't enough data for this plot."),
        h6("Try reducing the number of filters applied or adjust your data in the 'Import' tab.")
      )
    })

    output$plot <- renderPlot({
      df <- plotData()

      df$MIC <- as.character(df$MIC)

      out <- plot_mic_distribution(
        data = df,
        drug_col = "Antimicrobial",
        mic_col = "MIC",
        epsilon = 1e-4
      )

      out$plot
    })

    output$save_btn <- downloadHandler(
      filename = function() paste0(Sys.Date(), "_AMRVisualizer_MICDistributions.png"),
      content = function(file) {
        df <- plotData()
        df$MIC <- as.character(df$MIC)
        out <- plot_mic_distribution(df, drug_col = "Antimicrobial", mic_col = "MIC")
        ggplot2::ggsave(file, plot = out$plot, width = 12, height = 8, dpi = 300)
      }
    )
  })
}

mic_distribution_tab <- list(
  ui = ui,
  server = server
)

