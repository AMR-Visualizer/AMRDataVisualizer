micPageUI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      
      # Main Content ------------------------------------------------------------
      
      column(9,
             uiOutput(ns("content"))
      ),
      
      # Filters -----------------------------------------------------------------
      
      column(3,
             wellPanel(
               h4("Filters"),
                 uiOutput(ns("filters")),
               hr(),
               selectizeInput(
                 ns("groupingVar"),
                 label = "Grouping Variable",
                 choices = c("Month", "Region", "Source", "Species", "Subregion", "Year"),
                 selected = "Year",
                 multiple = F
               ),
               class = "contentWell"
               )
             )
    )
  )
}

micPageServer <- function(id, data, guideline) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$filters <- renderUI({
      req(data)
      tagList(
        selectizeInput(
          ns("moFilter"), 
          label = "Microorganism", 
          choices = sort(unique(data$Microorganism), na.last = NA), 
          selected = names(sort(table(data$Microorganism), decreasing = TRUE))[1],
          multiple = FALSE
        ),
        
        selectizeInput(
          ns("abFilter"), 
          label = "Antimicrobial", 
          choices = sort(unique(data$Antimicrobial), na.last = NA), 
          selected = names(sort(table(data$Antimicrobial), decreasing = TRUE))[1],
          multiple = FALSE
        ),
        
        selectizeInput(
          ns("typeFilter"), 
          label = "Type", 
          choices = c("Urinary", "Non-urinary"), 
          selected = "Urinary",
          multiple = FALSE
        ),
        
        if (!is.null(data$Species) && length(unique(na.omit(data$Species))) > 1)
          selectizeInput(
            ns("speciesFilter"),
            label = "Species",
            choices = sort(unique(data$Species), na.last = NA),
            selected = names(sort(table(data$Species), decreasing = TRUE))[1],
            multiple = FALSE
          )
      )
    })
    
    table_result <- reactive({
      req(input$moFilter, input$abFilter, input$typeFilter, input$groupingVar)
      species <- input$speciesFilter
      if (is.null(species)) species <- data$Species[[1]]
      
      create_mic_frequency_tables(
        data = data,
        group_by_var = input$groupingVar,
        ab = input$abFilter,
        mo = input$moFilter,
        type = input$typeFilter,
        species = species,
        guideline = guideline
      )
    })
    
    output$content <- renderUI({
      req(data, input$moFilter, input$abFilter, input$typeFilter, input$groupingVar)
      
      if (nrow(data) < 1 || is.null(table_result())) {
 
        wellPanel(
          style = "display: flex; align-items: center; justify-content: center; max-height: 80vh;",
          div(
            style = "min-width: 1150px; min-height: 750px; display: flex; align-items: center; justify-content: center;",
            uiOutput(ns("errorHandling"))
          ),
          class = "contentWell"
        )
        
      } else {

        tagList(
          wellPanel(
            style = "overflow-x: scroll; overflow-y: scroll; max-height: 80vh; min-width: 500px;",
            div(
              class = "ab-table-wrapper",
              withSpinner(gt_output(ns("plot")), type = 4, color = "#44CDC4")
            ),
            class = "contentWell"
          ),
          div(
            downloadButton(ns("save_table"), "Save Data", class = "plotSaveButton")
          )
        )
      }
    })
    
    output$errorHandling <- renderUI({
      div(style = "display: flex; align-items: center; justify-content: center; height: 100%; flex-direction: column; text-align: center;",
          icon("disease", style = "font-size:100px; color: #44CDC4"),
          h4("Oops... looks like there isn't enough data for this plot."),
          h6("This combination of filters does not appear in your data.")
      )
    })
    
    output$plot <- render_gt({
      req(table_result())
    })
    
    
  })
}
