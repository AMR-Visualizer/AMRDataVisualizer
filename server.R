# ------------------------------------------------------------------------------
# Main server script
# Author: Kurtis Sobkowich
# Description: Defines the server-side logic for the AMR Visualizer app
#              Coordinates multiple external modules to handle user input,
#              reactive processing, and dynamic outputs.
# ------------------------------------------------------------------------------

server <- function(input, output, session) {

  # ------------------------------------------------------------------------------
  # Sub-modules
  # ------------------------------------------------------------------------------

  homePageServer("home")
  importResults <- importDataServer("dataImport") # Gather cleaned data from `importDataModule.R`
  dataWithCustomBreakpoints <- micPageServer(
    "micModule",
    reactiveData = clean,
    processedGuideline = processedGuideline
  )

  # ------------------------------------------------------------------------------
  # Module variables
  # ------------------------------------------------------------------------------

  clean <- importResults$data
  processedGuideline <- importResults$guideline


  # ------------------------------------------------------------------------------
  # Reactives
  # ------------------------------------------------------------------------------

  # Check for cleaned data returned from `importDataModule.R`
  isDataPresent <- reactive({
    if (is.null(clean())) {
      return(FALSE)
    }
    !is.null(clean()) && nrow(clean()) > 0
  })

  # ------------------------------------------------------------------------------
  # Render UI
  # ------------------------------------------------------------------------------

  # If cleaned data exist, render full sidebar menu.
  # Selectively show sidebar menu if data is present.
  output$menu <- renderUI({
    if (isDataPresent()) {
      
      # Define menu items
      menu_items <- list(
        menuItem("Overview", tabName = "ovTab", icon = icon("chart-simple", class = "nav-icon")),
        menuItem("Antibiogram", tabName = "abTab", icon = icon("braille", class = "nav-icon")),
        menuItem("Map", tabName = "mapTab", icon = icon("map-location-dot", class = "nav-icon")),
        menuItem("Trends", tabName = "trendsTab", icon = icon("chart-line", class = "nav-icon")),
        menuItem("MicroGuide", tabName = "pathogenTab", icon = icon("bacteria", class = "nav-icon")),
        menuItem("MDR", tabName = "mdrTab", icon = icon("pills", class = "nav-icon")),
        menuItem("Explore", tabName = "exploreTab", icon = icon("table-list", class = "nav-icon"))
      )
    
      # Show "MIC Tables" tab if MIC data were imported
      if ("MIC" %in% names(clean())) {
        mic_item <- menuItem("MIC Tables", tabName = "micTab", icon = icon("vial", class = "nav-icon"))
        menu_items <- c(list(mic_item), menu_items)
      }
      
      sidebarMenu(id = "tabs", menu_items)
      
      # If cleaned data do not exist, show message to user
    } else {
      tagList(
        sidebarMenu(id = "tabs"),
        h6(em("Please import or select a data source to access additional tabs."), style = "color: #a7b6d4; margin:25px; text-align: center;")
      )
    }
  })


  # ------------------------------------------------------------------------------
  # Observes
  # ------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------
  # Selectively hide header bar if tab = "Home"                 
  # ------------------------------------------------------------------------------
  observe({
    req(input$tabs)
    if (input$tabs == "homeTab") {
      js$hideHeader()
    } else {
      js$showHeader()
    }
  })

  
# ------------------------------------------------------------------------------
# Switch "i" (information) modal content based on current tab                 
# ------------------------------------------------------------------------------
  observeEvent(input$info, {
    showModal(
      modalDialog(
        title = div(
          style = "text-align: center;",
          tags$img(
            src = "logoDark.png",
            height = "100px",
            style = "vertical-align: middle;"
          )
        ),

        switch(
          input$tabs,
          importTab   = includeMarkdown("Documentation/data-import.md"),
          ovTab       = includeMarkdown("Documentation/overview-plots.md"),
          micTab      = includeMarkdown("Documentation/mic-tables.md"),
          abTab       = includeMarkdown("Documentation/antibiograms.md"),
          mapTab      = includeMarkdown("Documentation/maps.md"),
          trendsTab   = includeMarkdown("Documentation/trends.md"),
          pathogenTab = includeMarkdown("Documentation/microguide.md"),
          mdrTab      = includeMarkdown("Documentation/mdr-matrices.md"),
          exploreTab  = includeMarkdown("Documentation/data-explore.md"),
          "Documentation Coming Soon."  # Fallback message
        ),
        
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
# ------------------------------------------------------------------------------
# Initialize server functions for each tab module       
# ------------------------------------------------------------------------------
  
  # Overview tab
  observe({
    req(clean())
    ovPageServer("overviewModule", clean())
  })
  
  # Antibiogram tab
  output$antibiogramUI <- renderUI({
    req(clean())
    abPageUI("antibiogramModule", clean())
  })
  
  observe({
    req(clean())
    abPageServer("antibiogramModule", clean())
  })

  # Map tab
  output$mapUI <- renderUI({
    req(clean())
    mapPageUI("mapModule", clean())
  })
  
  observe({
    req(clean())
    mapPageServer("mapModule", clean())
  })
  
  
  # Trends tab
  output$tsUI <- renderUI({
    req(clean())
    tsPageUI("tsModule", clean())
  })
  
  observe({
    req(clean())
    tsPageServer("tsModule", clean())
  })
  
  # Microguide tab
  output$pathogenUI <- renderUI({
    req(clean())
    pathogenPageUI("pathogenModule", clean())
  })
  
  observe({
    req(clean())
    pathogenPageServer("pathogenModule", clean())
  })
  
  #MDR tab
  output$mdrUI <- renderUI({
    req(clean())
    mdrPageUI("mdrModule", clean())
  })
  
  observe({
    req(clean())
    mdrPageServer("mdrModule", clean())
  })
  
  # Explore tab
  output$exploreUI <- renderUI({
    req(clean())
    explorePageUI("exModule", clean())
  })
  
  observe({
    req(clean())
    explorePageServer("exModule", clean())
  })
  
  
# ------------------------------------------------------------------------------
# End of main Server                 
# ------------------------------------------------------------------------------
}