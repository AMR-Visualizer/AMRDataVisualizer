
server <- function(input, output, session) {
  
  observe({
    req(input$tabs)
    if (input$tabs == "homeTab") {
      js$hideHeader()
    } else {
      js$showHeader()
    }
  })
  
  # Get Clean Data ----------------------------------------------------------------
  
  results <- importDataServer("dataImport")
  clean <- results$data
  guideline <- results$guideline
  
  # Determine if full sidebar should be shown -------------------------------
  showMenu <- reactiveVal(FALSE)
  dataPresent <- reactive({
    !is.null(clean()) && nrow(clean()) > 0
  })
  observe({
    showMenu(dataPresent())
  })
  output$menu <- renderUI({
    if (dataPresent()) {
      menu_items <- list(
        menuItem("Overview", tabName = "ovTab", icon = icon("chart-simple", class = "nav-icon")),
        menuItem("Antibiogram", tabName = "abTab", icon = icon("braille", class = "nav-icon")),
        menuItem("Map", tabName = "mapTab", icon = icon("map-location-dot", class = "nav-icon")),
        menuItem("Trends", tabName = "trendsTab", icon = icon("chart-line", class = "nav-icon")),
        menuItem("MicroGuide", tabName = "pathogenTab", icon = icon("bacteria", class = "nav-icon")),
        menuItem("MDR", tabName = "mdrTab", icon = icon("pills", class = "nav-icon")),
        menuItem("Explore", tabName = "exploreTab", icon = icon("table-list", class = "nav-icon"))
      )
    
      if ("MIC" %in% names(clean())) {
        mic_item <- menuItem("MIC Tables", tabName = "micTab", icon = icon("vial", class = "nav-icon"))
        menu_items <- c(list(mic_item), menu_items)
      }
      
      sidebarMenu(id = "tabs", menu_items)
    } else {
      tagList(
        sidebarMenu(id = "tabs"),
        h6(em("Please import or select a data source to access additional tabs."), style = "color: #a7b6d4; margin:25px; text-align: center;")
      )
    }
  })
  
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
  
  
  
  # Overview Page -----------------------------------------------------------
  observe({
    req(clean())
    ovPageServer("overviewModule", clean())
  })
  
  # MIC Page --------------------------------------------------------
  output$micUI <- renderUI({
    req(clean())
    micPageUI("micModule", clean())
  })
  
  observe({
    req(clean())
    micPageServer("micModule", data = clean(), guideline = guideline())
  })
  
  # Antibiogram Page --------------------------------------------------------
  output$antibiogramUI <- renderUI({
    req(clean())
    abPageUI("antibiogramModule", clean())
  })
  
  observe({
    req(clean())
    abPageServer("antibiogramModule", clean())
  })
  
  # Map Page ----------------------------------------------------------------
  output$mapUI <- renderUI({
    req(clean())
    mapPageUI("mapModule", clean())
  })
  
  observe({
    req(clean())
    mapPageServer("mapModule", clean())
  })
  
  
  # Time Series Page --------------------------------------------------------
  output$tsUI <- renderUI({
    req(clean())
    tsPageUI("tsModule", clean())
  })
  
  observe({
    req(clean())
    tsPageServer("tsModule", clean())
  })
  
  
  # Pathogen Page -----------------------------------------------------------
  output$pathogenUI <- renderUI({
    req(clean())
    pathogenPageUI("pathogenModule", clean())
  })
  
  observe({
    req(clean())
    pathogenPageServer("pathogenModule", clean())
  })
  
  # MDR Page --------------------------------------------------------
  output$mdrUI <- renderUI({
    req(clean())
    mdrPageUI("mdrModule", clean())
  })
  
  observe({
    req(clean())
    mdrPageServer("mdrModule", clean())
  })
  
  
  # Explore Page ------------------------------------------------------------
  
  output$exploreUI <- renderUI({
    req(clean())
    explorePageUI("exModule", clean())
  })
  
  observe({
    req(clean())
    explorePageServer("exModule", clean())
  })
  
  
  # Close server ------------------------------------------------------------
}