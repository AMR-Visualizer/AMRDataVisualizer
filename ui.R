# ------------------------------------------------------------------------------
# Main UI script
# Author: Kurtis Sobkowich
# Description: Main UI layout for the AMR Visualizer.
# ------------------------------------------------------------------------------

dashboardPage(
  
# ------------------------------------------------------------------------------
# Header                 
# ------------------------------------------------------------------------------
  dashboardHeader(
    title = "",  # No visible title in the top navbar
    tags$li(
      class = "dropdown",
      actionButton("info", icon("circle-info"), class = "info")  # Info/help button
    )
  ),
  
# ------------------------------------------------------------------------------
# Sidebar
# ------------------------------------------------------------------------------
  dashboardSidebar(
    # Logo
    img(
      src = "logo.png",
      height = "100px",
      style = "margin-left: 50px"
    ),
    hr(style = "border-color:#a7b6d4; margin:20px"),
    
    # Navigation Menu
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "homeTab", icon = icon("house", class = "nav-icon")),
      menuItem("Import", tabName = "importTab", icon = icon("file-import", class = "nav-icon")),
      style = "margin-top:25px"
    ),
    
    # Dynamically generated menu (e.g., based on presence of data)
    uiOutput("menu")
  ),
  
  # ---------------------------------------------------------------------------
  # Main body
  # ---------------------------------------------------------------------------
  dashboardBody(
    
    # Load custom styling and scripts
    includeCSS("styles.css"),
    includeScript("script.js"),
    
    # Set global font using `bslib` and `thematic`
    use_googlefont("Carme"),
    use_theme(create_theme(
      theme = "default",
      bs_vars_font(family_sans_serif = "Carme")
    )),
    
    # Enable JavaScript extensions
    useShinyjs(),
    
    # Define custom JS functions to hide/show the header
    extendShinyjs(
      text = "
        shinyjs.hideHeader = function() { $('.navbar').hide(); };
        shinyjs.showHeader = function() { $('.navbar').show(); };
      ",
      functions = c("hideHeader", "showHeader")
    ),
    
    # Initial header visibility (hidden by default)
    tags$style(HTML(".navbar { display: none; }")),
    
    # -------------------------------------------------------------------------
    # Tab menu items
    # -------------------------------------------------------------------------
    tabItems(
      tabItem(tabName = "homeTab",     homePageUI("home")),
      tabItem(tabName = "importTab",   importDataUI("dataImport")),
      tabItem(tabName = "ovTab",       ovPageUI("overviewModule")),
      tabItem(tabName = "micTab",      micPageUI("micModule")),
      tabItem(tabName = "abTab",       abPageUI("antibiogramModule")),
      tabItem(tabName = "mapTab",      mapPageUI("mapModule")),
      tabItem(tabName = "trendsTab",   uiOutput("tsUI")),
      tabItem(tabName = "pathogenTab", uiOutput("pathogenUI")),
      tabItem(tabName = "mdrTab",      uiOutput("mdrUI")),
      tabItem(tabName = "exploreTab",  uiOutput("exploreUI"))
    )
  )
  
  # ---------------------------------------------------------------------------
  # End of main UI
  # ---------------------------------------------------------------------------
)