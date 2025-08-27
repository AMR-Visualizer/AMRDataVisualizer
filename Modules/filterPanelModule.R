filterPanelUI <- function(id) {
  ns <- NS(id)
  bsCollapse(
    id = "collapsePanel",
    open = "Filters",
    multiple = T,
    bsCollapsePanel(
      title = HTML("Filters <span class='glyphicon glyphicon-chevron-down' data-toggle='collapse-icon' 
            style='float: right; color: #aaa;'></span>"),
      div(
        style = "display: flex; justify-content: center; align-items: center; position: relative;",
        actionButton(
          ns("editFilters"),
          label = "",
          icon = icon("pencil-alt", lib = "font-awesome"),
          class = "btn btn-default",
          style = "position: absolute; right: 10px;cbackground-color: #f0f0f0; color: grey; border: none;"
        ),
        style = "margin-top: 2px;"
      ),
      uiOutput(ns("filters")),
      actionButton(ns("applyFilter"), "Apply", class = "submitButton"),
      value = "Filters"
    )
  )
}

#' Filter module server.
#' 
#' @param id                Module id.
#' @param reactiveData      Df of data to filter (can be reactive or static).
#' @param default_filters   Character vector of column names to apply default filters on module load.
#' @param auto_populate     Named list of columns to auto-populate.
#' @return                  List containing:
#'                         - filteredData: Reactive filtered df.
#'                        - activeFilters: Reactive selected filters.
filterPanelServer <- function(id, data, default_filters, auto_populate = list()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------------------
    # Sub-modules
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Module variables
    # ------------------------------------------------------------------------------

    #' Filters we want to target with specific UI elements
    specificFilterMapping <- list(
      "Microorganism" = "microorganismFilterElement",
      "Date" = "dateFilterElement",
      "Resistant to" = "resistanceFilterElement",
      "WHO AWaRe Class" = "awareFilterElement",
      "Suppress Antimicrobials" = "suppressABFilterElement"
    )


    # ------------------------------------------------------------------------------
    # Reactives
    # ------------------------------------------------------------------------------

    # Handle both static and reactive data inputs
    reactiveData <- reactive({
      req(data)

      if (inherits(data, "reactive")) {
        req(data())
        return(data())
      }
      data
    })

    selected_filters <- reactiveValues(columns = default_filters)

    filteredData <- reactiveVal(NULL)

    # Visible filters to show in the UI and use for filtering
    visibleFilters <- reactive({
      fixed_order <- c("Microorganism", "Antimicrobial", "Source", "Species", "Date")
      fixed_order <- fixed_order[fixed_order %in% selected_filters$columns]

      other_filters <- setdiff(selected_filters$columns, fixed_order)
      other_filters <- sort(other_filters)

      ordered_filters <- c(fixed_order, other_filters)
      ordered_filters
    })

    # ------------------------------------------------------------------------------
    # Render UI
    # ------------------------------------------------------------------------------

    #' Filter UI elements.
    #' Dynamically generated based on the columns present in the data and the selected filters.
    #' If a column has a specific UI element defined in `specificFilterMapping`, that element is used.
    #' Otherwise, a generic multi-select input is created.
    output$filters <- renderUI({
      return(tagList(
        lapply(visibleFilters(), function(col) {
          if (col %in% names(specificFilterMapping)) {
            return(uiOutput(ns(specificFilterMapping[[col]])))
          }

          selectizeInput(
            ns(paste0(col, "Filter")),
            label = col,
            choices = sort(unique(reactiveData()[[col]]), na.last = NA),
            selected = getDefaultValue(col),
            multiple = TRUE
          )
        })
      ))
    })

    # Date input filter element.
    output$dateFilterElement <- renderUI({
      req("Date" %in% visibleFilters())
      d <- reactiveData()

      tagList(
        dateRangeInput(
          ns("timeFilter"),
          "Timeframe",
          min = min(d$Date, na.rm = TRUE),
          max = max(d$Date, na.rm = TRUE),
          start = min(d$Date, na.rm = TRUE),
          end = max(d$Date, na.rm = TRUE)
        ),
        div(
          actionButton(ns("last3Months"), "3 mo", class = "quickDateButton"),
          actionButton(ns("last6Months"), "6 mo", class = "quickDateButton"),
          actionButton(ns("pastYear"), "1 yr", class = "quickDateButton"),
          actionButton(ns("allData"), "All", class = "quickDateButton"),
          class = "quickDateButtonDiv"
        )
      )
    })

    # Resistance input filter element.
    output$resistanceFilterElement <- renderUI({
      req("Resistant to" %in% visibleFilters())
      selectizeInput(
        ns("resistanceFilter"),
        label = "Resistant to",
        choices = sort(unique(reactiveData()$Antimicrobial), na.last = NA),
        selected = NULL,
        multiple = TRUE
      )
    })

    # WHO AWaRe input filter element.
    output$awareFilterElement <- renderUI({
      req("WHO AWaRe Class" %in% visibleFilters())
      selectizeInput(
        ns("awareFilter"),
        label = "WHO AWaRe Class",
        choices = c("Access", "Reserve", "Watch"),
        selected = NULL,
        multiple = TRUE
      )
    })

    # Suppess AB input filter element.
    output$suppressABFilterElement <- renderUI({
      req("Suppress Antimicrobials" %in% visibleFilters())
      selectizeInput(
        ns("removeAB"),
        label = "Suppress Antimicrobials",
        choices = sort(unique(reactiveData()$Antimicrobial), na.last = NA),
        selected = NULL,
        multiple = TRUE
      )
    })

    # Microorganism input filter element.
    output$microorganismFilterElement <- renderUI({
      req("Microorganism" %in% visibleFilters())

      tagList(
        selectizeInput(
          ns("MicroorganismFilter"),
          label = "Microorganism",
          choices = sort(unique(reactiveData()$Microorganism), na.last = NA),
          selected = getDefaultValue("Microorganism"),
          multiple = TRUE
        )
      )
    })


    # ------------------------------------------------------------------------------
    # Utility functions
    # ------------------------------------------------------------------------------

    #' Get the default value for a filter input.
    #'
    #' Will return the first value of the column if:
    #' - The column name appears in the `auto_populate` list.
    #' - There is only one unique value in the data for that column.
    #'
    #' @param colName The name of the column to check.
    #' @return        The default value to use for the filter input, or NULL if no default is applicable.
    getDefaultValue <- function(colName) {
      d <- reactiveData()
      if (isTRUE(auto_populate[[colName]]) || length(unique(d[[colName]])) == 1) {
        return(names(sort(table(d[[colName]]), decreasing = TRUE))[1])
      }
      NULL
    }

    applyDefaultFilters <- function(targetData, filters, auto_populate) {
      filtered <- targetData
      for (col in filters) {
        if (col == "Date") {
          filtered <- filtered[filtered$Date >= min(targetData$Date, na.rm = TRUE) &
                                 filtered$Date <= max(targetData$Date, na.rm = TRUE), ]
        } else if (isTRUE(auto_populate[[col]])) {
          most_common_value <- names(sort(table(targetData[[col]]), decreasing = TRUE))[1]
          filtered <- filtered[filtered[[col]] == most_common_value, ]
        }
      }
      return(filtered)
    }

    # ------------------------------------------------------------------------------
    # Observes
    # ------------------------------------------------------------------------------

    # Open a modal to select which filters to show
    observeEvent(input$editFilters, {
      showModal(modalDialog(
        title = "Select Filters",
        checkboxGroupInput(ns("selectedFilters"), "Choose Filters", 
                           choices = sort(c(colnames(reactiveData()), 
                                            "Suppress Antimicrobials",
                                            "Resistant to", 
                                            "WHO AWaRe Class")), 
                           selected = selected_filters$columns),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("applyModalFilters"), "Apply")
        )
      ))
    })
    
    # Apply the selected filters from the modal and close the modal
    observeEvent(input$applyModalFilters, {
      selected_filters$columns <- input$selectedFilters
      removeModal()
    })

    # When the data changes, re-apply default filters
    observe({
      req(reactiveData())
      filteredData(applyDefaultFilters(reactiveData(), default_filters, auto_populate))
    })
    
    # Want this to update when "Apply" is clicked or when the data changes (meaning custom breakpoints were applied)
    observeEvent(input$applyFilter, {
      originalData <- reactiveData()
      filtered <- originalData
      
      if (!is.null(input$removeAB) && length(input$removeAB) > 0) {
        filtered <- originalData %>%
          filter(!Antimicrobial %in% input$removeAB)
      }
      
      if (!is.null(input$resistanceFilter) && input$resistanceFilter != "") {
        resistant_rows <- originalData %>%
          filter(Antimicrobial == input$resistanceFilter & Interpretation == "R")
        
        resistant_ids <- resistant_rows %>%
          select(ID, Date, Region, Subregion, Species, Source, Microorganism) %>%
          distinct()
        
        filtered <- originalData %>%
          inner_join(resistant_ids, by = colnames(resistant_ids))
      }
      
      if (!is.null(input$awareFilter) && length(input$awareFilter) > 0) {
        relevantAntimicrobials <- awareList %>%
          filter(awareGroup %in% input$awareFilter) %>%
          pull(Antimicrobial)
        
        filtered <- originalData %>%
          filter(Antimicrobial %in% relevantAntimicrobials)
      }
      
      for (col in selected_filters$columns) {

        if (col == "Date" && !is.null(input$timeFilter)) {
          filtered <- filtered[filtered$Date >= input$timeFilter[1] & filtered$Date <= input$timeFilter[2], ]
          
        } else if (!is.null(input[[paste0(col, "Filter")]])) {
          
          selected_vals <- input[[paste0(col, "Filter")]]
          
          if (length(selected_vals) > 0) {
            
            filtered <- filtered[filtered[[col]] %in% selected_vals, ]
            
          }
        }
      }
      
      if (!is.null(input$resistanceFilter) && input$resistanceFilter != "") {
        conflicting_rows <- filtered %>%
          filter(Antimicrobial == input$resistanceFilter & Interpretation == "S")
        
        if (nrow(conflicting_rows) > 0) {
          
          conflicting_bacteria <- conflicting_rows %>%
            group_by(Microorganism) %>%
            summarise(count = n()) %>%
            arrange(desc(count)) %>%
            pull(Microorganism)
          
          bacteria_list <- paste("<ul>", 
                                 paste("<li>", conflicting_bacteria, "</li>", collapse = ""),
                                 "</ul>")
          
          shinyalert(
            title = "Warning: Conflicting Data",
            text = paste("There appear to be conflicting duplicates in your data for the selected antimicrobial, with indistiguishable isolates marked as susceptible ('S') and others as resistant ('R'). We recommend reviewing your data to identify and resolve these discrepancies manually.<br> The following bacteria have conflicting interpretations:<br>", bacteria_list),
            type = "warning",
            html = TRUE,
            closeOnClickOutside = TRUE,
            className = 'alert'
          )
        }
      }
      
      filteredData(filtered)
    })
    
    
    observeEvent(input$last3Months, {
      d <- reactiveData()
      updateDateRangeInput(
        session,
        "timeFilter",
        min = min(d$Date),
        max = max(d$Date),
        start = max(d$Date) %m-% months(3),
        end = max(d$Date)
      )
    })
    
    observeEvent(input$last6Months, {
      d <- reactiveData()
      updateDateRangeInput(
        session,
        "timeFilter",
        min = min(d$Date),
        max = max(d$Date),
        start = max(d$Date) %m-% months(6),
        end = max(d$Date)
      )
    })
    
    observeEvent(input$pastYear, {
      d <- reactiveData()
      updateDateRangeInput(
        session,
        "timeFilter",
        min = min(d$Date),
        max = max(d$Date),
        start = max(d$Date) %m-% years(1),
        end = max(d$Date)
      )
    })
    
    observeEvent(input$allData, {
      d <- reactiveData()
      updateDateRangeInput(
        session,
        "timeFilter",
        min = min(d$Date),
        max = max(d$Date),
        start = min(d$Date),
        end = max(d$Date)
      )
    })
    
    #---
    
    return(list(
      filteredData = filteredData,
      activeFilters = reactive({
        filters <- list()
        originalData <- reactiveData()
        
        if (!is.null(input$removeAB) && length(input$removeAB) > 0) {
          filters[["Suppress Antimicrobials"]] <- input$removeAB
        }
        
        if (!is.null(input$resistanceFilter) && length(input$resistanceFilter) > 0) {
          filters[["Resistant to"]] <- input$resistanceFilter
        }
        
        if (!is.null(input$awareFilter) && length(input$awareFilter) > 0) {
          filters[["WHO AWaRe Class"]] <- input$awareFilter
        }
        
        for (col in selected_filters$columns) {
          
          if (col == "Date") {
            if (!is.null(input$timeFilter)) {
              filters[["Date"]] <- input$timeFilter
            } else {
              filters[["Date"]] <- c(min(originalData$Date, na.rm = TRUE), max(originalData$Date, na.rm = TRUE))
            }
            
          } else {
            val <- input[[paste0(col, "Filter")]]
            
            if (is.null(val) || length(val) == 0) {
              if (isTRUE(auto_populate[[col]])) {
                val <- names(sort(table(originalData[[col]]), decreasing = TRUE))[1]
              }
            }
            
            if (!is.null(val) && length(val) > 0) {
              filters[[col]] <- val
            }
          }
        }
        
        
        return(filters)
      })
    ))
    
    
    
    #---
  })
}




