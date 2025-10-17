micPageUI <- function(id, data) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Main Content ------------------------------------------------------------

      column(9, uiOutput(ns("content"))),

      # Filters -----------------------------------------------------------------

      column(
        3,
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
          br(),
          uiOutput(ns("customBreakpointsUI")),
          class = "contentWell"
        )
      )
    ),
    fluidRow(
      column(
        12,
        bsCollapse(
          id = ns("clinicalBpTableContainer"),
          open = NULL,
          bsCollapsePanel(
            title = actionButton(
              ns("clinicalBpTableToggle"),
              label = HTML(
                "AMR Clinical Breakpoints Lookup Table <span class='glyphicon glyphicon-chevron-down' data-toggle='collapse-icon' 
          style='float: right; color: #aaa;'></span>"
              ),
              class = "collapse-btn"
            ),
            value = "bpTable",
            DT::dataTableOutput(ns("clinicalBpTable"), width = "100%")
          )
        )
      )
    )
  )
}

micPageServer <- function(id, reactiveData, processedGuideline) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------------------
    # Sub-modules
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Module variables
    # ------------------------------------------------------------------------------

    abMapping <- data.frame(ab = unique(AMR::clinical_breakpoints$ab)) %>%
      mutate(ab_name = AMR::ab_name(ab))

    moMapping <- data.frame(mo = unique(AMR::clinical_breakpoints$mo)) %>%
      mutate(mo_name = AMR::mo_name(mo))

    #' `AMR::clinical_breakpoints` have the `ab` and `mo` columns as abbreviations.
    #' Add the actual names for easier searching in the table.
    fullClinicalBreakpoints <- AMR::clinical_breakpoints %>%
      left_join(abMapping, by = "ab") %>%
      left_join(moMapping, by = "mo")

    # A custom guideline name to use in the clinical breakpoints df.
    customBreakpointName <- getCustomGuidelineName()

    # The expected format of an empty clinical breakpoints df.
    emptyCustomBp <- AMR::clinical_breakpoints[0, ] %>%
      #' These columns will match the values found in the filtered data
      #' rather than the clinical breakpoints.
      #' Saves us from mutating over the whole filtered data df to format
      #' the values as expected for the custom breakpoints.
      mutate(
        Species = character(0),
        Source = character(0),
        Microorganism = character(0),
        Antimicrobial = character(0)
      )

    # ------------------------------------------------------------------------------
    # Reactives
    # ------------------------------------------------------------------------------

    #' Keep track of whether the clinical breakpoints table is open or closed.
    bpTableOpen <- reactiveVal(FALSE)

    #' A df to hold all the custom breakpoints created by the user.
    #' This is returned as used in the rest of the app.
    #'
    #' Starts off with just the expected columns but no rows.
    allCustomBreakpoints <- reactiveVal(emptyCustomBp)

    # Input values for custom breakpoints
    breakpointInputs <- reactive({
      # req(input$addCustomBreakpoints, input$deleteCustomBreakpoints)
      req(input$moFilter, input$abFilter, input$typeFilter)
      bp <- list(s = NULL, r = NULL)
      if (!is.null(input$customSBreakpoint) && !is.na(input$customSBreakpoint)) {
        bp$s <- as.numeric(input$customSBreakpoint)
      }
      if (!is.null(input$customRBreakpoint) && !is.na(input$customRBreakpoint)) {
        bp$r <- as.numeric(input$customRBreakpoint)
      }
      bp
    }) %>%
      bindEvent(
        input$addCustomBreakpoints,
        input$deleteCustomBreakpoints,
        input$moFilter,
        input$abFilter,
        input$typeFilter
      )

    # Boolean. Require that both breakpoint inputs are set to create custom breakpoints
    canCreateCustomBreakpoints <- reactive({
      all(!is.null(c(input$customSBreakpoint, input$customRBreakpoint))) &&
        all(!is.na(c(input$customSBreakpoint, input$customRBreakpoint)))
    })

    # Boolean. Are all the necessary inputs rendered and not null?
    allInputsReady <- reactive({
      inputs <- c(input$moFilter, input$abFilter, input$typeFilter, selectedSpecies())
      all(!is.null(inputs)) && length(inputs) == 4
    })

    #' The default guideline for the guideline selected by the user in data processing
    #' taking into account the selected microorganism, antimicrobial, type, and species.
    defaultGuideline <- reactive({
      bp <- AMR::clinical_breakpoints %>%
        filter(
          guideline == processedGuideline(),
          mo == as.mo(input$moFilter),
          ab == as.ab(input$abFilter),
          uti == (input$typeFilter == "Urinary"),
          host == tolower(selectedSpecies())
        )

      if ("MIC" %in% bp$method) {
        bp <- bp %>%
          filter(method == "MIC")
      }

      bp <- bp %>%
        slice(1)

      if (nrow(bp) == 0) {
        return(AMR::clinical_breakpoints[0, ])
      }
      bp
    }) %>%
      bindEvent(input$moFilter, input$abFilter, input$typeFilter, selectedSpecies())

    #' The guideline to use in the table generation.
    #' If custom breakpoints are set, use those.
    selectedBreakpoints <- reactive({
      req(allInputsReady())
      sel <- customRefData() %>%
        filter(
          # guideline %in% c(processedGuideline(), customBreakpointName),
          mo  == as.mo(input$moFilter),
          ab  == as.ab(input$abFilter),
          uti == (input$typeFilter == "Urinary"),
          host == tolower(selectedSpecies())
        )
      # If no custom breakpoints are set, use the default ones
      if (customBreakpointName %in% sel$guideline) {
        sel <- dplyr::filter(sel, guideline == customBreakpointName)
      } else {
        sel <- dplyr::filter(sel, guideline == processedGuideline())
      }
      dplyr::slice(sel, 1)
    })

    #' Reference data for the table generation.
    #' If custom breakpoints are set, append them to the clinical breakpoints.
    customRefData <- reactive({
      allCustomBreakpoints() %>%
        select(all_of(colnames(AMR::clinical_breakpoints))) %>%
        bind_rows(AMR::clinical_breakpoints)
    })

    # The selected species from the filter.
    selectedSpecies <- reactive({
      species <- input$speciesFilter
      if (is.null(species) && !is.null(reactiveData()$Species)) {
        species <- reactiveData()$Species[[1]]
      }
      species
    })

    appliedBpForDisplay <- reactive({
      req(input$moFilter, input$abFilter, input$typeFilter, selectedSpecies())
      uti  <- identical(input$typeFilter, "Urinary")
      host <- tolower(selectedSpecies())
      
      bp <- selectedBreakpoints()
      s_user <- if (nrow(bp) > 0) suppressWarnings(as.numeric(bp$breakpoint_S[1])) else NA_real_
      r_user <- if (nrow(bp) > 0) suppressWarnings(as.numeric(bp$breakpoint_R[1])) else NA_real_
      
      guide <- if (nrow(bp) > 0 && !is.na(bp$guideline[1]) && nzchar(bp$guideline[1])) {
        as.character(bp$guideline[1])
      } else {
        processedGuideline()
      }
      
      if (is.na(s_user) || is.na(r_user)) {
        AMR::sir_interpretation_history(clean = TRUE)
        dummy <- AMR::as.mic(2)
        try({
          invisible(AMR::as.sir(
            dummy,
            mo = AMR::as.mo(input$moFilter),
            ab = AMR::as.ab(input$abFilter),
            guideline = guide,
            reference_data = customRefData(),
            host = host,
            uti = uti
          ))
        }, silent = TRUE)
        
        hist <- AMR::sir_interpretation_history(clean = FALSE)
        if (nrow(hist) > 0) {
          hist <- tidyr::separate(hist, breakpoint_S_R, into = c("bp_s", "bp_r"),
                                  sep = "-", fill = "right", remove = FALSE)
          s_app <- suppressWarnings(as.numeric(hist$bp_s[1]))
          r_app <- suppressWarnings(as.numeric(hist$bp_r[1]))
          guide <- if (!is.na(hist$guideline[1]) && nzchar(hist$guideline[1])) hist$guideline[1] else guide
          applied_mo <- hist$mo[1]
        } else {
          s_app <- NA_real_; r_app <- NA_real_; applied_mo <- NA_character_
        }
      } else {
        s_app <- s_user; r_app <- r_user; applied_mo <- AMR::as.mo(input$moFilter)
      }
      
      list(s = s_app, r = r_app, guideline = guide, applied_mo = applied_mo)
    })
    

    # Table data based on filters.
    table_result <- reactive({
      req(input$moFilter, input$abFilter, input$typeFilter, input$groupingVar)
      sp <- selectedSpecies()
      ap <- appliedBpForDisplay()

      create_mic_frequency_tables(
        data = reactiveData(),
        group_by_var = input$groupingVar,
        ab = input$abFilter,
        mo = input$moFilter,
        type = input$typeFilter,
        species = sp,
        guideline = ap$guideline,
        s_bp = ap$s,
        r_bp = ap$r,
        reference_data = customRefData(),
        use_single_bp_as_both = TRUE
      )
    })

    # Boolean. Whether to show the error panel or the success panel.
    showErrorPanel <- reactive({
      nrow(reactiveData()) < 1 || is.null(table_result())
    })

    #' The cleaned data with custom breakpoints applied.
    #' Return this data to be used in the rest of the app.
    dataWithCustomBreakpoints <- reactive({
      data <- reactiveData()
      if (is.null(data)) {
        return(NULL)
      }

      data$Guideline <- processedGuideline()

      # If no custom breakpoints are set, return the data as is
      if (is.null(allCustomBreakpoints()) || nrow(allCustomBreakpoints()) == 0) {
        return(data)
      }

      # Merge custom breakpoints into the data

      sourceValues <- unique(data$Source)
      utiMatches <- findUtiMatch(sourceValues)

      for (i in seq_len(nrow(allCustomBreakpoints()))) {
        customBp <- allCustomBreakpoints()[i, ]
        customGuideline <- customBp %>%
          select(all_of(colnames(AMR::clinical_breakpoints)))

        if (customBp$uti) {
          utiValues <- sourceValues[utiMatches]
          isUti <- TRUE
        } else {
          utiValues <- sourceValues[!utiMatches]
          isUti <- FALSE
        }

        # Data which matches the custom breakpoint criteria
        matchingData <- data %>%
          filter(
            Species == customBp$Species,
            Antimicrobial == customBp$Antimicrobial,
            Microorganism == customBp$Microorganism,
            Source %in% utiValues
          )

        # Re-calculate MIC interpretations based on custom breakpoints
        newInterpretations <- data.frame(
          MIC = unique(matchingData$MIC)
        )
        
        newInterpretations$Interpretation <- as.sir(
          x = AMR::as.mic(newInterpretations$MIC),
          mo = customGuideline$mo,
          ab = customGuideline$ab,
          guideline = customGuideline$guideline,
          reference_data = customGuideline,
          host = customGuideline$host,
          uti = customGuideline$uti,
          method = "MIC",
          breakpoint_type = customGuideline$type
        )

        matchingData <- matchingData %>%
          select(-Interpretation, -Guideline) %>%
          left_join(newInterpretations, by = "MIC")
        matchingData$Guideline <- customBp$guideline

        data <- data %>%
          filter(!InternalID %in% matchingData$InternalID) %>%
          bind_rows(matchingData)
      }

      data %>%
        arrange(InternalID)
    })

    #' The clinical breakpoints filtered based on the selected antimicrobial.
    #' Use a `bandCache` to avoid re-filtering unnecessarily.
    ab_clinical_breakpoints <- reactive({
      data <- fullClinicalBreakpoints

      if (!is.null(input$abFilter)) {
        data <- data %>%
          filter(ab == as.ab(input$abFilter))
      }
      data %>%
        select(-ab, -mo) %>%
        rename(ab = ab_name, mo = mo_name) %>%
        select(all_of(colnames(AMR::clinical_breakpoints))) # Get the original column order
    }) %>%
      bindCache(input$abFilter)

    # ------------------------------------------------------------------------------
    # Render UI
    # ------------------------------------------------------------------------------

    output$filters <- renderUI({
      data <- reactiveData()
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

        if (!is.null(data$Species) && length(unique(na.omit(data$Species))) > 1) {
          selectizeInput(
            ns("speciesFilter"),
            label = "Species",
            choices = sort(unique(data$Species), na.last = NA),
            selected = names(sort(table(data$Species), decreasing = TRUE))[1],
            multiple = FALSE
          )
        }
      )
    })

    # Show either the error panel or the success panel based on whether data is available
    output$content <- renderUI({
      req(reactiveData(), input$moFilter, input$abFilter, input$typeFilter, input$groupingVar)
      tagList(
        uiOutput(ns("errorPanel")),
        uiOutput(ns("successPanel"))
      )
    })

    # Only show the error panel if there is no data to show
    output$errorPanel <- renderUI({
      req(showErrorPanel())
      wellPanel(
        style = "display: flex; align-items: center; justify-content: center; max-height: 80vh;",
        div(
          style = "min-width: 1150px; min-height: 750px; display: flex; align-items: center; justify-content: center;",
          uiOutput(ns("errorHandling"))
        ),
        class = "contentWell"
      )
    })

    # Only show the success panel (actual page content) if there is data to show
    output$successPanel <- renderUI({
      req(!showErrorPanel())
      tagList(
        wellPanel(
          style = "overflow-x: scroll; overflow-y: scroll; max-height: 80vh; min-width: 500px; position: relative;",
          uiOutput(ns("breaksMessage")),
          div(
            class = "ab-table-wrapper",
            withSpinner(gt_output(ns("plot")), type = 4, color = "#44CDC4")
          ),
          class = "contentWell"
        ),
        div(
          style = "display: flex; justify-content: end;",
          downloadButton(ns("save_table"), "Save Data", class = "plotSaveButton")
        )
      )
    })

    # Table showing all clinical breakpoints for the selected antimicrobial in collapse panel
    output$clinicalBpTable <- DT::renderDataTable({
      req(bpTableOpen())
      req(ab_clinical_breakpoints())

      DT::datatable(
        ab_clinical_breakpoints(),
        rownames = FALSE,
        options = list(autoWidth = TRUE)
      )
    })
    outputOptions(output, "clinicalBpTable", suspendWhenHidden = FALSE)

    # Inputs for user to select custom breakpoints
    output$customBreakpointsUI <- renderUI({
      req(!showErrorPanel())
      req(input$moFilter, input$abFilter, input$typeFilter, input$groupingVar)

      existingBp <- allCustomBreakpoints() %>%
        filter(
          Microorganism == input$moFilter,
          Antimicrobial == input$abFilter,
          Source == input$typeFilter
        )

      if (nrow(existingBp) == 0) {
        return(getCustomBpInputs())
      }
      getCustomBpInputs(as.numeric(existingBp$breakpoint_S), as.numeric(existingBp$breakpoint_R))
    })

    # Only show the custom breakpoints message if there is a single microorganism selected
    output$breaksMessage <- renderUI({
      req(!showErrorPanel())
      ap <- appliedBpForDisplay()
      mo_input <- AMR::as.mo(input$moFilter)
      used_label <- if (
        !is.null(ap$applied_mo) &&
          length(ap$applied_mo) == 1 &&
          !is.na(ap$applied_mo) &&
          ap$applied_mo != mo_input
      ) {
        paste0(" Inferred using ", AMR::mo_name(ap$applied_mo, language = "en"), " breakpoints.")
      } else {
        ""
      }

      htmltools::div(
        htmltools::strong(getBpLabel(ap$s, ap$r, ap$guideline)),
        htmltools::tags$small(used_label)
      )
    })

    output$errorHandling <- renderUI({
      div(
        style = "display: flex; align-items: center; justify-content: center; height: 100%; flex-direction: column; text-align: center;",
        icon("disease", style = "font-size:100px; color: #44CDC4"),
        h4("Oops... looks like there isn't enough data for this plot."),
        h6("This combination of filters does not appear in your data.")
      )
    })

    output$plot <- gt::render_gt({
      req(table_result())
      table_result()
    })

    # ------------------------------------------------------------------------------
    # Utility functions
    # ------------------------------------------------------------------------------

    #' Get the UI inputs for custom breakpoints.
    #'
    #' @param sValue  Initial numeric value for the S breakpoint input (default is NULL).
    #' @param rValue  Initial numeric value for the R breakpoint input (default is NULL).
    #' @return        A `div` containing the UI for custom breakpoints.
    getCustomBpInputs <- function(sValue = NULL, rValue = NULL) {
      div(
        class = "custom-breakpoints-container",
        tags$label("Custom breakpoints", class = "shiny-input-container"),
        div(
          column(
            6,
            numericInput(
              ns("customSBreakpoint"),
              label = "S",
              value = sValue
            )
          ),
          column(
            6,
            numericInput(
              ns("customRBreakpoint"),
              label = "R",
              value = rValue
            )
          )
        ),
        div(
          actionButton(
            ns("deleteCustomBreakpoints"),
            label = "Delete",
            class = "white-btn"
          ),
          actionButton(
            ns("addCustomBreakpoints"),
            label = "Add",
            class = "blue-btn"
          )
        )
      )
    }

    #' Enable multiple inputs by their IDs.
    #'
    #' @param inputIds A vector of input IDs to enable.
    #' @return         None. This function modifies the UI state.
    enableInputs <- function(inputIds) {
      lapply(inputIds, function(id) {
        shinyjs::enable(id)
      })
    }

    #' Disable multiple inputs by their IDs.
    #'
    #' @param inputIds A vector of input IDs to disable.
    #' @return         None. This function modifies the UI state.
    disableInputs <- function(inputIds) {
      lapply(inputIds, function(id) {
        shinyjs::disable(id)
      })
    }

    # ------------------------------------------------------------------------------
    # Observes
    # ------------------------------------------------------------------------------

    #' Controls the enabling/disabling of the custom breakpoint inputs and buttons.
    #'
    #' If a custom breakpoint is already set (for the specific grouping), then disable
    #' everything expect the delete button.
    #' If no custom breakpoint is set, and the inputs are both filled, disabled the
    #' delete button.
    #' If no custom breakpoint is set, and the inputs are not both filled, disable
    #' both the add and delete buttons.
    observe({
      req(allInputsReady())
      req(input$moFilter, input$abFilter, input$typeFilter, selectedSpecies())
      # Include all these inputs so that it is triggered when anything changes
      input$addCustomBreakpoints
      input$deleteCustomBreakpoints
      input$customSBreakpoint
      input$customRBreakpoint

      req(selectedBreakpoints())
      selectedBp <- selectedBreakpoints()

      if (customBreakpointName %in% selectedBp$guideline) {
        enableInputs("deleteCustomBreakpoints")
        disableInputs(c("addCustomBreakpoints", "customSBreakpoint", "customRBreakpoint"))
        req(FALSE)
      } else {
        disableInputs("deleteCustomBreakpoints")
      }

      # Only reaches here if no custom breakpoints are set (Add has not been clicked yet)
      enableInputs(c("customSBreakpoint", "customRBreakpoint"))
      if (canCreateCustomBreakpoints()) {
        enableInputs("addCustomBreakpoints")
      } else {
        disableInputs("addCustomBreakpoints")
      }
    })

    #' Add the custom breakpoints to a df containing all custom breakpoints.
    #' This df is used throughout the app.
    observe({
      newCustomBp <- defaultGuideline()

      if (nrow(newCustomBp) == 0) {
        #' A default bp is not found in `AMR::clinical_breakpoints` that meet the criteria.
        #' Create a new row with the necessary values.
        newCustomBp <- add_row(emptyCustomBp) |>
          mutate(
            mo = as.mo(input$moFilter),
            ab = as.ab(input$abFilter),
            method = "MIC",
            type = "animal",
            host = tolower(selectedSpecies()),
            uti = input$typeFilter == "Urinary",
            is_SDD = FALSE
          )
      }

      newCustomBp <- newCustomBp |>
        mutate(
          Microorganism = input$moFilter,
          Antimicrobial = input$abFilter,
          Species = selectedSpecies(),
          Source = input$typeFilter,
          guideline = customBreakpointName,
          breakpoint_S = as.numeric(input$customSBreakpoint),
          breakpoint_R = as.numeric(input$customRBreakpoint)
        )

      newCustomBps <- allCustomBreakpoints() %>%
        rbind(newCustomBp) %>%
        distinct()
      allCustomBreakpoints(newCustomBps)
    }) %>%
      bindEvent(input$addCustomBreakpoints)

    # Delete the saved custom breakpoints
    observe({
      customBpRemoved <- allCustomBreakpoints() %>%
        filter(
          !(guideline == customBreakpointName &
            mo == as.mo(input$moFilter) &
            ab == as.ab(input$abFilter) &
            uti == (input$typeFilter == "Urinary") &
            host == tolower(selectedSpecies()))
        )
      allCustomBreakpoints(customBpRemoved)
    }) %>%
      bindEvent(input$deleteCustomBreakpoints)

    # Toggle the clinical breakpoints table visibility
    observe({
      bpTableOpen(!bpTableOpen())
    }) %>%
      bindEvent(input$clinicalBpTableToggle)

    # ------------------------------------------------------------------------------
    # Module return
    # ------------------------------------------------------------------------------

    # Return the cleaned data with custom breakpoints applied and all custom breakpoints.
    return(list(
      dataWithCustomBreakpoints = dataWithCustomBreakpoints,
      customBreakpoints = allCustomBreakpoints
    ))
  })
}
