#' UI for the home tab module.
#'
#' @param id  Module ID.
#' @return    Module UI.
homePageUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML(
        "
        .homeContainer {
          width: calc(100vw - 229px);
          height: 100vh;
          margin: -15px;
          background-color: #fff;
          overflow: hidden;
          position: relative;
        }

        .homeHeader {
          width: 100%;
          height: 50%;
          position: relative;
          top: 0;
          left: 0;
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          background-image: url('img/homeHeaderImg.jpg');
          background-size: cover;
          background-repeat: no-repeat;
          background-position: center;
          border-top-left-radius: 0;
          border-top-right-radius: 0;
          border-bottom-left-radius: 50% 20%;
          border-bottom-right-radius: 50% 20%;
        }

        .homeButtons {
          font-size: 30px;
          border-radius: 1000px;
          height: 7vw;
          width: 7vw;
          margin: 10px;
          color: #44CDC4;
          border-width: 5px;
          border-color: #44CDC4;
          position: relative;
          overflow: hidden;
        }

        .homeButtons::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background-color: rgba(68, 205, 196, 0.5); /* Blue overlay with alpha */
          z-index: 1;
        }

        .homeButtons .fa {
          position: relative;
          z-index: 2;
        }
      "
      ))
    ),

    div(
      class = "homeContainer",
      div(
        class = "homeHeader",
        img(src = "img/logoLight.png", width = "300px"),
        h3(
          "Liberate your data into actionable insights.",
          style = "color: white; text-align: center; margin-top: 20px;"
        ),
        fluidRow(
          actionButton(ns("howTo"), "How-to guide", class = "clearButton"),
          actionButton(ns("about"), "About", class = "clearButton")
        )
      ),
      div(
        style = "margin-top: 20px; text-align: center;",
        actionButton(
          ns("getStarted"),
          "Get Started",
          class = "submitButton",
          style = "font-size: 24px"
        ),
        hr(style = "width: 500px; border-color: #34435a"),
        br(),
        h3("What's inside?"),
        actionButton(
          ns("antibiogram"),
          "Antibiograms",
          class = "homeButtons",
          style = "background-image: url('img/antibiogram.jpg');"
        ),
        actionButton(
          ns("map"),
          "Maps",
          class = "homeButtons",
          style = "background-image: url('img/map.jpg');"
        ),
        actionButton(
          ns("trends"),
          "Trends",
          class = "homeButtons",
          style = "background-image: url('img/trends.jpg');"
        ),
        actionButton(
          ns("pathogens"),
          "MicroGuide",
          class = "homeButtons",
          style = "background-image: url('img/homeHeaderImg.jpg');"
        ),
        actionButton(
          ns("mdr"),
          "MDR",
          class = "homeButtons",
          style = "background-image: url('img/mdr.jpg');"
        ),
        actionButton(
          ns("explore"),
          "Data Summary",
          class = "homeButtons",
          style = "background-image: url('img/summary.jpg');"
        )
      )
    )
  )
}

#' Server logic for the home tab module.
#'
#' @param id  The ID of the module.
#' @return    None.
homePageServer <- function(id) {
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
    # ------------------------------------------------------------------------------
    # Render UI
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Utility functions
    # ------------------------------------------------------------------------------

    #' Show the screenshot modal
    #'
    #' @param image Image path
    #' @param text  Optional text to display above the image
    #' @return      None
    showScreenshotModal <- function(image, text = NULL) {
      showModal(modalDialog(
        easyClose = TRUE,
        size = "l",
        title = div(
          style = "text-align: center;",
          tags$img(
            src = "img/logoDark.png",
            height = "100px",
            style = "vertical-align: middle;"
          )
        ),
        if (!is.null(text)) h4(text),
        img(src = image, width = "100%")
      ))
    }

    # ------------------------------------------------------------------------------
    # Observes
    # ------------------------------------------------------------------------------

    observeEvent(input$howTo, {
      showModal(modalDialog(
        easyClose = TRUE,
        size = "l",
        title = div(
          style = "text-align: center;",
          tags$img(
            src = "img/logoDark.png",
            height = "100px",
            style = "vertical-align: middle;"
          )
        ),
        includeMarkdown("Documentation/quick_start_guide.md")
      ))
    })

    observeEvent(input$about, {
      showModal(modalDialog(
        easyClose = T,
        size = "l",
        title = div(
          style = "text-align: center;",
          tags$img(
            src = "img/logoDark.png",
            height = "100px",
            style = "vertical-align: middle;"
          )
        ),
        includeMarkdown("Documentation/about.md")
      ))
    })

    observeEvent(input$getStarted, {
      showModal(modalDialog(
        easyClose = TRUE,
        size = "l",
        title = div(
          style = "text-align: center;",
          tags$img(
            src = "img/logoDark.png",
            height = "100px",
            style = "vertical-align: middle;"
          )
        ),
        h5("To get started, click on 'Import' on the left-hand sidebar."),
        div(
          style = "text-align: center;",
          img(src = "img/importScreenshot.png", style = "max-width: 20%; height: auto;")
        )
      ))
    })

    observeEvent(input$antibiogram, {
      showScreenshotModal(
        "img/antibiogramScreenshot.png",
        text = "Generate cumulative antibiograms using customizable filters. Tables display susceptibility percentages for each antimicrobial and organism combination, with support for stratification and export."
      )
    })

    observeEvent(input$map, {
      showScreenshotModal(
        "img/mapScreenshot.png",
        text = "Explore the geographic distribution of resistance patterns using choropleth maps. Resistance can be mapped at the regional or subregional level, depending on the data provided."
      )
    })

    observeEvent(input$trends, {
      showScreenshotModal(
        "img/tsScreenshot.png",
        text = "Plot resistance trends over time for one or more antimicrobial agents. Supports smoothing options (rolling mean, LOESS) to aid in interpretation of noisy time series."
      )
    })

    observeEvent(input$pathogens, {
      showScreenshotModal(
        "img/microguideScreenshot.png",
        text = "View clinically relevant summaries for microorganisms in your dataset, including gram stain, oxygen tolerance, treatment considerations, and epidemiology. Information is sourced from Firstline and tailored to veterinary contexts."
      )
    })

    observeEvent(input$mdr, {
      showScreenshotModal(
        "img/mdrScreenshot.png",
        text = "Examine pairwise co-resistance relationships using correlation heatmaps. Helps identify resistance clustering and guide further investigation into shared resistance mechanisms."
      )
    })

    observeEvent(input$explore, {
      showScreenshotModal(
        "img/exploreScreenshot.png",
        text = "Create customizable summary tables based on user-defined filters and stratifications. Ideal for internal summaries or external reporting when raw data cannot be shared."
      )
    })

    # ------------------------------------------------------------------------------
    # Download Handlers
    # ------------------------------------------------------------------------------
    # ------------------------------------------------------------------------------
    # Module return
    # ------------------------------------------------------------------------------
  })
}
