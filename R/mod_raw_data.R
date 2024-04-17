#' raw_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
mod_raw_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        plotlyOutput(ns("plot"))
      ),
      fluidRow(
        column(
          width = 3,
          selectInput(inputId = ns("station"),
                      label = "Stations",
                      choices = c("BE", "PI"))
        ),
        column(
          width = 3,
          selectInput(inputId = ns("parameter"),
                      label = "Parameter",
                      choices = c("level", "turbidity"))
        ),
        column(
          width = 3,
          actionButton(inputId = ns("compile_raw_data"),
                       label = "Compile raw data")
        ),
        column(
          width = 3,
          actionButton(inputId = ns("plot_raw_data"),
                       label = "Plot raw data")
        )
      )
      ### UI DEV TOOLS ####

      # fluidRow(
      #   column(
      #     width = 4,
      #     actionButton(ns("browser"), "browser")
      #   ),
      #   column(
      #     width = 8,
      #     verbatimTextOutput(ns("printcheck"))
      #   )
      # ) # fluidRow DEV TOOLS

      ### END DEV TOOLS
    )
  )
}

#' raw_data Server Functions
#'
#' @noRd
#' @importFrom plotly renderPlotly
mod_raw_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### DEV TOOLS ####
    output$printcheck = renderPrint({
      tryCatch({
        # event_data("plotly_hover")
        print(input$data)
        print("exists")
      },
      shiny.silent.error = function(e) {
        print("doesn't exist")
      }
      )
    })
    observeEvent(input$browser, {
      browser()
    })

    ### REACTIVES ####

    r_locals <- reactiveValues(
      data = NULL,
      plot = NULL
    )

    ### UI OUTPUT ####

    output$plot <- renderPlotly({
      r_locals$plot
    })

    ### EVENT ####

    #### Apply bttn ####
    observeEvent(input$compile, {
      r_locals$data <- compile_raw(input$station, input$parameter)
    })

    #### Plot bttn ####
    observeEvent(input$plot_raw_data, {
      if (is.null(r_locals$data)){
        r_locals$data <- read.csv(system.file("lrx_data", station, "raw_data", parameter,
                                     paste0(station,"_",parameter, "_", "compile.csv"),
                                     package = "louroux"),
                                  stringsAsFactors = FALSE,
                                  sep = ";",
                                  dec = ".")
      }
      r_locals$plot <- plot_main(data = r_locals$data,
                                 y = input$parameter,
                                 y_title = paste0(input$station , " ", input$parameter))
    })

  })
}

## To be copied in the UI
# mod_raw_data_ui("raw_data_1")

## To be copied in the server
# mod_raw_data_server("raw_data_1")
