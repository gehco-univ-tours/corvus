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
                      choices = c("BE" = "be",
                                  "PI" = "pi"))
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
      ),
      fluidRow(
        column(
          width = 12,
          verbatimTextOutput(ns("userinfo"))
        )
      )
      ### UI DEV TOOLS ####

      ,fluidRow(
        column(
          width = 4,
          actionButton(ns("browser"), "browser")
        ),
        column(
          width = 8,
          verbatimTextOutput(ns("printcheck"))
        )
      ) # fluidRow DEV TOOLS

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
      plot = NULL,
      userinfo = list("User information")
    )

    ### UI OUTPUT ####

    #### plot ####

    output$plot <- renderPlotly({
      r_locals$plot
    })

    #### userinfo ####
    output$userinfo <- renderPrint({
      r_locals$userinfo
    })

    ### EVENT ####

    #### Compile bttn ####
    observeEvent(input$compile_raw_data, {
      r_locals$userinfo$processing = "Compiling raw data"
        data <- compile_raw(con = db_con(),
                            station = input$station,
                            parameter = input$parameter)
        if (!is.null(data)) {
          r_locals$userinfo$processing = data
        } else {
          r_locals$userinfo$processing <- "Fail compiling raw data"
        }
    })

    #### Plot bttn ####
    observeEvent(input$plot_raw_data, {
      r_locals$data <- data_get_raw_data(con = db_con(),
                                         station = input$station,
                                         parameter = input$parameter)
      r_locals$plot <- plot_main(data = r_locals$data,
                                 y = input$station,
                                 y_title = input$parameter)
    })

  })
}

## To be copied in the UI
# mod_raw_data_ui("raw_data_1")

## To be copied in the server
# mod_raw_data_server("raw_data_1")
