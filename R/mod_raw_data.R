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
                      choices = c("Level", "Turbidity"))
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
mod_raw_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### DEV TOOLS ####
    output$printcheck = renderPrint({
      tryCatch({
        # event_data("plotly_hover")
        print(input$exploremap_center)
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

  })
}

## To be copied in the UI
# mod_raw_data_ui("raw_data_1")

## To be copied in the server
# mod_raw_data_server("raw_data_1")
