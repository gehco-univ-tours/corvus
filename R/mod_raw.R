#' raw UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
#' @importFrom shinybusy add_busy_bar
mod_raw_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        add_busy_bar(color = "#FF0000"),
        plotlyOutput(ns("plot"))
      )
    )

  )
}

#' raw Server Functions
#'
#' @noRd
mod_raw_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_raw_ui("raw_1")

## To be copied in the server
# mod_raw_server("raw_1")
