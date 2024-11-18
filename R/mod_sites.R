#' sites UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom leaflet leafletOutput
mod_sites_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        leafletOutput("sites_map")
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
    )
  )
}

#' sites Server Functions
#'
#' @noRd
mod_sites_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sites_ui("sites_1")

## To be copied in the server
# mod_sites_server("sites_1")
