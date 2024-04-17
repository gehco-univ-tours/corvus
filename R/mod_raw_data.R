#' raw_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_raw_data_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' raw_data Server Functions
#'
#' @noRd 
mod_raw_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_raw_data_ui("raw_data_1")
    
## To be copied in the server
# mod_raw_data_server("raw_data_1")
