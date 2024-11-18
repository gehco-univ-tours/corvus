#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  ### REACTIVES ####

  # Modules
  mod_sites_server("sites_1")
  mod_raw_server("raw_1")
  mod_edit_server("edit_1")
  mod_valid_server("valid_1")

}
