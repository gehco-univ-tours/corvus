#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  ### REACTIVES ####
  r_globals <- reactiveValues(
    all_stations = data_get_stations(db_con()),
    station = NULL
  )

  # Modules
  mod_stations_server("stations_1", r_globals)
  mod_raw_server("raw_1")
  mod_edit_server("edit_1", r_globals)
  mod_valid_server("valid_1")
  mod_database_server("database_1")

}
