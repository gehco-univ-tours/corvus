#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny reactiveValues
#' @noRd
app_server <- function(input, output, session) {

  ### REACTIVES ####
  r_globals <- reactiveValues(
    all_stations = db_get_all_stations(db_con()),
    station = NULL
  )
  # Modules
  mod_stations_server("stations_1", r_globals)
  mod_edit_server("edit_1", r_globals)
  mod_database_server("database_1")
}
