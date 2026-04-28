#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny reactiveValues
#' @importFrom DBI dbDisconnect
#' @noRd
app_server <- function(input, output, session) {

  con <- db_con()

  ### REACTIVES ####
  r_globals <- reactiveValues(
    all_stations = NULL,
    station = NULL
  )

  r_globals$con <- db_con()

  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
  })

  r_globals$all_stations <- db_get_all_stations(con)

  # Modules
  mod_stations_server("stations_1", con = con, r_globals)
  mod_edit_server("edit_1", con = con, r_globals)
  mod_database_server("database_1", con = con, r_globals)
}
