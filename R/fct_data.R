#' Get raw data from database
#'
#' @param con PqConnection: database connection
#' @param station character: station name
#' @param parameter character: parameter name
#'
#' @return data.frame
#' @export
#'
#' @importFrom glue glue
#' @importFrom DBI dbGetQuery dbDisconnect
#'
#' @examples
#' con <- db_con()
#' data_get_raw_data(con, "BE", "level")
data_get_raw_data <- function(con, station, parameter){
  query <-  glue::glue("SELECT date_time, {tolower(station)} FROM {parameter}")
  data <- DBI::dbGetQuery(con, query)
  dbDisconnect(con)
  return(data)
}
