#' authors list
#'
#' This function returns a list of authors from the database.
#'
#' @param con PqConnection: database connection
#'
#' @importFrom DBI dbGetQuery
#'
#' @return list
#' @export
params_get_authors <- function(con){
  authors <- dbGetQuery(con, "SELECT DISTINCT name FROM author")
  authors <- as.list(authors$name)
  return(authors)
}

#' stations list
#'
#' This function returns a list of stations from the database.
#'
#' @param con PqConnection: database connection
#'
#' @importFrom stats setNames
#' @importFrom DBI dbGetQuery
#'
#' @return list
#' @export
params_get_stations <- function(con){
  stations <- dbGetQuery(con, "SELECT code, name FROM station")
  stations <- setNames(stations$code, stations$name)
  return(stations)
}

#' parameters list
#'
#' This function returns a list of parameters from the database.
#'
#' @param con PqConnection: database connection
#'
#' @importFrom DBI dbGetQuery dbDisconnect sqlInterpolate dbQuoteIdentifier SQL
#'
#' @return list
#' @export
params_get_parameters <- function(con, station_code){
  sql <- "SELECT parameter.name
    FROM sensor
    LEFT JOIN station ON sensor.station_id = station.id
    LEFT JOIN parameter ON sensor.parameter_id = parameter.id
    WHERE station.code LIKE ?station_code;"
  query <- sqlInterpolate(con, sql, station_code = station_code)
  data <- dbGetQuery(con, query)
  data <- as.list(data$name)
  dbDisconnect(con)
  return(data)
}
