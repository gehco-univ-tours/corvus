#' authors list
#'
#' This function returns a list of authors from the database.
#'
#' @param con PqConnection: database connection
#'
#' @importFrom DBI dbGetQuery
#' @importFrom stats setNames
#'
#' @return list
#' @export
params_get_authors <- function(con){
  authors <- dbGetQuery(con, "SELECT id, name FROM author")
  authors <- setNames(authors$id, authors$name)
  dbDisconnect(con)
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
  stations <- dbGetQuery(con, "SELECT id, name FROM station")
  stations <- setNames(stations$id, stations$name)
  dbDisconnect(con)
  return(stations)
}

#' parameters list
#'
#' This function returns a list of parameters from the database.
#'
#' @param con PqConnection: database connection
#' @param station_id integer: station id
#'
#' @importFrom DBI dbGetQuery dbDisconnect sqlInterpolate dbQuoteIdentifier SQL
#'
#' @return list
#' @export
params_get_parameters <- function(con, station_id){
  sql <- "SELECT parameter.id, parameter.name
    FROM parameter
    JOIN sensor ON sensor.parameter_id = parameter.id
    WHERE station_id = ?station_id;"
  query <- sqlInterpolate(con, sql, station_id = station_id)
  data <- dbGetQuery(con, query)
  parameters <- setNames(data$id, data$name)
  dbDisconnect(con)
  return(parameters)
}

#' Sensor id
#'
#' This function returns the sensor id based on the station and parameter id.
#'
#' @param con PqConnection: database connection
#' @param station_id integer: station id
#' @param parameter_id integer: parameter id
#'
#' @importFrom stats setNames
#' @importFrom DBI dbGetQuery
#'
#' @return list
#' @export
params_get_sensor_id <- function(con, station_id, parameter_id){
  sql <- "SELECT sensor.id
    FROM sensor
    WHERE station_id = ?station_id AND parameter_id = ?parameter_id;"
  query <- sqlInterpolate(con, sql, station_id = station_id, parameter_id = parameter_id)
  sensor_id <- dbGetQuery(con, query)$id
  dbDisconnect(con)
  return(sensor_id)
}

#' Correction type list
#'
#' This function returns a list of the type of correction available from the database.
#'
#' @param con PqConnection: database connection
#'
#' @importFrom stats setNames
#' @importFrom DBI dbGetQuery
#'
#' @return list
#' @export
params_get_correction_type <- function(con){
  corrections <- dbGetQuery(con, "SELECT id, name FROM correction_type")
  corrections <- setNames(corrections$id, corrections$name)
  dbDisconnect(con)
  return(corrections)
}

#' Get main interval sensor measurement.
#'
#' @param con PqConnection: database connection
#' @param sensor_id integer: sensor id
#'
#' @importFrom stats setNames
#' @importFrom DBI dbGetQuery
#'
#' @return list
#' @export
params_get_interval <- function(con, sensor_id){
  sql <- "WITH intervals AS (
              SELECT
                  timestamp - LAG(timestamp) OVER (ORDER BY timestamp) AS interval
              FROM
                  measurement
          	WHERE sensor_id = ?sensor_id
          )
          SELECT
              interval,
              COUNT(*) AS count
          FROM
              intervals
          GROUP BY
              interval
          ORDER BY
              count DESC
          LIMIT 1;"
  query <- sqlInterpolate(con, sql, sensor_id = sensor_id)
  intervals <- dbGetQuery(con, query)$interval
  dbDisconnect(con)
  return(intervals)
}
