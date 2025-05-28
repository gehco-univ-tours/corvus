#' Postgresql database connection.
#'
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
#'
#' @return PqConnection
#' @export
db_con <- function(){
  db_con <- DBI::dbConnect(RPostgres::Postgres(),
                           host = Sys.getenv("DB_LOUROUX_PROD_HOST"),
                           port = Sys.getenv("DB_LOUROUX_PROD_PORT"),
                           dbname = Sys.getenv("DB_LOUROUX_PROD_NAME"),
                           user      = Sys.getenv("DB_LOUROUX_PROD_USER"),
                           password  = Sys.getenv("DB_LOUROUX_PROD_PWD"))
  return(db_con)
}

db_insert_station <- function(db_con, station){
  DBI::dbExecute(db_con, "INSERT INTO stations (id, name, lat, lon, alt) VALUES (?, ?, ?, ?, ?)", station)
}

#' Insert data into a table
#'
#' @param db_conn PqConnection: database connection
#' @param table_name character: table name
#' @param input_values list: list of input name and values
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute sqlInterpolate
#'
#' @return list
#' @export
db_insert_data <- function(db_conn, table_name, input_values) {

  field_placeholders <- paste(names(input_values), collapse = ", ")
  value_placeholders <- paste(rep("?",length(input_values)), collapse = ", ")
  sql <- sprintf("INSERT INTO %s (%s) VALUES (%s)", table_name, field_placeholders, value_placeholders)

  message <- tryCatch({
    query <- do.call(sqlInterpolate, c(list(db_conn, sql), unname(input_values)))
    # Execute the query
    dbExecute(db_conn, query)

    return(list(success = TRUE, message = glue::glue("Data {table_name} inserted successfully!")))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Database error:", e$message)))
  })
  return(message)
}

#' authors list
#'
#' This function returns a list of authors from the database.
#'
#' @param con PqConnection: database connection
#'
#' @importFrom DBI dbGetQuery dbDisconnect
#' @importFrom stats setNames
#'
#' @return vector
#' @export
db_get_authors <- function(con){
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
#' @importFrom DBI dbGetQuery dbDisconnect
#'
#' @return vector
#' @export
db_get_stations <- function(con){
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
#' @return vector
#' @export
db_get_station_parameters <- function(con, station_id){
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
#' @importFrom DBI dbGetQuery dbDisconnect
#'
#' @return vector
#' @export
db_get_sensor_id <- function(con, station_id, parameter_id){
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
#' @importFrom DBI dbGetQuery dbDisconnect
#'
#' @return data.frame
#' @export
db_get_correction_type <- function(con){
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
#' @importFrom DBI dbGetQuery dbDisconnect
#'
#' @return data.frame
#' @export
db_get_interval <- function(con, sensor_id){
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

#' Get table fields
#'
#' @param table_name character: table name
#' @param con PqConnection: database connection
#'
#' @importFrom DBI dbGetQuery sqlInterpolate dbDisconnect
#' @importFrom glue glue
#'
#' @return data.frame
#' @export
db_get_table_fields <- function(table_name, con){
  sql <- paste("SELECT column_name FROM information_schema.columns WHERE table_name = ?table_name AND column_name != 'id';")
  query <- sqlInterpolate(con, sql, table_name = table_name)
  fields <- dbGetQuery(con, query)$column_name
  dbDisconnect(con)
  return(fields)
}

#' Get parameters list
#' @param con PqConnection: database connection
#'
#' @importFrom stats setNames
#' @importFrom DBI dbGetQuery dbDisconnect
#'
#' @return vector
#' @export
db_get_parameters <- function(con){
  sql <- "SELECT parameter.id, parameter.name
          FROM parameter;"
  parameters <- dbGetQuery(con, sql)
  parameters <- setNames(parameters$id, parameters$name)
  dbDisconnect(con)
  return(parameters)
}

#' Get all the intervention from the database
#'
#' @param con PqConnection: database connection
#' @param station_id integer: station id
#' @param start_date POSIXct: start date in format 'YYYY-MM-DD'
#' @param end_date POSIXct: end date in format 'YYYY-MM-DD'
#'
#' @importFrom DBI dbGetQuery dbDisconnect sqlInterpolate
#'
#' @return data.frame
#' @export
db_get_field <- function(con, station_id, start_date, end_date){
  sql <- "SELECT * FROM field WHERE station_id = ?station_id AND timestamp >= ?start_date AND timestamp <= ?end_date;"
  query <- sqlInterpolate(con, sql, station_id = station_id, start_date = start_date, end_date = end_date)
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(data)
}

#' Min and Max date
#'
#' This function returns the minimum and maximum date of the measurements.
#'
#' @param con PqConnection: database connection
#'
#' @importFrom DBI dbGetQuery dbDisconnect sqlInterpolate
#'
#' @return data.frame
#' @export
db_min_max_date <- function(con){
  sql <- "SELECT DATE(min(timestamp)) AS min, DATE(max(timestamp))+1 AS max
    FROM measurement;"
  query <- sqlInterpolate(con, sql, sensor_id = sensor_id)
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(data)
}

#' Data
#'
#' This function returns the measurements based on the sensor id and the date range.
#'
#' @param con PqConnection: database connection
#' @param sensor_id integer: sensor id
#' @param min_date POSIXct: minimum date
#' @param max_date POSIXct: maximum date
#'
#' @importFrom DBI dbGetQuery dbDisconnect sqlInterpolate
#' @importFrom dplyr mutate
#' @importFrom lubridate with_tz
#'
#' @return data.frame
#' @export
db_get_measurement <- function(con, sensor_id, min_date, max_date){
  sql <- "SELECT timestamp, value, value_corr
    FROM measurement
    WHERE sensor_id = ?sensor_id AND timestamp >= ?min_date AND timestamp <= ?max_date
    ORDER BY timestamp;"
  query <- sqlInterpolate(con, sql, sensor_id = sensor_id, min_date = min_date, max_date = max_date)
  data <- dbGetQuery(con, query) %>%
    mutate(timestamp = as.POSIXct(timestamp, tz = 'UTC'))
    # mutate(timestamp = with_tz(timestamp, tzone = Sys.timezone()))
  dbDisconnect(con)
  return(data)
}


