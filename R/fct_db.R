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

#' Insert station into database
#'
#' @param db_con PqConnection: database connection
#' @param station list: list of station information (id, name, lat, lon)
#'
#' @importFrom DBI dbExecute sqlInterpolate
#' @importFrom glue glue
#'
#' @return list
#' @export
db_insert_station <- function(db_con, station){
  field_placeholders <- paste(names(station), collapse = ", ")
  value_placeholders <- paste(rep("?",length(station)), collapse = ", ")
  sql <- sprintf("INSERT INTO stations (%s) VALUES (%s)", field_placeholders, value_placeholders)
  message <- tryCatch({
    query <- do.call(sqlInterpolate, c(list(db_con, sql), unname(station)))
    # Execute the query
    dbExecute(db_con, query)

    return(list(success = TRUE, message = glue::glue("Station {station$name} inserted successfully!")))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Database error:", e$message)))
  })
}

#' Insert data into a table
#'
#' @param db_con PqConnection: database connection
#' @param table_name character: table name
#' @param input_values list: list of input name and values
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute sqlInterpolate
#'
#' @return list
#' @export
db_insert_data <- function(db_con, table_name, input_values) {

  field_placeholders <- paste(names(input_values), collapse = ", ")
  value_placeholders <- paste(rep("?",length(input_values)), collapse = ", ")
  sql <- sprintf("INSERT INTO %s (%s) VALUES (%s)", table_name, field_placeholders, value_placeholders)

  message <- tryCatch({
    query <- do.call(sqlInterpolate, c(list(db_con, sql), unname(input_values)))
    # Execute the query
    dbExecute(db_con, query)

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
#' @importFrom DBI dbGetQuery
#' @importFrom stats setNames
#'
#' @return vector
#' @export
db_get_authors <- function(con){
  authors <- dbGetQuery(con, "SELECT id, name FROM author")
  authors <- setNames(authors$id, authors$name)
  return(authors)
}

#' Get all the station from database
#'
#' @param con PqConnection: database connection
#'
#' @importFrom DBI dbGetQuery
#'
#' @return data.frame
#' @export
#' @examples
#' con <- db_con()
#' db_get_all_stations(con)
db_get_all_stations <- function(con){
  sql <- "SELECT * FROM station;"
  data <- dbGetQuery(con, sql)
  return(data)
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
#' @return vector
#' @export
db_get_stations <- function(con){
  stations <- dbGetQuery(con, "SELECT id, name FROM station")
  stations <- setNames(stations$id, stations$name)
  return(stations)
}

#' parameters list
#'
#' This function returns a list of parameters from the database.
#'
#' @param con PqConnection: database connection
#' @param station_id integer: station id
#'
#' @importFrom DBI dbGetQuery sqlInterpolate dbQuoteIdentifier SQL
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
  return(parameters)
}

#' get parameter unit
#'
#' @param con PqConnection: database connection
#' @param parameter_id integer: parameter id
#'
#' @importFrom DBI dbGetQuery sqlInterpolate dbQuoteIdentifier
#'
#' @return character
#' @export
db_get_parameter_unit <- function(con, parameter_id){
  sql <- "SELECT unit
    FROM parameter
    WHERE id = ?parameter_id;"
  query <- sqlInterpolate(con, sql, parameter_id = parameter_id)
  data <- dbGetQuery(con, query)
  unit <- data$unit
  return(unit)
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
#' @return vector
#' @export
db_get_sensor_id <- function(con, station_id, parameter_id){
  sql <- "SELECT sensor.id
    FROM sensor
    WHERE station_id = ?station_id AND parameter_id = ?parameter_id;"
  query <- sqlInterpolate(con, sql, station_id = station_id, parameter_id = parameter_id)
  sensor_id <- dbGetQuery(con, query)$id
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
#' @return data.frame
#' @export
db_get_correction_type <- function(con){
  corrections <- dbGetQuery(con, "SELECT id, name FROM correction_type")
  corrections <- setNames(corrections$id, corrections$name)
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
#' @return data.frame
#' @export
db_get_interval <- function(con, sensor_id){
  sql <- "WITH intervals AS (
              SELECT ts - LAG(ts) OVER (ORDER BY ts) AS interval
              FROM measurement
          	  WHERE sensor_id = ?sensor_id
          )
          SELECT interval, COUNT(*) AS count
          FROM intervals
          GROUP BY interval
          ORDER BY count DESC
          LIMIT 1;"
  query <- sqlInterpolate(con, sql, sensor_id = sensor_id)
  intervals <- dbGetQuery(con, query)$interval
  return(intervals)
}

#' Get table fields
#'
#' @param table_name character: table name
#' @param con PqConnection: database connection
#'
#' @importFrom DBI dbGetQuery sqlInterpolate
#' @importFrom glue glue
#'
#' @return data.frame
#' @export
db_get_table_fields <- function(table_name, con){
  sql <- paste("SELECT column_name FROM information_schema.columns WHERE table_name = ?table_name AND column_name != 'id';")
  query <- sqlInterpolate(con, sql, table_name = table_name)
  fields <- dbGetQuery(con, query)$column_name
  return(fields)
}

#' Get parameters list
#' @param con PqConnection: database connection
#'
#' @importFrom stats setNames
#' @importFrom DBI dbGetQuery
#'
#' @return vector
#' @export
db_get_parameters <- function(con){
  sql <- "SELECT parameter.id, parameter.name
          FROM parameter;"
  parameters <- dbGetQuery(con, sql)
  parameters <- setNames(parameters$id, parameters$name)
  return(parameters)
}

#' Get all the intervention from the database
#'
#' @param con PqConnection: database connection
#' @param station_id integer: station id
#' @param start_date POSIXct: start date in format 'YYYY-MM-DD'
#' @param end_date POSIXct: end date in format 'YYYY-MM-DD'
#'
#' @importFrom DBI dbGetQuery sqlInterpolate
#'
#' @return data.frame
#' @export
db_get_field <- function(con, station_id, start_date, end_date){
  sql <- "SELECT * FROM field WHERE station_id = ?station_id AND ts >= ?start_date AND ts <= ?end_date;"
  query <- sqlInterpolate(con, sql, station_id = station_id, start_date = start_date, end_date = end_date)
  data <- dbGetQuery(con, query)
  return(data)
}

#' Min and Max date
#'
#' This function returns the minimum and maximum date of the measurements.
#'
#' @param con PqConnection: database connection
#'
#' @importFrom DBI dbGetQuery sqlInterpolate
#'
#' @return data.frame
#' @export
db_min_max_date <- function(con){
  sql <- "SELECT DATE(min(ts)) AS min, DATE(max(ts))+1 AS max
    FROM measurement;"
  query <- sqlInterpolate(con, sql, sensor_id = sensor_id)
  data <- dbGetQuery(con, query)
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
#' @importFrom DBI dbGetQuery sqlInterpolate
#' @importFrom dplyr mutate
#' @importFrom lubridate with_tz
#'
#' @return data.frame
#' @export
db_get_measurement <- function(con, sensor_id, min_date, max_date){
  sql <- "SELECT ts, sensor_id, value, value_corr,
            CASE WHEN value_corr IS NULL THEN value ELSE value_corr END AS value_edit
    FROM measurement
    WHERE sensor_id = ?sensor_id AND ts >= ?min_date AND ts <= ?max_date
    ORDER BY ts;"
  query <- sqlInterpolate(con, sql, sensor_id = sensor_id, min_date = min_date, max_date = max_date)
  data <- dbGetQuery(con, query) %>%
    mutate(ts = as.POSIXct(ts, tz = 'UTC')) %>%
    mutate(ts = with_tz(ts, tzone = Sys.timezone()))
  return(data)
}

#' Field data
#'
#' This function returns the field data based on the station id and the date range.
#'
#' @param con PqConnection: database connection
#' @param station_id integer: station id
#' @param start_date POSIXct: start date in format 'YYYY-MM-DD'
#' @param end_date POSIXct: end date in format 'YYYY-MM-DD'
#'
#' @importFrom DBI dbGetQuery sqlInterpolate
#' @importFrom dplyr mutate
#' @importFrom lubridate with_tz
#'
#' @return data.frame
#' @export
db_get_fieldwork_data <- function(con, station_id, start_date, end_date){
  sql <- "SELECT ts, author.code AS author, comment
    FROM fieldwork
    JOIN author ON fieldwork.author_id = author.id
    WHERE station_id = ?station_id AND ts >= ?start_date AND ts <= ?end_date
    ORDER BY ts;"
  query <- sqlInterpolate(con, sql, station_id = station_id, start_date = start_date, end_date = end_date)
  data <- dbGetQuery(con, query) %>%
    mutate(ts = as.POSIXct(ts, tz = 'UTC')) %>%
    mutate(ts = with_tz(ts, tzone = Sys.timezone()))
  return(data)
}

#' Validated period data
#'
#' This function returns the validated period data based on the sensor id and the date range.
#'
#' @param con PqConnection: database connection
#' @param sensor_id integer: sensor id
#' @param start_date POSIXct: start date in format 'YYYY-MM-DD'
#' @param end_date POSIXct: end date in format 'YYYY-MM-DD'
#'
#' @importFrom DBI dbGetQuery sqlInterpolate
#' @importFrom dplyr mutate
#' @importFrom lubridate with_tz
#'
#' @return data.frame
#' @export
db_get_validated_period_data <- function(con, sensor_id, start_date, end_date){
  sql <- "SELECT *
    FROM validated_period
    WHERE sensor_id = ?sensor_id AND ts_start >= ?start_date AND ts_end <= ?end_date
    ORDER BY ts_start;"
  query <- sqlInterpolate(con, sql, sensor_id = sensor_id, start_date = start_date, end_date = end_date)
  data <- dbGetQuery(con, query) %>%
    mutate(ts_start = as.POSIXct(ts_start, tz = 'UTC')) %>%
    mutate(ts_end = as.POSIXct(ts_end, tz = 'UTC')) %>%
    mutate(ts_start = with_tz(ts_start, tzone = Sys.timezone())) %>%
    mutate(ts_end = with_tz(ts_end, tzone = Sys.timezone()))
  return(data)
}

#' Deleted period data
#'
#' This function returns the deleted period data based on the sensor id and the date range.
#'
#' @param con PqConnection: database connection
#' @param sensor_id integer: sensor id
#' @param start_date POSIXct: start date in format 'YYYY-MM-DD'
#' @param end_date POSIXct: end date in format 'YYYY-MM-DD'
#'
#' @importFrom DBI dbGetQuery sqlInterpolate
#' @importFrom dplyr mutate
#' @importFrom lubridate with_tz
#'
#' @return data.frame
#' @export
db_get_deleted_period_data <- function(con, sensor_id, start_date, end_date){
  sql <- "SELECT *
    FROM correction
    WHERE sensor_id = ?sensor_id AND ts_start >= ?start_date AND ts_end <= ?end_date
    AND correction_type = 3
    ORDER BY ts_start;"
  query <- sqlInterpolate(con, sql, sensor_id = sensor_id, start_date = start_date, end_date = end_date)
  data <- dbGetQuery(con, query) %>%
    mutate(ts_start = as.POSIXct(ts_start, tz = 'UTC')) %>%
    mutate(ts_end = as.POSIXct(ts_end, tz = 'UTC')) %>%
    mutate(ts_start = with_tz(ts_start, tzone = Sys.timezone())) %>%
    mutate(ts_end = with_tz(ts_end, tzone = Sys.timezone()))
  return(data)
}

#' Insert correction periods into correction table
#'
#' @param con DBIConnection
#' @param dataframe data.frame
#'
#' @importFrom DBI dbWriteTable dbExecute
#' @importFrom glue glue
#'
#' @return integer Number of rows inserted
#' @export
db_update_correction <- function(con, dataframe) {

  stopifnot(
    is.data.frame(dataframe),
    all(c("sensor_id", "author_id", "ts_start", "ts_end", "correction_type",
      "value", "comment"
    ) %in% names(dataframe))
  )

  tryCatch({
    DBI::dbWriteTable( con, name = "temp_correction", value = dataframe,
      temporary = TRUE, row.names = FALSE)

    sql <- glue::glue("
      INSERT INTO correction (sensor_id, author_id, ts_start, ts_end,
        correction_type, value, comment)
      SELECT
        sensor_id, author_id, ts_start, ts_end, correction_type, value, comment
      FROM temp_correction")
    rows <- DBI::dbExecute(con, sql)
    DBI::dbExecute(con, "DROP TABLE temp_correction")

    return(paste0(rows, " inserted"))

  }, error = function(e) {
    stop(e)
  })
}

#' Update measurement table with edited values
#'
#' @param con DBIConnection
#' @param dataframe data.frame with columns ts, sensor_id, value, value_corr and value_edit
#'
#' @importFrom DBI dbWriteTable dbExecute
#' @importFrom glue glue
#'
#' @return integer Number of rows updated
#' @export
db_update_measurement_edit <- function(con, dataframe){

  stopifnot(
    is.data.frame(dataframe),
    all(c("ts", "sensor_id", "value", "value_corr", "value_edit") %in% names(dataframe)),
    length(unique(dataframe$sensor_id)) == 1
  )

  tryCatch({
    DBI::dbWriteTable(con, name = "temp_measurement", value = dataframe,
                       temporary = TRUE, row.names = FALSE
    )
    sql <- glue::glue("
      UPDATE measurement
      SET value_corr = temp_measurement.value_edit
      FROM temp_measurement
      WHERE measurement.sensor_id = temp_measurement.sensor_id
      AND measurement.ts = temp_measurement.ts;
      ")
    rows <- DBI::dbExecute(con, sql)
    DBI::dbExecute(con, "DROP TABLE temp_measurement")

    return(paste0(rows, " updated"))

  }, error = function(e) {

    stop(e)
  })
}

#' Apply measurement edit and correction periods in a single transaction
#'
#' This function applies both measurement edits and correction periods in a single database transaction. If any part of the process fails, the entire transaction is rolled back to maintain data integrity.
#'
#' @param con DBIConnection
#' @param measurement_edit data.frame with columns ts, sensor_id, value, value_corr and value_edit (can be NULL if no measurement edit)
#' @param correction_period data.frame with columns sensor_id, author_id, ts_start, ts_end, correction_type, value, comment (can be NULL if no correction period)
#'
#' @importFrom DBI dbBegin dbCommit dbRollback
#' @importFrom glue glue
#'
#' @return character Message indicating the result of the operation
#' @export
db_apply_edit_with_correction <- function(con, measurement_edit, correction_period) {

  DBI::dbBegin(con)

  tryCatch({

    if (!is.null(measurement_edit)) {
      db_update_measurement_edit(con, measurement_edit)
    }

    db_update_correction(con, correction_period)

    DBI::dbCommit(con)

    return("Measurement + correction committed")

  }, error = function(e) {

    DBI::dbRollback(con)
    stop(e)

  })
}



