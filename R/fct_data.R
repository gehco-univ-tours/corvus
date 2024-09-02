#' Get measurement data from database
#'
#' @param con PqConnection: database connection
#' @param sensor integer: sensor id
#' @param start_date character: start date in format 'YYYY-MM-DD'
#' @param end_date character: end date in format 'YYYY-MM-DD'
#'
#' @return data.frame
#' @export
#'
#' @importFrom DBI dbGetQuery dbDisconnect sqlInterpolate dbQuoteIdentifier SQL
#'
#' @examples
#' con <- db_con()
#' data_get_measurement(con, 2, "2019-01-05", "2021-12-26")
data_get_measurement <- function(con, sensor, start_date, end_date){
  sql <- "SELECT timestamp, value, value_corr FROM measurement WHERE timestamp >= ?start_date AND timestamp <= ?end_date
    AND sensor_id = ?sensor ORDER BY timestamp;"
  query <- sqlInterpolate(con, sql, start_date = start_date, end_date = end_date,
                          sensor = sensor)
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(data)
}

#' Insert offset data into database
#'
#' @param con PqConnection: database connection
#' @param sensor integer: sensor id
#' @param date_time_start character: start date in format 'YYYY-MM-DD HH:MM'
#' @param date_time_end character: end date in format 'YYYY-MM-DD HH:MM'
#' @param correction_type character: correction type
#' @param offset_val numeric: offset value
#' @param author character: author name
#' @param comment character: comment
#'
#' @importFrom glue glue
#' @importFrom DBI dbSendQuery dbGetRowsAffected dbDisconnect
#'
#' @return character
#' @export
#'
#' @examples
#' con <- db_con()
#' data_insert_offset(con, 2, "Louis Mnaière",
#'   as.POSIXct("2021-01-01 00:00", format = "%Y-%m-%d %H:%M"),
#'    as.POSIXct("2021-01-02 00:00", format = "%Y-%m-%d %H:%M"),
#'    offset, 0.5, "Louis Manière", "my comment")
data_insert_offset <- function(con, sensor, author, date_time_start, date_time_end, correction_type, offset_val, comment){

  # Correction table
  sql_statement_correction <- glue::glue("INSERT INTO correction(sensor_id, author_id, timestamp_start, timestamp_end,
                                          correction_type, value, comment)
                    VALUES ($1, $2, $3, $4, $5, $6, $7);")
  # Prepare the SQL statement
  result_correction <- dbSendQuery(con, sql_statement_correction,
                        params = list(sensor, author, date_time_start, date_time_end, correction_type, offset_val, comment))
  # Get the number of rows affected by the query
  rows_affected_correction <- dbGetRowsAffected(result_correction)
  ##
  # Measurement table
  sql_statement_measurement <- glue::glue("UPDATE measurement SET value_corr = value_corr + $1
                    WHERE sensor_id = $2 AND timestamp >= $3 AND timestamp <= $4;")
  # Prepare the SQL statement
  result_measurement <- dbSendQuery(con, sql_statement_measurement,
                        params = list(offset_val, sensor, date_time_start, date_time_end))
  # Get the number of rows affected by the query
  rows_affected_correction <- dbGetRowsAffected(result_measurement)

  dbDisconnect(con)
  return(glue::glue("measurement table updated for {sensor} sensor id with {rows_affected_correction} rows inserted and
                    {rows_affected_correction} rows inserted in the correction table."))
}


