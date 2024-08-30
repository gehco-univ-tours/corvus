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
    AND sensor_id = ?sensor;"
  query <- sqlInterpolate(con, sql, start_date = start_date, end_date = end_date,
                          sensor = sensor)
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(data)
}

#' Insert offset data into database
#'
#' @param con PqConnection: database connection
#' @param station character: station name
#' @param parameter character: parameter name
#' @param date_time_start character: start date in format 'YYYY-MM-DD HH:MM'
#' @param date_time_end character: end date in format 'YYYY-MM-DD HH:MM'
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
#' data_insert_offset(con, "be", "level",
#'   as.POSIXct("2021-01-01 00:00", format = "%Y-%m-%d %H:%M"),
#'    as.POSIXct("2021-01-02 00:00", format = "%Y-%m-%d %H:%M"),
#'    0.5, "Louis Manière", "my comment")
data_insert_offset <- function(con, station, parameter, date_time_start, date_time_end, offset_val, author, comment){
  # Create the SQL statement for insertion
  sql_statement <- glue::glue("INSERT INTO corr_offset (station, parameter, date_time_start, date_time_end, offset_val, author, comment)
                    VALUES ($1,$2, $3, $4, $5, $6, $7)")
  # Prepare the SQL statement
  result <- dbSendQuery(con, sql_statement,
                        params = list(station, parameter,
                                      date_time_start, date_time_end,
                                      offset_val, author, comment))
  # Get the number of rows affected by the query
  rows_affected <- dbGetRowsAffected(result)

  dbDisconnect(con)
  return(glue::glue("corr_offset table updated for {toupper(station)} station with {rows_affected} rows inserted"))
}
