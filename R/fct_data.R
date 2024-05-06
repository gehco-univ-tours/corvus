#' Get raw data from database
#'
#' @param con PqConnection: database connection
#' @param station character: station name
#' @param parameter character: parameter name
#' @param start_date character: start date in format 'YYYY-MM-DD'
#' @param end_date character: end date in format 'YYYY-MM-DD'
#'
#' @return data.frame
#' @export
#'
#' @importFrom glue glue
#' @importFrom DBI dbGetQuery dbDisconnect sqlInterpolate dbQuoteIdentifier SQL
#'
#' @examples
#' con <- db_con()
#' data_get_raw_data(con, "BE", "level", "2019-01-05", "2021-12-26")
data_get_raw_data <- function(con, station, parameter, start_date, end_date){
  sql <- "SELECT date_time, ?station FROM ?parameter WHERE date_time >= ?start_date AND date_time <= ?end_date"
  query <- sqlInterpolate(con, sql, station = SQL(station),
                          parameter = dbQuoteIdentifier(con, parameter),
                          start_date = start_date, end_date = end_date)
  data <- DBI::dbGetQuery(con, query)
  dbDisconnect(con)
  return(data)
}

#' Get corr data from database
#'
#' @param con PqConnection: database connection
#' @param station character: station name
#' @param parameter character: parameter name
#' @param start_date character: start date in format 'YYYY-MM-DD'
#' @param end_date character: end date in format 'YYYY-MM-DD'
#'
#' @return data.frame
#' @export
#'
#' @importFrom glue glue
#' @importFrom DBI dbGetQuery dbDisconnect sqlInterpolate dbQuoteIdentifier SQL
#'
#' @examples
#' con <- db_con()
#' data_get_corr_data(con, "BE", "level", "2019-01-05", "2021-12-26")
data_get_corr_data <- function(con, station, parameter, start_date, end_date){
  parameter_corr <- paste0(parameter, "_corr")
  sql <- "SELECT date_time, ?station FROM ?parameter_corr WHERE date_time >= ?start_date AND date_time <= ?end_date"
  query <- sqlInterpolate(con, sql, station = SQL(station),
                          parameter_corr = dbQuoteIdentifier(con, parameter_corr),
                          start_date = start_date, end_date = end_date)
  data <- DBI::dbGetQuery(con, query)
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
#'
#' @importFrom glue glue
#' @importFrom DBI dbSendQuery dbGetRowsAffected dbDisconnect
#'
#' @return character
#' @export
#'
#' @examples
#' con <- db_con()
#' data_insert_offset(con, "be", "level", as.POSIXct("2021-01-01 00:00", format = "%Y-%m-%d %H:%M"), as.POSIXct("2021-01-02 00:00", format = "%Y-%m-%d %H:%M"), 0.5, "Louis Manière")
data_insert_offset <- function(con, station, parameter, date_time_start, date_time_end, offset_val, author){
  # Create the SQL statement for insertion
  sql_statement <- glue::glue("INSERT INTO corr_offset (station, parameter, date_time_start, date_time_end, offset_val, author)
                    VALUES ($1,$2, $3, $4, $5, $6)")
  # Prepare the SQL statement
  result <- dbSendQuery(con, sql_statement,
                        params = list(station, parameter,
                                      date_time_start, date_time_end,
                                      offset_val, author))
  # Get the number of rows affected by the query
  rows_affected <- dbGetRowsAffected(result)

  dbDisconnect(con)
  return(glue::glue("corr_offset table updated for {toupper(station)} station with {rows_affected} rows inserted"))
}
