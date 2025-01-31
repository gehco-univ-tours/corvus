#' Get measurement data from database
#'
#' @param con PqConnection: database connection
#' @param sensor integer: sensor id
#' @param start_date POSIXct: start date in format 'YYYY-MM-DD'
#' @param end_date POSIXct: end date in format 'YYYY-MM-DD'
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

#' Create drift correction column to edit data frame.
#'
#' @param timestamp POSIXct: timestamp
#' @param value_corr numeric: corrected value
#' @param drift_value numeric: drift value
#'
#' @importFrom dplyr mutate
#'
#' @return numeric
#' @export
#'
#' @examples
#' data <- data.frame(timestamp = as.POSIXct(c("2021-01-01 00:00", "2021-01-02 00:00",
#'                                             "2021-01-03 00:00"), format = "%Y-%m-%d %H:%M"),
#'                   value_corr = c(1, 2, 3)) %>%
#'                   dplyr::mutate(edit = data_edit_drift(timestamp, value_corr, 5))
data_edit_drift <- function(timestamp, value_corr, drift_value) {

  # Ensure timestamps are sorted for proper calculation
  sorted_indices <- order(timestamp)
  timestamp <- timestamp[sorted_indices]
  value_corr <- value_corr[sorted_indices]

  # Extract first and last point
  first_point <- list(timestamp = timestamp[1], value_corr = value_corr[1])
  last_point <- list(timestamp = timestamp[length(timestamp)], value_corr = value_corr[length(value_corr)])

  # Calculate slope and intercept without drift
  slope <- (last_point$value_corr - first_point$value_corr) / as.numeric(difftime(last_point$timestamp,
                                                                                  first_point$timestamp,
                                                                                  units = "secs"))
  intercept <- first_point$value_corr - slope * as.numeric(first_point$timestamp)

  # Calculate slope and intercept with drift
  slope_drift <- (last_point$value_corr - first_point$value_corr + drift_value) / as.numeric(difftime(last_point$timestamp,
                                                                                                      first_point$timestamp,
                                                                                                      units = "secs"))
  intercept_drift <- first_point$value_corr - slope_drift * as.numeric(first_point$timestamp)

  # Calculate drift_edit for each timestamp
  drift_edit <- value_corr - (slope * as.numeric(difftime(timestamp, first_point$timestamp, units = "secs")) -
                                slope_drift * as.numeric(difftime(timestamp, first_point$timestamp, units = "secs")))

  return(drift_edit)
}

#' Update measurement data into database
#'
#' @param con PqConnection: database connection
#' @param data data.frame: data frame with timestamp, value_corr and edit columns
#' @param sensor integer: sensor id
#' @param correction_type character: correction type id
#' @param value numeric: offset value
#' @param author integer: author id
#' @param comment character: comment
#'
#' @importFrom glue glue
#' @importFrom DBI dbSendQuery dbGetRowsAffected dbDisconnect dbWriteTable dbExecute
#'
#' @return character
#' @export
data_update_measurement <- function(con, data, sensor, author, correction_type, value, comment){

  # get first and last date
  date_time_start <- min(data$timestamp)
  date_time_end <- max(data$timestamp)

  # Correction table
  sql_statement_correction <- glue::glue("INSERT INTO correction(sensor_id, author_id, timestamp_start, timestamp_end,
                                          correction_type, value, comment)
                    VALUES ($1, $2, $3, $4, $5, $6, $7);")
  # Execute the SQL statement
  result_correction <- dbSendQuery(con, sql_statement_correction,
                        params = list(sensor, author, date_time_start, date_time_end, correction_type, value, comment))
  # Get the number of rows affected by the query
  rows_affected_correction <- dbGetRowsAffected(result_correction)
  ##
  # Upload the data frame to a temporary table in PostgreSQL
  dbWriteTable(con, "temp_update", data, temporary = TRUE, row.names = FALSE)

  # Perform a single update command using a join
  sql_statement_measurement <- glue::glue("
    UPDATE measurement
    SET value_corr = temp_update.edit
    FROM temp_update
    WHERE measurement.sensor_id = $1
      AND measurement.timestamp = temp_update.timestamp")

  # Execute the SQL statement
  result_measurement <- dbSendQuery(con, sql_statement_measurement,
            params = list(sensor))

  # Get the number of rows affected by the query
  rows_affected_correction <- dbGetRowsAffected(result_measurement)

  # Drop the temporary table
  dbExecute(con, "DROP TABLE temp_update")

  dbDisconnect(con)
  return(glue::glue("measurement table updated for {sensor} sensor id with {rows_affected_correction} rows inserted and
                    {rows_affected_correction} rows inserted in the correction table."))
}

#' Get measurement missing period by interval
#'
#' @param con PqConnection: database connection
#' @param sensor_id integer: sensor id
#' @param start_date POSIXct: start date in format 'YYYY-MM-DD'
#' @param end_date POSIXct: end date in format 'YYYY-MM-DD'
#' @param interval_time character: interval in format '1 day', '1 hour', '1 minute', '1 second'
#'
#' @importFrom DBI dbGetQuery dbDisconnect sqlInterpolate dbQuoteIdentifier SQL
#'
#' @return data.frame
#' @export
data_get_missing_period <- function(con, sensor_id, start_date, end_date, interval_time){
  sql <- "WITH missing_data AS (
            SELECT
                time_series.time AS timestamp,
                ?sensor_id AS sensor_id
            FROM
                generate_series(
                    ?start_date,
                    ?end_date,
                    CAST(?interval_time AS interval)
                ) AS time_series(time)
            LEFT JOIN
                measurement
            ON
                time_series.time = measurement.timestamp
                AND measurement.sensor_id = ?sensor_id
            WHERE
                measurement.timestamp IS NULL
        ),
        grouped_missing_data AS (
            SELECT
                timestamp,
                sensor_id,
                timestamp - INTERVAL ?interval_time * ROW_NUMBER() OVER (ORDER BY timestamp) AS gap_group
            FROM
                missing_data
        )
        SELECT
            MIN(timestamp) AS time_start,
            MAX(timestamp) AS time_end,
            sensor_id
        FROM
            grouped_missing_data
        GROUP BY
            gap_group,
            sensor_id
        ORDER BY
            time_start;"
  query <- sqlInterpolate(con, sql, sensor_id = sensor_id, start_date = start_date, end_date = end_date, interval_time = interval_time)
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(data)
}

#' Get measurement available period by interval
#'
#' @param con PqConnection: database connection
#' @param sensor_id integer: sensor id
#' @param start_date POSIXct: start date in format 'YYYY-MM-DD'
#' @param end_date POSIXct: end date in format 'YYYY-MM-DD'
#' @param interval_time character: interval in format '1 day', '1 hour', '1 minute', '1 second'
#'
#' @importFrom DBI dbGetQuery dbDisconnect sqlInterpolate dbQuoteIdentifier SQL
#'
#' @return data.frame
#' @export
data_get_available_period <- function(con, sensor_id, start_date, end_date, interval_time){
  sql <- "WITH grouped_available_data AS
          (SELECT
          	timestamp,
          	sensor_id,
          	timestamp - INTERVAL ?interval_time * ROW_NUMBER() OVER (ORDER BY timestamp) AS gap_group
          FROM
          	measurement
          WHERE sensor_id = ?sensor_id
          	AND timestamp >= ?start_date
          	AND timestamp <= ?end_date)
          SELECT
          	MIN(timestamp) AS time_start,
          	MAX(timestamp) AS time_end,
          	sensor_id
          FROM
          	grouped_available_data
          GROUP BY
          	gap_group,
          	sensor_id
          ORDER BY
          	time_start;"
  query <- sqlInterpolate(con, sql, sensor_id = sensor_id, start_date = start_date, end_date = end_date, interval_time = interval_time)
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(data)
}

#' Get min and max date from measurement table
#'
#' @param con PqConnection: database connection
#' @param station_id integer: station id
#'
#' @importFrom DBI dbGetQuery dbDisconnect sqlInterpolate
#'
#' @return data.frame
#' @export
#' @examples
#' con <- db_con()
#' get_min_max_date(con, 3)
get_min_max_date <- function(con, station_id){
  sql <- "SELECT
            MIN(timestamp) AS min_date,
            MAX(timestamp) AS max_date
    FROM measurement
    WHERE sensor_id IN (SELECT id FROM sensor WHERE station_id = ?station_id);"
  query <- sqlInterpolate(con, sql, station_id = station_id)
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(data)
}

#' Get all the station from database
#'
#' @param con PqConnection: database connection
#'
#' @importFrom DBI dbGetQuery dbDisconnect
#'
#' @return data.frame
#' @export
#' @examples
#' con <- db_con()
#' data_get_stations(con)
data_get_stations <- function(con){
  sql <- "SELECT * FROM station;"
  data <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return(data)
}

data_get_intervention <- function(con, station_id, start_date, end_date){
  sql <- "SELECT * FROM intervention WHERE station_id = ?station_id AND timestamp >= ?start_date AND timestamp <= ?end_date;"
  query <- sqlInterpolate(con, sql, station_id = station_id, start_date = start_date, end_date = end_date)
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(data)
}
