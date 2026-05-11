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

#' Format deleted period from deleted threshold
#'
#' @param dataframe data.frame: data frame with ts, value, value_corr and value_edit columns
#' @param sensor_id integer: sensor id
#' @param delete_threshold numeric: threshold value to consider a value as deleted
#' @param author_id integer: author id
#' @param correction_type integer: correction type id
#' @param comment character: comment
#'
#' @importFrom dplyr arrange mutate lag filter group_by summarise transmute first
#'
#' @return data.frame
#' @export
data_get_deleted_periods <- function(dataframe, sensor_id, delete_threshold,
                                     author_id, correction_type, comment){


  stopifnot(
    is.data.frame(dataframe),
    all(c("ts", "value_edit") %in% names(dataframe))
  )

  data <- dataframe %>%
    arrange(ts) %>%
    mutate(
      flag_delete = value_edit > delete_threshold,
      new_period = flag_delete != lag(flag_delete, default = first(flag_delete)),
      period_id = cumsum(new_period)
    ) %>%
    filter(flag_delete) %>%
    group_by(period_id) %>%
    summarise(
      ts_start = min(ts),
      ts_end   = max(ts),
      .groups  = "drop"
    ) %>%
    transmute(
      sensor_id = sensor_id,
      author_id = author_id,
      ts_start,
      ts_end,
      correction_type = correction_type,
      value = delete_threshold,
      comment = comment
    )
  return(data)
}

#' Format correction period from edited values
#'
#' @param dataframe data.frame: data frame with ts, value, value_corr and value_edit columns
#' @param sensor_id integer: sensor id
#' @param value numeric: offset value to consider a value as corrected
#' @param author_id integer: author id
#' @param correction_type integer: correction type id
#' @param comment character: comment
#'
#' @importFrom dplyr summarise transmute
#'
#' @return data.frame
data_get_correction_period <- function(dataframe, sensor_id, value,
                                       author_id, correction_type, comment){

  stopifnot(
    is.data.frame(dataframe),
    all(c("ts") %in% names(dataframe))
  )

  data <- dataframe %>%
    summarise(
      ts_start = min(ts),
      ts_end = max(ts)
    ) %>%
    transmute(
      sensor_id = sensor_id,
      author_id = author_id,
      ts_start,
      ts_end,
      correction_type = correction_type,
      value = value,
      comment = comment
    )
  return(data)
}

#' Prepare edited measurement data and correction period for database update
#'
#' This function prepares the edited measurement data and the corresponding correction period based on the specified correction type. It filters the measurement data for the given date range and applies the appropriate correction (offset, drift, delete, or set value) to the `value_edit` column. It also generates a correction period data frame that can be used to update the database with the correction details.
#'
#' @param measurement_tocorr data.frame: data frame with ts, value, value_corr and value_edit columns
#' @param correction_type integer: correction type id (1 for offset, 2 for drift, 3 for delete, 4 for set value)
#' @param sensor_id integer: sensor id
#' @param start_date POSIXct: start date in format 'YYYY-MM-DD'
#' @param end_date POSIXct: end date in format 'YYYY-MM-DD'
#' @param author_id integer: author id
#' @param comment character: comment
#' @param offset numeric: offset value to apply for correction type 1 (offset)
#' @param drift numeric: drift value to apply for correction type 2 (drift correction)
#' @param delete_threshold numeric: threshold value to consider a value as deleted for correction type 3 (delete)
#' @param set_value numeric: value to set for correction type 4 (set value correction)
#' @importFrom dplyr filter mutate
#'
#' @return list with measurement_edit data frame and correction_period data frame
#' @export
data_prepare_edit_and_correction <- function(
    measurement_tocorr,
    correction_type,
    sensor_id,
    start_date,
    end_date,
    author_id,
    comment,
    offset = NULL,
    drift = NULL,
    delete_threshold = NULL,
    set_value = NULL
) {

  measurement_edit <- measurement_tocorr %>%
    dplyr::filter(ts >= start_date, ts <= end_date)

  if (correction_type == 1) { # offset
    measurement_edit <- measurement_edit %>%
      mutate(value_edit = value_edit + offset,
             status_id = 1) # measurement status = corrected

    correction_period <- data_get_correction_period(
      dataframe = measurement_edit,
      sensor_id = sensor_id,
      value = offset,
      author_id = author_id,
      correction_type = correction_type,
      comment = comment
    )
  }

  if (correction_type == 2) { # drift
    measurement_edit <- measurement_edit %>%
      mutate(value_edit = data_edit_drift(ts, value_edit, drift),
             status_id = 1) # measurement status = corrected

    correction_period <- data_get_correction_period(
      dataframe = measurement_edit,
      sensor_id = sensor_id,
      value = drift,
      author_id = author_id,
      correction_type = correction_type,
      comment = comment
    )
  }

  if (correction_type == 3) { # delete

    measurement_edit <- measurement_edit %>%
      mutate(status_id = 2) # measurement status = deleted

    correction_period <- data_get_deleted_periods(
      dataframe = measurement_edit,
      sensor_id = sensor_id,
      delete_threshold = delete_threshold,
      author_id = author_id,
      correction_type = correction_type,
      comment = comment
    )
  }

  if (correction_type == 4) { # set value
    measurement_edit <- measurement_edit %>%
      mutate(value_edit = set_value,
             status_id = 1) # measurement status = corrected

    correction_period <- data_get_correction_period(
      dataframe = measurement_edit,
      sensor_id = sensor_id,
      value = set_value,
      author_id = author_id,
      correction_type = correction_type,
      comment = comment
    )
  }

  list(
    measurement_edit = measurement_edit,
    correction_period = correction_period
  )
}

#' Get measurement missing period by interval
#'
#' @param con PqConnection: database connection
#' @param sensor_id integer: sensor id
#' @param start_date POSIXct: start date in format 'YYYY-MM-DD'
#' @param end_date POSIXct: end date in format 'YYYY-MM-DD'
#' @param interval_time character: interval in format '1 day', '1 hour', '1 minute', '1 second'
#'
#' @importFrom DBI dbGetQuery sqlInterpolate dbQuoteIdentifier SQL
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
#' @importFrom DBI dbGetQuery sqlInterpolate dbQuoteIdentifier SQL
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
  return(data)
}
