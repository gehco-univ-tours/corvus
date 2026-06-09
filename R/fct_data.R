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

  sorted_indices <- order(timestamp)
  timestamp <- timestamp[sorted_indices]
  value_corr <- value_corr[sorted_indices]

  t0 <- timestamp[1]
  tN <- timestamp[length(timestamp)]
  vN <- value_corr[length(value_corr)]

  # écart à corriger
  drift <- drift_value - vN

  dt_total <- as.numeric(difftime(tN, t0, units = "secs"))
  dt <- as.numeric(difftime(timestamp, t0, units = "secs"))

  # correction progressive (0 au début, drift à la fin)
  correction <- drift * (dt / dt_total)

  drift_edit <- value_corr + correction

  return(drift_edit)
}

# data_edit_drift <- function(timestamp, value_corr, drift_value) {
#
#   # Ensure timestamps are sorted for proper calculation
#   sorted_indices <- order(timestamp)
#   timestamp <- timestamp[sorted_indices]
#   value_corr <- value_corr[sorted_indices]
#
#   # Extract first and last point
#   first_point <- list(timestamp = timestamp[1], value_corr = value_corr[1])
#   last_point <- list(timestamp = timestamp[length(timestamp)], value_corr = value_corr[length(value_corr)])
#
#   # Calculate slope and intercept without drift
#   slope <- (last_point$value_corr - first_point$value_corr) / as.numeric(difftime(last_point$timestamp,
#                                                                                   first_point$timestamp,
#                                                                                   units = "secs"))
#   intercept <- first_point$value_corr - slope * as.numeric(first_point$timestamp)
#
#   # Calculate slope and intercept with drift
#   slope_drift <- (last_point$value_corr - first_point$value_corr + drift_value) / as.numeric(difftime(last_point$timestamp,
#                                                                                                       first_point$timestamp,
#                                                                                                       units = "secs"))
#   intercept_drift <- first_point$value_corr - slope_drift * as.numeric(first_point$timestamp)
#
#   # Calculate drift_edit for each timestamp
#   drift_edit <- value_corr - (slope * as.numeric(difftime(timestamp, first_point$timestamp, units = "secs")) -
#                                 slope_drift * as.numeric(difftime(timestamp, first_point$timestamp, units = "secs")))
#
#   return(drift_edit)
# }

#' Format deleted period from deleted threshold
#'
#' @param dataframe data.frame: data frame with ts, value columns
#' @param sensor_id integer: sensor id
#' @param delete_threshold numeric: threshold value to consider a value as deleted
#' @param author_id integer: author id
#' @param correction_type integer: correction type id
#' @param comment character: comment
#'
#' @importFrom dplyr arrange mutate lag filter group_by summarise transmute first last
#'
#' @return data.frame
#' @export
data_get_deleted_periods <- function(dataframe, sensor_id, delete_threshold,
                                     author_id, correction_type, comment){


  stopifnot(
    is.data.frame(dataframe),
    all(c("ts", "value") %in% names(dataframe))
  )

  data <- dataframe %>%
    arrange(ts) %>%
    mutate(
      flag_delete = is.na(value),
      flag_delete_clean = flag_delete %in% TRUE,
      period_id = cumsum(flag_delete_clean != lag(flag_delete_clean, default = flag_delete_clean[1]))
    ) %>%
    filter(flag_delete_clean) %>%
    group_by(period_id) %>%
    summarise(
      ts_start = first(ts),
      ts_end   = last(ts),
      .groups  = "drop"
    ) %>%
    transmute(
      sensor_id = sensor_id,
      author_id = author_id,
      ts_start,
      ts_end,
      correction_type = correction_type,
      value1 = delete_threshold,
      value2 = NULL,
      comment = comment
    )
  return(data)
}

#' Format correction period from edited values
#'
#' @param dataframe data.frame: data frame with ts, value columns
#' @param sensor_id integer: sensor id
#' @param value1 numeric: tool parameter1
#' @param value2 numeric: tool parameter2
#' @param author_id integer: author id
#' @param correction_type integer: correction type id
#' @param comment character: comment
#'
#' @importFrom dplyr summarise transmute
#'
#' @return data.frame
data_get_correction_period <- function(dataframe, sensor_id, value1, value2,
                                       author_id, correction_type, comment){

  stopifnot(
    is.data.frame(dataframe),
    all(c("ts", "value") %in% names(dataframe))
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
      value1 = value1,
      value2 = value2,
      comment = comment
    )
  return(data)
}

#' Prepare edited measurement data and correction period for database update
#'
#' This function prepares the edited measurement data and the corresponding correction period based on the specified correction type.
#'
#' @param measurement_edit data.frame: data frame with ts, value columns
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
#' @param median_interval numeric: time interval to calculate median (min)
#' @param loess_span numeric: loess span from 0 to 1
#' @param hampel_interval numeric: Hampel filter time interval (min)
#' @param hampel_value numeric: Hampel threshold value to set median value instead of raw data
#' @param mean_interval numeric: rolling mean time interval (min)
#'
#' @importFrom dplyr mutate
#'
#' @return list with measurement_edit data frame and correction_period data frame
#' @export
data_prepare_edit_and_correction <- function(
    measurement_edit,
    correction_type,
    sensor_id,
    start_date,
    end_date,
    author_id,
    comment,
    offset = NULL,
    drift = NULL,
    delete_threshold = NULL,
    set_value = NULL,
    median_interval = NULL,
    loess_span = NULL,
    hampel_interval = NULL,
    hampel_value = NULL,
    mean_interval = NULL,
    tsclean_iteration = NULL
) {

  if (correction_type == 1) { # offset

    correction_period <- data_get_correction_period(
      dataframe = measurement_edit,
      sensor_id = sensor_id,
      value1 = offset,
      value2 = NA_real_,
      author_id = author_id,
      correction_type = correction_type,
      comment = comment
    )
  }

  if (correction_type == 2) { # drift

    correction_period <- data_get_correction_period(
      dataframe = measurement_edit,
      sensor_id = sensor_id,
      value1 = drift,
      value2 = NA_real_,
      author_id = author_id,
      correction_type = correction_type,
      comment = comment
    )
  }

  if (correction_type == 3) { # delete

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

    correction_period <- data_get_correction_period(
      dataframe = measurement_edit,
      sensor_id = sensor_id,
      value1 = set_value,
      value2 = NA_real_,
      author_id = author_id,
      correction_type = correction_type,
      comment = comment
    )
  }

  if (correction_type == 5) { # Median filter

    correction_period <- data_get_correction_period(
      dataframe = measurement_edit,
      sensor_id = sensor_id,
      value1 = median_interval,
      value2 = NA_real_,
      author_id = author_id,
      correction_type = correction_type,
      comment = comment
    )
  }

  if (correction_type == 6) { # Loess filter

    correction_period <- data_get_correction_period(
      dataframe = measurement_edit,
      sensor_id = sensor_id,
      value1 = loess_span,
      value2 = NA_real_,
      author_id = author_id,
      correction_type = correction_type,
      comment = comment
    )
  }

  if (correction_type == 7) { # Hampel filter

    correction_period <- data_get_correction_period(
      dataframe = measurement_edit,
      sensor_id = sensor_id,
      value1 = hampel_interval,
      value2 = hampel_value,
      author_id = author_id,
      correction_type = correction_type,
      comment = comment
    )
  }

  if (correction_type == 8) { # Mean filter

    correction_period <- data_get_correction_period(
      dataframe = measurement_edit,
      sensor_id = sensor_id,
      value1 = mean_interval,
      value2 = NA_real_,
      author_id = author_id,
      correction_type = correction_type,
      comment = comment
    )
  }

  if (correction_type == 9) { # tsclean filter

    correction_period <- data_get_correction_period(
      dataframe = measurement_edit,
      sensor_id = sensor_id,
      value1 = tsclean_iteration,
      value2 = NA_real_,
      author_id = author_id,
      correction_type = correction_type,
      comment = comment
    )
  }

  list(
    measurement_edit = measurement_edit,
    correction_period = correction_period %>%
      mutate(ts_corr = as.POSIXct(Sys.time(), tz = "UTC"))
  )
}

#' Get measurement data with applied corrections
#'
#' This function takes the raw measurement data and the corresponding corrections, and applies the corrections to the raw data. It performs a left join between the raw measurement data and the correction data based on the timestamp and sensor ID. The corrected value is calculated using the `coalesce` function, which returns the corrected value if it exists, or the original value if there is no correction. The resulting data frame contains the timestamp, sensor ID, and the final value after applying corrections.
#'
#' @param measurement_raw data.frame: raw measurement data with columns ts, sensor_id, value
#' @param measurement_corr data.frame: correction data with columns ts, sensor_id, value
#'
#' @importFrom dplyr bind_rows group_by slice_tail ungroup select arrange filter
#'
#' @return data.frame with columns ts, sensor_id, value (corrected)
#' @export
data_get_measurement_edit <- function(measurement_raw, measurement_corr){
  data <- bind_rows(
    measurement_raw,
    measurement_corr
  ) %>%
    group_by(ts, sensor_id) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(ts, sensor_id, value) %>%
    filter(!is.na(value)) %>%
    arrange(ts)
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

#' Filter value with Hampel filter
#'
#' @param x POSIXct: date time date in format 'YYYY-MM-DD'
#' @param k number: Hampel factor threshold
#'
#' @importFrom stats median mad
#'
#' @return value
#' @export
data_hampel_filter <- function(x, k = 3) {
  med <- median(x)
  mad_val <- mad(x, constant = 1.4826)

  x0 <- x[length(x)] # Last interval point = present

  if (mad_val < 1e-9) {
    return(x0)   # mad_val too small to compare
  }

  if (abs(x0 - med) > k * mad_val) med else x0
}

