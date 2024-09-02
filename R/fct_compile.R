#' Compile raw data into database
#'
#' @param con PqConnection: database connection
#' @param station character: station code
#' @param parameter character: parameter name
#' @param sensor integer: sensor id
#'
#' @importFrom dplyr bind_rows mutate_all select distinct mutate filter arrange
#' @importFrom lubridate ymd_hms dmy_hms
#' @importFrom utils write.table read.csv
#' @importFrom DBI dbSendQuery dbClearResult dbDisconnect dbGetQuery dbGetRowsAffected sqlInterpolate
#' @importFrom glue glue
#'
#' @return string: message
#' @export
compile_raw <- function(con, station, parameter, sensor){

  # create directory path
  dir <- system.file("ext_data", station, "raw_data", parameter, package = "louroux")
  # list all files
  files <- list.files(path = dir,
                               pattern = "*\\.csv$",
                               full.names = TRUE)
  # compile files
  compile <- bind_rows(lapply(files, function(file) {
    read.csv(file, stringsAsFactors = FALSE, sep = ";", row.names = NULL, header = FALSE)
  }))

  # rename columns
  colnames(compile) <- c("date", "time", parameter)

  # clean and format data
  compile <- compile %>%
    mutate_all(~replace(., . == "---", NA)) %>%
    filter(!is.na(date) & !is.na(time)) %>%
    mutate(timestamp = dmy_hms(paste(date, time, sep = " ")),
           data = as.numeric(.data[[parameter]])) %>%
    distinct(timestamp, .keep_all = TRUE) %>%
    filter(!is.na(timestamp)) %>%
    select(timestamp, data)

  compile <- compile %>%
    mutate(sensor_id = rep(sensor, length(compile$timestamp)))

  # insert data
  # Create the SQL statement for insertion
  sql <- glue::glue("INSERT INTO measurement (timestamp, sensor_id, value, value_corr)
                     VALUES ($1, $2, $3, $4) ON CONFLICT (timestamp, sensor_id) DO NOTHING")

  # Prepare the SQL statement
  result <- dbSendQuery(con, sql,
                        params = list(compile$timestamp,
                                      compile$sensor_id,
                                      compile$data,
                                      compile$data))

  # Check if the query was executed successfully
  if (!inherits(result, "DBIResult")) {
    stop("Query execution failed!")
  }

  # Get the number of rows affected by the query
  rows_affected <- dbGetRowsAffected(result)

  # Close the result set
  dbClearResult(result)

  # disconnect from the database
  dbDisconnect(con)

  return(glue::glue("measurement table updated for {toupper(station)} station with {parameter} measures with {rows_affected} rows inserted"))
}
