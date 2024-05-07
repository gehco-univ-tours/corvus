#' Compile raw data
#'
#' @param con PqConnection: database connection
#' @param station string: station name
#' @param parameter string: parameter name
#' @param append logical: append to existing file
#' @param col_names logical: write column names
#'
#' @return string: message
#' @export
#'
#' @importFrom dplyr bind_rows mutate_all select distinct mutate filter arrange
#' @importFrom lubridate ymd_hm
#' @importFrom utils write.table read.csv
#' @importFrom DBI dbSendQuery dbClearResult dbDisconnect dbGetQuery dbGetRowsAffected
#' @importFrom glue glue
compile_raw <- function(con,
                        station,
                        parameter,
                        append = FALSE,
                        col_names = TRUE){
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
    mutate(date_time = ymd_hm(paste(.data[["date"]], .data[["time"]], sep = " ")),
           !!parameter := as.numeric(.data[[parameter]])) %>%
    distinct(date_time, .keep_all = TRUE) %>%
    filter(!is.na(date_time)) %>%
    select(date_time, !!parameter)

  # Create the SQL statement for insertion
  sql_statement <- glue::glue("INSERT INTO {parameter} (date_time, {station})
                          VALUES ($1, $2) ON CONFLICT (date_time) DO NOTHING")

  # Prepare the SQL statement
  result <- dbSendQuery(con, sql_statement,
                      params = list(compile$date_time,
                                    compile[[parameter]]))

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

  return(glue::glue("{parameter} table updated for {toupper(station)} station with {rows_affected} rows inserted"))
}
