#' Compile raw data
#'
#' @param station string: station name
#' @param parameter string: parameter name
#' @param append logical: append to existing file
#' @param col_names logical: write column names
#'
#' @return string: message
#' @export
#'
#' @importFrom dplyr bind_rows mutate_all select distinct mutate filter arrange
#' @importFrom utils write.table read.csv
#' @importFrom DBI dbSendQuery dbBind dbClearResult dbDisconnect dbFetch dbExecute dbGetQuery
#' @importFrom glue glue
#'
#' @examples
#' compile_raw("BE", "level")
compile_raw <- function(station,
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
    mutate(date_time = as.POSIXct(paste(date, time, sep = " "), format = "%d/%m/%Y %H:%M:%S"),
           parameter = as.numeric({{parameter}})) %>%
    distinct(date_time, .keep_all = TRUE) %>%
    filter(!is.na(date_time)) %>%
    select(date_time, {{parameter}})

  # compile_null <- compile %>% filter(is.na(date_time))
  # print(compile_null)


  # Create the SQL statement for insertion
  sql_statement <- glue::glue("INSERT INTO {parameter} (date_time, {tolower(station)})
                          VALUES ($1, $2) ON CONFLICT (date_time) DO NOTHING")
  # print(sql_statement)

  # Prepare the SQL statement
  stmt <- dbSendQuery(con, sql_statement,
                      params = list(compile$date_time,
                                    compile[[parameter]]))

  # Bind parameters and execute the statement for each row of data
  # dbBind(stmt, compile)
  dbExecute(stmt)

  # Close the statement and connection
  dbClearResult(stmt)

  # Get the number of rows affected
  num_affected_rows <- dbGetQuery(con, glue::glue("SELECT COUNT(*) FROM {parameter}"))

  print(num_affected_rows)

  dbDisconnect(con)

  return(glue::glue("{parameter} table updated for {station} station with {rows_insert} inserted"))
}
