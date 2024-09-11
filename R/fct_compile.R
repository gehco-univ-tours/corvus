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

  # get station code from DB
  sql <- "SELECT code FROM station WHERE id = ?station_id;"
  query <- sqlInterpolate(con, sql, station_id = station)
  station_name <- dbGetQuery(con, query)$code
  # get parameter name from DB
  sql <- "SELECT name FROM parameter WHERE id = ?parameter_id;"
  query <- sqlInterpolate(con, sql, parameter_id = parameter)
  parameter_name <- dbGetQuery(con, query)$name

  # create directory path
  dir <- system.file("ext_data", station_name, "raw_data", parameter_name, package = "louroux")
  # list all files
  files <- list.files(path = dir,
                               pattern = "*\\.csv$",
                               full.names = TRUE)
  # compile files
  compile <- bind_rows(lapply(files, function(file) {
    read.csv(file, stringsAsFactors = FALSE, sep = ";", row.names = NULL, header = FALSE)
  }))

  # rename columns
  colnames(compile) <- c("date", "time", station_name)

  # clean and format data
  compile <- compile %>%
    mutate_all(~replace(., . == "---", NA)) %>%
    filter(!is.na(date) & !is.na(time)) %>%
    mutate(timestamp = dmy_hms(paste(date, time, sep = " ")),
           data = as.numeric(.data[[station_name]])) %>%
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

  return(glue::glue("measurement table updated for {toupper(station_name)} station with {station_name} measures with {rows_affected} rows inserted"))
}

#' Download GB data from FTPS
#'
#' @importFrom curl curl_download new_handle
#'
#' @return string: message
#' @export
download_gb <- function(){

  ### FTP DIR ##
  # FTPS params
  ftp_username <- Sys.getenv("LOUROUX_FTP_USER")
  ftp_password <- Sys.getenv("LOUROUX_FTP_PWD")
  ftp_server <- Sys.getenv("LOUROUX_FTP_SERVER")
  ftp_port <- Sys.getenv("LOUROUX_FTP_PORT")
  ftp_data_dir <- "/louroux/GB/"

  # full url with directory
  url <- paste0("ftps://", ftp_username, ":", ftp_password, "@", ftp_server, ":", ftp_port, ftp_data_dir)  # Directory path you want to list

  # Construct the curl command
  curl_command <- sprintf('curl.exe -k --ftp-ssl --max-time 2 %s',url)

  # Run the curl command with system
  output <- system(curl_command, intern = TRUE)

  # list all the csv files in the ftp directory
  ftp_data_files <- unlist(regmatches(output, gregexpr("\\b\\w+\\.CSV\\b", output)))

  ### LOCAL DIR ##

  # list all the ISTO csv files in local directory
  local_data_dir <- system.file("ext_data", "GB", package = "louroux")
  local_data_files <- list.files(path = local_data_dir, pattern = "*.CSV",
                                 full.names = FALSE)
  ### DOWNLOAD ##

  # get all the file names in FTP not already in local
  ftp_data_files_dl <- setdiff(ftp_data_files, local_data_files)

  # download the files not in local
  downloaded_files <- c()
  for (file in ftp_data_files_dl){
    download_url <- paste0(url, file)
    tryCatch({
      curl_download(download_url, file.path(local_data_dir, file),
                    handle = new_handle(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE, ftp_use_epsv = FALSE))
      downloaded_files <- c(downloaded_files, file)
      print(downloaded_files)
      print(paste0("Success ext_data/GB/", file, " downloaded."))
    }, error = function(e) {
      print(paste0("Error ext_data/GB/", file, ", retry later."))
    })
  }
  not_downloaded_files <- setdiff(ftp_data_files_dl, downloaded_files)
  return(paste0("Not downloaded files: ", not_downloaded_files))
}

#' Compile GB raw data into database
#'
#' @param con PqConnection: database connection
#'
#' @importFrom dplyr bind_rows mutate select distinct filter arrange rename_all rename across
#' @importFrom lubridate dmy_hms
#' @importFrom DBI dbSendQuery dbGetRowsAffected dbClearResult
#' @importFrom glue glue
#' @importFrom utils read.csv
#' @importFrom tidyselect everything
#'
#' @return string: message
#' @export
compile_gb <- function(con){

  station <- "GB"

  # create directory path
  dir <- system.file("ext_data", station, package = "louroux")
  # list all files
  files <- list.files(path = dir,
                      pattern = "*\\.CSV$",
                      full.names = TRUE)

  compile <- data.frame()
  for (file in files) {
    # read the file
    data <- read.csv(file, stringsAsFactors = FALSE, sep = ";", row.names = NULL, header = TRUE) %>%
      filter (!is.na(Date) & !is.na(Time)) %>%
      # format date/time to timestamp AND set timezone to UTC+1 without changing time (summer/winter)
      mutate(timestamp = dmy_hms(paste(Date, Time, sep = " "))) %>%
      select(c(-Date, -Time)) %>%
      # remove non numeric values from the data but not the timestamp
      mutate(across(-timestamp, ~ as.numeric(as.character(.)))) %>%
      distinct(timestamp, .keep_all = TRUE) %>%
      filter(!is.na(timestamp)) %>%
      # Move timestamp to the first column
      select(timestamp, everything()) %>%
      # remove capital letters from the column names
      rename_all(tolower)
    # append the data to the compile data frame
    compile <- bind_rows(compile, data)
  }

  compile <- compile %>%
    distinct(timestamp, .keep_all = TRUE) %>%
    arrange(timestamp)

  # sensor id
  sensor <- c("level"=4, "temperature"=5, "conductivity"=6, "turbidity"=7, "do_concentration"=8, "do_saturation"=9, "ph"=10)

  # create list of data frame for each sensor with their respective sensor id
  sensor_data <- list()
  for (i in 1:length(sensor)){
    sensor_data[[names(sensor)[[i]]]] <- compile %>%
      select(timestamp, names(sensor)[i]) %>%
      filter(!is.na(.data[[names(sensor)[i]]])) %>%
      mutate(sensor_id = rep(sensor[[i]], length(timestamp))) %>%
      rename(value = names(sensor)[i])
  }

  # Insert data into the database
  # Create the SQL statement for insertion
  sql <- glue::glue("INSERT INTO measurement (timestamp, sensor_id, value, value_corr)
                     VALUES ($1, $2, $3, $4) ON CONFLICT (timestamp, sensor_id) DO NOTHING")

  # create a list with the number of rows inserted for each sensor
  rows_affected <- c()
  for (i in 1:length(sensor_data)){
    # Prepare the SQL statement
    result <- dbSendQuery(con, sql,
                          params = list(sensor_data[[i]]$timestamp,
                                        sensor_data[[i]]$sensor_id,
                                        sensor_data[[i]]$value,
                                        sensor_data[[i]]$value))

    # Check if the query was executed successfully
    if (!inherits(result, "DBIResult")) {
      stop("Query execution failed!")
    }

    # append the number of rows inserted to the rows_affected
    rows_affected[names(sensor_data[i])] = dbGetRowsAffected(result)

    # Close the result set
    dbClearResult(result)
  }

  # disconnect from the database
  dbDisconnect(con)

  vector_text <- paste(names(rows_affected), rows_affected, sep = " = ", collapse = ", ")

  full_message <- paste0("Number of rows inserted in measurement table for GB station: ", vector_text)

  # return the number of rows inserted for each sensor
  return(full_message)

}
