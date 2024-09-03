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

#' Compile raw data into database
#'
#' @importFrom curl curl_download new_handle
#'
#' @return string: message
#' @export
download_gb <- function(){

  ### FTP DIR ####
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

  ### LOCAL DIR ####

  # list all the ISTO csv files in local directory
  local_data_dir <- system.file("ext_data", "GB", package = "louroux")
  local_data_files <- list.files(path = local_data_dir, pattern = "*.CSV",
                                 full.names = FALSE)
  ### DOWNLOAD ####

  # get all the file names in FTP not already in local
  ftp_data_files_dl <- setdiff(ftp_data_files, local_data_files)

  print(paste0(as.character(length(ftp_data_files_dl)), " files to download."))

  # download the files not in local
  downloaded_files <- c()
  for (file in ftp_data_files_dl){
    download_url <- paste0(url, file)
    print(download_url)
    print(file.path(local_data_dir, file))
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
  return(paste0("Downloaded files: ", downloaded_files))
}


