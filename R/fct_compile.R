#' Compile raw data
#'
#' @param station string: station name
#' @param parameter string: parameter name
#' @param append logical: append to existing file
#' @param col_names logical: write column names
#'
#' @return data frame
#' @export
#'
#' @importFrom dplyr bind_rows mutate_all select
#' @importFrom utils write.table read.csv
#'
#' @examples
#' compile_raw("BE", "level")
compile_raw <- function(station,
                        parameter,
                        append = FALSE,
                        col_names = TRUE){
  # create directory path
  dir <- system.file("lrx_data", station, "raw_data", parameter, package = "louroux")
  # list all files
  files <- list.files(path = dir,
                               pattern = "*\\.csv$",
                               full.names = TRUE)
  # compile files
  compile <- bind_rows(lapply(files, function(file) {
    read.csv(file, stringsAsFactors = FALSE, sep = ";", row.names = NULL, header = FALSE)
  }))
  # rename columns
  colnames(compile) <- c("date", "time", "level")
  # clean and format data
  compile <- compile %>%
    mutate_all(~replace(., . == "---", NA)) %>%
    mutate(date_time = as.POSIXct(paste(date, time, sep = " "), format = "%d/%m/%Y %H:%M:%S"),
           level_m = as.numeric(level_m)) %>%
    select(date_time, level_m)

  # write compile
  write.table(compile,
              file = file.path(dir, paste0(station,"_",parameter, "_", "compile.csv")),
              sep=";",dec=".", append = append, row.names = FALSE,
              col.names = col_names, fileEncoding = "utf-8")
  return(compile)
}
