#' Get actions list to update database
#'
#' @return vector
#' @export
params_get_db_actions <- function(){
  actions <- c(
    "Add station" = "station",
    "add parameter" = "parameter",
    "Add sensor" = "sensor",
    "Add author" = "author"
  )
}
