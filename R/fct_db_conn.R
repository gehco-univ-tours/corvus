#' Postgresql database connection.
#'
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
#'
#' @return PqConnection
#' @export
db_con <- function(){
  db_con <- DBI::dbConnect(RPostgres::Postgres(),
                           host = Sys.getenv("DB_LOUROUX_DEV_HOST"),
                           port = Sys.getenv("DB_LOUROUX_DEV_PORT"),
                           dbname = Sys.getenv("DB_LOUROUX_DEV_NAME"),
                           user      = Sys.getenv("DB_LOUROUX_DEV_USER"),
                           password  = Sys.getenv("DB_LOUROUX_DEV_PWD"))
  return(db_con)
}
