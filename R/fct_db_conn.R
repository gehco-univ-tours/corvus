#' Postgresql database connection.
#'
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
#'
#' @return PqConnection
#' @export
db_con <- function(){
  db_con <- DBI::dbConnect(RPostgres::Postgres(),
                           host = Sys.getenv("DB_LOUROUX_HOST"),
                           port = Sys.getenv("DB_LOUROUX_PORT"),
                           dbname = Sys.getenv("DB_LOUROUX_NAME"),
                           user      = Sys.getenv("DB_LOUROUX_USER"),
                           password  = Sys.getenv("DB_LOUROUX_PWD"))
  return(db_con)
}
