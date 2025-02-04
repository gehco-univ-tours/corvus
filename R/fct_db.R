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

db_insert_station <- function(db_con, station){
  DBI::dbExecute(db_con, "INSERT INTO stations (id, name, lat, lon, alt) VALUES (?, ?, ?, ?, ?)", station)
}

#' Insert data into a table
#'
#' @param db_conn PqConnection: database connection
#' @param table_name character: table name
#' @param input_values list: list of input name and values
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute sqlInterpolate
#'
#' @return list
#' @export
db_insert_data <- function(db_conn, table_name, input_values) {

  field_placeholders <- paste(names(input_values), collapse = ", ")
  value_placeholders <- paste(rep("?",length(input_values)), collapse = ", ")
  sql <- sprintf("INSERT INTO %s (%s) VALUES (%s)", table_name, field_placeholders, value_placeholders)

  message <- tryCatch({
    query <- do.call(sqlInterpolate, c(list(db_conn, sql), unname(input_values)))
    # Execute the query
    dbExecute(db_conn, query)

    return(list(success = TRUE, message = glue::glue("✅ Data  {table_name} inserted successfully!")))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("❌ Database error:", e$message)))
  })
  return(message)
}
