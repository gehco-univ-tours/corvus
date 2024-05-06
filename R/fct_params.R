#' authors list
#'
#' This function returns a list of authors from the database.
#'
#' @param con PqConnection: database connection
#'
#' @return list
#' @export
params_get_authors <- function(con){
  authors <- DBI::dbGetQuery(con, "SELECT DISTINCT name FROM authors")
  authors <- as.list(authors$name)
  return(authors)
}
