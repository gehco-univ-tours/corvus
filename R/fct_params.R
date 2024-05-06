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

#' stations list
#'
#' This function returns a list of stations from the database.
#'
#' @param con PqConnection: database connection
#'
#' importFrom stats setNames
#'
#' @return list
#' @export
params_get_stations <- function(con){
  stations <- DBI::dbGetQuery(con, "SELECT DISTINCT name FROM stations")
  stations <- c(stations$name)
  upper_stations <- toupper(stations)
  stations <- setNames(stations, upper_stations)
  return(stations)
}
