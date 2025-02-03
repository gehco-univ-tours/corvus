#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    navbarPage(
      theme = bs_theme(version = 5, bootswatch = "simplex"),
      title =
        div(
          img(src = "www/logo_gehco.png", height = 30, width = 35, style = "margin-right: 10px;"),
          "Louroux",
          style = "display: flex; float: left;"
        ),
      tabPanel("Stations", mod_stations_ui("stations_1")),
      tabPanel("Raw Data", mod_raw_ui("raw_1")),
      tabPanel("Edition", mod_edit_ui("edit_1")),
      tabPanel("Validation", mod_valid_ui("valid_1")),
      tabPanel("Database", mod_database_ui("database_1"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "louroux"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
