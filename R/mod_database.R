#' database UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_database_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        add_busy_bar(color = "#FF0000")
      ),
      fluidRow(
        column(
          width = 2,
          selectInput(inputId = ns("action"),
                      label = "Action",
                      choices = params_get_db_actions())
        ),
        column(
          width = 2,
          uiOutput(ns("field"))
        )
      )
      ### UI DEV TOOLS ####

      ,fluidRow(
        column(
          width = 4,
          actionButton(ns("browser"), "browser")
        ),
        column(
          width = 8,
          verbatimTextOutput(ns("printcheck"))
        )
      ) # fluidRow DEV TOOLS

      ### END DEV TOOLS
    )
  )
}

#' database Server Functions
#'
#' @noRd
mod_database_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### DEV TOOLS ####
    output$printcheck = renderPrint({
      tryCatch({
        print("exists")
      },
      shiny.silent.error = function(e) {
        print("doesn't exist")
      }
      )
    })
    observeEvent(input$browser, {
      browser()
    })

    ### UI ACTION ####
    observeEvent(input$action, {
      if(input$action == "add_station"){
        output$field <- renderUI({
          req(input$action)

          if (input$action == "add_station") {
            fields = params_get_table_fields("station", db_con())
            lapply(fields, function(field) {
              textInput(field, label = field)
            })
          } else if (input$action == "add_parameter") {
            fields = params_get_table_fields("parameter", db_con())
            device_names <- params_get_device_names(db_con())
            lapply(fields, function(field) {
              if (field == "device_id") {
                selectInput(field, label = field, choices = device_names)
              } else {
                textInput(field, label = field)
              }
            })
          }
        })
      }
    })
  }
  )
}

## To be copied in the UI
# mod_database_ui("database_1")

## To be copied in the server
# mod_database_server("database_1")
