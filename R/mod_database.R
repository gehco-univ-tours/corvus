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
                      choices = params_get_db_actions()),
          actionButton(inputId = ns("submit"),
                       label = "Submit")
        ),
        column(
          width = 2,
          uiOutput(ns("field"))
        ),
        column(
          width = 6,
          verbatimTextOutput(ns("userinfo"))
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
#' @importFrom stringr str_remove
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

    ### REACTIVES ####

    r_locals <- reactiveValues(
      userinfo = list("User information"),
      table_name = NULL,
      fields = NULL
    )

    #### userinfo ####
    output$userinfo <- renderPrint({
      r_locals$userinfo
    })

    ### UI ACTION ####
    observeEvent(input$action, {
      req(input$action)
      r_locals$table_name <- input$action
      r_locals$fields = params_get_table_fields(r_locals$table_name, db_con())
      # Station
      output$field <- renderUI({
        if (input$action == "station") {
          lapply(r_locals$fields, function(field) {
            textInput(ns(field), label = field)
          })
        }
        # parameter
        else if (input$action == "parameter") {
          devices <- params_get_devices(db_con())
          lapply(r_locals$fields, function(field) {
            if (field == "device_id") {
              selectInput(ns(field), label = field, choices = devices)
            } else {
              textInput(ns(field), label = field)
            }
          })
        }
        # author
        else if (input$action == "author"){
          lapply(r_locals$fields, function(field) {
            textInput(ns(field), label = field)
          })
        }
        # sensor
        else if (input$action == "sensor"){
          stations <- params_get_stations(db_con())
          parameters <- params_get_parameters(db_con())
          lapply(r_locals$fields, function(field) {
            if (field == "station_id") {
              selectInput(ns(field), label = field, choices = stations)
            } else if (field == "parameter_id") {
              selectInput(ns(field), label = field, choices = parameters)
            } else {
              textInput(ns(field), label = field)
            }
          })
        }
        # device
        else if (input$action == "device"){
          device_models <- params_get_device_models(db_con())
          lapply(r_locals$fields, function(field) {
            if (field == "device_model_id") {
              selectInput(ns(field), label = field, choices = device_models)
            } else {
              textInput(ns(field), label = field)
            }
          })
        }
        # device model
        else if (input$action == "device_model"){
          lapply(r_locals$fields, function(field) {
            textInput(ns(field), label = field)
          })
        }
      })
    })

    ### SUBMIT ACTION ####
    observeEvent(input$submit, {
      input_values <- sapply(r_locals$fields, function(field) input[[field]], simplify = FALSE)
      r_locals$userinfo$database <- db_insert_data(db_con(), r_locals$table_name, input_values)
    })
  })
}

## To be copied in the UI
# mod_database_ui("database_1")

## To be copied in the server
# mod_database_server("database_1")
