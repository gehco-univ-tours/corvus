#' valid UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
#' @importFrom shinybusy add_busy_bar
mod_valid_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        add_busy_bar(color = "#FF0000"),
        plotlyOutput(ns("plot"))
      ),
      fluidRow(
        column(
          width = 2,
          selectInput(inputId = ns("station"),
                      label = "Stations",
                      choices = params_get_stations(db_con()),
                      selected = "be")
        ),
        column(
          width = 2,
          selectInput(inputId = ns("parameter"),
                      label = "Parameter",
                      choices = NULL)
        ),
        column(
          width = 2,
          dateRangeInput(inputId = ns("date"),
                         label = "Date",
                         start =  "2019-01-01",
                         end =  "2019-02-28"
          )
        ),
        column(
          width = 1,
          tags$div(style = "margin-top: 20px;"),
          actionButton(inputId = ns("plot_valid_data"),
                       label = "Plot valid data"),
          tags$div(style = "margin-bottom: 20px;")
        )
      )
    )
  )
}

#' valid Server Functions
#'
#' @noRd
mod_valid_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### UI OUTPUT ####

    #### plot ####

    output$plot <- renderPlotly({
      r_locals$plot
    })

    ### EVENT ####

    ### REACTIVES ####

    r_locals <- reactiveValues(
      sensor_id = NULL,
      measurement = NULL,
      plot = NULL,
      plot_layer = 0,
      valid_plot_layer = FALSE
    )

    #### Station ####
    observeEvent(input$station, {
      r_locals$userinfo$station <- glue::glue("Station ID: {input$station}")
      updateSelectInput(session, "parameter",
                        choices = params_get_parameters(db_con(), input$station))
    })

    #### Parameter ####
    observeEvent(input$parameter, {
      req(input$parameter) # avoid error at init
      r_locals$sensor_id <- params_get_sensor_id(db_con(), input$station, input$parameter)
    })

    #### Plot bttn ####
    observeEvent(input$plot_valid_data, {
      r_locals$measurement <- data_get_measurement(con = db_con(),
                                                   sensor = r_locals$sensor_id,
                                                   start_date = input$date[1],
                                                   end_date = input$date[2])
      r_locals$plot <- plot_main(data = r_locals$measurement,
                                 y = "value_corr",
                                 y_title = input$parameter)

      r_locals$valid_plot_layer <- TRUE
      r_locals$plot_layer <- 1
    })

  })
}

## To be copied in the UI
# mod_valid_ui("valid_1")

## To be copied in the server
# mod_valid_server("valid_1")
