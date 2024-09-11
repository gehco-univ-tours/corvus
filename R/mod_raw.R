#' raw UI Function
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
mod_raw_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 2,
          selectInput(inputId = ns("station"),
                      label = "Stations",
                      choices = params_get_stations(db_con()),
                      selected = 3)
        ),
        column(
          width = 2,
          dateRangeInput(inputId = ns("date"),
                         label = "Date",
                         start =  get_min_max_date(db_con(), 3)$min_date,
                         end =  get_min_max_date(db_con(), 3)$max_date
          )
        ),
        column(
          width = 2,
          tags$div(style = "margin-top: 20px;"),
          actionButton(inputId = ns("plot_available_data"),
                       label = "Plot available data"),
          tags$div(style = "margin-bottom: 20px;")
        ),
        column(
          width = 2,
          tags$div(style = "margin-top: 20px;"),
          actionButton(inputId = ns("compile_raw_data"),
                       label = "Compile raw data")
        ),
        column(
          width = 2,
          tags$div(style = "margin-top: 20px;"),
          actionButton(inputId = ns("download_data"),
                       label = "Download raw data")
        ),
      ), # fluidRow
      fluidRow(
        add_busy_bar(color = "#FF0000"),
        plotlyOutput(ns("plot"),
                     height = "800px")
      ),
      tags$hr(), # add horizontal line
      fluidRow(
        column(
          width = 12,
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
    ) # fluidPage
  )
}

#' raw Server Functions
#'
#' @noRd
mod_raw_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### DEV TOOLS ####
    output$printcheck = renderPrint({
      tryCatch({
        # event_data("plotly_hover")
        print(r_locals$plot_layer)
        print(input$data)
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

    ### UI OUTPUT ####

    #### plot ####

    output$plot <- renderPlotly({
      r_locals$plot
    })

    #### userinfo ####
    output$userinfo <- renderPrint({
      r_locals$userinfo
    })

    ### REACTIVES ####

    r_locals <- reactiveValues(
      sensor_id = NULL,
      available_data = NULL,
      missing_data = NULL,
      plot = NULL,
      plot_layer = 0,
      valid_plot_layer = FALSE,
      userinfo = list("User information")
    )

    ### EVENT ####

    #### Station ####
    observeEvent(input$station, {
      r_locals$userinfo$station <- glue::glue("Station ID: {input$station}")
      updateSelectInput(session, "parameter",
                        choices = params_get_parameters(db_con(), input$station))
    })

    #### Plot bttn ####
    observeEvent(input$plot_available_data, {
      r_locals$plot <- plot_available_raw(station_id = input$station, date_start = input$date[1], date_end = input$date[2])
    })

    #### Compile bttn ####
    observeEvent(input$compile_raw_data, {
      if (input$station == 3){ # GB station
        data <- compile_gb(con = db_con())
      } else {
        data <- compile_raw(con = db_con(),
                            station = input$station,
                            parameter = input$parameter,
                            sensor = r_locals$sensor_id)
      }
      if (!is.null(data)) {
        r_locals$userinfo$processing <- data
      } else {
        r_locals$userinfo$processing <- "Fail compiling raw data"
      }
    })

    #### Download data ####
    observeEvent(input$download_data, {
      r_locals$userinfo$processing = "Downloading data"
      data <- download_gb()
      r_locals$userinfo$processing = data
    })

  })
}

## To be copied in the UI
# mod_raw_ui("raw_1")

## To be copied in the server
# mod_raw_server("raw_1")
