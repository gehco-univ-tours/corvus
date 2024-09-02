#' raw_data UI Function
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
#' @importFrom shinyWidgets switchInput timeInput
mod_raw_data_ui <- function(id){
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
        ),
        column(
          width = 1,
          tags$div(style = "margin-top: 20px;"),
          uiOutput(ns("plot_raw_data_ui")),
          tags$div(style = "margin-bottom: 20px;")
        ),
        column(
          width = 3,
          tags$div(style = "margin-top: 20px;"),
          actionButton(inputId = ns("compile_raw_data"),
                       label = "Compile raw data")
        ),
      ), # fluidRow
      tags$hr(), # add horizontal line
      #### Edition mode UI ####
      fluidRow(
        column(
          width = 2,
          switchInput(inputId = ns("edition"),
                      label = "Edition",
                      onStatus = "success"),
          uiOutput(ns("correction_ui")),
          uiOutput(ns("author_ui"))
        ),
        column(
          width = 4,
          uiOutput(ns("date_edit_ui")),
          fluidRow(
            column(
              width = 6,
              uiOutput(ns("time_datestart_edit_ui"))
            ),
            column(
              width = 6,
              uiOutput(ns("time_dateend_edit_ui"))
            ),
          ),
        ),
        column(
          width = 3,
          uiOutput(ns("value_offset_ui")),
          uiOutput(ns("value_drift_ui")),
          uiOutput(ns("plot_edit_ui")),
          tags$div(style = "margin-top: 20px;"),
          uiOutput(ns("validate_edit_ui"))
        ),
        column(
          width = 3,
          uiOutput(ns("comment_ui"))
        )
      ), # fluidRow
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

      ### END DEV TOOLS
    )
  )
}

#' raw_data Server Functions
#'
#' @noRd
#' @importFrom plotly renderPlotly plotlyProxy plotlyProxyInvoke
#' @importFrom dplyr mutate
#' @importFrom lubridate hm ymd ymd_hm
mod_raw_data_server <- function(id){
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

    ### REACTIVES ####

    r_locals <- reactiveValues(
      station_id = NULL,
      sensor_id = NULL,
      measurement = NULL,
      plot = NULL,
      valid_plot_layer = FALSE,
      raw_plot_layer = FALSE,
      edit_plot_layer = FALSE,
      plot_layer = NULL,
      corr_plot = FALSE,
      edit_plot = FALSE,
      userinfo = list("User information"),
      edit_data = NULL,
      start_datetime_edit = NULL,
      end_datetime_edit = NULL
    )

    ### UI OUTPUT ####

    #### plot ####

    output$plot <- renderPlotly({
      r_locals$plot
    })

    #### userinfo ####
    output$userinfo <- renderPrint({
      r_locals$userinfo
    })

    ### EVENT ####

    #### Station ####
    observeEvent(input$station, {
      r_locals$station_id <- params_get_station_id(db_con(), input$station)
      r_locals$userinfo$station <- glue::glue("Station ID: {r_locals$station_id}")
      updateSelectInput(session, "parameter",
                        choices = params_get_parameters(db_con(), input$station))
    })

    #### Parameter ####
    observeEvent(input$parameter, {
      r_locals$sensor_id <- params_get_sensor_id(db_con(), input$station, input$parameter)
      r_locals$userinfo$parameter <- glue::glue("Sensor ID: {r_locals$sensor_id}")
    })

    #### Compile bttn ####
    observeEvent(input$compile_raw_data, {
      r_locals$userinfo$processing = "Compiling raw data"
        data <- compile_raw(con = db_con(),
                            station = input$station,
                            parameter = input$parameter,
                            sensor = r_locals$sensor_id)
        if (!is.null(data)) {
          r_locals$userinfo$processing = data
        } else {
          r_locals$userinfo$processing <- "Fail compiling raw data"
        }
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

      # add renderUI action button for plot_raw_data_ui
      output$plot_raw_data_ui <- renderUI({
        actionButton(inputId = ns("plot_raw_data"),
                     label = "Plot raw data")
      })
    })

    #### Plot raw data bttn ####
    observeEvent(input$plot_raw_data, {

      plot_raw <- plot_add_raw_trace(data = r_locals$measurement,
                                       y = "value",
                                       y_label = input$parameter)

      plotlyProxy("plot") %>%
        plotlyProxyInvoke("deleteTraces", r_locals$plot_layer) %>%
        plotlyProxyInvoke("addTraces", plot_raw, r_locals$plot_layer)

      if (r_locals$raw_plot_layer == FALSE){
        r_locals$plot_layer <- r_locals$plot_layer+1
        r_locals$raw_plot_layer <- TRUE
      }

    })

    #### Edition mode UI ####
    observeEvent(input$edition, {
      if (input$edition == TRUE) {
        output$author_ui <- renderUI({
          selectInput(inputId = ns("author"),
                      label = "Author",
                      choices = params_get_authors(db_con()))
        })
        output$correction_ui <- renderUI({
          selectInput(inputId = ns("correction"),
                      label = "Correction",
                      choices = params_get_correction_type(db_con()))
        })
        output$date_edit_ui <- renderUI({
          dateRangeInput(inputId = ns("date_edit"),
                         label = "Date",
                         start =  input$date[1],
                         end =  input$date[2],
                         startview = "month",
                         min = input$date[1],
                         max = input$date[2])
        })
        output$time_datestart_edit_ui <- renderUI({
          timeInput(inputId = ns("time_datestart_edit"),
                    label = "Date start time",
                    value = "00:00")
        })
        output$time_dateend_edit_ui <- renderUI({
          timeInput(inputId = ns("time_dateend_edit"),
                    label = "Date end time",
                    value = "00:00")
        })
        output$plot_edit_ui <- renderUI({
          actionButton(inputId = ns("plot_edit"),
                       label = "Plot change")
        })
        output$validate_edit_ui <- renderUI({
          actionButton(inputId = ns("validate_edit"),
                       label = "Validate")
        })
        output$comment_ui <- renderUI({
          textAreaInput(inputId = ns("comment"),
                        label = "Comment",
                        value = "")
        })

      } else {
        output$author_ui <- renderUI({
          NULL
        })
        output$correction_ui <- renderUI({
          NULL
        })
        output$date_edit_ui <- renderUI({
          NULL
        })
        output$time_datestart_edit_ui <- renderUI({
          NULL
        })
        output$time_dateend_edit_ui <- renderUI({
          NULL
        })
        output$value_edit_ui <- renderUI({
          NULL
        })
        output$plot_edit_ui <- renderUI({
          NULL
        })
        output$validate_edit_ui <- renderUI({
          NULL
        })
        output$comment_ui <- renderUI({
          NULL
        })
      }
    })

    ##### Edit date ####

    observeEvent(c(ymd(input$date_edit), hm(input$time_datestart_edit), hm(input$time_dateend_edit)), {
      r_locals$start_datetime_edit <- ymd_hm(paste(input$date_edit[1], input$time_datestart_edit, sep = " "))
      r_locals$end_datetime_edit <- ymd_hm(paste(input$date_edit[2], input$time_dateend_edit, sep = " "))

      r_locals$userinfo$edition <- glue::glue("Edition from {r_locals$start_datetime_edit} to {r_locals$end_datetime_edit}")
    })

    ##### Edition mode ####
    observeEvent(input$correction, {

      if (input$correction == 1) { # offset

        output$value_offset_ui <- renderUI({
          numericInput(inputId = ns("offset_edit"),
                       label = "Offset value",
                       value = 0)
        })
        output$value_drift_ui <- renderUI({
          NULL
        })
      } else if (input$correction == 2){ # drift
        output$value_drift_ui <- renderUI({
          numericInput(inputId = ns("drift_edit"),
                       label = "Drift end value",
                       value = 0)
        })
        output$value_offset_ui <- renderUI({
          NULL
        })
      } else {
        output$value_offset_ui <- renderUI({
          NULL
        })
        output$value_drift_ui <- renderUI({
          NULL
        })
      }
    })

    #### Plot change ####
    observeEvent(input$plot_edit, {

      if (input$correction == 1) { # offset
        r_locals$edit_data <- r_locals$measurement %>%
          filter(timestamp >= r_locals$start_datetime_edit & timestamp <= r_locals$end_datetime_edit) %>%
          mutate(edit = value_corr + input$offset_edit)
      } else if (input$correction == 2) { # drift
        r_locals$edit_data <- r_locals$measurement %>%
          filter(timestamp >= r_locals$start_datetime_edit & timestamp <= r_locals$end_datetime_edit) %>%
          mutate(edit = data_edit_drift(timestamp, value_corr, input$drift_edit))
      }

      plot_edit <- plot_add_edit_trace(data = r_locals$edit_data,
                                       y = "edit",
                                       y_label = input$parameter)

        plotlyProxy("plot") %>%
          plotlyProxyInvoke("deleteTraces", r_locals$plot_layer) %>%
          plotlyProxyInvoke("addTraces", plot_edit, r_locals$plot_layer)

        if (r_locals$edit_plot_layer == FALSE){
          r_locals$plot_layer <- r_locals$plot_layer+1
          r_locals$edit_plot_layer <- TRUE
        }
    })

    #### Validate change ####
    observeEvent(input$validate_edit, {
      data <- data_update_measurement(con = db_con(),
                                      data = r_locals$edit_data,
                                      sensor = r_locals$sensor_id,
                                      author = input$author,
                                      correction_type = input$correction,
                                      value = input$offset_edit,
                                      comment = input$comment)

      if (!is.null(data)) {
        r_locals$userinfo$processing = data
        plotlyProxy("plot") %>%
          plotlyProxyInvoke("deleteTraces", 1)

        r_locals$measurement <- data_get_measurement(con = db_con(),
                                                     sensor = r_locals$sensor_id,
                                                     start_date = input$date[1],
                                                     end_date = input$date[2])

        r_locals$plot <- plot_main(data = r_locals$measurement,
                                   y = "value_corr",
                                   y_title = input$parameter)

        r_locals$valid_plot_layer <- TRUE
        r_locals$plot_layer <- 1
      } else {
        r_locals$userinfo$processing <- "Fail insert edits"
      }
    })
  })
}

## To be copied in the UI
# mod_raw_data_ui("raw_data_1")

## To be copied in the server
# mod_raw_data_server("raw_data_1")
