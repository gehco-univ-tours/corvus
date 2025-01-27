#' edit UI Function
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
#' @importFrom shinyjs useShinyjs
mod_edit_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      useShinyjs(),  # Initialize shinyjs
      fluidRow(
        add_busy_bar(color = "#FF0000"),
        plotlyOutput(ns("plot"))
      ),
      fluidRow(
        column(
          width = 2,
          checkboxInput(inputId = ns("plot_raw_data"),
                        label = "Plot raw data",
                        value = FALSE)
        ),
      ),
      fluidRow(
        column(
          width = 2,
          selectInput(inputId = ns("station"),
                      label = "Stations",
                      choices = params_get_stations(db_con()))
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
                         # actual date - 1 month
                         start =  Sys.Date() - 30,
                         end =  Sys.Date()
                         )
        ),
        column(
          width = 2,
          tags$div(style = "margin-top: 20px;"),
          actionButton(inputId = ns("plot_valid_data"),
                       label = "Plot valid data"),
          tags$div(style = "margin-bottom: 20px;")
        ),
      #   column(
      #     width = 1,
      #     tags$div(style = "margin-top: 20px;"),
      #     uiOutput(ns("plot_raw_data_ui")),
      #     tags$div(style = "margin-bottom: 20px;")
      #   ),
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

#' edit Server Functions
#'
#' @noRd
#' @importFrom plotly renderPlotly plotlyProxy plotlyProxyInvoke
#' @importFrom shinyjs disable enable hide show
#' @importFrom dplyr mutate
#' @importFrom lubridate hm ymd ymd_hm
mod_edit_server <- function(id, r_globals){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ### DEV TOOLS ####
    output$printcheck = renderPrint({
      tryCatch({
        # event_data("plotly_hover")
        print(paste0("keep_raw_plot_layer = ", r_locals$keep_raw_plot_layer))
        print(paste0("input$plot_raw_data = ", input$plot_raw_data))
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
      sensor_id = NULL,
      measurement = NULL,
      plot = NULL,
      keep_raw_plot_layer = FALSE,
      # valid_plot_layer = FALSE,
      plot_layer = NULL,
      corr_plot = FALSE,
      edit_plot = FALSE,
      userinfo = list("User information"),
      edit_data = NULL,
      start_datetime_edit = NULL,
      end_datetime_edit = NULL
    )

    ### INIT ####
    shinyjs::disable("plot_raw_data")
    shinyjs::disable("plot_edit")

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

    ### UI ####
    # update input$station if r_globals$station is not NULL
    observeEvent(r_globals$station, {
      updateSelectInput(session, "station", selected = r_globals$station)
    })

    #### Station ####
    observeEvent(input$station, {
      if (is.null(r_globals$station) || input$station != r_globals$station$id) {
        # update r_globals$station
        r_globals$station <- r_globals$all_stations[r_globals$all_stations$id == input$station,]
      }
        r_locals$userinfo$station <- glue::glue("Station ID: {r_globals$station$id}")
        updateSelectInput(session, "parameter",
                          choices = params_get_parameters(db_con(), r_globals$station$id))
    })

    #### Parameter ####
    observeEvent(input$parameter, {
      req(input$parameter) # avoid error at init
      r_locals$sensor_id <- params_get_sensor_id(db_con(), input$station, input$parameter)
      r_locals$userinfo$parameter <- glue::glue("Sensor ID: {r_locals$sensor_id}")
    })

    #### Plot bttn ####
    observeEvent(input$plot_valid_data, {

      shinyjs::enable("plot_raw_data")
      shinyjs::enable("plot_edit")

      # set input$plot_raw_data and input$plot_edit to FALSE before new plot
      if (input$plot_raw_data == TRUE){
        r_locals$keep_raw_plot_layer = TRUE
        updateCheckboxInput(session, "plot_raw_data", value = FALSE)
      }

      if (input$plot_edit == TRUE){
        updateCheckboxInput(session, "plot_edit", value = FALSE)
      }

      r_locals$measurement <- data_get_measurement(con = db_con(),
                                                   sensor = r_locals$sensor_id,
                                                   start_date = input$date[1],
                                                   end_date = input$date[2])

      r_locals$plot <- plot_main(data = r_locals$measurement,
                                 y = "value_corr",
                                 y_title = input$parameter)

      # redraw raw plot if input$plot_raw_data was TRUE
      if (r_locals$keep_raw_plot_layer == TRUE){
        updateCheckboxInput(session, "plot_raw_data", value = TRUE)
        r_locals$keep_raw_plot_layer <- FALSE
      }

    })

    #### Plot raw data bttn ####
    observeEvent(input$plot_raw_data, {

      if (input$plot_raw_data == TRUE){
        plot_raw <- plot_add_raw_trace(data = r_locals$measurement,
                                       y = "value",
                                       y_label = input$parameter)

        plotlyProxy("plot") %>%
          plotlyProxyInvoke("addTraces", plot_raw, 0) # z-index = 0 to be below valid data

      } else {
        plotlyProxy("plot") %>%
          plotlyProxyInvoke("deleteTraces", 0)
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
          checkboxInput(inputId = ns("plot_edit"),
                       label = "Plot change",
                       value = FALSE)
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

      if(input$plot_edit == TRUE){

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
            plotlyProxyInvoke("addTraces", plot_edit, 2) # z-index = 2 to be above valid data

      } else {
        plotlyProxy("plot") %>%
          plotlyProxyInvoke("deleteTraces", 2)
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

        # set input$plot_raw_data and input$plot_edit to FALSE before new plot
        if (input$plot_raw_data == TRUE){
          r_locals$keep_raw_plot_layer = TRUE
          updateCheckboxInput(session, "plot_raw_data", value = FALSE)
        }

        if (input$plot_edit == TRUE){
          updateCheckboxInput(session, "plot_edit", value = FALSE)
        }

        r_locals$measurement <- data_get_measurement(con = db_con(),
                                                     sensor = r_locals$sensor_id,
                                                     start_date = input$date[1],
                                                     end_date = input$date[2])

        r_locals$plot <- plot_main(data = r_locals$measurement,
                                   y = "value_corr",
                                   y_title = input$parameter)

        # redraw raw plot if input$plot_raw_data was TRUE
        if (r_locals$keep_raw_plot_layer == TRUE){
          updateCheckboxInput(session, "plot_raw_data", value = TRUE)
          r_locals$keep_raw_plot_layer <- FALSE
        }

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
