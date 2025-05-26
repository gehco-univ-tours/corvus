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
        plotlyOutput(ns("plot")),
        uiOutput(ns("date_ui"))
      ),
      fluidRow(
        column(
          width = 2,
          checkboxInput(inputId = ns("plot_corr_data"),
                        label = "Plot corr data",
                        value = FALSE)
        ),
        column(
          width = 2,
          checkboxInput(inputId = ns("plot_field"),
                        label = "Plot plot_field",
                        value = FALSE)
        )
      ),
      fluidRow(
        column(
          width = 2,
          selectInput(inputId = ns("station"),
                      label = "Stations",
                      choices = db_get_stations(db_con()))
        ),
        column(
          width = 2,
          selectInput(inputId = ns("parameter"),
                      label = "Parameter",
                      choices = NULL)
        ),
        # column(
        #   width = 2,
        #   dateRangeInput(inputId = ns("date"),
        #                  label = "Date",
        #                  # actual date - 1 month
        #                  start =  Sys.Date() - 180,
        #                  end =  Sys.Date()
        #                  )
        # ),
        column(
          width = 2,
          tags$div(style = "margin-top: 30px;"),
          actionButton(inputId = ns("plot_raw_data"),
                       label = "Plot raw data"),
          tags$div(style = "margin-bottom: 20px;")
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
          fluidRow(
            column(
              width = 6,
              uiOutput(ns("select_datestart_ui"))
            ),
            column(
              width = 6,
              uiOutput(ns("select_dateend_ui"))
            )
          )
          # uiOutput(ns("date_edit_ui")),
          # fluidRow(
          #   column(
          #     width = 6,
          #     uiOutput(ns("time_datestart_edit_ui"))
          #   ),
          #   column(
          #     width = 6,
          #     uiOutput(ns("time_dateend_edit_ui"))
          #   ),
          # ),
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
#' @importFrom plotly renderPlotly plotlyProxy plotlyProxyInvoke event_data
#' @importFrom shinyjs disable enable hide show
#' @importFrom dplyr mutate
#' @importFrom lubridate hm ymd ymd_hm
mod_edit_server <- function(id, r_globals){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ### DEV TOOLS ####
    output$printcheck = renderPrint({
      tryCatch({
        print(event_data("plotly_click"))
        print(paste0("keep_corr_plot_layer = ", r_locals$keep_corr_plot_layer))
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
      start_date_slider = TRUE,
      start_plot = TRUE,
      sensor_id = NULL,
      station_parameters = NULL,
      parameter = NULL,
      parameter_name = NULL,
      measurement = NULL,
      measurement_filter = NULL,
      min_max_date = NULL,
      date_min = NULL,
      date_max = NULL,
      plot_update = 0,
      date_slider_update = 0,
      plot_field = NULL,
      vertical_lines = c(marker = NULL, field = NULL),
      plot = NULL,
      keep_corr_plot_layer = FALSE,
      plot_layer = NULL,
      corr_plot = FALSE,
      edit_plot = FALSE,
      userinfo = list("User information"),
      edit_data = NULL,
      start_datetime_edit = NULL,
      end_datetime_edit = NULL,
      select_datestart = NULL,
      select_dateend = NULL
    )

    ### INIT ####
    shinyjs::disable("plot_corr_data")
    shinyjs::disable("plot_field")
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

    #### UI ####
    # update input$station if r_globals$station is not NULL
    observeEvent(r_globals$station, {
      updateSelectInput(session, "station", selected = r_globals$station)
    })

    #### Station ####
    observeEvent(input$station, {
      req(input$station)

      # update r_globals$station
      if (is.null(r_globals$station) || input$station != r_globals$station$id) {
        r_globals$station <- r_globals$all_stations[r_globals$all_stations$id == input$station,]
      }

      # get all parameters for the station and update parameter UI
      r_locals$station_parameters <- db_get_station_parameters(db_con(), r_globals$station$id)
      updateSelectInput(session, "parameter",
                        choices = r_locals$station_parameters)
      r_locals$parameter <- r_locals$station_parameters[1]
      r_locals$parameter_name <- names(which(r_locals$station_parameters == r_locals$parameter))

      # force sensor_id event if same input$parameter as previous station
      if (r_locals$parameter == input$parameter){
        r_locals$parameter_update = r_locals$parameter_update + 1
      }

      # User info
      r_locals$userinfo$station <- glue::glue("Station ID: {r_globals$station$id}")
    })

    #### Parameter ####
    observeEvent(list(input$parameter, r_locals$parameter_update), {
      req(input$parameter)
      r_locals$sensor_id <- db_get_sensor_id(db_con(), input$station, input$parameter)

      # user info
      r_locals$userinfo$parameter_id <- glue::glue("Parameter id: {input$parameter}")
      r_locals$userinfo$sensor_id <- glue::glue("Sensor id: {r_locals$sensor_id}")
    })

    #### Plot bttn ####
    observeEvent(input$plot_raw_data, {
      print("input$plot_raw_data")

      shinyjs::enable("plot_corr_data")
      shinyjs::enable("plot_edit")
      shinyjs::enable("plot_field")

      # set input$plot_corr_data and input$plot_edit to FALSE before new plot
      if (input$plot_corr_data == TRUE){
        r_locals$keep_corr_plot_layer = TRUE
        updateCheckboxInput(session, "plot_corr_data", value = FALSE)
      }

      # check if input$plot_edit exist then remove the trace if is TRUE
      if (!is.null(input$plot_edit) && input$plot_edit == TRUE){
        updateCheckboxInput(session, "plot_edit", value = FALSE)
      }

      min_max_date <- db_min_max_date(db_con(), r_locals$sensor_id)

      output$date_ui <- renderUI({
        sliderInput(ns("date"),
                    "",
                    min = min_max_date$min,
                    max = min_max_date$max,
                    value = c(min_max_date$max - 180, min_max_date$max),
                    timeFormat="%Y-%m-%d",
                    width = "100%",
                    timezone = Sys.timezone())
      })

      r_locals$measurement <- db_get_measurement(db_con(), r_locals$sensor_id,
                                                 min_max_date$min, min_max_date$max)

      print("input$plot_raw_data_end")

      # r_locals$measurement_filter <- r_locals$measurement %>%
      #   filter(timestamp >= r_locals$date_min & timestamp <= r_locals$date_max)
      #
      # r_locals$plot <- plot_main(data = r_locals$measurement_filter,
      #                            y = "value",
      #                            y_title = r_locals$parameter_name)

      # redraw corr plot if input$plot_corr_data was TRUE
      # if (r_locals$keep_corr_plot_layer == TRUE){
      #   updateCheckboxInput(session, "plot_corr_data", value = TRUE)
      #   r_locals$keep_corr_plot_layer <- FALSE
      # }

    })

    #### Date slider ####
    observeEvent(input$date, {
      req(input$date, r_locals$measurement)
      print("input$date")

      r_locals$measurement_filter <- r_locals$measurement %>%
        filter(timestamp >= input$date[1] & timestamp <= input$date[2])

      r_locals$plot_update <- r_locals$plot_update+1

      # user info
      r_locals$userinfo$date <- glue::glue("Date: {input$date}")
    })

    #### Marker plot ####
    observeEvent(event_data("plotly_click"), {
      r_locals$vertical_lines$marker <- plot_lines(as.POSIXct(event_data("plotly_click")$x), "orange")
      plotlyProxy("plot") %>%
        plotlyProxyInvoke("relayout",  list (shapes = c(r_locals$vertical_lines$marker$shapes,
                                                        r_locals$vertical_lines$field$shapes)))
    })

    #### update plot ####
    observeEvent(r_locals$plot_update, {
      print("plot_update")

      if (r_locals$start_plot == TRUE){
        r_locals$plot <- plot_main(data = r_locals$measurement_filter,
                                   y = "value",
                                   y_title = r_locals$parameter_name,
                                   date_min = input$date[1],
                                   date_max = input$date[2])
        r_locals$start_plot <- FALSE
      } else {
        # application already started : update plot
        proxy_plot <- plot_update_main(r_locals$measurement_filter, "value", r_locals$parameter_name,
                                       date_min = input$date[1],
                                       date_max = input$date[2])

        plotlyProxy("plot") %>%
          plotlyProxyInvoke("deleteTraces", 0) %>%
          plotlyProxyInvoke("addTraces", proxy_plot$trace, 0) %>%
          plotlyProxyInvoke("relayout", proxy_plot$layout, 0)
        print("plot_update_main")
      }

      if (r_locals$keep_corr_plot_layer == TRUE){
        updateCheckboxInput(session, "plot_corr_data", value = TRUE)
        r_locals$keep_corr_plot_layer <- FALSE
      }

    }, ignoreInit=TRUE)

    #### Plot corr data bttn ####
    observeEvent(input$plot_corr_data, {

      if (input$plot_corr_data == TRUE){
        plot_corr <- plot_add_corr_trace(data = r_locals$measurement,
                                       y = "value",
                                       y_label = r_locals$parameter_name)

        plotlyProxy("plot") %>%
          plotlyProxyInvoke("addTraces", plot_corr, 0) # z-index = 0 to be below valid data

      } else {
        plotlyProxy("plot") %>%
          plotlyProxyInvoke("deleteTraces", 0)
      }
    })

    #### Plot plot_field bttn ####
    observeEvent(input$plot_field, {

      if (input$plot_field == TRUE){

        r_locals$plot_field <- db_get_field(con = db_con(),
                                            station_id = r_globals$station$id,
                                            start_date = input$date[1],
                                            end_date = input$date[2])

        r_locals$vertical_lines$field <- plot_lines(as.POSIXct(r_locals$plot_field[["timestamp"]]), "green")

        plotlyProxy("plot") %>%
          plotlyProxyInvoke("relayout",  list (shapes = c(r_locals$vertical_lines$marker$shapes,
                                                          r_locals$vertical_lines$field$shapes)))

      } else {
        r_locals$vertical_lines$field <- NULL
        plotlyProxy("plot") %>%
          plotlyProxyInvoke("relayout",  list (shapes = c(r_locals$vertical_lines$marker$shapes,
                                                          r_locals$vertical_lines$field$shapes)))
      }
    })

    #### Edition mode UI ####
    observeEvent(input$edition, {
      if (input$edition == TRUE) {
        output$author_ui <- renderUI({
          selectInput(inputId = ns("author"),
                      label = "Author",
                      choices = db_get_authors(db_con()))
        })
        output$correction_ui <- renderUI({
          selectInput(inputId = ns("correction"),
                      label = "Correction",
                      choices = db_get_correction_type(db_con()))
        })
        output$select_datestart_ui <- renderUI({
          switchInput(inputId = ns("select_datestart"),
                      label = "Select date/time start",
                      onStatus = "success")
        })
        output$select_dateend_ui <- renderUI({
          switchInput(inputId = ns("select_dateend"),
                      label = "Select date/time end",
                      onStatus = "success")
        })
        # output$date_edit_ui <- renderUI({
        #   dateRangeInput(inputId = ns("date_edit"),
        #                  label = "Date",
        #                  start =  input$date[1],
        #                  end =  input$date[2],
        #                  startview = "month",
        #                  min = input$date[1],
        #                  max = input$date[2])
        # })
        # output$time_datestart_edit_ui <- renderUI({
        #   timeInput(inputId = ns("time_datestart_edit"),
        #             label = "Date start time",
        #             value = "00:00")
        # })
        # output$time_dateend_edit_ui <- renderUI({
        #   timeInput(inputId = ns("time_dateend_edit"),
        #             label = "Date end time",
        #             value = "00:00")
        # })
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
        output$select_datestart_ui <- renderUI({
          NULL
        })
        output$select_dateend_ui <- renderUI({
          NULL
        })
        # output$date_edit_ui <- renderUI({
        #   NULL
        # })
        # output$time_datestart_edit_ui <- renderUI({
        #   NULL
        # })
        # output$time_dateend_edit_ui <- renderUI({
        #   NULL
        # })
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

    ##### Select date time ####

    observeEvent(input$select_datestart, {
      if (input$select_datestart == TRUE) {
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
            mutate(edit = value + input$offset_edit)
        } else if (input$correction == 2) { # drift
          r_locals$edit_data <- r_locals$measurement %>%
            filter(timestamp >= r_locals$start_datetime_edit & timestamp <= r_locals$end_datetime_edit) %>%
            mutate(edit = data_edit_drift(timestamp, value, input$drift_edit))
        }

        plot_edit <- plot_add_edit_trace(data = r_locals$edit_data,
                                         y = "edit",
                                         y_label = r_locals$parameter_name)

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

        # set input$plot_corr_data and input$plot_edit to FALSE before new plot
        if (input$plot_corr_data == TRUE){
          r_locals$keep_corr_plot_layer = TRUE
          updateCheckboxInput(session, "plot_corr_data", value = FALSE)
        }

        if (input$plot_edit == TRUE){
          updateCheckboxInput(session, "plot_edit", value = FALSE)
        }

        r_locals$measurement <- data_get_measurement(con = db_con(),
                                                     sensor = r_locals$sensor_id,
                                                     start_date = input$date[1],
                                                     end_date = input$date[2])

        r_locals$plot <- plot_main(data = r_locals$measurement,
                                   y = "value",
                                   y_title = r_locals$parameter_name)

        # redraw corr plot if input$plot_corr_data was TRUE
        if (r_locals$keep_corr_plot_layer == TRUE){
          updateCheckboxInput(session, "plot_corr_data", value = TRUE)
          r_locals$keep_corr_plot_layer <- FALSE
        }

      } else {
        r_locals$userinfo$processing <- "Fail insert edits"
      }
    })
  })
}
