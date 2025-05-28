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
        # uiOutput(ns("date_ui"))
      ),
      fluidRow(
        column(
          width = 2,
          checkboxInput(inputId = ns("plot_raw_data"),
                        label = "Plot raw data",
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
        column(
          width = 2,
          tags$div(style = "margin-top: 30px;"),
          actionButton(inputId = ns("plot_corr_data"),
                       label = "Plot corr data"),
          tags$div(style = "margin-bottom: 20px;")
        ),
        column(
          width = 2,
          tags$div(style = "margin-top: 30px;"),
          actionButton(inputId = ns("clean_all"),
                       label = "Clean all"),
          tags$div(style = "margin-bottom: 20px;")
        ),
      ), # fluidRow
      fluidRow(
        column(
          width = 12,
          sliderInput(ns("date"),
                      "",
                      min = db_min_max_date(db_con())$min,
                      max = db_min_max_date(db_con())$max,
                      value = c(db_min_max_date(db_con())$max - 180, db_min_max_date(db_con())$max),
                      timeFormat="%Y-%m-%d",
                      width = "100%",
                      timezone = "UTC"
          )
        )
      ),
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
        print(paste0("z-index =", r_locals$plot_index))
        print(paste0("keep_raw_plot_layer = ", r_locals$keep_raw_plot_layer))
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
      # measurement_filter = NULL,
      min_max_date = NULL,
      date_min = NULL,
      date_max = NULL,
      plot_update = 0,
      plot_index = 1,
      date_slider_update = 0,
      plot_field = NULL,
      vertical_lines = c(marker = NULL, field = NULL, start = NULL, end = NULL),
      plot = NULL,
      keep_raw_plot_layer = FALSE,
      plot_layer = NULL,
      corr_plot = FALSE,
      edit_plot = FALSE,
      plot_edit_exist = FALSE,
      plot_raw_exist = FALSE,
      userinfo = list("User information"),
      edit_data = NULL,
      start_datetime_edit = NULL,
      end_datetime_edit = NULL,
      select_datestart = NULL,
      select_dateend = NULL
    )

    ### INIT ####
    shinyjs::disable("plot_raw_data")
    shinyjs::disable("plot_field")

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
    observeEvent(input$plot_corr_data, {

      shinyjs::enable("plot_raw_data")
      shinyjs::enable("plot_field")

      # set input$plot_raw_data and input$plot_edit to FALSE before new plot
      # if (input$plot_raw_data == TRUE){
      #   r_locals$keep_raw_plot_layer = TRUE
      #   updateCheckboxInput(session, "plot_raw_data", value = FALSE)
      # }

      # check if input$plot_edit exist then remove the trace if is TRUE
      # if (!is.null(input$plot_edit) && input$plot_edit == TRUE){
      #   updateCheckboxInput(session, "plot_edit", value = FALSE)
      # }

      # min_max_date <- db_min_max_date(db_con(), r_locals$sensor_id)

      # output$date_ui <- renderUI({
      #   sliderInput(ns("date"),
      #               "",
      #               min = min_max_date$min,
      #               max = min_max_date$max,
      #               value = c(min_max_date$max - 180, min_max_date$max),
      #               timeFormat="%Y-%m-%d",
      #               width = "100%"
      #               # timezone = Sys.timezone()
      #   )
      # })

      r_locals$measurement <- db_get_measurement(db_con(), r_locals$sensor_id,
                                                 input$date[1], input$date[2])

      r_locals$plot <- plot_main(data = r_locals$measurement,
                                 y = "value_corr",
                                 y_title = r_locals$parameter_name,
                                 date_min = input$date[1],
                                 date_max = input$date[2])

      r_locals$plot_index <- 0

    })

    #### Date slider ####
    # observeEvent(input$date, {
    #   req(input$date, r_locals$measurement)
    #   print("input$date")
    #
    #   r_locals$measurement_filter <- r_locals$measurement %>%
    #     filter(timestamp >= input$date[1] & timestamp <= input$date[2])
    #
    #   r_locals$plot_update <- r_locals$plot_update+1
    #
    #   # user info
    #   r_locals$userinfo$date <- glue::glue("Date: {input$date}")
    # })

    #### Update plot ####
    # observeEvent(r_locals$plot_update, {
    #   print("plot_update")
    #
    #   if (r_locals$start_plot == TRUE){
    #     r_locals$plot <- plot_main(data = r_locals$measurement_filter,
    #                                y = "value_corr",
    #                                y_title = r_locals$parameter_name,
    #                                date_min = input$date[1],
    #                                date_max = input$date[2])
    #     r_locals$start_plot <- FALSE
    #   } else {
    #     # application already started : update plot
    #     proxy_plot <- plot_update_main(r_locals$measurement_filter, "value_corr", r_locals$parameter_name,
    #                                    date_min = input$date[1],
    #                                    date_max = input$date[2])
    #
    #     plotlyProxy("plot") %>%
    #       plotlyProxyInvoke("deleteTraces", 0) %>%
    #       plotlyProxyInvoke("addTraces", proxy_plot$trace, 0) %>%
    #       plotlyProxyInvoke("relayout", proxy_plot$layout, 0)
    #     print("plot_update_main")
    #   }
    #
    #   if (r_locals$keep_raw_plot_layer == TRUE){
    #     updateCheckboxInput(session, "plot_raw_data", value = TRUE)
    #     r_locals$keep_raw_plot_layer <- FALSE
    #   }
    #
    # }, ignoreInit=TRUE)

    #### Marker plot ####
    observeEvent(event_data("plotly_click"), {
      r_locals$vertical_lines$marker <- plot_lines(as.POSIXct(event_data("plotly_click")$x, tz = 'UTC'), "orange")
      plotlyProxy("plot") %>%
        plotlyProxyInvoke("relayout",  list (shapes = c(r_locals$vertical_lines$marker$shapes,
                                                        r_locals$vertical_lines$field$shapes,
                                                        r_locals$vertical_lines$start$shapes,
                                                        r_locals$vertical_lines$end$shapes)))
    })

    #### Plot raw data bttn ####
    observeEvent(input$plot_raw_data, {

      if (input$plot_raw_data == TRUE){
        plot_raw <- plot_add_trace(data = r_locals$measurement,
                                   y = "value",
                                   y_label = r_locals$parameter_name)

        # r_locals$plot_index <- r_locals$plot_index + 1

        plotlyProxy("plot") %>%
          plotlyProxyInvoke("addTraces", plot_raw)  # z-index = 0 to be below raw data

        r_locals$plot_raw_exist <- TRUE

        if (r_locals$plot_edit_exist == TRUE){
          # move edit trace to the top
          plotlyProxy("plot") %>%
            plotlyProxyInvoke("moveTraces", 2, 1)
        }

      } else {

        plotlyProxy("plot") %>%
          plotlyProxyInvoke("deleteTraces", 1)
        # r_locals$plot_index <- r_locals$plot_index - 1

        r_locals$plot_raw_exist <- FALSE

        if (r_locals$plot_edit_exist == TRUE){
          # move edit trace to the top
          plotlyProxy("plot") %>%
            plotlyProxyInvoke("moveTraces", 2, 1)
        }
      }
    })

    #### Plot plot_field bttn ####
    observeEvent(input$plot_field, {

      if (input$plot_field == TRUE){

        r_locals$plot_field <- db_get_field(con = db_con(),
                                            station_id = r_globals$station$id,
                                            start_date = input$date[1],
                                            end_date = input$date[2])

        r_locals$vertical_lines$field <- plot_lines(as.POSIXct(r_locals$plot_field[["timestamp"]], tz = 'UTC'), "green")

        plotlyProxy("plot") %>%
          plotlyProxyInvoke("relayout",  list (shapes = c(r_locals$vertical_lines$marker$shapes,
                                                          r_locals$vertical_lines$field$shapes,
                                                          r_locals$vertical_lines$start$shapes,
                                                          r_locals$vertical_lines$end$shapes)))

      } else {
        r_locals$vertical_lines$field <- NULL
        plotlyProxy("plot") %>%
          plotlyProxyInvoke("relayout",  list (shapes = c(r_locals$vertical_lines$marker$shapes,
                                                          r_locals$vertical_lines$field$shapes,
                                                          r_locals$vertical_lines$start$shapes,
                                                          r_locals$vertical_lines$end$shapes)))
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
          actionButton(inputId = ns("select_datestart"),
                      label = "Start date")
        })
        output$select_dateend_ui <- renderUI({
          actionButton(inputId = ns("select_dateend"),
                      label = "End date")
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

        shinyjs::delay(100, shinyjs::disable("plot_edit"))
        shinyjs::delay(100, shinyjs::disable("validate_edit"))

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
      r_locals$select_datestart <- event_data("plotly_click")$x
      # create a blue vertical line at select_datestart
      r_locals$vertical_lines$start <- plot_lines(as.POSIXct(r_locals$select_datestart, tz = 'UTC'), "cornflowerblue")

      plotlyProxy("plot") %>%
        plotlyProxyInvoke("relayout",  list (shapes = c(r_locals$vertical_lines$marker$shapes,
                                                        r_locals$vertical_lines$field$shapes,
                                                        r_locals$vertical_lines$start$shapes,
                                                        r_locals$vertical_lines$end$shapes)))
      # info
      r_locals$userinfo$select_datestart <- glue::glue("Select start date: {r_locals$select_datestart}")

    })

    observeEvent(input$select_dateend, {
      r_locals$select_dateend <- event_data("plotly_click")$x
      # create a red vertical line at select_dateend
      r_locals$vertical_lines$end <- plot_lines(as.POSIXct(r_locals$select_dateend, tz = 'UTC'), "red")

      plotlyProxy("plot") %>%
        plotlyProxyInvoke("relayout",  list (shapes = c(r_locals$vertical_lines$marker$shapes,
                                                        r_locals$vertical_lines$field$shapes,
                                                        r_locals$vertical_lines$start$shapes,
                                                        r_locals$vertical_lines$end$shapes)))
      # info
      r_locals$userinfo$select_dateend <- glue::glue("Select end date: {r_locals$select_dateend}")
    })

    ##### Enable/disable plot_edit button ####
    observeEvent(c(r_locals$select_datestart, r_locals$select_dateend), {

      shinyjs::disable("validate_edit")

      # check edit condition
      if (!is.null(r_locals$select_datestart) && !is.null(r_locals$select_dateend) &&
          r_locals$select_dateend > r_locals$select_datestart) {
        shinyjs::enable("plot_edit")
      } else {
        shinyjs::disable("plot_edit")
      }
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

      # if(input$plot_edit == TRUE){

      if (input$correction == 1) { # offset
        r_locals$edit_data <- r_locals$measurement %>%
          filter(timestamp >= r_locals$select_datestart & timestamp <= r_locals$select_dateend) %>%
          mutate(edit = value_corr + input$offset_edit)
      } else if (input$correction == 2) { # drift
        r_locals$edit_data <- r_locals$measurement %>%
          filter(timestamp >= r_locals$select_datestart & timestamp <= r_locals$select_dateend) %>%
          mutate(edit = data_edit_drift(timestamp, value_corr, input$drift_edit))
      }

      plot_edit <- plot_add_edit_trace(data = r_locals$edit_data,
                                       y = "edit",
                                       y_label = r_locals$parameter_name)

      if (r_locals$plot_edit_exist == FALSE){

        # r_locals$plot_index <- r_locals$plot_index + 1

        plotlyProxy("plot") %>%
          plotlyProxyInvoke("addTraces", plot_edit)
        r_locals$plot_edit_exist <- TRUE
      } else {

        if (r_locals$plot_raw_exist == TRUE){
          plotlyProxy("plot") %>%
            plotlyProxyInvoke("deleteTraces", 2)
        } else {
          plotlyProxy("plot") %>%
            plotlyProxyInvoke("deleteTraces", 1)
        }

        # update existing trace
        plotlyProxy("plot") %>%
          plotlyProxyInvoke("addTraces", plot_edit)
      }


      # } else {
      #   plotlyProxy("plot") %>%
      #     plotlyProxyInvoke("deleteTraces", r_locals$plot_index)
      #   r_locals$plot_index <- r_locals$plot_index - 1
      # }
    })

    #### Validate change ####
    observeEvent(input$validate_edit, {
      # data <- data_update_measurement(con = db_con(),
      #                                 data = r_locals$edit_data,
      #                                 sensor = r_locals$sensor_id,
      #                                 author = input$author,
      #                                 correction_type = input$correction,
      #                                 value = input$offset_edit,
      #                                 comment = input$comment)
      #
      # if (!is.null(data)) {
      #   r_locals$userinfo$processing = data
      #
      #   r_locals$plot_update <- r_locals$plot_update+1
      #
      #   # set input$plot_raw_data and input$plot_edit to FALSE before new plot
      #   if (input$plot_raw_data == TRUE){
      #     r_locals$keep_raw_plot_layer = TRUE
      #     updateCheckboxInput(session, "plot_raw_data", value = FALSE)
      #   }
      #
      #   if (input$plot_edit == TRUE){
      #     updateCheckboxInput(session, "plot_edit", value = FALSE)
      #   }
      #
      #   r_locals$measurement <- db_get_measurement(con = db_con(),
      #                                                sensor = r_locals$sensor_id,
      #                                                start_date = input$date[1],
      #                                                end_date = input$date[2])
      #
      #   r_locals$plot <- plot_main(data = r_locals$measurement,
      #                              y = "value_corr",
      #                              y_title = r_locals$parameter_name)
      #
      #   # redraw corr plot if input$plot_corr_data was TRUE
      #   if (r_locals$keep_raw_plot_layer == TRUE){
      #     updateCheckboxInput(session, "plot_raw_data", value = TRUE)
      #     r_locals$keep_raw_plot_layer <- FALSE
      #   }
      #
      # } else {
      #   r_locals$userinfo$processing <- "Fail insert edits"
      # }
    })
  })
}
