#' edit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidPage fluidRow column selectInput
#' @importFrom shiny actionButton sliderInput verbatimTextOutput renderPrint
#' @importFrom dygraphs dygraphOutput
#' @importFrom shinybusy add_busy_bar
#' @importFrom shinyWidgets switchInput timeInput
#' @importFrom shinyjs useShinyjs
#' @importFrom DT dataTableOutput
mod_edit_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      useShinyjs(),  # Initialize shinyjs
      fluidRow(
        add_busy_bar(color = "#FF0000"),
        column(
          width = 2,
          uiOutput(ns("checkbox_measurement_raw_ui")),
          uiOutput(ns("checkbox_measurement_corr_ui")),
          uiOutput(ns("checkbox_measurement_filter_ui")),
          uiOutput(ns("checkbox_measurement_edit_ui")),
          uiOutput(ns("checkbox_measurement_add_ui")),
          uiOutput(ns("checkbox_fieldwork_ui")),
          uiOutput(ns("checkbox_deleted_period_ui"))
        ),
        column(
          width = 10,
        dygraphOutput(ns("plot"))
        )
      ),
      fluidRow(
        column(
          width = 2,
          selectInput(inputId = ns("station"),
                      label = "Stations",
                      choices = NULL)
        ),
        column(
          width = 2,
          selectInput(inputId = ns("parameter_tocorr"),
                      label = "Parameter to correct",
                      choices = NULL)
        ),
        column(
          width = 2,
          selectInput(inputId = ns("parameter_add"),
                      label = "Parameter additional",
                      choices = NULL)
        ),
        column(
          tags$div(style = "margin-top: 30px;"),
          width = 2,
          actionButton(inputId = ns("plot_bttn"),
                       label = "Plot")
        ),
      ), # fluidRow
      fluidRow(
        column(
          width = 12,
          uiOutput(ns("date_ui"))
        )
      ),
      fluidRow(
        DT::dataTableOutput(ns("fieldwork_table"))
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
          width = 3,
          fluidRow(
            column(
              width = 4,
              uiOutput(ns("set_start_date_ui"))
            ),
            column(
              width = 4,
              uiOutput(ns("set_end_date_ui"))
            ),
            column(
              width = 4,
              uiOutput(ns("reset_start_end_date_ui"))
            )
          )
        ),
        column(
          width = 3,
          uiOutput(ns("value_edit_ui")),
          uiOutput(ns("value_edit2_ui")),
          uiOutput(ns("plot_edit_ui")),
          tags$div(style = "margin-top: 20px;"),
          uiOutput(ns("validate_edit_ui"))
        ),
        column(
          width = 4,
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
#' @importFrom shiny moduleServer observeEvent renderUI updateSelectInput
#' @importFrom shiny selectInput actionButton sliderInput verbatimTextOutput
#' @importFrom shiny renderPrint req reactiveValues checkboxInput
#' @importFrom shiny updateCheckboxInput textAreaInput numericInput
#' @importFrom dygraphs renderDygraph dyEvent dyShading
#' @importFrom shinyjs disable enable hide show
#' @importFrom dplyr mutate filter arrange group_by summarise transmute
#' @importFrom lubridate hm ymd ymd_hm seconds
#' @importFrom slider slide_index_dbl
#' @importFrom stats predict
#' @importFrom DT datatable renderDataTable
#' @importFrom forecast tsclean
mod_edit_server <- function(id, con, r_globals){
  moduleServer(id, function(input, output, session){
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
      station_code = NULL,
      station_name = NULL,
      station_parameters = NULL,
      parameter_tocorr = NULL,
      parameter_tocorr_name = NULL,
      paramter_tocorr_unit = NULL,
      parameter_add = NULL,
      parameter_add_name = NULL,
      parameter_add_unit = NULL,
      sensor_id_tocorr = NULL,
      sensor_id_add = NULL,
      parameter_tocorr_update = 0,
      parameter_add_update = 0,
      data = list(
        measurement_raw = NULL,
        measurement_corr = NULL,
        measurement_filter = NULL,
        measurement_edit = NULL,
        measurement_add = NULL,
        fieldwork = NULL,
        validated = NULL,
        deleted = NULL
      ),
      checkbox_graph = list(
        measurement_raw = TRUE,
        measurement_corr = FALSE,
        measurement_filter = FALSE,
        measurement_add = FALSE,
        measurement_edit = FALSE,
        fieldwork = FALSE,
        deleted_period = FALSE,
        correction_period = FALSE,
        validated_period = FALSE
      ),
      update_plot = 0,
      trigger_checkbox_update = 0,
      dygraph = NULL,
      fieldwork_table = NULL,
      start_or_end = NULL,
      start_date = NULL,
      end_date = NULL,
      userinfo = list()
    )

    ### INIT ####

    #### UI output ####

    output$date_ui <- renderUI({
      req(con)

      limits <- db_min_max_date(con)

      sliderInput(
        inputId = session$ns("date"),
        label = "",
        min = limits$min,
        max = limits$max,
        value = c(limits$max - 180, limits$max),
        timeFormat = "%Y-%m-%d",
        width = "100%",
        timezone = "UTC"
      )
    })

    output$userinfo <- renderPrint({
      r_locals$userinfo
    })

    ### EVENT ####

    #### UI ####
    # update input$station if r_globals$station is not NULL

    observeEvent(r_globals$all_stations, {
      updateSelectInput(
        session,
        inputId = "station",
        choices = db_get_stations(con)
      )
    })

    #### Station ####

    observeEvent(input$station, {
      req(input$station)
      r_locals$station_code <- r_globals$all_stations$code[r_globals$all_stations$id == input$station]
      r_locals$station_name <- r_globals$all_stations$name[r_globals$all_stations$id == input$station]
      r_locals$station_parameters <- db_get_station_parameters(con, input$station)
      updateSelectInput(session, "parameter_tocorr", choices = r_locals$station_parameters)
      updateSelectInput(session, "parameter_add", choices = r_locals$station_parameters)
      r_locals$parameter_tocorr <- r_locals$station_parameters[1]
      r_locals$parameter_add <- r_locals$station_parameters[1]

      # force sensor_id event if same input$parameter as previous station
      if (r_locals$parameter_tocorr == input$parameter_tocorr){
        r_locals$parameter_tocorr_update = r_locals$parameter_tocorr_update + 1
      }

      # force sensor_id event if same input$parameter as previous station
      if (r_locals$parameter_add == input$parameter_add){
        r_locals$parameter_add_update = r_locals$parameter_add_update + 1
      }

      # userinfo
      r_locals$userinfo$station_code <- r_locals$station_code

      print("Station")
    })

    #### Parameter to correct ####

    observeEvent(list(input$parameter_tocorr, r_locals$parameter_tocorr_update), {
      req(input$parameter_tocorr)
      r_locals$parameter_tocorr <- input$parameter_tocorr
      r_locals$parameter_tocorr_name <-  names(which(r_locals$station_parameters == input$parameter_tocorr))
      r_locals$parameter_tocorr_unit <- db_get_parameter_unit(con, input$parameter_tocorr)

      r_locals$sensor_id_tocorr <- db_get_sensor_id(con, input$station, input$parameter_tocorr)

      # userinfo
      r_locals$userinfo$parameter_tocorr_name <- r_locals$parameter_tocorr_name
      r_locals$userinfo$sensor_id_tocorr <- r_locals$sensor_id_tocorr

      print("Parameter to correct")
    })

    #### Parameter additional ####

    observeEvent(list(input$parameter_add, r_locals$parameter_add_update), {
      req(input$parameter_add)
      r_locals$parameter_add <- input$parameter_add
      r_locals$parameter_add_name <-  names(which(r_locals$station_parameters == input$parameter_add))
      r_locals$parameter_add_unit <- db_get_parameter_unit(con, input$parameter_add)

      r_locals$sensor_id_add <- db_get_sensor_id(con, input$station, input$parameter_add)

      # userinfo
      r_locals$userinfo$parameter_add_name <- r_locals$parameter_add_name
      r_locals$userinfo$sensor_id_add <- r_locals$sensor_id_add

      print("Parameter additional")
    })

    #### Plot bttn ####

    observeEvent(input$plot_bttn, {
      req(r_locals$sensor_id_tocorr)
      req(r_locals$sensor_id_add)

      # get data
      r_locals$data$measurement_raw <- db_get_measurement_raw(con, r_locals$sensor_id_tocorr, input$date[1], input$date[2])
      r_locals$data$measurement_corr <- db_get_measurement_corr(con, r_locals$sensor_id_tocorr, input$date[1], input$date[2])
      r_locals$data$measurement_filter <- db_get_measurement_filter(con, r_locals$sensor_id_tocorr, input$date[1], input$date[2])
      r_locals$data$measurement_add <- db_get_measurement_raw_corr(con, r_locals$sensor_id_add, input$date[1], input$date[2])
      r_locals$data$fieldwork <- db_get_fieldwork_data(con, input$station, input$date[1], input$date[2])
      r_locals$data$deleted <- db_get_deleted_period_data(con, r_locals$sensor_id_tocorr, input$date[1], input$date[2])

      # update checkbox UI
      output$checkbox_measurement_raw_ui <- renderUI({
        checkboxInput(inputId = ns("checkbox_measurement_raw"),
                      label = paste("Raw data (", r_locals$parameter_tocorr_name, ")", sep = ""),
                      value = TRUE)
      })
      output$checkbox_measurement_corr_ui <- renderUI({
        checkboxInput(inputId = ns("checkbox_measurement_corr"),
                      label = paste("Corrected data (", r_locals$parameter_tocorr_name, ")", sep = ""),
                      value = FALSE)
      })
      output$checkbox_measurement_filter_ui <- renderUI({
        checkboxInput(inputId = ns("checkbox_measurement_filter"),
                      label = paste("Filtered data (", r_locals$parameter_tocorr_name, ")", sep = ""),
                      value = FALSE)
      })
      output$checkbox_measurement_edit_ui <- renderUI({
        checkboxInput(inputId = ns("checkbox_measurement_edit"),
                      label = paste("Edited data (", r_locals$parameter_tocorr_name, ")", sep = ""),
                      value = FALSE)
      })
      output$checkbox_measurement_add_ui <- renderUI({
        checkboxInput(inputId = ns("checkbox_measurement_add"),
                      label = paste("Additional data (", r_locals$parameter_add_name, ")", sep = ""),
                      value = FALSE)
      })
      output$checkbox_fieldwork_ui <- renderUI({
        checkboxInput(inputId = ns("checkbox_fieldwork"),
                      label = "Fieldwork",
                      value = FALSE)
      })
      output$checkbox_deleted_period_ui <- renderUI({
        checkboxInput(inputId = ns("checkbox_deleted_period"),
                      label = "Deleted period",
                      value = FALSE)
      })

      # reset start and end editing
      r_locals$start_or_end = NULL
      r_locals$start_date = NULL
      r_locals$end_date = NULL

      # reset measurement edit
      r_locals$data$measurement_edit = NULL

      r_locals$trigger_checkbox_update <- r_locals$trigger_checkbox_update +1

      print("Plot bttn")

    })

    #### Checkboxes ####

    observeEvent(list(input$checkbox_measurement_raw,
                      input$checkbox_measurement_corr,
                      input$checkbox_measurement_filter,
                      input$checkbox_measurement_add,
                      input$checkbox_measurement_edit,
                      r_locals$trigger_checkbox_update
                      ), ignoreInit = TRUE, {

      r_locals$checkbox_graph$measurement_raw <- input$checkbox_measurement_raw
      r_locals$checkbox_graph$measurement_corr <- input$checkbox_measurement_corr
      r_locals$checkbox_graph$measurement_filter <- input$checkbox_measurement_filter
      r_locals$checkbox_graph$measurement_add <- input$checkbox_measurement_add
      r_locals$checkbox_graph$measurement_edit <- input$checkbox_measurement_edit

      r_locals$update_plot = r_locals$update_plot + 1

      print("Checkboxes")
    })


    #### Deleted period and fieldwork ####

    observeEvent(list(input$checkbox_fieldwork,
                      input$checkbox_deleted_period), ignoreInit = TRUE, {

      r_locals$checkbox_graph$fieldwork <- input$checkbox_fieldwork
      r_locals$checkbox_graph$deleted_period <- input$checkbox_deleted_period

      if(r_locals$checkbox_graph$fieldwork){
        r_locals$fieldwork_table <- DT::datatable(
          data = r_locals$data$fieldwork
        )
      } else {
        r_locals$fieldwork_table <- NULL
      }

      print("Deleted period or fieldworks")
    })

    #### Update plot ####

    observeEvent(r_locals$update_plot, {
      req(r_locals$data$measurement_raw, r_locals$data$measurement_corr,
          r_locals$data$measurement_filter, r_locals$data$measurement_add)

      r_locals$dygraph <- plot_dygraph(
        data = r_locals$data,
        parameter_tocorr_name = r_locals$parameter_tocorr_name,
        parameter_add_name = r_locals$parameter_add_name,
        display_opts = r_locals$checkbox_graph
      )

      print("Update plot")
      })

    #### Plot ####

    output$plot <- renderDygraph({
      req(r_locals$dygraph)

      plot <- r_locals$dygraph

      # start edit vertical line
      if (!is.null(r_locals$start_date)) {
        plot <- plot %>% dyEvent(r_locals$start_date, color = "red", label = "Start")
      }

      # start edit vertical line
      if (!is.null(r_locals$end_date)) {
        plot <- plot %>% dyEvent(r_locals$end_date, color = "blue", label = "End")
      }

      # editing zone
      if (!is.null(r_locals$start_date) && !is.null(r_locals$end_date)) {
        from <- min(r_locals$start_date, r_locals$end_date)
        to   <- max(r_locals$start_date, r_locals$end_date)

        plot <- plot %>% dyShading(from = from, to = to,
                                   color = "rgba(150,150,150,0.2)")
      }

      # deleted periods (red zone)
      if (isTRUE(r_locals$checkbox_graph$deleted_period) && !is.null(r_locals$data$deleted)) {
        for (i in seq_len(nrow(r_locals$data$deleted))) {
          plot <- plot %>%
            dygraphs::dyShading(
              from = r_locals$data$deleted$ts_start[i],
              to   = r_locals$data$deleted$ts_end[i],
              color = "rgba(255,0,0,0.3)"
            )
        }
      }

      # fieldwork (orange vertical lines)
      if (isTRUE(r_locals$checkbox_graph$fieldwork) && !is.null(r_locals$data$fieldwork)) {
        for (i in seq_len(nrow(r_locals$data$fieldwork))) {
          plot <- plot %>%
            dygraphs::dyEvent(
              r_locals$data$fieldwork$ts[i],
              label = "Fieldwork",
              color = "orange"
            )
        }
      }

      plot
    })

    #### Click on plot ####

    # set start or end editing
    observeEvent(list(input$plot_click, input$reset_start_end_date), {
      req(r_locals$start_or_end)

      click_time <- as.POSIXct(
          input$plot_click$x,
          format = "%Y-%m-%dT%H:%M:%OSZ",
          tz = "UTC"
        )


      if (r_locals$start_or_end == "start") {
        r_locals$start_date <- click_time
      }

      if (r_locals$start_or_end == "end") {
        r_locals$end_date <- click_time
      }

      r_locals$start_or_end <- NULL

      if (!is.null(r_locals$start_date) && !is.null(r_locals$end_date)){
        shinyjs::delay(100, shinyjs::enable("plot_edit"))
      } else {
        shinyjs::delay(100, shinyjs::disable("plot_edit"))
        shinyjs::delay(100, shinyjs::disable("validate_edit"))
      }

      print("Click on plot")
    })

    #### Fieldwork table ####

    output$fieldwork_table <-  DT::renderDataTable(
      r_locals$fieldwork_table
    )

    #### Date slider ####

    #### Marker plot ####

    #### Plot raw data bttn ####

    #### Plot plot_field bttn ####

    #### Edition mode UI ####
    observeEvent(input$edition, {
      if (input$edition == TRUE) {
        output$author_ui <- renderUI({
          selectInput(inputId = ns("author"),
                      label = "Author",
                      choices = db_get_authors(con))
        })
        output$correction_ui <- renderUI({
          selectInput(inputId = ns("correction"),
                      label = "Correction",
                      choices = db_get_correction_type(con))
        })
        output$set_start_date_ui <- renderUI({
          actionButton(inputId = ns("set_start_date"),
                      label = "Set start date")
        })
        output$set_end_date_ui <- renderUI({
          actionButton(inputId = ns("set_end_date"),
                      label = "Set end date")
        })
        output$reset_start_end_date_ui <- renderUI({
          actionButton(inputId = ns("reset_start_end_date"),
                       label = "Reset dates")
        })
        output$plot_edit_ui <- renderUI({
          actionButton(inputId = ns("plot_edit"),
                       label = "Plot edit")
        })
        output$validate_edit_ui <- renderUI({
          actionButton(inputId = ns("validate_edit"),
                       label = "Validate edit")
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
        output$set_start_date_ui <- renderUI({
          NULL
        })
        output$set_end_date_ui <- renderUI({
          NULL
        })
        output$reset_start_end_date_ui <- renderUI({
          NULL
        })
        output$value_edit_ui <- renderUI({
          NULL
        })
        output$value_edit2_ui <- renderUI({
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

    ##### Set start date button ####

    observeEvent(input$set_start_date, {
      r_locals$start_or_end = "start"
    })

    ##### Set end date button ####

    observeEvent(input$set_end_date, {
      r_locals$start_or_end = "end"
    })

    ##### Reset start and end date button ####

    observeEvent(input$reset_start_end_date, {
      r_locals$start_or_end = NULL
      r_locals$start_date = NULL
      r_locals$end_date = NULL
    })

    ##### Enable/disable plot_edit button ####

    ##### Edition mode ####
    observeEvent(input$correction, {

      if (input$correction == 1) { # offset

        output$value_edit_ui <- renderUI({
          numericInput(inputId = ns("offset_edit"),
                       label = "Offset value",
                       value = 0)
        })
        output$value_edit2_ui <- renderUI({
          NULL
        })
      } else if (input$correction == 2){ # drift
        output$value_edit_ui <- renderUI({
          numericInput(inputId = ns("drift_edit"),
                       label = "Drift end value",
                       value = 0)
        })
        output$value_edit2_ui <- renderUI({
          NULL
        })
      } else if (input$correction == 3){ # delete
        output$value_edit_ui <- renderUI({
            numericInput(inputId = ns("delete_threshold"),
                         label = "Delete above this value",
                         value = 0)
        })
        output$value_edit2_ui <- renderUI({
          NULL
        })
      } else if (input$correction == 4){ # set values
        output$value_edit_ui <- renderUI({
          numericInput(inputId = ns("set_value"),
                       label = "Set value",
                       value = 0)
        })
        output$value_edit2_ui <- renderUI({
          NULL
        })
      } else if (input$correction == 5){ # median filter
        output$value_edit_ui <- renderUI({
          numericInput(inputId = ns("median_interval"),
                       label = "Filter interval (min)",
                       value = 15)
        })
        output$value_edit2_ui <- renderUI({
          NULL
        })
      } else if (input$correction == 6){ # loess filter
        output$value_edit_ui <- renderUI({
          numericInput(inputId = ns("loess_span"),
                       label = "Loess span",
                       value = 0.1)
        })
        output$value_edit2_ui <- renderUI({
          NULL
        })
      } else if (input$correction == 7){ # Hampel interval
        output$value_edit_ui <- renderUI({
          numericInput(inputId = ns("hampel_interval"),
                       label = "Filter interval (min)",
                       value = 15)
        })
        output$value_edit2_ui <- renderUI({ # Hampel filter
          numericInput(inputId = ns("hampel_value"),
                       label = "Hampel k",
                       value = 3)
        })
      } else if (input$correction == 8){ # mean filter
        output$value_edit_ui <- renderUI({
          numericInput(inputId = ns("mean_interval"),
                       label = "Filter interval (min)",
                       value = 15)
        })
        output$value_edit2_ui <- renderUI({
          NULL
        })
      } else if (input$correction == 9){ # tsclean filter
        output$value_edit_ui <- renderUI({
          numericInput(inputId = ns("tsclean_iteration"),
                       label = "Number of iteration",
                       value = 2)
        })
        output$value_edit2_ui <- renderUI({
          NULL
        })
      } else {
        output$value_edit_ui <- renderUI({
          NULL
        })
        output$value_edit2_ui <- renderUI({
          NULL
        })
      }
    })

    #### Plot edit ####

    observeEvent(input$plot_edit, {
      req(r_locals$start_date, r_locals$end_date)

      # get data to correct
      r_locals$data$measurement_edit <- data_get_measurement_edit(measurement_raw = r_locals$data$measurement_raw,
                                                                  measurement_corr = r_locals$data$measurement_corr) %>%
        dplyr::filter(ts >= r_locals$start_date & ts <= r_locals$end_date) %>%
        dplyr::arrange(ts)

      # apply correction
      if (input$correction == 1) { # offset
        r_locals$data$measurement_edit <- r_locals$data$measurement_edit %>%
          dplyr::mutate(value = value + input$offset_edit)
      }
      if (input$correction == 2){ # drift
        r_locals$data$measurement_edit <- r_locals$data$measurement_edit %>%
          dplyr::mutate(value = data_edit_drift(ts, value, input$drift_edit))
      }
      if (input$correction == 3){ # delete
        r_locals$data$measurement_edit <- r_locals$data$measurement_edit %>%
          dplyr::mutate(
            value = as.numeric(ifelse(value > input$delete_threshold, NA, value)))
      }
      if (input$correction == 4){ # set value
        r_locals$data$measurement_edit <- r_locals$data$measurement_edit %>%
          dplyr::mutate(value = input$set_value)
      }
      if (input$correction == 5){ # median filter
        r_locals$data$measurement_edit <- r_locals$data$measurement_edit %>%
          dplyr::mutate(value = slide_index_dbl(
            value,
            .i = ts,
            .f = median,
            .before = seconds(input$median_interval/2*60),
            .after = seconds(input$median_interval/2*60),
            .complete = FALSE
          ))
      }
      if (input$correction == 6){ # loess filter
        r_locals$data$measurement_edit <- r_locals$data$measurement_edit %>%
          dplyr::mutate(value = predict(
            loess(value ~ as.numeric(ts), span = input$loess_span)
          ))
      }
      if (input$correction == 7){ # Hampel filter
        r_locals$data$measurement_edit <- r_locals$data$measurement_edit %>%
          mutate(
            value = slide_index_dbl(
              value,
              ts,
              .before = seconds(input$hampel_interval*60),
              .after = 0,  # causal
              .f = function(x) {
                med <- median(x)
                mad_val <- max(mad(x, constant = 1.4826), 1e-6)
                x0 <- x[length(x)]

                if (abs(x0 - med) > 3 * mad_val) med else x0
              }
            )
          )
          # dplyr::mutate(value = slide_index_dbl(
          #   value,
          #   .i = ts,
          #   .f = ~ data_hampel_filter (.x, k = input$hampel_value),
          #   .before = seconds(input$hampel_interval*60),
          #   .after = seconds(input$hampel_interval*60),
          #   complete = FALSE
          # ))
      }
      if (input$correction == 8){ # mean filter
        r_locals$data$measurement_edit <- r_locals$data$measurement_edit %>%
          dplyr::mutate(value = slide_index_dbl(
            value,
            .i = ts,
            .f = mean,
            .before = seconds(input$mean_interval/2*60),
            .after = seconds(input$mean_interval/2*60),
            .complete = FALSE
          ))
      }

      if (input$correction == 9){ # tsclean filter
        ts_data <- ts(r_locals$data$measurement_edit$value, frequency = 1)
        r_locals$data$measurement_edit$value <- as.numeric(tsclean(ts_data,
                                                                   replace.missing = FALSE,
                                                                   iterate = input$tsclean_iteration))
      }

      # plot
      if(!isTRUE(input$checkbox_measurement_edit)){
        updateCheckboxInput(session, "checkbox_measurement_edit", value = TRUE)
      } else {
        r_locals$update_plot <- r_locals$update_plot + 1
      }

      shinyjs::enable("validate_edit")

      print("Plot edit")
    })

    #### Validate change ####

    observeEvent(input$validate_edit, {

      prepared <- data_prepare_edit_and_correction(
        measurement_edit = r_locals$data$measurement_edit,
        correction_type = as.integer(input$correction),
        sensor_id = r_locals$sensor_id_tocorr,
        start_date = r_locals$start_date,
        end_date = r_locals$end_date,
        author_id = as.integer(input$author),
        comment = input$comment,
        offset = input$offset_edit,
        drift = input$drift_edit,
        delete_threshold = input$delete_threshold,
        set_value = input$set_value,
        median_interval = input$median_interval,
        loess_span = input$loess_span,
        hampel_interval = input$hampel_interval,
        hampel_value = input$hampel_value,
        mean_interval = input$mean_interval,
        tsclean_iteration = input$tsclean_iteration
      )

      r_locals$userinfo$db_result <- db_apply_edit_with_correction(
        con = con,
        correction_type = as.integer(input$correction),
        measurement_edit = prepared$measurement_edit,
        correction_period = prepared$correction_period
      )

      r_locals$data$measurement_edit <- NULL
      r_locals$data$measurement_corr <- db_get_measurement_corr(con, r_locals$sensor_id_tocorr, input$date[1], input$date[2])
      r_locals$data$measurement_filter <- db_get_measurement_filter(con, r_locals$sensor_id_tocorr, input$date[1], input$date[2])
      r_locals$data$deleted <- db_get_deleted_period_data(con, r_locals$sensor_id_tocorr, input$date[1], input$date[2])
      shinyjs::disable("validate_edit")
      r_locals$update_plot <- r_locals$update_plot + 1

      print("Validated")

    })
  })
}
