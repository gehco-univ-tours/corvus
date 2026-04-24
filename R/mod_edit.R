#' edit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
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
          uiOutput(ns("checkbox_measurement_tocorr_raw_ui")),
          uiOutput(ns("checkbox_measurement_tocorr_corr_ui")),
          uiOutput(ns("checkbox_measurement_additional_ui")),
          uiOutput(ns("checkbox_fieldwork_ui")),
          uiOutput(ns("checkbox_validated_period_ui"))
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
                      choices = db_get_stations(db_con()))
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
          width = 4,
          fluidRow(
            column(
              width = 6,
              uiOutput(ns("set_start_date_ui"))
            ),
            column(
              width = 6,
              uiOutput(ns("set_end_date_ui"))
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
#' @importFrom dygraphs renderDygraph dyEvent dyShading
#' @importFrom shinyjs disable enable hide show
#' @importFrom dplyr mutate
#' @importFrom lubridate hm ymd ymd_hm
#' @importFrom DT datatable renderDataTable
mod_edit_server <- function(id, r_globals){
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
        measurement_tocorr = NULL,
        measurement_add = NULL,
        fieldwork = NULL,
        validated = NULL
      ),
      checkbox_graph = list(
        measurement_tocorr_raw = TRUE,
        measurement_tocorr_corr = FALSE,
        measurement_additional = FALSE,
        fieldwork = FALSE,
        validated_period = FALSE
      ),
      update_plot = 0,
      dygraph = NULL,
      fieldwork_table = NULL,
      start_or_end = NULL,
      start_date = NULL,
      end_date = NULL,
      userinfo = list()
    )

    ### INIT ####

    ### UI OUTPUT ####

    #### plot ####

    #### userinfo ####
    output$userinfo <- renderPrint({
      r_locals$userinfo
    })

    ### EVENT ####

    #### UI ####
    # update input$station if r_globals$station is not NULL

    #### Station ####

    observeEvent(input$station, {
      req(input$station)
      r_locals$station_code <- r_globals$all_stations$code[r_globals$all_stations$id == input$station]
      r_locals$station_name <- r_globals$all_stations$name[r_globals$all_stations$id == input$station]
      r_locals$station_parameters <- db_get_station_parameters(db_con(), input$station)
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
      r_locals$parameter_tocorr_unit <- db_get_parameter_unit(db_con(), input$parameter_tocorr)

      r_locals$sensor_id_tocorr <- db_get_sensor_id(db_con(), input$station, input$parameter_tocorr)

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
      r_locals$parameter_add_unit <- db_get_parameter_unit(db_con(), input$parameter_add)

      r_locals$sensor_id_add <- db_get_sensor_id(db_con(), input$station, input$parameter_add)

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
      r_locals$data$measurement_tocorr <- db_get_measurement(db_con(), r_locals$sensor_id_tocorr, input$date[1], input$date[2])
      r_locals$data$measurement_add <- db_get_measurement(db_con(), r_locals$sensor_id_add, input$date[1], input$date[2])
      r_locals$data$fieldwork <- db_get_fieldwork_data(db_con(), input$station, input$date[1], input$date[2])
      r_locals$data$validated <- db_get_validated_period_data(db_con(), r_locals$sensor_id_tocorr, input$date[1], input$date[2])

      # update checkbox UI
      output$checkbox_measurement_tocorr_raw_ui <- renderUI({
        checkboxInput(inputId = ns("checkbox_measurement_tocorr_raw"),
                      label = paste("Raw data (", r_locals$parameter_tocorr_name, ")", sep = ""),
                      value = TRUE)
      })
      output$checkbox_measurement_tocorr_corr_ui <- renderUI({
        checkboxInput(inputId = ns("checkbox_measurement_tocorr_corr"),
                      label = paste("Corrected data (", r_locals$parameter_tocorr_name, ")", sep = ""),
                      value = FALSE)
      })
      output$checkbox_measurement_additional_ui <- renderUI({
        checkboxInput(inputId = ns("checkbox_measurement_additional"),
                      label = paste("Additional data (", r_locals$parameter_add_name, ")", sep = ""),
                      value = FALSE)
      })
      output$checkbox_fieldwork_ui <- renderUI({
        checkboxInput(inputId = ns("checkbox_fieldwork"),
                      label = "Fieldwork",
                      value = FALSE)
      })
      output$checkbox_validated_period_ui <- renderUI({
        checkboxInput(inputId = ns("checkbox_validated_period"),
                      label = "Validated period",
                      value = FALSE)
      })

      # reset start and end editing
      r_locals$start_or_end = NULL
      r_locals$start_date = NULL
      r_locals$end_date = NULL

      r_locals$update_plot <- r_locals$update_plot + 1

      print("Plot bttn")

    })

    #### Checkboxes ####

    observeEvent(list(input$checkbox_measurement_tocorr_raw,
                      input$checkbox_measurement_tocorr_corr,
                      input$checkbox_measurement_additional
                      # input$checkbox_fieldwork
                      # input$checkbox_validated_period
                      ), ignoreInit = TRUE, {

      r_locals$checkbox_graph$measurement_tocorr_raw <- input$checkbox_measurement_tocorr_raw
      r_locals$checkbox_graph$measurement_tocorr_corr <- input$checkbox_measurement_tocorr_corr
      r_locals$checkbox_graph$measurement_additional <- input$checkbox_measurement_additional
      # r_locals$checkbox_graph$fieldwork <- input$checkbox_fieldwork
      # r_locals$checkbox_graph$validated_period <- input$checkbox_validated_period

      r_locals$update_plot = r_locals$update_plot + 1

      print("Checkboxes")
    })


    #### Validated period and fieldwork ####

    observeEvent(list(input$checkbox_validated_period,
                      input$checkbox_fieldwork), ignoreInit = TRUE, {

      r_locals$checkbox_graph$validated_period <- input$checkbox_validated_period
      r_locals$checkbox_graph$fieldwork <- input$checkbox_fieldwork

      if(r_locals$checkbox_graph$fieldwork){
        r_locals$fieldwork_table <- DT::datatable(
          data = r_locals$data$fieldwork
        )
      } else {
        r_locals$fieldwork_table <- NULL
      }

      print("Validated period or fieldwork")
    })

    #### Update plot ####

    observeEvent(r_locals$update_plot, {
      req(r_locals$data$measurement_tocorr, r_locals$data$measurement_add)

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

      # validated periods (green zone)
      if (r_locals$checkbox_graph$validated_period && !is.null(r_locals$data$validated)) {
        for (i in seq_len(nrow(r_locals$data$validated))) {
          plot <- plot %>%
            dygraphs::dyShading(
              from = r_locals$data$validated$ts_start[i],
              to   = r_locals$data$validated$ts_end[i],
              color = "rgba(0,255,0,0.1)"
            )
        }
      }

      # fieldwork (orange vertical lines)
      if (r_locals$checkbox_graph$fieldwork && !is.null(r_locals$data$fieldwork)) {
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
    observeEvent(input$plot_click, {
      req(r_locals$start_or_end)

      if (r_locals$start_or_end == "start") {
        r_locals$start_date <- input$plot_click$x
      }

      if (r_locals$start_or_end == "end") {
        r_locals$end_date <- input$plot_click$x
      }

      r_locals$start_or_end <- NULL

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
                      choices = db_get_authors(db_con()))
        })
        output$correction_ui <- renderUI({
          selectInput(inputId = ns("correction"),
                      label = "Correction",
                      choices = db_get_correction_type(db_con()))
        })
        output$set_start_date_ui <- renderUI({
          actionButton(inputId = ns("set_start_date"),
                      label = "Set start date")
        })
        output$set_end_date_ui <- renderUI({
          actionButton(inputId = ns("set_end_date"),
                      label = "Set end date")
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

    ##### Set start date button ####

    observeEvent(input$set_start_date, {
      r_locals$start_or_end = "start"
    })

    ##### Set end date button ####

    observeEvent(input$set_end_date, {
      r_locals$start_or_end = "end"
    })


    ##### Enable/disable plot_edit button ####

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

    #### Validate change ####
  })
}
