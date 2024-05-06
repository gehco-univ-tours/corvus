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
          width = 3,
          selectInput(inputId = ns("station"),
                      label = "Stations",
                      choices = c("BE" = "be",
                                  "PI" = "pi"))
        ),
        column(
          width = 3,
          selectInput(inputId = ns("parameter"),
                      label = "Parameter",
                      choices = c("level", "turbidity"))
        ),
        column(
          width = 3,
          dateRangeInput(inputId = ns("date"),
                         label = "Date",
                         start =  "2019-01-01",
                         end =  "2019-02-28"
                         )
        ),
      ), # fluidRow
      fluidRow(
        column(
          width = 4,
          actionButton(inputId = ns("compile_raw_data"),
                       label = "Compile raw data")
        ),
        column(
          width = 3,
          actionButton(inputId = ns("plot_raw_data"),
                       label = "Plot raw data")
        ),
        column(
          width = 3,
          uiOutput(ns("plot_corr_data_ui"))
        )
      ), # fluidRow
      tags$hr(), # add horizontal line
      fluidRow(
        column(
          width = 3,
          switchInput(inputId = ns("edition"),
                      label = "Edition",
                      onStatus = "success"),
          uiOutput(ns("author_ui"))
        ),
        column(
          width = 3,
          uiOutput(ns("correction_ui"))
        )
      ), # fluidRow
      fluidRow(
        column(
          width = 3,
        ),
        column(
          width = 4,
          uiOutput(ns("date_offset_ui"))
        ),
        column(
          width = 3,
          uiOutput(ns("value_offset_ui"))
        )
      ), # fluidRow
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 2,
          uiOutput(ns("time_datestart_offset_ui"))
        ),
        column(
          width = 5,
          uiOutput(ns("time_dateend_offset_ui"))
        ),
        column(
          width = 1,
          uiOutput(ns("plot_offset_ui"))
        ),
        column(
          width = 1,
          uiOutput(ns("validate_offset_ui"))
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
mod_raw_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### DEV TOOLS ####
    output$printcheck = renderPrint({
      tryCatch({
        # event_data("plotly_hover")
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
      raw_data = NULL,
      corr_data = NULL,
      plot = NULL,
      userinfo = list("User information"),
      edit_data = NULL
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

    #### Compile bttn ####
    observeEvent(input$compile_raw_data, {
      r_locals$userinfo$processing = "Compiling raw data"
        data <- compile_raw(con = db_con(),
                            station = input$station,
                            parameter = input$parameter,
                            start_date = input$date[1],
                            end_date = input$date[2])
        if (!is.null(data)) {
          r_locals$userinfo$processing = data
        } else {
          r_locals$userinfo$processing <- "Fail compiling raw data"
        }
    })

    #### Plot bttn ####
    observeEvent(input$plot_raw_data, {
      r_locals$raw_data <- data_get_raw_data(con = db_con(),
                                         station = input$station,
                                         parameter = input$parameter,
                                         start_date = input$date[1],
                                         end_date = input$date[2])
      r_locals$plot <- plot_main(data = r_locals$raw_data,
                                 y = input$station,
                                 y_title = input$parameter)
      output$plot_corr_data_ui <- renderUI({
        checkboxInput(inputId = ns("plot_corr_data"),
                      label = "Plot corr data",
                      value = FALSE)
      })
    })

    #### Plot corr data ####
    observeEvent(input$plot_corr_data, {
      if (input$plot_corr_data == TRUE) {
        r_locals$corr_data <- data_get_corr_data(con = db_con(),
                                                 station = input$station,
                                                 parameter = input$parameter,
                                                 start_date = input$date[1],
                                                 end_date = input$date[2])
        plot_edit <- plot_add_trace(data = r_locals$corr_data,
                                    y = input$station,
                                    y_label = input$parameter)

        plotlyProxy("plot") %>%
          plotlyProxyInvoke("deleteTraces", 1) %>%
          plotlyProxyInvoke("addTraces", plot_edit, 1)
      } else {
        plotlyProxy("plot") %>%
          plotlyProxyInvoke("deleteTraces", 1)
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
                      choices = c("Offset" = "offset",
                                  "Deviation" = "deviation",
                                  "Delete" = "delete",
                                  "Interpolate" = "interpolate"))
        })
      } else {
        output$author_ui <- renderUI({
          NULL
        })
        output$correction_ui <- renderUI({
          NULL
        })
        output$date_offset_ui <- renderUI({
          NULL
        })
        output$time_datestart_offset_ui <- renderUI({
          NULL
        })
        output$time_dateend_offset_ui <- renderUI({
          NULL
        })
        output$value_offset_ui <- renderUI({
          NULL
        })
        output$plot_offset_ui <- renderUI({
          NULL
        })
        output$validate_offset_ui <- renderUI({
          NULL
        })
      }
    })

    #### Edition offset ####
    observeEvent(input$correction, {
      if (input$correction == "offset") {
        output$date_offset_ui <- renderUI({
          dateRangeInput(inputId = ns("date_offset"),
                         label = "Date",
                         start =  input$date[1],
                         end =  input$date[2],
                         startview = "month",
                         min = input$date[1],
                         max = input$date[2])
        })
        output$time_datestart_offset_ui <- renderUI({
          timeInput(inputId = ns("time_datestart_offset"),
                    label = "Date start time",
                    value = "00:00")
        })
        output$time_dateend_offset_ui <- renderUI({
          timeInput(inputId = ns("time_dateend_offset"),
                    label = "Date end time",
                    value = "00:00")
        })
        output$value_offset_ui <- renderUI({
          numericInput(inputId = ns("value_offset"),
                       label = "Offset value",
                       value = 0)
        })
        output$plot_offset_ui <- renderUI({
          actionButton(inputId = ns("plot_offset"),
                       label = "Plot change")
        })
        output$validate_offset_ui <- renderUI({
          actionButton(inputId = ns("validate_offset"),
                       label = "Validate")
        })
      } else {
        output$date_offset_ui <- renderUI({
          NULL
        })
        output$time_datestart_offset_ui <- renderUI({
          NULL
        })
        output$time_dateend_offset_ui <- renderUI({
          NULL
        })
        output$value_offset_ui <- renderUI({
          NULL
        })
        output$plot_offset_ui <- renderUI({
          NULL
        })
        output$validate_offset_ui <- renderUI({
          NULL
        })
      }
    })

    ##### Plot change ####
    observeEvent(input$plot_offset, {
      r_locals$edit_data <- data_get_raw_data(con = db_con(),
                                              station = input$station,
                                              parameter = input$parameter,
                                              start_date = as.POSIXct(paste(input$date_offset[1],
                                                                            input$time_datestart_offset,
                                                                            sep = " "),
                                                                      format = "%Y-%m-%d %H:%M"),
                                              end_date = as.POSIXct(paste(input$date_offset[2],
                                                                          input$time_dateend_offset,
                                                                          sep = " "),
                                                                    format = "%Y-%m-%d %H:%M")) %>%
        mutate(!!input$station := .data[[input$station]] + input$value_offset)

      plot_edit <- plot_add_trace(data = r_locals$edit_data,
                                  y = input$station,
                                  y_label = input$parameter)

        plotlyProxy("plot") %>%
          plotlyProxyInvoke("deleteTraces", 2) %>%
          plotlyProxyInvoke("addTraces", plot_edit, 2)
    })

    ##### Validate change ####
    observeEvent(input$validate_offset, {
      data <- data_insert_offset(con = db_con(),
                                 station = input$station,
                                 parameter = input$parameter,
                                 date_time_start = as.POSIXct(paste(input$date_offset[1],
                                                                    input$time_datestart_offset,
                                                                    sep = " "),
                                                              format = "%Y-%m-%d %H:%M"),
                                 date_time_end = as.POSIXct(paste(input$date_offset[2],
                                                                  input$time_dateend_offset,
                                                                  sep = " "),
                                                            format = "%Y-%m-%d %H:%M"),
                                 offset_val = input$value_offset,
                                 author = input$author)

      if (!is.null(data)) {
        r_locals$userinfo$processing = data
      } else {
        r_locals$userinfo$processing <- "Fail compiling raw data"
      }
    })
  })
}

## To be copied in the UI
# mod_raw_data_ui("raw_data_1")

## To be copied in the server
# mod_raw_data_server("raw_data_1")
