#' download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),  # Initialize shinyjs
    fluidPage(
      fluidRow(
        column(
          width = 2,
          selectInput(inputId = ns("station"),
                      label = "Stations",
                      choices = NULL)
        ),
        column(
          width = 2,
          selectInput(inputId = ns("parameter_to_download"),
                      label = "Parameter",
                      choices = NULL)
        ),
        column(
          width = 2,
          selectInput(inputId = ns("data_level"),
                      label = "Data level treatment",
                      choices = c("Raw", "Corrected"),
                      selected = 1)
        )
      ),
      fluidRow(
        column(
          width = 3,
          dateRangeInput(inputId = ns("dates"),
                         label = "Date range")
        ),
        column(
          width = 2,
          selectInput(inputId = ns("format"),
                      label = "format",
                      choices = c("csv"))
        )
      ),
      fluidRow(
        column(
          width = 2,
          actionButton(inputId = ns("check_download"),
                       label = "Download",
                       icon = shiny::icon("download"))
        ),
        column(
          width = 2,
          tags$div(
            id = ns("download_container"),
            style = "position:absolute; left:-9999px;",
            downloadButton(outputId = ns("download"),
                           label = "Download")
          )
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

#' download Server Functions
#'
#' @noRd
#' @importFrom shinyjs click
#' @importFrom dplyr arrange mutate select rename
mod_download_server <- function(id, con, r_globals){
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
      parameter_to_download = NULL,
      parameter_to_download_name = NULL,
      parameter_to_download_unit = NULL,
      parameter_to_download_update = 0,
      sensor_id_to_download = NULL,
      data_download = NULL,
      data_download_parameter_col_name = NULL,
      download_name = NULL
    )

    ### INIT ####

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
      updateSelectInput(session, "parameter_to_download", choices = r_locals$station_parameters)
      r_locals$parameter_to_download <- r_locals$station_parameters[1]

      # force sensor_id event if same input$parameter as previous station
      if (r_locals$parameter_to_download == input$parameter_to_download){
        r_locals$parameter_to_download_update = r_locals$parameter_to_download_update + 1
      }

      print("Station")
    })

    #### Parameter to download ####

    observeEvent(list(input$parameter_to_download, r_locals$parameter_to_download_update), {
      req(input$parameter_to_download)
      r_locals$parameter_to_download <- input$parameter_to_download
      r_locals$parameter_to_download_name <-  names(which(r_locals$station_parameters == input$parameter_to_download))
      r_locals$parameter_to_download_unit <- db_get_parameter_unit(con, input$parameter_to_download)
      r_locals$data_download_parameter_col_name <- gsub(" ", "_",
                                                        tolower(
                                                          paste0(r_locals$parameter_to_download_name, "_",
                                                                 r_locals$parameter_to_download_unit)))

      r_locals$sensor_id_to_download <- db_get_sensor_id(con, input$station, input$parameter_to_download)

      print("Parameter to download")
    })

    #### Check download ####

    observeEvent(input$check_download, {
      if(input$dates[2]<input$dates[1]){
        showNotification("Date end > date start",
                         type = "error")
        return()
      }

       if (input$data_level=="Raw"){
        r_locals$data_download <- db_get_measurement_raw(con, r_locals$sensor_id_to_download, input$dates[1], input$dates[2])
      } else if (input$data_level == "corrected"){
        r_locals$data_download <- db_get_measurement_corr(con, r_locals$sensor_id_to_download, input$dates[1], input$dates[2])
      }

      if(is.null(r_locals$data_download) || nrow(r_locals$data_download)==0){
        showNotification("No data for this period and/or this data level",
                         type = "error")
        return()
      }

      r_locals$data_download <- r_locals$data_download %>%
        select(-sensor_id) %>%
        arrange(ts) %>%
        rename(!!r_locals$data_download_parameter_col_name := value,
               ts_utc = ts) %>%
        mutate(ts_utc = as.character(format(ts_utc)))


      r_locals$download_name <- gsub(" ", "_",
                                     paste0("LRX_", r_locals$station_code, "_", r_locals$parameter_to_download_name, "_",
                                     format(input$dates[1], "%Y%m%d"), "_", format(input$dates[2], "%Y%m%d"), ".csv"))

      # create click when condition are checked
      shinyjs::click("download")

    })

    #### download ####

    output$download <- downloadHandler(
      filename = function() {
        r_locals$download_name
      },
      content = function(file) {
        write.table(r_locals$data_download, file, sep = ";", dec = ".", row.names = FALSE)
      }
    )


  })
}

## To be copied in the UI
# mod_download_ui("download_1")

## To be copied in the server
# mod_download_server("download_1")
