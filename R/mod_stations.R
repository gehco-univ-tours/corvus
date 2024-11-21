#' Stations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom leaflet leafletOutput
mod_stations_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        leafletOutput(ns("map"), height = 400),
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
    )
  )
}

#' stations Server Functions
#'
#' @noRd
#'
#' @importFrom leaflet leaflet renderLeaflet addTiles addMarkers fitBounds addScaleBar scaleBarOptions labelOptions
mod_stations_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### DEV TOOLS ####
    output$printcheck = renderPrint({
      tryCatch({
        print("exists")
        print(input$map_marker_click)
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
      stations = NULL
    )

    ### MAP ###

    output$map <- renderLeaflet({

      # get stations
      r_locals$stations <- data_get_stations(db_con())
      # Calculate map bounds to center on points
      bounds <- list(
        min_lng = min(r_locals$stations$longitude),
        max_lng = max(r_locals$stations$longitude),
        min_lat = min(r_locals$stations$latitude),
        max_lat = max(r_locals$stations$latitude)
      )

      leaflet(data = r_locals$stations) %>%
        addTiles() %>%
        addMarkers(~longitude, ~latitude, label = ~name,
                   labelOptions = labelOptions (permanent=TRUE, direction = "auto"),
                   layerId = ~id  # Assign the ID as the layer ID for each marker
                   ) %>%
        fitBounds(bounds$min_lng, bounds$min_lat, bounds$max_lng, bounds$max_lat) %>%
        addScaleBar(position = "bottomleft",
                    scaleBarOptions(metric = TRUE, imperial = FALSE))
    })

  })
}

## To be copied in the UI
# mod_stations_ui("stations_1")

## To be copied in the server
# mod_stations_server("stations_1")
