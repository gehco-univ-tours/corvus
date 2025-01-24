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
mod_stations_server <- function(id, r_globals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### DEV TOOLS ####
    output$printcheck = renderPrint({
      tryCatch({
        print("exists")
        print(input$map_marker_click)
        print(r_globals$station)
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

    # r_locals <- reactiveValues(
    # )

    ### MAP ###

    output$map <- renderLeaflet({

      # Calculate map bounds to center on points
      bounds <- list(
        min_lng = min(r_globals$all_stations$longitude),
        max_lng = max(r_globals$all_stations$longitude),
        min_lat = min(r_globals$all_stations$latitude),
        max_lat = max(r_globals$all_stations$latitude)
      )

      leaflet(data = r_globals$all_stations) %>%
        addTiles() %>%
        addMarkers(~longitude, ~latitude, label = ~name,
                   labelOptions = labelOptions (permanent=TRUE, direction = "auto"),
                   layerId = ~id  # Assign the ID as the layer ID for each marker
                   ) %>%
        fitBounds(bounds$min_lng, bounds$min_lat, bounds$max_lng, bounds$max_lat) %>%
        addScaleBar(position = "bottomleft",
                    scaleBarOptions(metric = TRUE, imperial = FALSE))
    })

    # get station id from click
    observeEvent(input$map_marker_click,{
      r_globals$station <- r_globals$all_stations[r_globals$all_stations$id == input$map_marker_click$id,]
    })

  })
}

## To be copied in the UI
# mod_stations_ui("stations_1")

## To be copied in the server
# mod_stations_server("stations_1")
