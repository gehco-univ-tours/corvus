#' Create a dygraph plot for the given data and parameters.
#'
#' @param data A list containing the measurement data to be plotted.
#' @param parameter_tocorr_name The name of the parameter to be corrected.
#' @param parameter_add_name The name of the additional parameter to be plotted (optional).
#' @param display_opts A list of boolean values indicating which series to display (raw, corrected, additional).
#'
#' @importFrom xts xts
#' @importFrom dygraphs dygraph dyRangeSelector dySeries dyAxis dyCrosshair
#'
#' @return A dygraph object that can be rendered in a Shiny app or R Markdown document.
#' @export
plot_dygraph <- function(data, parameter_tocorr_name, parameter_add_name, display_opts){

  if (all(display_opts == FALSE)) {
    # display empty dygraph plot
    df_empty <- data.frame(ts = seq.POSIXt(from = Sys.time() - 3600, to = Sys.time(), by = "min"), value = NA)
    empty_xts <- xts::xts(df_empty$value, order.by = as.POSIXct(df_empty$ts))
    dy <- dygraphs::dygraph(empty_xts) %>%
      dygraphs::dyRangeSelector()

    return(dy)
  }

  # create xts object
  raw_xts <- xts::xts(
    data$measurement_tocorr[["value"]],
    order.by = data$measurement_tocorr$ts
  )

  corr_xts <- xts::xts(
    data$measurement_tocorr[["value_corr"]],
    order.by = data$measurement_tocorr$ts
  )

  add_xts <- NULL
  if (!is.null(data$measurement_add)) {
    add_xts <- xts::xts(
      data$measurement_add[["value"]],
      order.by = data$measurement_add$ts
    )
  }

  edit_xts <- NULL
  if (!is.null(data$measurement_edit[["value_edit"]])) {
    edit_xts <- xts::xts(
      data$measurement_edit[["value_edit"]],
      order.by = data$measurement_edit$ts
    )
  }

  # merge series
  series_list <- list()

  if (display_opts$measurement_tocorr_raw) series_list$raw <- raw_xts
  if (display_opts$measurement_tocorr_corr) series_list$corr <- corr_xts
  if (display_opts$measurement_add && !is.null(add_xts)) series_list$add <- add_xts
  if (display_opts$measurement_edit && !is.null(edit_xts)) series_list$edit <- edit_xts

  # merge intelligent
  all_series <- do.call(merge, series_list)

  colnames(all_series) <- names(series_list)

  # create dygraph
  dy <- dygraphs::dygraph(all_series) %>%
    dygraphs::dyCrosshair(direction = "vertical") %>%
    dygraphs::dyRangeSelector()

  # add series
  if ("add" %in% colnames(all_series)) {
    dy <- dy %>%
      dygraphs::dySeries("add",
                         label = parameter_add_name,
                         color = "blue",
                         axis = "y2")
  }

  if ("raw" %in% colnames(all_series)) {
    dy <- dy %>%
      dygraphs::dySeries("raw",
                         label = paste0(parameter_tocorr_name, " raw"),
                         color = "black")
  }

  if ("corr" %in% colnames(all_series)) {
    dy <- dy %>%
      dygraphs::dySeries("corr",
                         label = paste0(parameter_tocorr_name, " corrected"),
                         color = "green")
  }

  if ("edit" %in% colnames(all_series)) {
    dy <- dy %>%
      dygraphs::dySeries("edit",
                        label =paste0(parameter_tocorr_name, " edited"),
                        color = "orange")
  }

  # second y axis if additional parameter is displayed
  if ("add" %in% colnames(all_series)) {
    dy <- dy %>%
      dygraphs::dyAxis("y2", independentTicks = TRUE)
  }

  return(dy)
}

#' plotly add missing periods.
#'
#' @param data data frame containing the selected axis data.
#'
#' @return list
#' @export
plot_add_missing_period <- function(data){

  # add vertical bar for missing data
  shapes_list <- list()

  for (i in 1:nrow(data)){
    shapes_list <- c(shapes_list,
                     list(
                       list(
                         type = "rect",
                         fillcolor = "tomato",
                         opacity = 0.5,
                         line = list(width = 0),
                         x0 = data[i, "time_start"],
                         x1 = data[i, "time_end"],
                         xref = "x",
                         y0 = 0,
                         y1 = 4000,
                         yref = "y"
                       )
                     )
    )
  }

  proxy_layout <- list(shapes = shapes_list)
  return(proxy_layout)
}

#' plotly add available periods.
#'
#' @param data data frame containing the selected axis data.
#'
#' @return list
#' @export
plot_add_valid_period <- function(data){

  # create a plot frame with x-axis min max from time period
  plot <- plot_ly() %>%
    add_trace(x = data[["ts"]],
              y = data[["value_corr"]],
              type = 'scatter',
              mode = 'lines+markers',
              name = "raw",
              line = list(color = 'black')
    ) %>%
    layout(
      xaxis = list(
        title = "Date time"
      ),
      yaxis = list(
        title = "Value"
      ),
      hovermode = "x unified"
    )

  # add vertical bar for missing data
  shapes_list <- list()

  for (i in 1:nrow(data)){
    shapes_list <- c(shapes_list,
                     list(
                       list(
                         type = "rect",
                         fillcolor = "lightgreen",
                         opacity = 0.5,
                         line = list(width = 0),
                         x0 = data[i, "time_start"],
                         x1 = data[i, "time_end"],
                         xref = "x",
                         y0 = 0,
                         y1 = 1,
                         yref = "y"
                       )
                     )
    )
  }

  proxy_layout <- list(shapes = shapes_list)
  return(proxy_layout)
}

#' plotly add available periods.
#'
#' @param station_id integer station id.
#' @param date_start date start date.
#' @param date_end date end date.
#'
#' @importFrom plotly plot_ly add_bars layout add_annotations subplot
#'
#' @return plotly graph
#' @export
plot_available_raw <- function(station_id, date_start, date_end){

  # Get the parameters for the station
  parameters <- db_get_station_parameters(con = db_con(), station_id = station_id)

  # Initialize an empty list to store the plots
  plot_list <- list()

  for (i in 1:length(parameters)) {
    sensor_id <- db_get_sensor_id(con = db_con(), station_id = station_id, parameter_id = parameters[[i]])
    interval_time <- db_get_interval(con = db_con(), sensor_id = sensor_id)

    # get available data
    data <- data_get_available_period(con = db_con(), sensor_id = sensor_id, start_date = date_start, end_date = date_end, interval_time = interval_time)
    data$width <- as.numeric(difftime(data$time_end, data$time_start, units = "secs"))
    data$name <- names(parameters[i])

    # get missing data
    missing_data <- data_get_missing_period(con = db_con(), sensor_id = sensor_id, start_date = date_start, end_date = date_end, interval_time = interval_time)
    missing_data$width <- as.numeric(difftime(missing_data$time_end, missing_data$time_start, units = "secs"))
    missing_data$name <- names(parameters[i])

    # Get the minimum and maximum time for the x-axis range
    min_time <- min(c(min(data$time_start), min(missing_data$time_start)))
    max_time <- max(c(max(data$time_end), max(missing_data$time_end)))

    # create bar plot with time period and y = 1
    plot <- plot_ly(data = data) %>%
      add_bars(x = ~time_start,
               y = ~1,
               width = ~width * 1000,
               offset = 0,  # No offset needed if width starts from time_start
               # green color
               marker = list(color = "lightgreen"),
               # mouse hover show period
               text = ~paste(name, " : ",time_start, " - ", time_end),
               hoverinfo = "text"
      )
    plot <- plot %>%
      add_trace(data = missing_data,
                x = ~time_start,
                y = ~1,
                width = ~width * 1000,
                offset = 0,  # No offset needed if width starts from time_start
                type="bar",
                marker = list(color = "tomato"),
                text = ~paste(name, " : ",time_start, " - ", time_end),
                hoverinfo = "text"
      )
    plot <- plot %>% layout(
      yaxis = list(
        # title = names(parameters[i]),
        showticklabels = FALSE),
      showlegend = FALSE
    ) %>%
      add_annotations(
        x = min_time + (max_time - min_time) / 2,
        y = 1,
        text = names(parameters[i]),
        showarrow = FALSE,
        xref = "x",
        yref = "y",
        yshift = 10
      )
    plot_list[[i]] <- plot
  }

  # Combine both plots into a subplot with shared x-axis
  combined_plot <- subplot(plot_list, nrows = length(parameters), shareX = TRUE) %>%
    layout(
      xaxis = list(type = 'date',
                   # tickformat = "%Y-%m-%d %H:%M",
                   rangeslider = list(type = "date"))  # Shared x-axis slider
    )

  return(combined_plot)
}
