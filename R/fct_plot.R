#' Plot a plotly graph with selected parameter.
#'
#' @param data dataframe sensors dataset.
#' @param y text parameter selected.
#' @param y_title text parameter selected name.
#'
#' @importFrom plotly plot_ly layout add_trace
#'
#' @return plotly graph.
#' @export
plot_main <- function(data, y, y_title){
  plot <- plot_ly() %>%
    add_trace(x = data[["timestamp"]],
              y = data[[y]],
              type = 'scatter',
              mode = 'lines',
              name = "raw",
              line = list(color = 'black')
  ) %>%
    layout(
      xaxis = list(
        title = "Date time",
        rangeslider = list(type = "date")
      ),
      yaxis = list(
        title = y_title
      ),
      hovermode = "x unified"
    )
  return(plot)
}

#' plotly add raw data trace.
#'
#' @param data data frame containing the selected axis data.
#' @param y text metric to be plotted on the y-axis.
#' @param y_label text name of the metric plotted.
#'
#' @return list
#' @export
plot_add_corr_trace <- function(data, y, y_label){
  trace <- list(
    x = data[["timestamp"]],
    y = data[[y]],
    type = 'scatter',
    mode = 'lines',
    name = "corr",
    line = list(color = 'lightblue')
  )
  return(trace)
}

#' Create a vertical dashed line annotation for longitudinal profile plots
#'
#' This function generates a vertical dashed line annotation for longitudinal profile
#' plots using the 'plotly' package.
#'
#' @param x The x-coordinate where the vertical line should be positioned.
#' @param color The color of the vertical dashed line (default is "green").
#'
#' @return A list object representing a vertical dashed line annotation.
#'
#' @export
plot_vertical_line <- function(x = 0, color = "green") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    xref = "x",
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, width = 2, dash="dash")
  )
}

#' Create a list of vertical dashed line annotations for longitudinal profile plots
#'
#' @param data A data frame containing the x-coordinates of the vertical lines.
#'
#' @return A list object containing the vertical dashed line annotations.
#'
#' @export
plot_intervention_lines <- function(data){
  shapes_list <- list (
    shapes = lapply(data, function(x) {
      plot_vertical_line(x = x, color = "green")
    })
  )
  return (shapes_list)
}

#' plotly add edit trace.
#'
#' @param data data frame containing the selected axis data.
#' @param y text metric to be plotted on the y-axis.
#' @param y_label text name of the metric plotted.
#'
#' @return list
#' @export
plot_add_edit_trace <- function(data, y, y_label){
  trace <- list(
    x = data[["timestamp"]],
    y = data[[y]],
    type = 'scatter',
    mode = 'lines',
    name = "edit",
    line = list(color = 'orange')
  )
  return(trace)
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
    add_trace(x = data[["timestamp"]],
              y = data[["value_corr"]],
              type = 'scatter',
              mode = 'lines+markers',
              name = "raw",
              line = list(color = 'black')
    ) %>%
    layout(
      xaxis = list(
        title = "Date time",
        rangeslider = list(type = "date")
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
