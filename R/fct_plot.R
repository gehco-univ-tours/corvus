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
plot_add_raw_trace <- function(data, y, y_label){
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
                         y1 = Inf,
                         yref = "y"
                       )
                     )
    )
  }

  proxy_layout <- list(shapes = shapes_list)
  return(proxy_layout)
}
