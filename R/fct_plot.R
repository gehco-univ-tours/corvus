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
    layout(xaxis = list(
      title = "Date time"),
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
