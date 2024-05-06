#' Plot a plotly graph with selected parameter.
#'
#' @param data dataframe sensors dataset.
#' @param y text parameter selected.
#' @param y_title text parameter selected name.
#'
#' @importFrom plotly plot_ly layout
#'
#' @return plotly graph.
#' @export
plot_main <- function(data, y, y_title){
  plot <- plot_ly(x = data[["date_time"]],
                  y = data[[y]],
                  type = 'scatter', mode = 'lines'
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

#' plotly add trace.
#'
#' @param data data frame containing the selected axis data.
#' @param y text metric to be plotted on the y-axis.
#' @param y_label text name of the metric plotted.
#'
#' @return list
#' @export
plot_add_trace <- function(data, y, y_label){
  trace <- list(
    x = data[["date_time"]],
    y = data[[y]],
    type = 'scatter',
    mode = 'lines'
  )
  # layout <- list(
  #   yaxis = list(
  #     overlaying = 'y',
  #     showgrid = FALSE,  # Hide the gridlines for the second y-axis
  #     zeroline = FALSE,
  #     showline = FALSE,  # Hide the axis line for the second y-axis
  #     range = c(0, max(data[[y]]))
  #   )
  # )
  # proxy <- list("trace" = trace,
  #               "layout" = layout)
  return(trace)
}
