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
  plot <- plot_ly(data, x = ~date_time,
                  y = ~y,
                  type = 'scatter', mode = 'lines'
  ) %>%
    layout(xaxis = list(
      title = 'Date Time'),
      yaxis = list(
        title = y_title
      ),
      hovermode = "x unified"
    )
  return(plot)
}
