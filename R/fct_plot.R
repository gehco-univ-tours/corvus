#' Create a dygraph plot for the given data and parameters.
#'
#' @param data A list containing the measurement data to be plotted.
#' @param parameter_tocorr_name The name of the parameter to be corrected.
#' @param parameter_add_name The name of the additional parameter to be plotted (optional).
#' @param y_title text the parameter name and unit.
#' @param y_title_add text additionnal parameter name and unit.
#' @param display_opts A list of boolean values indicating which series to display (raw, corrected, additional).
#'
#' @importFrom xts xts
#' @importFrom dygraphs dygraph dyRangeSelector dySeries dyAxis dyCrosshair dyOptions dyAxis
#'
#' @return A dygraph object that can be rendered in a Shiny app or R Markdown document.
#' @export
plot_dygraph <- function(data, parameter_tocorr_name, parameter_add_name,
                         y_title, y_title_add, display_opts){

  no_data <- (
    is.null(data$measurement_raw) ||
      length(data$measurement_raw[["value"]]) == 0
  )

  if (all(display_opts == FALSE) || no_data) {
    # display empty dygraph plot
    df_empty <- data.frame(ts = seq.POSIXt(from = Sys.time() - 3600, to = Sys.time(), by = "min"), value = NA)
    empty_xts <- xts::xts(df_empty$value, order.by = as.POSIXct(df_empty$ts))
    dy <- dygraphs::dygraph(empty_xts) %>%
      dygraphs::dyRangeSelector()

    return(dy)
  }

  # create xts object
  raw_xts <- xts::xts(
    data$measurement_raw[["value"]],
    order.by = data$measurement_raw$ts
  )

  corr_xts <- NULL
  if (!is.null(data$measurement_corr) &&
      length(data$measurement_corr[["value"]]) > 0) {
    corr_xts <- xts::xts(
      data$measurement_corr[["value"]],
      order.by = data$measurement_corr$ts
    )
  }

  filter_xts <- NULL
  if (!is.null(data$measurement_filter) &&
      length(data$measurement_filter[["value"]]) > 0) {
    filter_xts <- xts::xts(
      data$measurement_filter[["value"]],
      order.by = data$measurement_filter$ts
    )
  }

  add_xts <- NULL
  if (!is.null(data$measurement_add) &&
      length(data$measurement_add[["value"]]) > 0) {
    add_xts <- xts::xts(
      data$measurement_add[["value"]],
      order.by = data$measurement_add$ts
    )
  }

  edit_xts <- NULL
  if (!is.null(data$measurement_edit[["value"]]) &&
      length(data$measurement_edit[["value"]]) > 0) {
    edit_xts <- xts::xts(
      data$measurement_edit[["value"]],
      order.by = data$measurement_edit$ts
    )
  }

  # merge series
  series_list <- list()

  if (display_opts$measurement_raw) series_list$raw <- raw_xts
  if (display_opts$measurement_corr && !is.null(corr_xts)) series_list$corr <- corr_xts
  if (display_opts$measurement_filter && !is.null(filter_xts)) series_list$filter <- filter_xts
  if (display_opts$measurement_add && !is.null(add_xts)) series_list$add <- add_xts
  if (display_opts$measurement_edit && !is.null(edit_xts)) series_list$edit <- edit_xts

  # merge intelligent
  all_series <- do.call(merge, series_list)

  colnames(all_series) <- names(series_list)

  # create dygraph
  dy <- dygraphs::dygraph(
      all_series, ylab = y_title) %>%
    dygraphs::dyCrosshair(direction = "vertical") %>%
    dygraphs::dyRangeSelector() %>%
    dygraphs::dyOptions(useDataTimezone = TRUE,
                        retainDateWindow = TRUE)

  # add series
  if ("add" %in% colnames(all_series)) {
    dy <- dy %>%
      dygraphs::dySeries("add",
                         label = parameter_add_name,
                         color = "blue",
                         axis = "y2") %>%
      dygraphs::dyAxis("y2", label = y_title_add)
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

  if ("filter" %in% colnames(all_series)) {
    dy <- dy %>%
      dygraphs::dySeries("filter",
                         label = paste0(parameter_tocorr_name, " filtered"),
                         color = "magenta")
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

