#' Create HRV trend plot
#' @param metrics Tibble of HRV metrics
#' @param metric Which metric to plot (rmssd or sdnn)
#' @return ggplot object
#' @export
plot_hrv_trend <- function(metrics, metric = "rmssd") {
  metrics %>%
    pivot_longer(
      cols = contains(metric),
      names_to = "position",
      values_to = "value"
    ) %>%
    ggplot(aes(date, value, color = position)) +
    geom_point() +
    geom_smooth(method = "loess") +
    labs(y = toupper(metric), x = "Date")
}
