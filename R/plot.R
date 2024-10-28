#' Create HRV trend plot
#' @param metrics Tibble of HRV metrics
#' @param metric Which metric to plot ("rmssd", "sdnn", or "hr")
#' @return ggplot object
#' @export
plot_hrv_trend <- function(metrics, metric = "rmssd") {
  y_labels <- c(
    rmssd = "RMSSD (ms)",
    sdnn = "SDNN (ms)",
    hr = "Heart Rate (bpm)"
  )
  
  metrics %>%
    pivot_longer(
      cols = contains(metric),
      names_to = "position",
      values_to = "value"
    ) %>%
    ggplot(aes(date, value, color = position)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", se = FALSE) +
    facet_wrap(~time_of_day) +
    labs(
      y = y_labels[metric],
      x = "Date",
      color = "Position"
    ) +
    theme_minimal()
}
