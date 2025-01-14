#' Plot HRV summaries for a single orthostatic test fit file.
#'
#' @param fit_file_path Path to the fit file
#' @param base "HR" if x axis should be heart rate or "RR" of x axis should be RR.
#' @param filter_factor Used to filter RR values that are 1.x times smaller or
#'   larger than the last one as suspected false readings.
#'
#' @return A ggplot2 plot object.
#' @export
#'
#' @importFrom FITfileR readFitFile
#' @importFrom dplyr '%>%'
#' @importFrom ggplot2 ggplot geom_line geom_vline scale_x_continuous annotate aes xlab theme_bw ggtitle
hrv_plot <- function(file_path, base = "HR", filter_factor = 0.175) {
  fit_object <- FITfileR::readFitFile(file_path)

  # Extract RR intervals
  RR <- extract_rr_intervals(
    fit_object = fit_object,
    filter_factor = filter_factor
  )
  HR <- get_HR(fit_object = fit_object)

  metrics <- process_fit_file(
    file_path = file_path,
    filter_factor = filter_factor
  )

  standing_RR <- RR$standing
  laying_RR <- RR$laying
  RR <- data.frame(
    RR = c(laying_RR, standing_RR),
    time = cumsum(c(laying_RR, standing_RR))
  )
  RR$RR_ms <- RR$RR * 1000

  if (base == "RR") {
    p <- RR %>%
      ggplot(aes(x = time, y = RR)) +
      geom_line() +
      geom_vline(xintercept = 180, linetype = "dashed", color = "red") +
      scale_x_continuous(limits = c(0, 360), breaks = c(0, 60, 120, 180, 240, 300, 360), expand = c(0, 0)) +
      annotate("text",
        color = "red", x = 80, y = mean(standing_RR),
        label = paste0("SDNN(ms): ", metrics$laying_sdnn)
      ) +
      annotate("text",
        color = "red", x = 80, y = mean(standing_RR) - 0.04,
        label = paste0("rMSSD(ms): ", metrics$laying_rmssd)
      ) +
      annotate("text",
        color = "red", x = 80, y = mean(standing_RR) - 0.08,
        label = paste0("resting HR(bpm): ", metrics$laying_resting_hr)
      ) +
      annotate("text",
        color = "red", x = 280, y = mean(laying_RR) + 0.1,
        label = paste0("SDNN(ms): ", metrics$standing_sdnn)
      ) +
      annotate("text",
        color = "red", x = 280, y = mean(laying_RR) + 0.06,
        label = paste0("rMSSD(ms): ", metrics$standing_rmssd)
      ) +
      annotate("text",
        color = "red", x = 218, y = min(RR$RR),
        label = paste0("max HR(bpm): ", max(HR))
      ) +
      xlab("Time (s)") +
      ggtitle(metrics$time_of_day) +
      theme_bw(base_size = 12)
  } else if (base == "HR") {
    p <- data.frame(HR = HR) %>%
      ggplot(aes(y = HR, x = seq_along(HR))) +
      geom_line() +
      geom_vline(xintercept = 180, linetype = "dashed", color = "red") +
      scale_x_continuous(
        limits = c(0, 360),
        breaks = c(0, 60, 120, 180, 240, 300, 360),
        expand = c(0, 0)
      ) +
      annotate("text",
        color = "red", x = 80, y = mean(HR[180:length(HR)]) + 15,
        label = paste0("SDNN(ms): ", metrics$laying_sdnn)
      ) +
      annotate("text",
        color = "red", x = 80, y = mean(HR[180:length(HR)]) + 11,
        label = paste0("rMSSD(ms): ", metrics$laying_rmssd)
      ) +
      annotate("text",
        color = "red", x = 80, y = mean(HR[180:length(HR)]) + 7,
        label = paste0("Resting HR(bpm): ", metrics$laying_resting_hr)
      ) +
      annotate("text",
        color = "red", x = 280, y = mean(HR[1:180]) + 5,
        label = paste0("SDNN(ms): ", metrics$standing_sdnn)
      ) +
      annotate("text",
        color = "red", x = 280, y = mean(HR[1:180]) + 1,
        label = paste0("rMSSD(ms): ", metrics$standing_rmssd)
      ) +
      annotate("text",
        color = "red", x = 218, y = max(HR[10:length(HR)]),
        label = paste0("max HR(bpm): ", max(HR))
      ) +
      xlab("Time (s)") +
      ggtitle(metrics$time_of_day) +
      theme_bw(base_size = 12)
  } else {
    stop("No proper base given. Must be 'RR' or 'HR'")
  }
  return(p)
}



#' Trend plots of HRV summaries for orthostatic tests
#'
#' @param metrics A tibble or DF containing the metrics. Output of
#'  \link{process_fit_directory}
#'
#' @return A ggplot2 plot object.
#' @export
#' @importFrom dplyr '%>%' group_by summarise filter across bind_rows as_tibble
#' @importFrom ggplot2 ggplot geom_point geom_line geom_hline facet_grid theme_bw aes
hrv_trend_plot <- function(metrics, just_rssme = FALSE) {
  metrics <- dplyr::select(metrics, !c(source_file, package_version, activity))
  metrics <- calculate_moving_averages(metrics)

  long_metrics <- metrics %>%
    tidyr::pivot_longer(
      !c(date, week, time_of_day),
      names_to = c("position", "metric"),
      names_pattern = "(.+?)_(.*)",
      values_to = "value"
    )

  overall_means <- as.data.frame(long_metrics) %>%
    dplyr::group_by(across(c("time_of_day", "position", "metric"))) %>%
    dplyr::summarise(mean = mean(value)) %>%
    dplyr::mutate(
      position = dplyr::case_when(
        metric == "resting_hr" ~ "resting_hr",
        .default = position
      )
    )

  weekly_means <- as.data.frame(long_metrics) %>%
    dplyr::group_by(across(c("time_of_day", "position", "metric", "week"))) %>%
    dplyr::summarise(mean = mean(value))

  long_metrics <- long_metrics %>%
    dplyr::left_join(
      weekly_means,
      by = c("time_of_day", "position", "metric", "week")
    ) %>%
    dplyr::rename(weekly_mean = mean)

  if (just_rssme) {
    long_metrics %>%
      dplyr::filter(
        metric == "rmssd" | metric == "resting_hr",
        time_of_day == "Morning"
      ) %>%
      dplyr::mutate(
        position = dplyr::case_when(
          metric == "resting_hr" ~ "resting_hr",
          .default = position
        ),
        date = as.Date(date)
      ) %>%
      ggplot(aes(x = date, y = value, color = position)) +
      geom_point() +
      geom_line() +
      geom_line(aes(y = weekly_mean), linetype = "dashed") +
      geom_hline(
        data = dplyr::filter(
          overall_means,
          time_of_day == "Morning",
          metric == "rmssd" | metric == "resting_hr"
        ),
        aes(yintercept = mean, color = position),
        linetype = "dotted"
      ) +
      ylab("rMSSD") +
      xlab("Date") +
      # facet_grid(metric ~ morning, scales = "free_y") +
      theme_bw()
  } else {
    long_metrics %>%
      ggplot(aes(x = date, y = value, color = position)) +
      geom_point() +
      geom_line() +
      geom_line(aes(y = weekly_mean), linetype = "dashed") +
      geom_hline(
        data = overall_means,
        aes(yintercept = mean, color = position),
        linetype = "dotted"
      ) +
      facet_grid(metric ~ time_of_day, scales = "free_y") +
      theme_bw()
  }
}

#' @importFrom ggplot2 ggplot geom_ribbon geom_line scale_color_manual scale_fill_manual theme_minimal labs
#' @export
plot_hrv_trends <- function(data) {
  data %>%
    calculate_moving_averages() %>%
    ggplot2::ggplot(aes(x = date)) +
    ggplot2::geom_line(aes(y = laying_rmssd, color = "Daily RMSSD")) +
    ggplot2::geom_line(aes(y = rmssd_ma, color = "7-day MA"), size = 1) +
    ggplot2::geom_ribbon(
      aes(
        ymin = rmssd_ma * 0.9, # 10% below baseline
        ymax = rmssd_ma * 1.1, # 10% above baseline
        fill = "Normal Range"
      ),
      alpha = 0.2
    ) +
    ggplot2::scale_color_manual(
      values = c("Daily RMSSD" = "grey50", "7-day MA" = "blue")
    ) +
    ggplot2::scale_fill_manual(values = c("Normal Range" = "green")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "HRV Trend with Moving Average",
      y = "RMSSD",
      color = "Metric"
    )
}