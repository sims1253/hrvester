#' Create comprehensive HRV dashboard
#'
#' Generates a 4-panel dashboard showing key HRV metrics including:
#' - RMSSD trend with training zones
#' - Orthostatic response
#' - Heart rate recovery
#' - Resting heart rate trend
#'
#' @param data A data frame containing HRV metrics with columns:
#'   \itemize{
#'     \item date: Date of measurement
#'     \item laying_rmssd: RMSSD in laying position
#'     \item rmssd_ma: 7-day moving average of RMSSD
#'     \item standing_hr: Standing heart rate
#'     \item laying_hr: Laying heart rate
#'     \item hrr_60s: Heart rate recovery after 60 seconds
#'     \item laying_resting_hr: Resting heart rate
#'     \item resting_hr_ma: 7-day moving average of resting HR
#'   }
#' @return A patchwork object combining four ggplot2 plots
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_tile geom_smooth scale_color_manual scale_fill_manual labs theme_minimal theme_bw theme element_text scale_y_continuous geom_hline
#' @importFrom dplyr mutate filter %>%
#' @importFrom stats loess
#' @importFrom scales seq_gradient_pal
plot_hrv_dashboard <- function(data) {
  # Pre-process data
  data <- data %>%
    mutate(
      date = as.Date(date),
      group = 1 # Add constant group for geom_line
    ) %>%
    calculate_moving_averages() %>%
    mutate(
      ortho_response = standing_hr - laying_hr,
      ortho_ma = calculate_robust_ma(ortho_response),
      hrr_ma = calculate_robust_ma(hrr_60s)
    ) %>%
    calculate_neural_recovery()

  # Get recommendations
  current_day <- max(data$date)
  current_metrics <- data %>%
    filter(date == current_day) %>%
    slice(1)

  baseline_metrics <- data %>%
    filter(date < current_day & date >= current_day - 7)

  readiness <- analyze_readiness(current_metrics, baseline_metrics)
  training_rec <- generate_training_recommendations(
    current_metrics$neural_recovery_score,
    primary_type = "BJJ"
  )

  # Base theme
  base_theme <- theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  
  # 1. RMSSD Plot
  p1 <- ggplot(data, aes(x = date, group = group)) +
    geom_ribbon(
      aes(
        ymin = rmssd_ma * 0.8,
        ymax = rmssd_ma * 0.9,
        fill = "Caution"
      ),
      alpha = 0.15
    ) +
    geom_ribbon(
      aes(
        ymin = rmssd_ma * 0.9,
        ymax = rmssd_ma * 1.0,
        fill = "Normal"
      ),
      alpha = 0.2
    ) +
    geom_ribbon(
      aes(
        ymin = rmssd_ma * 1.0,
        ymax = rmssd_ma * 1.1,
        fill = "Optimal"
      ),
      alpha = 0.25
    ) +
    geom_line(aes(y = laying_rmssd, color = "Daily")) +
    geom_line(aes(y = rmssd_ma, color = "7-day MA")) +
    scale_color_manual(
      values = c("Daily" = "#56B4E9", "7-day MA" = "#E69F00"),
      name = "Measurement"
    ) +
    scale_fill_manual(
      values = c("Optimal" = "#0072B2", "Normal" = "#009E73", "Caution" = "#D55E00"),
      name = "Recovery Zones",
      breaks = c("Optimal", "Normal", "Caution")
    ) +
    labs(title = "RMSSD Trend", y = "RMSSD", x = NULL) +
    base_theme

  # 2. Orthostatic Response Plot
  p2 <- ggplot(data, aes(x = date, group = group)) +
    geom_line(aes(y = ortho_response, color = "Daily")) +
    geom_line(aes(y = ortho_ma, color = "7-day MA")) +
    geom_hline(yintercept = 15, linetype = "dotted", color = "gray50") +
    scale_color_manual(
      values = c("Daily" = "#56B4E9", "7-day MA" = "#E69F00"),
      name = "Measurement"
    ) +
    labs(title = "Orthostatic Response", y = "HR Increase (bpm)", x = NULL) +
    base_theme

  # 3. Heart Rate Recovery Plot
  p3 <- ggplot(data, aes(x = date, group = group)) +
    geom_line(aes(y = hrr_60s, color = "Daily")) +
    geom_line(aes(y = hrr_ma, color = "7-day MA")) +
    geom_hline(
      yintercept = c(12, 20, 25),
      linetype = "dotted",
      color = "gray50"
    ) +
    scale_color_manual(
      values = c("Daily" = "#56B4E9", "7-day MA" = "#E69F00"),
      name = "Measurement"
    ) +
    labs(title = "Heart Rate Recovery (60s)", y = "Recovery (bpm)", x = "Date") +
    base_theme

  # 4. Resting HR Plot
  p4 <- ggplot(data, aes(x = date, group = group)) +
    geom_line(aes(y = laying_resting_hr, color = "Daily")) +
    geom_line(aes(y = resting_hr_ma, color = "7-day MA")) +
    scale_color_manual(
      values = c("Daily" = "#56B4E9", "7-day MA" = "#E69F00"),
      name = "Measurement"
    ) +
    labs(title = "Resting Heart Rate", y = "RHR (bpm)", x = "Date") +
    base_theme

  # Create a dummy plot for the legend
  legend_data <- data.frame(
    x = 1:3,
    y = 1:3,
    type = factor(c("Daily", "7-day MA", "Recovery Zones")),
    fill_type = factor(c("Optimal", "Normal", "Caution"),
                         levels = c("Optimal", "Normal", "Caution"))
  )

  legend_plot <- ggplot(legend_data) +
    geom_tile(aes(x = x, y = y, fill = fill_type),
              color = NA, width = 0.9, height = 0.9) +
    geom_line(aes(x = x, y = y, color = type)) +
    scale_fill_manual(
      values = c(
        "Optimal" = "#0072B2",
        "Normal" = "#009E73",
        "Caution" = "#D55E00"
      ),
      name = "Recovery Zones"
    ) +
    scale_color_manual(
      values = c("Daily" = "#56B4E9", "7-day MA" = "#E69F00"),
      name = "Measurement"
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.box = "vertical",
      legend.direction = "vertical",
      legend.spacing.y = unit(15, "pt"),
      legend.box.just = "left"
    ) +
    guides(
      fill = guide_legend(order = 2, nrow = 1),
      color = guide_legend(order = 1, nrow = 1)
    )

  # Extract legend
  tmp <- ggplot_gtable(ggplot_build(legend_plot))
  legend <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[legend]]

  # Create text elements
  left_text <- grid::textGrob(
    paste(
      "Recommendations based on relative state:",
      sprintf("BJJ: %s", readiness$recommendations$bjj),
      sprintf("Strength: %s", readiness$recommendations$strength),
      sprintf("Cardio: %s", readiness$recommendations$cardio),
      sep = "\n"
    ),
    gp = grid::gpar(fontsize = 12),
    just = "right"
  )

  right_text <- grid::textGrob(
    paste(
      "Recommendations based on absolute state:",
      sprintf("Intensity: %s", training_rec$intensity),
      sprintf("Volume: %s", training_rec$volume),
      sprintf("Focus: %s", training_rec$focus),
      sep = "\n"
    ),
    gp = grid::gpar(fontsize = 12),
    just = "left"
  )

  # Create layout
  main_plots <- gridExtra::arrangeGrob(
    p1, p2, p3, p4,
    nrow = 2,
    ncol = 2
  )

  # Create the title
  title <- grid::textGrob("HRV Recovery Dashboard",
    gp = grid::gpar(fontface = "bold", fontsize = 14)
  )

  # Create the bottom row with text and legend
  bottom_row <- gridExtra::arrangeGrob(
    left_text,
    legend,
    right_text,
    ncol = 3,
    widths = unit(c(5, 5, 5), "cm"),
    padding = unit(5, "mm")
  )

  # Combine everything
  final_plot <- gridExtra::arrangeGrob(
    title,
    main_plots,
    bottom_row,
    heights = unit(c(0.5, 4, 1), "null"),
    padding = unit(0, "mm")
  )
  gridExtra::grid.arrange(
    final_plot
  )
}

#' Create weekly summary heatmap
#'
#' Generates a heatmap showing weekly recovery status based on RMSSD changes.
#' Status categories are:
#' - Fresh: RMSSD change > 5%
#' - Normal: RMSSD change between -5% and 5%
#' - Caution: RMSSD change between -10% and -5%
#' - Warning: RMSSD change < -10%
#'
#' @param data A data frame containing HRV metrics with columns:
#' @param method Either neural_recovery_score or rmssd_change
#'   \itemize{
#'     \item date: Date of measurement
#'     \item rmssd_change: Percentage change in RMSSD
#'   }
#' @return A ggplot2 heatmap object
#' @export
plot_weekly_heatmap <- function(data, method = "neural_recovery_score") {
  # Define weekday order (Monday to Sunday)
  weekday_order <- c(
    "Montag", "Dienstag", "Mittwoch", "Donnerstag",
    "Freitag", "Samstag", "Sonntag"
  )

  # Calculate daily status
  if (method == "rmssd_change") {
    data <- data %>%
      calculate_moving_averages() %>%
      mutate(
        day_of_week = weekdays(as.Date(date)),
        # Convert to factor with correct order
        day_of_week = factor(day_of_week, levels = weekday_order),
        week = format(as.Date(date), "%Y-W%V"),
        status = case_when(
          rmssd_change > 5 ~ "Fresh",
          rmssd_change >= -5 ~ "Normal",
          rmssd_change >= -10 ~ "Caution",
          TRUE ~ "Warning"
        ),
        # Convert status to factor with desired order
        status = factor(status, levels = c("Warning", "Caution", "Normal", "Fresh"))
      )
  } else if (method == "neural_recovery_score") {
    data <- data %>%
      calculate_neural_recovery() %>%
      mutate(
        day_of_week = weekdays(as.Date(date)),
        # Convert to factor with correct order
        day_of_week = factor(day_of_week, levels = weekday_order),
        week = format(as.Date(date), "%Y-W%V"),
        status = case_when(
          neural_recovery_score >= 80 ~ "Fresh",
          neural_recovery_score >= 70 ~ "Good",
          neural_recovery_score >= 40 ~ "Reduced",
          TRUE ~ "Low"
        ),
        # Convert status to factor with desired order
        status = factor(status, levels = c("Low", "Reduced", "Good", "Fresh"))
      )
  }

  # Create heatmap with ordered factors
  ggplot(data, aes(x = day_of_week, y = week)) +
    geom_tile(aes(fill = status), color = "white") +
    scale_fill_manual(
      values = c(
        "Fresh" = "#0072B2",
        "Good" = "#56B4E9",
        "Normal" = "#56B4E9",
        "Reduced" = "#F0E442",
        "Caution" = "#F0E442",
        "Low" = "#D55E00",
        "Warning" = "#D55E00"
      ),
      # Legend will follow the factor order defined above
      guide = guide_legend(reverse = TRUE) # Reverse to show best status at top
    ) +
    labs(
      title = "Weekly Recovery Status",
      x = "Day of Week",
      y = "Week",
      fill = "Status"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
}

#' Plot BJJ-specific recovery metrics
#'
#' Generates a neural recovery score plot specifically tailored for Brazilian
#' Jiu-Jitsu athletes. The score combines:
#' - RMSSD relative to 7-day average
#' - Orthostatic response
#'
#' @param data A data frame containing HRV metrics with columns:
#'   \itemize{
#'     \item date: Date of measurement
#'     \item laying_rmssd: RMSSD in laying position
#'     \item rmssd_ma: 7-day moving average of RMSSD
#'     \item standing_hr: Standing heart rate
#'     \item laying_hr: Laying heart rate
#'   }
#' @return A ggplot2 line plot showing neural recovery score over time
#' @export
plot_bjj_metrics <- function(data) {
  # Calculate neural recovery score (simplified version)
  data <- data %>%
    calculate_moving_averages() %>%
    mutate(
      neural_score = (laying_rmssd / rmssd_ma * 50) +
        (1 - (standing_hr - laying_hr) / 30) * 50 # Normalize to 0-100 scale
    )

  ggplot(data, aes(x = as.Date(date))) +
    geom_line(aes(y = neural_score, color = "Neural Recovery")) +
    geom_smooth(
      aes(y = neural_score),
      method = "loess",
      span = 0.3,
      se = FALSE,
      color = "blue"
    ) +
    scale_y_continuous(
      limits = c(0, 150),
      breaks = seq(0, 150, 50)
    ) +
    geom_hline(
      yintercept = c(40, 60, 80),
      linetype = "solid",
      color = c("red", "yellow", "green"),
      alpha = 0.5
    ) +
    labs(
      title = "BJJ Neural Recovery Score",
      y = "Recovery Score",
      color = "Metric"
    ) +
    theme_bw()
}
