#' @import ggplot2
#' @import patchwork
#' @import dplyr
#' @importFrom stats loess
#' @importFrom scales seq_gradient_pal
# Enhanced HRV visualization functions using ggplot2 and patchwork

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
plot_hrv_dashboard <- function(data) {
  # Calculate moving averages first
  data <- calculate_moving_averages(data)

  # 1. RMSSD Plot with training zones
  p1 <- ggplot(data, aes(x = as.Date(date))) +
    # Zone bands
    geom_ribbon(
      aes(
        ymin = rmssd_ma * 0.9,
        ymax = rmssd_ma * 1.1,
        fill = "Normal Zone"
      ),
      alpha = 0.2
    ) +
    geom_ribbon(
      aes(
        ymin = rmssd_ma * 0.85,
        ymax = rmssd_ma * 0.9,
        fill = "Caution Zone"
      ),
      alpha = 0.2
    ) +
    # Lines
    geom_line(aes(y = laying_rmssd, color = "Daily RMSSD"), size = 0.7) +
    geom_line(aes(y = rmssd_ma, color = "7-day MA"), size = 1) +
    # Styling
    scale_color_manual(
      values = c("Daily RMSSD" = "grey50", "7-day MA" = "blue")
    ) +
    scale_fill_manual(
      values = c(
        "Normal Zone" = "green",
        "Caution Zone" = "yellow"
      )
    ) +
    labs(
      title = "RMSSD Trend with Training Zones",
      y = "RMSSD",
      color = "Metric",
      fill = "Zone"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # 2. Orthostatic Response Plot
  p2 <- ggplot(data, aes(x = as.Date(date))) +
    geom_line(aes(y = standing_hr - laying_hr, color = "HR Response")) +
    geom_smooth(
      aes(y = standing_hr - laying_hr),
      method = "loess",
      span = 0.3,
      se = FALSE,
      color = "blue"
    ) +
    geom_hline(
      yintercept = 15,
      linetype = "dashed",
      color = "red",
      alpha = 0.5
    ) +
    labs(
      title = "Orthostatic Response",
      y = "HR Increase (bpm)",
      color = "Metric"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # 3. Heart Rate Recovery Plot
  p3 <- ggplot(data, aes(x = as.Date(date))) +
    geom_line(aes(y = hrr_60s, color = "60s HRR")) +
    geom_smooth(
      aes(y = hrr_60s),
      method = "loess",
      span = 0.3,
      se = FALSE,
      color = "blue"
    ) +
    geom_hline(
      yintercept = c(12, 20, 25),
      linetype = "dashed",
      color = c("red", "yellow", "green"),
      alpha = 0.5
    ) +
    labs(
      title = "Heart Rate Recovery (60s)",
      y = "Recovery (bpm)",
      color = "Metric"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # 4. Resting HR Plot
  p4 <- ggplot(data, aes(x = as.Date(date))) +
    geom_line(aes(y = laying_resting_hr, color = "Daily RHR")) +
    geom_line(aes(y = resting_hr_ma, color = "7-day MA"), size = 1) +
    geom_smooth(
      aes(y = laying_resting_hr),
      method = "loess",
      span = 0.3,
      se = FALSE,
      color = "blue"
    ) +
    labs(
      title = "Resting Heart Rate Trend",
      y = "RHR (bpm)",
      color = "Metric"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Combine plots using patchwork
  combined_plot <- (p1 + p2) / (p3 + p4) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = "HRV Recovery Dashboard",
      theme = theme_minimal()
    )

  return(combined_plot)
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
      linetype = "dashed",
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
