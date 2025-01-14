# HRV Alert and Status Functions

#' Analyze HRV readiness status
#' @param current_metrics Current day's HRV metrics
#' @param baseline_metrics Previous 7 days' metrics
#' @return List containing status and recommendations
#' @export
analyze_readiness <- function(current_metrics, baseline_metrics) {
  # Calculate baseline values (7-day rolling)
  baseline <- list(
    rmssd = mean(baseline_metrics$laying_rmssd, na.rm = TRUE),
    resting_hr = mean(baseline_metrics$laying_resting_hr, na.rm = TRUE),
    standing_response = mean(baseline_metrics$orthostatic_rise, na.rm = TRUE)
  )

  # Calculate current deviations
  deviations <- list(
    rmssd_dev = (current_metrics$laying_rmssd - baseline$rmssd) / baseline$rmssd * 100,
    hr_dev = (current_metrics$laying_resting_hr - baseline$resting_hr) / baseline$resting_hr * 100,
    ortho_dev = (current_metrics$orthostatic_rise - baseline$standing_response) / baseline$standing_response * 100
  )

  # Define training status based on BJJ-specific thresholds
  status <- if (deviations$rmssd_dev > 5) {
    "FRESH" # Enhanced recovery state
  } else if (deviations$rmssd_dev >= -5) {
    "NORMAL" # Good to train normally
  } else if (deviations$rmssd_dev >= -10) {
    "CAUTION" # Reduce intensity/volume
  } else {
    "WARNING" # Active recovery only
  }

  # Generate BJJ-specific recommendations
  recommendations <- dplyr::case_when(
    status == "FRESH" ~ list(
      bjj = "Full training, good day for hard rolls",
      strength = "Proceed with planned session",
      cardio = "Good day for higher intensity work"
    ),
    status == "NORMAL" ~ list(
      bjj = "Regular training as planned",
      strength = "Proceed as planned",
      cardio = "Stay in Zone 1-2"
    ),
    status == "CAUTION" ~ list(
      bjj = "Technical work only, limit sparring",
      strength = "Reduce volume by 20%, maintain intensity",
      cardio = "Zone 1 only, max 30 minutes"
    ),
    status == "WARNING" ~ list(
      bjj = "Technique or drilling only, no sparring",
      strength = "Active recovery or rest day",
      cardio = "Light mobility work only"
    )
  )

  # Additional BJJ-specific flags
  flags <- list(
    high_fatigue = deviations$hr_dev > 5 && deviations$rmssd_dev < -5,
    poor_recovery = deviations$ortho_dev > 15,
    overreaching_risk = all(tail(baseline_metrics$laying_rmssd, 3) < baseline$rmssd * 0.9)
  )

  return(list(
    status = status,
    recommendations = recommendations,
    flags = flags,
    metrics = list(
      baseline = baseline,
      deviations = deviations
    )
  ))
}

#' Generate daily HRV report
#' @param data HRV data frame
#' @return Formatted report
generate_daily_report <- function(data) {
  # Get latest metrics
  current_day <- max(data$date)
  current_metrics <- data %>%
    filter(date == current_day) %>%
    head(1)

  # Get baseline period
  baseline_metrics <- data %>%
    filter(
      date < current_day,
      date >= current_day - 7
    )

  # Get readiness analysis
  readiness <- analyze_readiness(current_metrics, baseline_metrics)

  # Format report
  report <- sprintf(
    "
HRV Status Report for %s

Status: %s

Current Metrics:
- RMSSD: %.1f (%.1f%% from baseline)
- Resting HR: %d (%.1f%% from baseline)
- Orthostatic Response: %.1f%% (%.1f%% from baseline)

Recommendations:
BJJ: %s
Strength: %s
Cardio: %s

Flags:%s%s%s

7-Day Trends:
- RMSSD Trend: %s
- HR Trend: %s
",
    format(as.Date(current_day), "%B %d, %Y"),
    readiness$status,
    current_metrics$laying_rmssd,
    readiness$metrics$deviations$rmssd_dev,
    current_metrics$laying_resting_hr,
    readiness$metrics$deviations$hr_dev,
    current_metrics$orthostatic_rise,
    readiness$metrics$deviations$ortho_dev,
    readiness$recommendations$bjj,
    readiness$recommendations$strength,
    readiness$recommendations$cardio,
    if (readiness$flags$high_fatigue) "\n- High Fatigue Detected" else "",
    if (readiness$flags$poor_recovery) "\n- Poor Recovery Response" else "",
    if (readiness$flags$overreaching_risk) "\n- Risk of Overreaching" else "",
    calculate_trend_direction(tail(baseline_metrics$laying_rmssd, 7)),
    calculate_trend_direction(tail(baseline_metrics$laying_resting_hr, 7))
  )

  return(report)
}

#' Helper function to calculate trend direction
#' @param values Numeric vector of values
#' @return Character string describing trend
calculate_trend_direction <- function(values) {
  # Fit linear model
  model <- lm(values ~ seq_along(values))
  slope <- coef(model)[2]

  # Determine trend magnitude
  if (abs(slope) < 0.1) {
    return("Stable")
  } else if (slope > 0) {
    return(if (slope > 0.5) "Strong Increase" else "Slight Increase")
  } else {
    return(if (slope < -0.5) "Strong Decrease" else "Slight Decrease")
  }
}

# Example usage in your main script:
process_fit_directory <- function(dir_path, ...) {
  # Your existing code...

  # Generate daily report if new data
  if (length(new_files) > 0 || length(outdated_entries) > 0) {
    report <- generate_daily_report(all_data)
    cat(report)

    # Save report to file
    report_file <- file.path(dirname(cache_file), "hrv_report.txt")
    writeLines(report, report_file)
  }

  return(all_data)
}


#' Calculate full neural recovery score based on BioForce HRV methodology
#' @param data Dataframe containing HRV measurements
#' @return Dataframe with added neural recovery scores
#' @export
calculate_neural_recovery <- function(data, window_size = 7) {
  # First calculate baseline metrics
  data <- calculate_moving_averages(data, window_size)

  # Calculate component scores
  data <- data %>%
    dplyr::mutate(
      # 1. RMSSD Score (0-40 points)
      # Measures parasympathetic recovery state
      rmssd_ratio = laying_rmssd / rmssd_ma,
      rmssd_score = dplyr::case_when(
        rmssd_ratio >= 1.3 ~ 40, # Highly recovered
        rmssd_ratio >= 1.1 ~ 35 + (rmssd_ratio - 1.1) * 25,
        rmssd_ratio >= 0.9 ~ 25 + (rmssd_ratio - 0.9) * 50,
        rmssd_ratio >= 0.7 ~ 15 + (rmssd_ratio - 0.7) * 50,
        TRUE ~ pmax(0, rmssd_ratio * 21.43)
      ),

      # 2. Orthostatic Response Score (0-30 points)
      # Measures autonomic balance
      ortho_response = standing_hr - laying_hr,
      ortho_score = dplyr::case_when(
        ortho_response <= 12 ~ 30, # Excellent response
        ortho_response <= 15 ~ 25, # Good response
        ortho_response <= 20 ~ 20, # Normal response
        ortho_response <= 25 ~ 15, # Elevated response
        ortho_response <= 30 ~ 10, # High response
        TRUE ~ 5 # Excessive response
      ),

      # 3. HRR Score (0-30 points)
      # Measures recovery capacity
      hrr_score = dplyr::case_when(
        hrr_60s >= 25 ~ 30, # Excellent recovery
        hrr_60s >= 20 ~ 25, # Good recovery
        hrr_60s >= 15 ~ 20, # Normal recovery
        hrr_60s >= 12 ~ 15, # Reduced recovery
        hrr_60s >= 8 ~ 10, # Poor recovery
        TRUE ~ 5 # Very poor recovery
      ),

      # Calculate final score (0-100)
      neural_recovery_score = rmssd_score + ortho_score + hrr_score,

      # Add status classification
      recovery_status = dplyr::case_when(
        neural_recovery_score >= 80 ~ "Fresh",
        neural_recovery_score >= 70 ~ "Good",
        neural_recovery_score >= 55 ~ "Normal",
        neural_recovery_score >= 40 ~ "Reduced",
        TRUE ~ "Low"
      )
    )

  return(data)
}

#' Generate training recommendations based on neural recovery score
#' @param score Neural recovery score
#' @param primary_type Primary training type ("BJJ" or "Strength")
#' @return List of training recommendations
#' @export
generate_training_recommendations <- function(score, primary_type = "BJJ") {
  base_rec <- dplyr::case_when(
    score >= 80 ~ list(
      status = "Fresh",
      intensity = "High",
      volume = "Normal to High",
      focus = "Progress training load"
    ),
    score >= 70 ~ list(
      status = "Good",
      intensity = "Normal to High",
      volume = "Normal",
      focus = "Maintain planned training"
    ),
    score >= 55 ~ list(
      status = "Normal",
      intensity = "Normal",
      volume = "Normal to Reduced",
      focus = "Maintain technical focus"
    ),
    score >= 40 ~ list(
      status = "Reduced",
      intensity = "Reduced",
      volume = "Reduced",
      focus = "Technical work priority"
    ),
    TRUE ~ list(
      status = "Low",
      intensity = "Low",
      volume = "Minimum",
      focus = "Active recovery"
    )
  )

  # BJJ-specific modifications
  if (primary_type == "BJJ") {
    base_rec$bjj_specific <- dplyr::case_when(
      score >= 80 ~ "Good day for hard rolls and new techniques",
      score >= 70 ~ "Normal training with full sparring",
      score >= 55 ~ "Focus on drilling and controlled rolls",
      score >= 40 ~ "Technical work and light positional sparring",
      TRUE ~ "Drill only or rest day"
    )
  }

  base_rec$score = score

  return(base_rec)
}
