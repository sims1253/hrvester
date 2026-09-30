# HRV Alert and Status Functions

#' Analyze HRV readiness status
#'
#' This function evaluates an athlete's readiness for training based on HRV
#' metrics.
#' It compares current day metrics against a 7-day rolling baseline and provides
#' a training status along with specific recommendations.
#'
#' @param current_metrics A dataframe containing the current day's HRV metrics.
#'   Must contain exactly one row.
#'   Should include:
#'   \itemize{
#'     \item laying_rmssd (numeric): Current day's RMSSD during laying position
#'     \item laying_resting_hr (numeric): Current day's resting heart rate
#'     \item orthostatic_rise (numeric): Current day's orthostatic response
#'   }
#' @param baseline_metrics A dataframe containing the previous 7 days' HRV
#'   metrics.
#'   Should include:
#'   \itemize{
#'     \item laying_rmssd (numeric): Historical RMSSD values for baseline
#'           calculation
#'     \item laying_resting_hr (numeric): Historical resting heart rate values
#'     \item orthostatic_rise (numeric): Historical orthostatic response values
#'   }
#' @return A list containing:
#'   \itemize{
#'     \item status (character): Training status
#'           ("FRESH", "NORMAL", "CAUTION", "WARNING")
#'     \item recommendations (list): Training recommendations for BJJ,
#'           strength, and cardio
#'     \item flags (list): Additional warning flags for high fatigue, poor
#'           recovery, and overreaching risk
#'     \item metrics (list): Baseline values and deviation calculations
#'   }
#' @importFrom dplyr case_when
#' @export
analyze_readiness <- function(current_metrics, baseline_metrics) {
  # Input validation
  if (nrow(current_metrics) != 1) {
    stop("current_metrics must contain exactly one row")
  }
  if (nrow(baseline_metrics) != 7) {
    warning("baseline_metrics should contain 7 days of data")
  }
  # Calculate baseline values (7-day rolling)
  baseline <- list(
    rmssd = mean(baseline_metrics$laying_rmssd, na.rm = TRUE),
    resting_hr = mean(baseline_metrics$laying_resting_hr, na.rm = TRUE),
    standing_response = mean(baseline_metrics$orthostatic_rise, na.rm = TRUE)
  )

  # Calculate current deviations
  deviations <- list(
    rmssd_dev = (current_metrics$laying_rmssd - baseline$rmssd) /
      baseline$rmssd *
      100,
    hr_dev = (current_metrics$laying_resting_hr - baseline$resting_hr) /
      baseline$resting_hr *
      100,
    ortho_dev = (current_metrics$orthostatic_rise -
      baseline$standing_response) /
      baseline$standing_response *
      100
  )

  any_deviation_missing <- any(
    vapply(deviations, function(d) is.na(d) || !is.finite(d), logical(1))
  )

  # Define training status based on BJJ-specific thresholds; missing inputs
  # produce an explicit insufficient-data status, never a training state
  status <- if (any_deviation_missing) {
    "INSUFFICIENT_DATA"
  } else if (deviations$rmssd_dev > 5) {
    "FRESH" # Enhanced recovery state
  } else if (deviations$rmssd_dev >= -5) {
    "NORMAL" # Good to train normally
  } else if (deviations$rmssd_dev >= -10) {
    "CAUTION" # Reduce intensity/volume
  } else {
    "WARNING" # Active recovery only
  }

  # Generate BJJ-specific recommendations
  recommendations <- if (status == "FRESH") {
    list(
      bjj = "Full training, good day for hard rolls",
      strength = "Proceed with planned session",
      cardio = "Good day for higher intensity work"
    )
  } else if (status == "NORMAL") {
    list(
      bjj = "Regular training as planned",
      strength = "Proceed as planned",
      cardio = "Stay in Zone 1-2"
    )
  } else if (status == "CAUTION") {
    list(
      bjj = "Technical work only, limit sparring",
      strength = "Reduce volume by 20%, maintain intensity",
      cardio = "Zone 1 only, max 30 minutes"
    )
  } else if (status == "WARNING") {
    list(
      bjj = "Technique or drilling only, no sparring",
      strength = "Active recovery or rest day",
      cardio = "Light mobility work only"
    )
  } else {
    # INSUFFICIENT_DATA
    list(
      bjj = "No recommendation: insufficient baseline data",
      strength = "No recommendation: insufficient baseline data",
      cardio = "No recommendation: insufficient baseline data"
    )
  }

  # Additional BJJ-specific flags
  flags <- list(
    high_fatigue = isTRUE(deviations$hr_dev > 5) &&
      isTRUE(deviations$rmssd_dev < -5),
    poor_recovery = isTRUE(deviations$ortho_dev > 15),
    overreaching_risk = isTRUE(
      all(tail(baseline_metrics$laying_rmssd, 3) < baseline$rmssd * 0.9)
    )
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
#'
#' Creates a formatted text report summarizing current HRV status, metrics, and
#' recommendations.
#'
#' @param data A dataframe containing HRV metrics for at least 8 consecutive
#'             days (current day plus 7-day baseline). Should include:
#'   \itemize{
#'     \item date (Date or character): Measurement dates in ISO format;
#'       character dates (as produced by the processing pipeline and cache)
#'       are accepted and normalized
#'     \item laying_rmssd (numeric): RMSSD during laying position
#'     \item laying_resting_hr (numeric): Resting heart rate; fractional
#'       values are supported
#'     \item orthostatic_rise (numeric): Orthostatic response
#'   }
#' @return A formatted character string containing the HRV report with sections:
#'   \itemize{
#'     \item Date and status header
#'     \item Current metrics with baseline comparisons
#'     \item Training recommendations for BJJ, strength, and cardio
#'     \item Warning flags if present
#'     \item 7-day trend analysis for RMSSD and heart rate
#'   }
#'
#' @details When several measurements share the latest date, the first row
#'   of that date (in input order) is reported. At least one usable baseline
#'   day within the seven days before the current day is required; the
#'   baseline does not need to be complete.
#'
#' @importFrom dplyr filter %>%
#' @importFrom utils head tail
#' @export
generate_daily_report <- function(data) {
  # Input validation
  if (nrow(data) < 8) {
    stop(
      "data must contain at least 8 consecutive days of measurements
          (current day plus 7-day baseline)"
    )
  }

  # Normalize dates (the pipeline and cache store them as character)
  data$date <- tryCatch(
    as.Date(data$date),
    error = function(e) {
      stop("data$date contains invalid dates: ", conditionMessage(e))
    }
  )
  if (any(is.na(data$date))) {
    stop("data$date contains invalid dates")
  }

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

  if (!any(!is.na(baseline_metrics$laying_rmssd))) {
    stop("No usable baseline measurements within 7 days before the current day")
  }

  # Get readiness analysis
  readiness <- analyze_readiness(current_metrics, baseline_metrics)

  # Format report
  report <- sprintf(
    "
    HRV Status Report for %s

    Status: %s

    Current Metrics:
    - RMSSD: %.1f (%.1f%% from baseline)
    - Resting HR: %.1f (%.1f%% from baseline)
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
    format(current_day, "%B %d, %Y"),
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
    if (isTRUE(readiness$flags$high_fatigue)) "\n- High Fatigue Detected" else "",
    if (isTRUE(readiness$flags$poor_recovery)) "\n- Poor Recovery Response" else "",
    if (isTRUE(readiness$flags$overreaching_risk)) "\n- Risk of Overreaching" else "",
    calculate_trend_direction(tail(baseline_metrics$laying_rmssd, 7)),
    calculate_trend_direction(tail(baseline_metrics$laying_resting_hr, 7))
  )

  return(report)
}

#' Helper function to calculate trend direction
#'
#' Determines the direction of trends in HRV metrics using linear regression.
#' The function fits a linear model to the values and categorizes the slope
#' into descriptive trend categories.
#'
#' @param values Numeric vector of values to analyze for trend. Should contain at least 2 values.
#' @return Character string describing the trend direction and magnitude:
#'   \itemize{
#'     \item "Stable" (abs(slope) < 0.1)
#'     \item "Slight Increase" (0.1 <= slope <= 0.5)
#'     \item "Strong Increase" (slope > 0.5)
#'     \item "Slight Decrease" (-0.5 <= slope < -0.1)
#'     \item "Strong Decrease" (slope < -0.5)
#'   }
#' @importFrom stats lm coef
#' @export
calculate_trend_direction <- function(values) {
  # Input validation
  if (length(values) < 2) {
    stop("values must contain at least 2 data points")
  }

  # Remove NA values
  values <- values[!is.na(values)]
  if (length(values) < 2) {
    stop("values must contain at least 2 non-NA data points")
  }

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

#' Calculate full neural recovery score based on BioForce HRV methodology
#'
#' This function calculates a comprehensive neural recovery score by combining
#' multiple HRV metrics. The score is based on the following components:
#' \itemize{
#'   \item RMSSD (parasympathetic recovery)
#'   \item Orthostatic response (autonomic balance)
#'   \item Heart rate recovery (recovery capacity)
#' }
#'
#' @param data Dataframe containing HRV measurements. Should include:
#'   \itemize{
#'     \item laying_rmssd (numeric): RMSSD during laying position
#'     \item laying_resting_hr (numeric): Resting heart rate
#'     \item rmssd_ma (numeric): Moving average of RMSSD (added by
#'           calculate_moving_averages)
#'     \item standing_hr (numeric): Heart rate during standing position
#'     \item hrr_60s (numeric): Heart rate recovery after 60 seconds
#'   }
#' @param window_size Number of days to use for calculating moving averages
#'        (default: 7). Used to establish baseline values for comparison.
#'        Must be a positive integer.
#' @return Dataframe with added columns:
#'   \itemize{
#'     \item rmssd_score (numeric): Score based on RMSSD ratio (0-40 points)
#'     \item ortho_score (numeric): Score based on orthostatic response (0-30 points)
#'     \item hrr_score (numeric): Score based on heart rate recovery (0-30 points)
#'     \item neural_recovery_score (numeric): Combined total score (0-100);
#'       NA when any component cannot be computed (for example, no usable
#'       moving-average baseline yet, or a missing measurement)
#'     \item recovery_status (character): Classification based on total score;
#'       "Insufficient data" when the score is NA, so unknown recovery is
#'       never presented as poor recovery
#'   }
#' @importFrom rlang .data
#' @importFrom dplyr mutate case_when %>%
#' @export
calculate_neural_recovery <- function(data, window_size = 7) {
  # Input validation
  if (
    !all(
      c("laying_rmssd", "laying_resting_hr", "standing_hr", "hrr_60s") %in%
        names(data)
    )
  ) {
    stop(
      "data must contain all required columns: laying_rmssd, laying_resting_hr, standing_hr, hrr_60s"
    )
  }

  if (
    !is.numeric(window_size) ||
      window_size < 1 ||
      window_size != round(window_size)
  ) {
    stop("window_size must be a positive integer")
  }

  # Validate numeric columns contain non-negative values
  if (
    any(
      data[
        c(
          "laying_rmssd",
          "laying_resting_hr",
          "standing_hr"
        )
      ] <
        0,
      na.rm = TRUE
    )
  ) {
    stop("All HRV metrics must be non-negative values")
  }

  # First calculate baseline metrics
  data <- calculate_moving_averages(data, window_size)

  # Calculate component scores
  data <- data %>%
    mutate(
      rmssd_ratio = .data$laying_rmssd / .data$rmssd_ma,
      rmssd_score = case_when(
        .data$rmssd_ratio >= 1.3 ~ 40,
        .data$rmssd_ratio >= 1.1 ~ 35 + (.data$rmssd_ratio - 1.1) * 25,
        .data$rmssd_ratio >= 0.9 ~ 25 + (.data$rmssd_ratio - 0.9) * 50,
        .data$rmssd_ratio >= 0.7 ~ 15 + (.data$rmssd_ratio - 0.7) * 50,
        TRUE ~ pmax(0, .data$rmssd_ratio * 21.43)
      ),

      # 2. Orthostatic Response Score (0-30 points)
      ortho_response = .data$standing_hr - .data$laying_resting_hr,
      ortho_score = case_when(
        is.na(.data$ortho_response) ~ NA_real_, # Missing measurement
        .data$ortho_response <= 12 ~ 30, # Excellent response
        .data$ortho_response <= 15 ~ 25, # Good response
        .data$ortho_response <= 20 ~ 20, # Normal response
        .data$ortho_response <= 25 ~ 15, # Elevated response
        .data$ortho_response <= 30 ~ 10, # High response
        TRUE ~ 5 # Excessive response
      ),

      # 3. HRR Score (0-30 points)
      hrr_score = case_when(
        is.na(.data$hrr_60s) ~ NA_real_, # Missing measurement
        .data$hrr_60s >= 25 ~ 30, # Excellent recovery
        .data$hrr_60s >= 20 ~ 25, # Good recovery
        .data$hrr_60s >= 15 ~ 20, # Normal recovery
        .data$hrr_60s >= 12 ~ 15, # Reduced recovery
        .data$hrr_60s >= 8 ~ 10, # Poor recovery
        TRUE ~ 5 # Very poor recovery
      ),

      # Calculate final score (0-100); NA when any component is missing
      neural_recovery_score = .data$rmssd_score +
        .data$ortho_score +
        .data$hrr_score,

      # Add status classification
      recovery_status = case_when(
        is.na(.data$neural_recovery_score) ~ "Insufficient data",
        .data$neural_recovery_score >= 80 ~ "Fresh",
        .data$neural_recovery_score >= 70 ~ "Good",
        .data$neural_recovery_score >= 55 ~ "Normal",
        .data$neural_recovery_score >= 40 ~ "Reduced",
        TRUE ~ "Low"
      )
    )

  return(data)
}

#' Generate training recommendations based on neural recovery score
#'
#' Provides training recommendations based on the calculated neural recovery
#' score. Offers different recommendations for BJJ training and general
#' strength training.
#'
#' @param score Neural recovery score (0-100)
#' @param primary_type Primary training type ("BJJ" or "Strength",
#'                     case-insensitive)
#' @return A list containing training recommendations including:
#'   \itemize{
#'     \item status (character): Current recovery status
#'     \item intensity (character): Recommended training intensity
#'     \item volume (character): Recommended training volume
#'     \item focus (character): Recommended training focus
#'     \item bjj_specific (character): BJJ-specific recommendations
#'           (if applicable)
#'     \item score (numeric): The original neural recovery score
#'   }
#'
#'   An `NA` score (insufficient data) returns an explicit
#'   "Insufficient data" status without a training prescription instead of
#'   an error.
#'
#' @importFrom dplyr case_when
#' @export
training_recommendations <- function(score, primary_type = "BJJ") {
  # Input validation; any length-1 NA is treated as insufficient data
  if (
    length(score) != 1 ||
      (!is.na(score) && (!is.numeric(score) || score < 0 || score > 100))
  ) {
    stop("score must be a numeric value between 0 and 100")
  }

  primary_type <- toupper(primary_type)
  if (!primary_type %in% c("BJJ", "STRENGTH")) {
    stop("primary_type must be either 'BJJ' or 'Strength'")
  }

  if (is.na(score)) {
    insufficient <- list(
      status = "Insufficient data",
      intensity = "Not assessable",
      volume = "Not assessable",
      focus = "Collect more measurements before prescribing training",
      score = NA_real_
    )
    if (primary_type == "BJJ") {
      insufficient$bjj_specific <-
        "No recommendation: insufficient recovery data"
    }
    return(insufficient)
  }

  base_rec <- if (score >= 80) {
    list(
      status = "Fresh",
      intensity = "High",
      volume = "Normal to High",
      focus = "Progress training load"
    )
  } else if (score >= 70) {
    list(
      status = "Good",
      intensity = "Normal to High",
      volume = "Normal",
      focus = "Maintain planned training"
    )
  } else if (score >= 55) {
    list(
      status = "Normal",
      intensity = "Normal",
      volume = "Normal to Reduced",
      focus = "Maintain technical focus"
    )
  } else if (score >= 40) {
    list(
      status = "Reduced",
      intensity = "Reduced",
      volume = "Reduced",
      focus = "Technical work priority"
    )
  } else {
    list(
      status = "Low",
      intensity = "Low",
      volume = "Minimum",
      focus = "Active recovery"
    )
  }

  # BJJ-specific modifications
  if (primary_type == "BJJ") {
    base_rec$bjj_specific <- case_when(
      score >= 80 ~ "Good day for hard rolls and new techniques",
      score >= 70 ~ "Normal training with full sparring",
      score >= 55 ~ "Focus on drilling and controlled rolls",
      score >= 40 ~ "Technical work and light positional sparring",
      TRUE ~ "Drill only or rest day"
    )
  }

  base_rec$score <- score

  return(base_rec)
}
