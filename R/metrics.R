#' Calculate HRV metrics from RR intervals
#'
#' @description
#' Calculates key heart rate variability metrics including RMSSD and SDNN
#'
#' @param rr_intervals Numeric vector of RR intervals in seconds
#' @return Named list containing:
#'   \item{rmssd}{Root Mean Square of Successive Differences (ms)}
#'   \item{sdnn}{Standard Deviation of NN intervals (ms)}
#'
#' @export
calculate_hrv <- function(rr_intervals) {
  if (length(rr_intervals) < 2) {
    return(list(
      rmssd = NA_real_,
      sdnn = NA_real_
    ))
  }

  return(list(
    rmssd = round(sqrt(mean(diff(rr_intervals)^2, na.rm = TRUE)), digits = 2),
    sdnn = round(stats::sd(rr_intervals, na.rm = TRUE), digits = 2)
  ))
}

#' Calculate rolling mean
#'
#' @description
#' Computes rolling mean with minimum data requirements
#'
#' @param x Numeric vector
#' @param window Window size
#' @param min_fraction Minimum fraction of non-NA values required
#' @return Numeric vector of rolling means
#' @export
calculate_robust_ma <- function(x, window = 7, min_fraction = 0.7) {
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  if (window < 1) {
    stop("Window size must be positive")
  }
  if (min_fraction <= 0 || min_fraction > 1) {
    stop("min_fraction must be between 0 and 1")
  }

  n <- length(x)
  result <- numeric(n)
  min_obs <- ceiling(window * min_fraction)

  for (i in seq_len(n)) {
    start_idx <- max(1, i - window + 1)
    window_data <- x[start_idx:i]

    result[i] <- if (sum(!is.na(window_data)) >= min_obs) {
      mean(window_data, na.rm = TRUE)
    } else {
      NA_real_
    }
  }

  return(result)
}

#' Calculate all moving averages
#'
#' @description
#' Adds moving averages and relative changes to HRV data
#'
#' @param data Dataframe of HRV measurements
#' @param window_size Number of days for moving average
#' @param min_fraction Minimum fraction of data required
#' @return Dataframe with added moving averages
#' @export
#' @importFrom rlang .data
calculate_moving_averages <- function(
    data,
    window_size = 7,
    min_fraction = 0.7) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  required_cols <- c("date", "laying_rmssd", "laying_resting_hr", "standing_hr")
  if (!all(required_cols %in% names(data))) {
    stop(
      "Missing required columns: ",
      paste(setdiff(required_cols, names(data)), collapse = ", ")
    )
  }

  data %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      rmssd_ma = calculate_robust_ma(
        .data$laying_rmssd,
        window_size,
        min_fraction
      ),
      resting_hr_ma = calculate_robust_ma(
        .data$laying_resting_hr,
        window_size,
        min_fraction
      ),
      standing_hr_ma = calculate_robust_ma(
        .data$standing_hr,
        window_size,
        min_fraction
      ),
      rmssd_change = (.data$laying_rmssd - .data$rmssd_ma) / .data$rmssd_ma * 100,
      hr_change = (.data$laying_resting_hr - .data$resting_hr_ma) / .data$resting_hr_ma * 100
    )
}

#' Calculate resting heart rate
#'
#' @description
#' Estimates resting heart rate using various methods
#'
#' @param hr_data Vector of heart rate values
#' @param method Calculation method to use
#' @param stability_threshold Allowable HR variation
#' @param window_size Window size in samples
#' @return Numeric resting heart rate value
#' @export
calculate_resting_hr <- function(
    hr_data,
    method = "lowest_sustained",
    stability_threshold = 3,
    window_size = 30) {
  if (is.null(hr_data) || length(hr_data) < window_size) {
    return(NA_real_)
  }

  if (method == "last_30s") {
    idx_range <- (length(hr_data) - window_size + 1):length(hr_data)
    return(mean(hr_data[idx_range], na.rm = TRUE))
  }

  if (method == "min_30s") {
    window_means <- sapply(
      1:(length(hr_data) - window_size + 1),
      function(i) mean(hr_data[i:(i + window_size - 1)], na.rm = TRUE)
    )
    return(min(window_means, na.rm = TRUE))
  }

  if (method == "lowest_sustained") {
    n_windows <- length(hr_data) - window_size + 1
    ranges <- means <- numeric(n_windows)

    for (i in seq_len(n_windows)) {
      window <- hr_data[i:(i + window_size - 1)]
      ranges[i] <- diff(range(window, na.rm = TRUE))
      means[i] <- mean(window, na.rm = TRUE)
    }

    window_df <- data.frame(range = ranges, mean = means)
    stable_windows <- window_df$range <= stability_threshold

    if (sum(stable_windows) > 0) {
      return(min(window_df$mean[stable_windows], na.rm = TRUE))
    }

    # Try progressively higher thresholds
    for (new_threshold in (stability_threshold + 1):(stability_threshold + 5)) {
      stable_windows <- window_df$range <= new_threshold
      if (sum(stable_windows) > 0) {
        warning(sprintf(
          "No windows with <=%dbpm variation found. Using %dbpm threshold.",
          stability_threshold,
          new_threshold
        ))
        return(min(window_df$mean[stable_windows], na.rm = TRUE))
      }
    }

    warning("No stable windows found. Falling back to min_30s method.")
    return(calculate_resting_hr(hr_data, method = "min_30s"))
  }

  stop("Invalid method specified. Use 'last_30s', 'min_30s', or 'lowest_sustained'")
}

#' Calculate Heart Rate Recovery metrics
#'
#' @description
#' Computes various metrics related to heart rate recovery
#'
#' @param standing_hr Vector of heart rate values after standing
#' @param baseline_hr Resting heart rate before standing
#' @return List containing recovery metrics:
#'   \item{hrr_60s}{Absolute recovery in 60 seconds}
#'   \item{hrr_relative}{Relative recovery percentage}
#'   \item{orthostatic_rise}{Initial HR increase}
#' @export
calculate_hrr <- function(standing_hr, baseline_hr) {
  if (length(standing_hr) < 60) {
    return(list(
      hrr_60s = NA_real_,
      hrr_relative = NA_real_,
      orthostatic_rise = NA_real_
    ))
  }

  # Calculate peak HR in first 20 seconds
  hr_peak <- max(standing_hr[1:20], na.rm = TRUE)
  hr_60s <- standing_hr[60]

  # Calculate metrics
  hrr_60s <- hr_peak - hr_60s
  hrr_relative <- (hr_peak - hr_60s) / (hr_peak - baseline_hr) * 100
  orthostatic_rise <- hr_peak - baseline_hr

  return(list(
    hrr_60s = hrr_60s,
    hrr_relative = hrr_relative,
    orthostatic_rise = orthostatic_rise
  ))
}
