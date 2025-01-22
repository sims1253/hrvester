#' Extract RR intervals from FIT file
#'
#' @param fit_object FITfileR object
#' @param filter_factor Used to filter RR values that are 1.x times smaller or
#'   larger than the last one as suspected false readings.
#' @return List of laying and standing RR intervals in ms
#' @keywords internal
extract_rr_intervals <- function(fit_object,
                                 filter_factor,
                                 laying_time = 180,
                                 standing_time = 180) {
  # Get HRV data directly using FITfileR methods
  if ("hrv" %in% FITfileR::listMessageTypes(fit_object)) {
    # hopefully time between heartbeats is below 2 seconds.
    hrv_data <- dplyr::filter(FITfileR::hrv(fit_object), time < 2)$time
  } else {
    return(list(laying = numeric(), standing = numeric()))
  }

  # Pre-calculate differences for RMSSD
  RR <- c(hrv_data[1])
  for (i in 2:length(hrv_data)) {
    if (hrv_data[i - 1] * (1 - filter_factor) < hrv_data[i] &
      hrv_data[i - 1] * (1 + filter_factor) > hrv_data[i]) {
      RR <- append(RR, hrv_data[i])
    }
  }

  # Get heartbeat timestamps
  timestamps <- cumsum(RR)

  # Return RR intervals for each position
  return(
    list(
      laying = RR[1:which.max(timestamps > 180) - 1],
      standing = RR[
        which.max(timestamps > 180):which.max(timestamps > 360) - 1
      ]
    )
  )
}

#' Calculate HRV metrics from RR intervals
#'
#' @param rr_intervals Numeric vector of RR intervals in milliseconds
#' @return Named list containing:
#'   \item{rmssd}{Root Mean Square of Successive Differences}
#'   \item{sdnn}{Standard Deviation of NN intervals}
#'   \item{mean_hr}{Mean Heart Rate in BPM}
#' @keywords internal
calculate_hrv <- function(rr_intervals) {
  if (length(rr_intervals) < 2) {
    return(list(
      rmssd = NA_real_,
      sdnn = NA_real_,
      mean_hr = NA_real_
    ))
  }

  return(
    list(
      rmssd = round(
        sqrt(mean(diff(1000 * rr_intervals)^2, na.rm = TRUE)),
        digits = 2
      ),
      sdnn = round(sd(1000 * rr_intervals, na.rm = TRUE), digits = 2)
    )
  )
}

#' Extract HR data from \code{FitFile} object
#'
#' @param fit_object An object of class \code{FitFile}, eg.
#'   from \code{\link[FITfileR::readFitFile]{FITfileR::readFitFile}}
#'
#' @return A vector containing up to the first three minutes of heart rate data.
#' @export
get_HR <- function(fit_object) {
  HR <- FITfileR::records(fit_object)
  if (all(class(HR) == "list")) {
    HR <- HR[[which.max(sapply(HR, nrow))]]$heart_rate
    HR <- HR[1:min(360, length(HR))]
  } else {
    HR <- HR$heart_rate[1:min(360, nrow(HR))]
  }
  return(HR)
}

#' Process a single FIT file to extract HRV metrics
#'
#' @param file_path Path to FIT file
#' @return Tibble row containing:
#'   \item{source_file}{Path to processed file}
#'   \item{date}{Date of measurement}
#'   \item{time_of_day}{Morning or Evening}
#'   \item{laying_rmssd}{RMSSD during laying position}
#'   \item{laying_sdnn}{SDNN during laying position}
#'   \item{laying_hr}{Mean HR during laying position}
#'   \item{standing_rmssd}{RMSSD during standing position}
#'   \item{standing_sdnn}{SDNN during standing position}
#'   \item{standing_hr}{Mean HR during standing position}
#' @return NULL if processing fails
#' @export
process_fit_file <- function(file_path, filter_factor) {
  tryCatch(
    {
      # Read file once
      fit_object <- FITfileR::readFitFile(file_path)
      hr_data <- get_HR(fit_object = fit_object)

      # Calculate resting HR using new method
      resting_hr <- calculate_resting_hr(hr_data[30:180], method = "lowest_sustained")

      # Calculate HRR after standing
      hrr_metrics <- calculate_hrr(hr_data[181:240], resting_hr)

      # Extract RR intervals
      rr_data <- extract_rr_intervals(
        fit_object = fit_object,
        filter_factor = filter_factor
      )

      # Only process if we have enough data
      if (length(rr_data$laying) >= 2 && length(rr_data$standing) >= 2) {
        # Get times once
        date <- clock::as_date(FITfileR::getMessagesByType(fit_object, "session")$timestamp)
        week <- as.numeric(strftime(FITfileR::getMessagesByType(fit_object, "session")$timestamp, format = "%W"))
        time_of_day <- ifelse(
          (clock::get_hour(FITfileR::getMessagesByType(fit_object, "session")$timestamp) > 4) &
            (clock::get_hour(FITfileR::getMessagesByType(fit_object, "session")$timestamp) < 13),
          "Morning",
          "Evening"
        )

        # Calculate metrics
        laying <- calculate_hrv(rr_data$laying)
        standing <- calculate_hrv(rr_data$standing)

        # Create single row efficiently
        tibble::tibble(
          source_file = file_path,
          date = as.character(date),
          week = week,
          time_of_day = time_of_day,
          laying_rmssd = laying$rmssd,
          laying_sdnn = laying$sdnn,
          laying_hr = round(mean(hr_data[30:150], na.rm = TRUE), digits = 2),
          laying_resting_hr = resting_hr,
          standing_rmssd = standing$rmssd,
          standing_sdnn = standing$sdnn,
          standing_hr = round(mean(hr_data[220:330], na.rm = TRUE), digits = 2),
          standing_max_hr = max(hr_data[181:220]),
          hrr_60s = hrr_metrics$hrr_60s,
          hrr_relative = hrr_metrics$hrr_relative,
          orthostatic_rise = hrr_metrics$orthostatic_rise,
          package_version = as.character(packageVersion("hrvester")),
          RR_filter = filter_factor,
          activity = FITfileR::getMessagesByType(fit_object, "sport")$name
        )
      } else {
        tibble::tibble(
          source_file = file_path,
          date = as.character(date),
          week = week,
          time_of_day = time_of_day,
          laying_rmssd = NA,
          laying_sdnn = NA,
          laying_hr = NA,
          laying_resting_hr = NA,
          standing_rmssd = NA,
          standing_sdnn = NA,
          standing_hr = NA,
          standing_max_hr = NA,
          hrr_60s = NA,
          hrr_relative = NA,
          orthostatic_rise = NA,
          package_version = as.character(packageVersion("hrvester")),
          RR_filter = filter_factor,
          activity = FITfileR::getMessagesByType(fit_object, "sport")$name
        ) # TODO change this to do HR without HRV probably?
      }
    },
    error = function(e) {
      tibble::tibble(
        source_file = file_path,
        date = NA,
        week = NA,
        time_of_day = NA,
        laying_rmssd = NA,
        laying_sdnn = NA,
        laying_hr = NA,
        laying_resting_hr = NA,
        standing_rmssd = NA,
        standing_sdnn = NA,
        standing_hr = NA,
        standing_max_hr = NA,
        hrr_60s = NA,
        hrr_relative = NA,
        orthostatic_rise = NA,
        package_version = as.character(packageVersion("hrvester")),
        RR_filter = filter_factor,
        activity = NA
      )
    }
  )
}

#' Calculate rolling mean with minimum data requirement
#' @param x Numeric vector
#' @param window Size of rolling window
#' @param min_fraction Minimum fraction of non-NA values required (0-1)
calculate_robust_ma <- function(x, window = 7, min_fraction = 0.7) {
  n <- length(x)
  result <- numeric(n)
  min_obs <- ceiling(window * min_fraction)

  for (i in 1:n) {
    start_idx <- max(1, i - window + 1)
    window_data <- x[start_idx:i]

    # Only calculate mean if we have enough valid observations
    if (sum(!is.na(window_data)) >= min_obs) {
      result[i] <- mean(window_data, na.rm = TRUE)
    } else {
      result[i] <- NA
    }
  }

  return(result)
}

#' Calculate all moving averages for HRV data
#' @param data Dataframe containing HRV measurements
#' @param window_size Number of days for moving average
#' @param min_fraction Minimum fraction of data required
#' @return Dataframe with added moving averages
calculate_moving_averages <- function(data, window_size = 7, min_fraction = 0.7) {
  # Ensure data is ordered by date
  data <- data %>%
    arrange(date) %>%
    mutate(
      # Calculate moving averages
      rmssd_ma = calculate_robust_ma(laying_rmssd, window_size, min_fraction),
      resting_hr_ma = calculate_robust_ma(laying_resting_hr, window_size, min_fraction),
      standing_hr_ma = calculate_robust_ma(standing_hr, window_size, min_fraction),

      # Calculate relative changes from baseline
      rmssd_change = (laying_rmssd - rmssd_ma) / rmssd_ma * 100,
      hr_change = (laying_resting_hr - resting_hr_ma) / resting_hr_ma * 100
    )

  return(data)
}

#' Calculate resting heart rate using different methods
#' @param hr_data Vector of heart rate values
#' @param method Method to use ("min_30s", "last_30s", or "lowest_sustained")
#' @param stability_threshold Allowable HR variation for "sustained" (default 3 bpm)
#' @param window_size Window size in samples (default 30)
#' @return Numeric resting heart rate value
calculate_resting_hr <- function(
    hr_data,
    method = "lowest_sustained",
    stability_threshold = 3,
    window_size = 30) {
  # Input validation
  if (is.null(hr_data) || length(hr_data) < window_size) {
    return(NA_real_)
  }

  if (method == "last_30s") {
    # Current method - last window_size seconds of laying position
    return(mean(hr_data[(length(hr_data) - window_size + 1):length(hr_data)],
      na.rm = TRUE
    ))
  } else if (method == "min_30s") {
    # Find lowest window
    window_means <- sapply(1:(length(hr_data) - window_size + 1), function(i) {
      mean(hr_data[i:(i + window_size - 1)], na.rm = TRUE)
    })
    return(min(window_means, na.rm = TRUE))
  } else if (method == "lowest_sustained") {
    # Calculate ranges and means for all windows
    ranges <- numeric(length(hr_data) - window_size + 1)
    means <- numeric(length(hr_data) - window_size + 1)

    for (i in 1:(length(hr_data) - window_size + 1)) {
      window <- hr_data[i:(i + window_size - 1)]
      ranges[i] <- diff(range(window, na.rm = TRUE))
      means[i] <- mean(window, na.rm = TRUE)
    }

    # Create data frame
    window_df <- data.frame(range = ranges, mean = means)

    # Find stable windows
    stable_windows <- window_df$range <= stability_threshold

    if (sum(stable_windows) > 0) {
      # If we found stable windows, use the lowest one
      return(min(window_df$mean[stable_windows], na.rm = TRUE))
    } else {
      # If no stable windows found, gradually increase threshold
      for (new_threshold in seq(stability_threshold + 1, stability_threshold + 5)) {
        stable_windows <- window_df$range <= new_threshold
        if (sum(stable_windows) > 0) {
          warning(sprintf(
            "No windows with ≤%dbpm variation found. Using %dbpm threshold instead.",
            stability_threshold, new_threshold
          ))
          return(min(window_df$mean[stable_windows], na.rm = TRUE))
        }
      }

      # If still no stable windows, fall back to min_30s method
      warning(sprintf("No stable windows found. Falling back to min_30s method."))
      return(calculate_resting_hr(hr_data, method = "min_30s"))
    }
  }
}

#' Calculate Heart Rate Recovery metrics
#' @param standing_hr Vector of heart rate values after standing
#' @param baseline_hr Resting heart rate before standing
#' @return List of HRR metrics
calculate_hrr <- function(standing_hr, baseline_hr) {
  # First 60 seconds after standing
  hr_peak <- max(standing_hr[1:20], na.rm = TRUE) # Peak HR in first 20s
  hr_60s <- standing_hr[60] # HR at 60s

  return(list(
    hrr_60s = hr_peak - hr_60s, # Absolute recovery in 60s
    hrr_relative = (hr_peak - hr_60s) / (hr_peak - baseline_hr) * 100, # Relative recovery
    orthostatic_rise = hr_peak - baseline_hr # Initial HR increase
  ))
}
