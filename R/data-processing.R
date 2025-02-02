#' Extract RR intervals from FIT file
#'
#' @description
#' Extracts heart rate variability data from FIT files by analyzing RR intervals
#' during both laying and standing positions. Implements robust filtering to maintain
#' data quality and properly handle phase transitions.
#'
#' @param fit_object FITfileR object containing the FIT file data
#' @param filter_factor Numeric value (0-1) for filtering outliers. Higher values
#'   allow more variation between successive intervals
#' @param laying_time Duration in seconds for laying position (default: 180)
#' @param standing_time Duration in seconds for standing position (default: 180)
#' @param transition_buffer Time in seconds to exclude around position change (default: 5)
#'
#' @return List containing:
#'   \item{laying}{Numeric vector of RR intervals during laying position (seconds)}
#'   \item{standing}{Numeric vector of RR intervals during standing position (seconds)}
#'
#' @importFrom FITfileR readFitFile
#' @importFrom dplyr "%>%"
#' @importFrom rlang .data
#' @export
extract_rr_data <- function(
    fit_object,
    filter_factor = 0.15,
    laying_time = 180,
    standing_time = 180,
    transition_buffer = 5) {
  # Input validation with descriptive errors
  if (!inherits(fit_object, "FitFile")) {
    stop("fit_object must be a FitFile object")
  }
  if (!is.numeric(filter_factor) || filter_factor <= 0 || filter_factor >= 1) {
    stop("filter_factor must be between 0 and 1")
  }
  if (!is.numeric(laying_time) || laying_time <= 0) {
    stop("laying_time must be positive")
  }
  if (!is.numeric(standing_time) || standing_time <= 0) {
    stop("standing_time must be positive")
  }
  if (!is.numeric(transition_buffer) || transition_buffer < 0) {
    stop("transition_buffer must be non-negative")
  }

  # Check for HRV data availability
  # Check for HRV data availability using your method
  hrv_data_check <- FITfileR::hrv(fit_object)
  if (is.null(hrv_data_check) || nrow(hrv_data_check) == 0) {
    warning("No HRV data found in FIT file")
    return(list(laying = numeric(), standing = numeric(), quality_metrics = numeric()))
  }

  # Extract raw RR intervals
  hrv_data <- dplyr::filter(hrv_data_check, .data$time < 2) # remove the random 65.5 values
  if (nrow(hrv_data) == 0) {
    warning("Empty HRV data in FIT file")
    return(list(laying = numeric(), standing = numeric(), quality_metrics = numeric()))
  }

  # Check data density
  total_time <- sum(hrv_data$time)
  expected_beats <- (laying_time + standing_time) / mean(hrv_data$time)
  actual_beats <- nrow(hrv_data)

  if (actual_beats / expected_beats < 0.8) { # Require at least 80% of expected beats
    warning(sprintf(
      "Insufficient data density: %.1f%% of expected beats",
      100 * actual_beats / expected_beats
    ))
    return(list(laying = numeric(), standing = numeric()))
  }

  # Convert to seconds if necessary (some devices record in milliseconds)
  rr_intervals <- if (mean(hrv_data$time, na.rm = TRUE) > 10) {
    hrv_data$time / 1000 # Convert from ms to seconds
  } else {
    hrv_data$time
  }

  # Initialize filtered intervals vector
  filtered_intervals <- numeric(length(rr_intervals))
  filtered_intervals[1] <- rr_intervals[1]
  n_filtered <- 1

  # Apply adaptive filtering with phase-specific thresholds
  phase_factors <- list(
    laying = filter_factor * 1.3, # Up to ~20% variation for RSA
    transition = filter_factor * 2.0, # Up to ~30% during transition
    standing = filter_factor * 0.8 # ~12% standing (less RSA)
  )

  for (i in 2:length(rr_intervals)) {
    # Determine current phase based on cumulative time
    cum_time <- sum(filtered_intervals[1:n_filtered])
    current_factor <- if (cum_time < laying_time - transition_buffer) {
      phase_factors$laying
    } else if (cum_time < laying_time + transition_buffer) {
      phase_factors$transition
    } else {
      phase_factors$standing
    }

    # Use last valid interval as reference
    reference <- filtered_intervals[n_filtered]
    current <- rr_intervals[i]

    # Calculate phase-specific bounds
    lower_bound <- reference * (1 - current_factor)
    upper_bound <- reference * (1 + current_factor)

    # Apply filter
    if (current >= lower_bound && current <= upper_bound) {
      n_filtered <- n_filtered + 1
      filtered_intervals[n_filtered] <- current
    }
  }

  # Trim unused space
  filtered_intervals <- filtered_intervals[1:n_filtered]

  # Calculate cumulative times
  cumulative_times <- cumsum(filtered_intervals)

  # Calculate position indices with transition buffer
  laying_end <- max(which(cumulative_times <= (laying_time - transition_buffer)))
  standing_start <- min(which(cumulative_times >= (laying_time + transition_buffer)))
  standing_end <- max(which(cumulative_times <= (laying_time + standing_time)))

  # Validate indices
  if (is.infinite(laying_end) || is.infinite(standing_start) ||
    is.infinite(standing_end) || laying_end >= standing_start ||
    standing_start > standing_end) {
    warning("Invalid position transitions detected")
    return(list(laying = numeric(), standing = numeric()))
  }

  # Apply physiological quality checks
  laying_intervals <- filtered_intervals[1:laying_end]
  standing_intervals <- filtered_intervals[standing_start:standing_end]

  # Convert to HR for checks (60/RR)
  laying_hr <- 60 / laying_intervals
  standing_hr <- 60 / standing_intervals

  # Quality checks based on physiological norms
  quality_checks <- list(
    laying_hr_range = range(laying_hr),
    standing_hr_range = range(standing_hr),
    laying_rmssd = sqrt(mean(diff(laying_intervals * 1000)^2)),
    standing_rmssd = sqrt(mean(diff(standing_intervals * 1000)^2)),
    orthostatic_response = mean(standing_hr) - mean(laying_hr)
  )

  # Flag potentially problematic measurements
  warnings <- character(0)
  if (quality_checks$laying_hr_range[1] < 30 || quality_checks$laying_hr_range[2] > 100) {
    warnings <- c(warnings, "Unusual laying HR range detected")
  }
  if (quality_checks$standing_hr_range[1] < 40 || quality_checks$standing_hr_range[2] > 120) {
    warnings <- c(warnings, "Unusual standing HR range detected")
  }
  if (quality_checks$orthostatic_response < 0 || quality_checks$orthostatic_response > 40) {
    warnings <- c(warnings, "Unusual orthostatic response detected")
  }

  if (length(warnings) > 0) {
    warning(paste(warnings, collapse = "\n"))
  }

  # Return intervals and quality metrics
  list(
    laying = laying_intervals,
    standing = standing_intervals,
    quality_metrics = quality_checks
  )
}

#' Extract HR data from FIT object
#'
#' @description
#' Extracts heart rate data handling both list and data frame formats
#'
#' @param fit_object An object of class FitFile
#' @return Numeric vector of heart rate values
#' @export
#' @importFrom FITfileR records
get_HR <- function(fit_object) {
  if (!inherits(fit_object, "FitFile")) {
    stop("fit_object must be a FitFile object")
  }

  HR <- FITfileR::records(fit_object)

  if (inherits(HR, "list")) {
    # Find record with most data points
    max_rows_idx <- which.max(sapply(HR, nrow))
    HR <- HR[[max_rows_idx]]$heart_rate
  } else {
    HR <- HR$heart_rate
  }

  # Limit to first 360 seconds
  HR <- HR[1:min(360, length(HR))]

  return(HR)
}

#' Process a single FIT file
#'
#' @description
#' Calculates HRV metrics from a FIT file with error handling
#'
#' @param file_path Path to FIT file
#' @param filter_factor Numeric value for RR filtering
#' @return Tibble containing HRV metrics
#' @export
process_fit_file <- function(file_path, filter_factor) {
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  result <- tryCatch(
    {
      fit_object <- FITfileR::readFitFile(file_path)
      hr_data <- get_HR(fit_object)

      # Calculate metrics
      resting_hr <- calculate_resting_hr(
        hr_data[30:180],
        method = "lowest_sustained"
      )

      hrr_metrics <- calculate_hrr(hr_data[181:240], resting_hr)
      rr_data <- extract_rr_data(fit_object, filter_factor)

      # Get session data
      session <- FITfileR::getMessagesByType(fit_object, "session")
      date <- clock::as_date(session$timestamp)
      week <- as.numeric(strftime(session$timestamp, format = "%W"))

      hour <- clock::get_hour(session$timestamp)
      time_of_day <- if (hour > 4 && hour < 13) "Morning" else "Evening"

      # Process if we have enough data
      if (length(rr_data$laying) >= 2 && length(rr_data$standing) >= 2) {
        laying <- calculate_hrv(rr_data$laying)
        standing <- calculate_hrv(rr_data$standing)

        tibble::tibble(
          source_file = file_path,
          date = as.character(date),
          week = week,
          time_of_day = time_of_day,
          laying_rmssd = laying$rmssd,
          laying_sdnn = laying$sdnn,
          laying_hr = round(mean(hr_data[30:150], na.rm = TRUE), 2),
          laying_resting_hr = resting_hr,
          standing_rmssd = standing$rmssd,
          standing_sdnn = standing$sdnn,
          standing_hr = round(mean(hr_data[220:330], na.rm = TRUE), 2),
          standing_max_hr = max(hr_data[181:220], na.rm = TRUE),
          hrr_60s = hrr_metrics$hrr_60s,
          hrr_relative = hrr_metrics$hrr_relative,
          orthostatic_rise = hrr_metrics$orthostatic_rise,
          package_version = as.character(utils::packageVersion("hrvester")),
          RR_filter = filter_factor,
          activity = FITfileR::getMessagesByType(fit_object, "sport")$name
        )
      } else {
        create_empty_result(file_path, date, week, time_of_day, filter_factor)
      }
    },
    error = function(e) {
      create_empty_result(file_path, NA, NA, NA, filter_factor)
    }
  )

  return(result)
}

#' Create empty result row
#'
#' @description
#' Helper function to create empty result row with NA values
#'
#' @param file_path Source file path
#' @param date Date of measurement
#' @param week Week number
#' @param time_of_day Time of day
#' @param filter_factor RR filter factor used
#' @return Tibble with NA values
#' @keywords internal
create_empty_result <- function(file_path, date, week, time_of_day, filter_factor) {
  tibble::tibble(
    source_file = file_path,
    date = as.character(date),
    week = week,
    time_of_day = time_of_day,
    laying_rmssd = NA_real_,
    laying_sdnn = NA_real_,
    laying_hr = NA_real_,
    laying_resting_hr = NA_real_,
    standing_rmssd = NA_real_,
    standing_sdnn = NA_real_,
    standing_hr = NA_real_,
    standing_max_hr = NA_real_,
    hrr_60s = NA_real_,
    hrr_relative = NA_real_,
    orthostatic_rise = NA_real_,
    package_version = as.character(utils::packageVersion("hrvester")),
    RR_filter = filter_factor,
    activity = NA_character_
  )
}