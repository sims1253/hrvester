#' Extract RR Interval Data from a FIT File Object
#'
#' Extracts RR interval data (HRV data) from a FIT file object. This function
#' is designed for situations where you expect the FIT file to contain HRV data.
#' If no HRV data is found, it issues a warning and returns an empty tibble.
#'
#' @param fit_object A FIT file object, typically created by
#'   [`FITfileR::readFitFile()`]. The object should be the result of
#'   reading a FIT file that is expected to contain HRV (RR interval) data.
#'
#' @return A data frame containing the RR data, obtained directly from
#'   [`FITfileR::hrv()`]. If no HRV data is found, the function
#'   returns an empty tibble with a `time` column (`tibble(time = numeric())`).
#'   Unlike previous versions, this version does not perform any filtering
#'   on the returned HRV data.
#'
#' @details
#' The function performs the following steps:
#' 1. **Input Validation:** Validates the `fit_object` using 
#'    [`validate_fit_object()`] (assumed to be defined elsewhere in the package).
#' 2. **HRV Data Extraction:** Attempts to extract HRV data using
#'    [`FITfileR::hrv()`].
#' 3. **HRV Data Check:** Checks if the extracted HRV data is `NULL` or
#'    empty (has zero rows).
#' 4. **Return Value:**
#'    - If HRV data is found and is not empty, returns the data frame from
#'      [`FITfileR::hrv()`].
#'    - If HRV data is `NULL` or empty, issues a warning message 
#'      ("No HRV data found in FIT file") and returns an empty tibble.
#'
#' @examples
#' \dontrun{
#' # Assuming you have a FIT file named "activity.fit" that should
#' # contain HRV data.
#' fit_file <- FITfileR::readFitFile("activity.fit")
#' rr_data <- extract_rr_data(fit_file)
#'
#' # If HRV data was present:
#' # rr_data will be a data.frame (the result of FITfileR::hrv()).
#'
#' # If no HRV data was found:
#' # rr_data will be tibble(time = numeric()).
#' }
#'
#' @seealso [FITfileR::hrv()], [FITfileR::readFitFile()]
#' @importFrom FITfileR hrv
#' @importFrom tibble tibble
#' @export
extract_rr_data <- function(fit_object) {
  # Input validation with descriptive errors
  validate_fit_object(fit_object)

  # Check for HRV data availability
  hrv_data <- FITfileR::hrv(fit_object)
  if (is.null(hrv_data) || nrow(hrv_data) == 0) {
    warning("No HRV data found in FIT file")
    return(tibble::tibble(time = numeric()))
  }

  return(hrv_data)
}

#' Filter RR Intervals by Physiological Plausibility
#'
#' Filters RR intervals to remove physiologically implausible values based on
#' minimum and maximum thresholds. This is typically the first step in RR
#' interval preprocessing.
#'
#' @param rr_intervals Numeric vector of RR intervals in milliseconds
#' @param min_rr Minimum physiologically plausible RR interval in milliseconds.
#'   Default is 272 ms (equivalent to ~220 bpm)
#' @param max_rr Maximum physiologically plausible RR interval in milliseconds.
#'   Default is 2000 ms (equivalent to ~30 bpm)
#'
#' @return A logical vector of the same length as `rr_intervals`, where `TRUE`
#'   indicates the RR interval is within physiological bounds
#'
#' @details
#' This function applies basic physiological constraints to RR intervals:
#' - Values below `min_rr` are considered too fast (unrealistic heart rates)
#' - Values above `max_rr` are considered too slow (unrealistic heart rates)
#' - The default range corresponds to heart rates between 30-220 bpm
#'
#' @examples
#' \dontrun{
#' rr_data <- c(800, 850, 200, 900, 2500, 820)  # Some with artifacts
#' valid_mask <- filter_physiological_rr(rr_data)
#' clean_rr <- rr_data[valid_mask]
#' }
#'
#' @export
filter_physiological_rr <- function(rr_intervals, min_rr = 272, max_rr = 2000) {
  if (!is.numeric(rr_intervals)) {
    stop("rr_intervals must be numeric")
  }
  
  if (!is.numeric(min_rr) || length(min_rr) != 1 || min_rr <= 0) {
    stop("min_rr must be a single positive number")
  }
  
  if (!is.numeric(max_rr) || length(max_rr) != 1 || max_rr <= 0) {
    stop("max_rr must be a single positive number")
  }
  
  if (min_rr >= max_rr) {
    stop("min_rr must be less than max_rr")
  }
  
  # Return logical vector indicating valid intervals
  rr_intervals >= min_rr & rr_intervals <= max_rr & !is.na(rr_intervals)
}

#' Detect RR Interval Artifacts Using Moving Average
#'
#' Identifies artifacts in RR intervals by comparing each interval to a moving
#' average of surrounding intervals. This implements a robust artifact detection
#' algorithm commonly used in HRV analysis.
#'
#' @param rr_intervals Numeric vector of RR intervals in milliseconds
#' @param window_size Size of the moving window for artifact detection.
#'   Default is 7 intervals
#' @param threshold Threshold for artifact detection as a proportion.
#'   Default is 0.2 (20% deviation from moving average)
#' @param centered_window Logical indicating whether the moving window should
#'   be centered around each point. Default is FALSE (backward-looking window)
#'
#' @return A logical vector of the same length as `rr_intervals`, where `TRUE`
#'   indicates the RR interval is considered valid (not an artifact)
#'
#' @details
#' The algorithm works by:
#' 1. Computing a moving average of RR intervals within a sliding window
#' 2. Comparing each interval to the moving average
#' 3. Flagging intervals that deviate by more than the threshold proportion
#' 4. Using a centered or backward-looking window based on `centered_window`
#'
#' This is a key preprocessing step for reliable HRV analysis, as artifacts
#' can significantly affect calculated metrics.
#'
#' @examples
#' \dontrun{
#' rr_data <- c(800, 850, 400, 900, 820, 890, 870)  # 400 is likely an artifact
#' valid_mask <- detect_rr_artifacts(rr_data, window_size = 5, threshold = 0.2)
#' clean_rr <- rr_data[valid_mask]
#' }
#'
#' @export
detect_rr_artifacts <- function(rr_intervals, window_size = 7, threshold = 0.2, 
                               centered_window = FALSE) {
  if (!is.numeric(rr_intervals)) {
    stop("rr_intervals must be numeric")
  }
  
  if (length(rr_intervals) == 0) {
    return(logical(0))
  }
  
  if (!is.numeric(window_size) || length(window_size) != 1 || window_size < 1) {
    stop("window_size must be a positive integer")
  }
  
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold < 0 || threshold > 1) {
    stop("threshold must be a number between 0 and 1")
  }
  
  if (!is.logical(centered_window) || length(centered_window) != 1) {
    stop("centered_window must be a single logical value")
  }
  
  n <- length(rr_intervals)
  if (n < window_size) {
    # If we have fewer intervals than window size, return all as valid
    return(rep(TRUE, n))
  }
  
  valid <- rep(TRUE, n)
  
  for (i in seq_along(rr_intervals)) {
    if (is.na(rr_intervals[i])) {
      valid[i] <- FALSE
      next
    }
    
    # Determine window boundaries
    if (centered_window) {
      half_window <- floor(window_size / 2)
      start_idx <- max(1, i - half_window)
      end_idx <- min(n, i + half_window)
    } else {
      # Backward-looking window
      start_idx <- max(1, i - window_size + 1)
      end_idx <- i
    }
    
    # Get window data excluding current point for comparison
    window_data <- rr_intervals[start_idx:end_idx]
    if (centered_window) {
      window_data <- window_data[window_data != rr_intervals[i] | 
                                is.na(window_data) != is.na(rr_intervals[i])]
    } else {
      window_data <- window_data[-length(window_data)]  # Remove current point
    }
    
    # Skip if not enough valid data in window
    valid_window <- window_data[!is.na(window_data)]
    if (length(valid_window) < 2) {
      next
    }
    
    # Calculate moving average
    moving_avg <- mean(valid_window)
    
    # Check if current value deviates too much from moving average
    deviation <- abs(rr_intervals[i] - moving_avg) / moving_avg
    if (deviation > threshold) {
      valid[i] <- FALSE
    }
  }
  
  return(valid)
}

#' Preprocess RR Intervals with Multiple Filtering Steps
#'
#' Applies a comprehensive preprocessing pipeline to RR intervals, including
#' physiological filtering and artifact detection. This is a convenience function
#' that combines multiple preprocessing steps.
#'
#' @param rr_intervals Numeric vector of RR intervals in milliseconds
#' @param min_rr Minimum physiologically plausible RR interval in milliseconds.
#'   Default is 272 ms
#' @param max_rr Maximum physiologically plausible RR interval in milliseconds.
#'   Default is 2000 ms
#' @param window_size Size of the moving window for artifact detection.
#'   Default is 7 intervals
#' @param threshold Threshold for artifact detection as a proportion.
#'   Default is 0.2
#' @param centered_window Logical indicating whether the moving window should
#'   be centered. Default is FALSE
#'
#' @return A list containing:
#'   - `cleaned_rr`: Numeric vector of cleaned RR intervals (artifacts removed)
#'   - `valid_mask`: Logical vector indicating which original intervals are valid
#'   - `n_removed_physiological`: Number of intervals removed for physiological reasons
#'   - `n_removed_artifacts`: Number of intervals removed as artifacts
#'   - `n_original`: Original number of intervals
#'   - `n_final`: Final number of valid intervals
#'
#' @details
#' The preprocessing pipeline applies the following steps in order:
#' 1. Physiological filtering using [`filter_physiological_rr()`]
#' 2. Artifact detection using [`detect_rr_artifacts()`]
#' 3. Combines both filters to create final valid mask
#' 4. Returns both cleaned data and diagnostic information
#'
#' This function is designed to be the main entry point for RR interval
#' preprocessing in the HRV analysis pipeline.
#'
#' @examples
#' \dontrun{
#' rr_data <- c(800, 850, 200, 900, 2500, 820, 890, 870)
#' result <- preprocess_rr_intervals(rr_data)
#' print(paste("Removed", result$n_removed_physiological + result$n_removed_artifacts, 
#'             "of", result$n_original, "intervals"))
#' clean_data <- result$cleaned_rr
#' }
#'
#' @export
preprocess_rr_intervals <- function(rr_intervals, min_rr = 272, max_rr = 2000,
                                   window_size = 7, threshold = 0.2, 
                                   centered_window = FALSE) {
  if (!is.numeric(rr_intervals)) {
    stop("rr_intervals must be numeric")
  }
  
  n_original <- length(rr_intervals)
  
  if (n_original == 0) {
    return(list(
      cleaned_rr = numeric(0),
      valid_mask = logical(0),
      n_removed_physiological = 0,
      n_removed_artifacts = 0,
      n_original = 0,
      n_final = 0
    ))
  }
  
  # Step 1: Physiological filtering
  physio_valid <- filter_physiological_rr(rr_intervals, min_rr, max_rr)
  n_removed_physiological <- sum(!physio_valid)
  
  # Step 2: Artifact detection on physiologically valid data
  artifact_valid <- rep(TRUE, n_original)
  if (any(physio_valid)) {
    # Only check artifacts on physiologically valid intervals
    temp_rr <- rr_intervals
    temp_rr[!physio_valid] <- NA  # Mark invalid as NA for artifact detection
    artifact_valid <- detect_rr_artifacts(temp_rr, window_size, threshold, centered_window)
  }
  
  # Combine both validation steps
  final_valid <- physio_valid & artifact_valid
  n_removed_artifacts <- sum(physio_valid & !artifact_valid)
  n_final <- sum(final_valid)
  
  # Create cleaned data
  cleaned_rr <- rr_intervals[final_valid]
  
  return(list(
    cleaned_rr = cleaned_rr,
    valid_mask = final_valid,
    n_removed_physiological = n_removed_physiological,
    n_removed_artifacts = n_removed_artifacts,
    n_original = n_original,
    n_final = n_final
  ))
}