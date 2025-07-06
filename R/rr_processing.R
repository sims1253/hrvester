#' Split RR Intervals Into Orthostatic Test Phases
#'
#' This function splits RR intervals into the orthostatic test phases
#' (laying, transition, and standing) based on specified time thresholds.
#'
#' @param rr_intervals A data frame containing RR interval data.  Must include
#'   a column named `time`.  The function will add an `elapsed_time` column
#'   and a `phase` column.
#' @param session_info A list containing session information.  Must include a
#'   numeric element named `duration` representing the total duration of the
#'   recording (in the same units as `laying_time`, `transition_time`, and
#'   `standing_time`).
#' @param laying_time Numeric. The duration of the laying phase (e.g., in
#'   seconds). Must be non-negative.
#' @param transition_time Numeric. The duration of the transition phase (e.g.,
#'   in seconds). Must be non-negative.
#' @param standing_time Numeric. The duration of the standing phase (e.g., in
#'   seconds). Must be non-negative.
#' @param centered_transition Indicates if the transition time should be split
#'   into laying and standig times. FALSE, if transition time is only taken
#'   from the laying phase.
#'
#' @return A data frame with the same data as `rr_intervals`, but with two
#'   added columns:
#'   \item{elapsed_time}{The cumulative time from the start of the recording.}
#'   \item{phase}{A character string indicating the phase: "laying",
#'   "transition", or "standing".}
#'
#' @details The sum of `laying_time` and `standing_time` cannot exceed
#'   `session_info$duration`. If `rr_intervals` is an empty data frame, a
#'   warning is issued, and an empty data frame with a `phase` column is
#'   returned.
#'
#' @export
#'
#' @examples
#' # Example with valid inputs
#' rr_data <- data.frame(time = 1:100)
#' session_data <- list(duration = 100)
#' result <- split_rr_phases(rr_data, session_data, 30, 20, 50)
#' head(result)
#'
#' # Example with an empty rr_intervals data frame
#' rr_empty <- data.frame(time = numeric())
#' session_data <- list(duration = 60)
#' result_empty <- split_rr_phases(rr_empty, session_data, 20, 10, 30)
#' print(result_empty)
#'
#' \dontrun{
#' # Example with invalid input (sum of times exceeds duration)
#' rr_data <- data.frame(time = 1:100)
#' session_data <- list(duration = 100)
#' # This will throw an error
#' result_error <- split_rr_phases(rr_data, session_data, 50, 60, 70)
#' }
#' @importFrom stats time
split_rr_phases <- function(
  rr_intervals,
  session_info,
  laying_time = 180,
  transition_time = 20,
  standing_time = 180,
  centered_transition = TRUE
) {
  if (length(rr_intervals) == 0) {
    return(rr_intervals)
  }
  # rr_intervals checks
  if (!is.data.frame(rr_intervals)) {
    stop("rr_intervals must be a data frame")
  }
  # Convert to tibble if it's a data.frame
  if (!is(rr_intervals, "tbl_df")) {
    rr_intervals <- dplyr::as_tibble(rr_intervals)
  }
  if (!("time" %in% colnames(rr_intervals))) {
    stop("rr_intervals must contain a 'time' column.")
  }
  if (nrow(rr_intervals) == 0) {
    warning(
      "rr_intervals is an empty tibble. An empty tibble with phase
            column will be returned."
    )
    rr_intervals$phase <- character() # Add the 'phase' column
    return(rr_intervals)
  }

  # session_info checks
  if (!is.list(session_info) || is.null(session_info$duration)) {
    stop("session_info must be a list containing a 'duration' element.")
  }
  if (!is.numeric(session_info$duration) || session_info$duration <= 0) {
    stop("session_info$duration must be a positive numeric value")
  }

  # laying_time checks
  if (!is.numeric(laying_time) || length(laying_time) != 1 || laying_time < 0) {
    stop("laying_time must be a non-negative numeric value.")
  }

  # transition_time checks
  if (
    !is.numeric(transition_time) ||
      length(transition_time) != 1 ||
      transition_time < 0
  ) {
    stop("transition_time must be a non-negative numeric value.")
  }

  # standing_time checks
  if (
    !is.numeric(standing_time) ||
      length(standing_time) != 1 ||
      standing_time < 0
  ) {
    stop("standing_time must be a non-negative numeric value.")
  }

  # Time duration checks
  if (laying_time + standing_time > session_info$duration) {
    stop(
      "The sum of laying_time, and standing_time cannot exceed
         session_info$duration."
    )
  }
  if (centered_transition) {
    if (
      transition_time / 2 > standing_time || transition_time / 2 > laying_time
    ) {
      stop("transition_time/2 must not exceed laying_time or standing_time.")
    }
  } else {
    if (transition_time > standing_time) {
      stop("transition_time must not exceed the standing_time.")
    }
  }

  rr_intervals$elapsed_time <- seq(
    from = session_info$duration / nrow(rr_intervals),
    to = session_info$duration,
    by = session_info$duration / nrow(rr_intervals)
  )

  if (centered_transition) {
    rr_intervals <- rr_intervals %>%
      dplyr::mutate(
        phase = dplyr::case_when(
          elapsed_time <= laying_time - transition_time / 2 ~ "laying",
          elapsed_time <= laying_time + transition_time / 2 ~ "transition",
          .default = "standing"
        )
      )
  } else {
    rr_intervals <- rr_intervals %>%
      dplyr::mutate(
        phase = dplyr::case_when(
          elapsed_time <= laying_time ~ "laying",
          elapsed_time <= laying_time + transition_time ~ "transition",
          .default = "standing"
        )
      )
  }

  return(rr_intervals)
}

#' Validate RR Interval Segment
#'
#' This function validates an RR interval segment, ensuring it is a numeric
#' vector containing only positive values and no NA values.
#'
#' @param rr_segment A numeric vector of RR intervals (in milliseconds).
#'
#' @keywords internal
validate_rr <- function(rr_segment) {
  if (!is.numeric(rr_segment)) {
    stop("`rr_segment` must be a numeric vector.")
  }
  if (any(is.na(rr_segment))) {
    stop("`rr_segment` cannot contain NA values.")
  }
  if (any(rr_segment <= 0)) {
    stop("`rr_segment` must contain only positive values.")
  }
}

#' Validate Validity Vector
#'
#' This function validates the validity vector used in RR interval processing,
#' ensuring it is a logical vector of the correct length and contains no NA values.
#'
#' @param is_valid A logical vector indicating validity of RR intervals.
#' @param rr_segment A numeric vector of RR intervals (for length checking).
#'
#' @keywords internal
validate_validity <- function(is_valid, rr_segment) {
  if (!is.logical(is_valid)) {
    stop("`is_valid` must be a logical vector.")
  }
  if (length(is_valid) != length(rr_segment)) {
    stop("`is_valid` must have the same length as `rr_segment`.")
  }
  if (any(is.na(is_valid))) {
    stop("`is_valid` cannot contain NA values.")
  }
}

#' RR validation that flags intervals of 65.535 as invalid
#'
#' When taking the raw RR output from a Garmin watch, most of the entries are 65.535, which is
#' just an empty measurement, as no new heartbeat has been detected yet.
#'
#' @param rr_segment A numeric vector of RR intervals (in milliseconds).
#' @param is_valid A logical vector indicating which intervals are valid initially. Defaults to all TRUE.
#'
#' @return A list containing two elements:
#'   \item{is_valid}{A logical vector of the same length as `rr_segment`, where
#'     `TRUE` indicates a valid RR interval and `FALSE` indicates an invalid
#'     interval.}
#'   \item{cleaned_rr}{A numeric vector containing only the valid RR intervals.}
#'
#' @export
rr_validate_measure_artifacts <- function(
  rr_segment,
  is_valid = rep(TRUE, length(rr_segment))
) {
  validate_rr(rr_segment)
  validate_validity(is_valid, rr_segment)

  # Mark intervals as invalid if they are artifacts (65.535 or 65535)
  is_valid <- is_valid & (rr_segment != 65.535 & rr_segment != 65535)

  return(list(
    is_valid = is_valid,
    cleaned_rr = rr_segment[is_valid]
  ))
}

#' Validate RR Intervals Based on Physiological Limits
#'
#' This function validates RR intervals based on minimum and maximum
#' physiologically plausible values. Intervals outside the specified range
#' are marked as invalid.
#'
#' @param rr_segment A numeric vector of RR intervals (in milliseconds).
#' @param is_valid A logical vector indicating which intervals are valid initially. Defaults to all TRUE.
#' @param min_rr The minimum acceptable RR interval (in milliseconds).
#'   Defaults to 272 ms (corresponding to a maximum heart rate of ~220 bpm).
#' @param max_rr The maximum acceptable RR interval (in milliseconds).
#'   Defaults to 2000 ms (corresponding to a minimum heart rate of 30 bpm).
#'
#' @return A list containing two elements:
#'   \item{is_valid}{A logical vector of the same length as `rr_segment`, where
#'     `TRUE` indicates a valid RR interval and `FALSE` indicates an invalid
#'     interval after applying the physiological limits.}
#'   \item{cleaned_rr}{A numeric vector containing only the valid RR intervals.}
#'
#' @examples
#' # Example with some values outside the default range
#' rr_data <- c(250, 500, 1000, 1800, 2500)
#' result <- rr_validate_physiological(rr_data)
#' print(result)
#'
#' # Example with pre-existing invalid values and a custom range
#' rr_data2 <- c(250, 500, 1000, 1800, 2500)
#' is_valid_initial <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' result2 <- rr_validate_physiological(rr_data2,
#'   is_valid = is_valid_initial,
#'   min_rr = 400,
#'   max_rr = 1500
#' )
#' print(result2)
#'
#' # Example with empty input
#' result_empty <- rr_validate_physiological(numeric(0))
#' print(result_empty)
#'
#' @export
rr_validate_physiological <- function(
  rr_segment,
  is_valid = rep(TRUE, length(rr_segment)),
  min_rr = 272,
  max_rr = 2000
) {
  # Input Validation
  validate_rr(rr_segment)
  validate_validity(is_valid, rr_segment)

  if (
    !is.numeric(min_rr) ||
      length(min_rr) != 1 ||
      !is.finite(min_rr) ||
      min_rr <= 0
  ) {
    stop("`min_rr` must be a single, finite, positive value.")
  }

  if (
    !is.numeric(max_rr) ||
      length(max_rr) != 1 ||
      !is.finite(max_rr) ||
      max_rr <= 0
  ) {
    stop("`max_rr` must be a single, finite, positive value.")
  }

  if (max_rr <= min_rr) {
    stop("`max_rr` must be greater than `min_rr`.")
  }

  # Apply physiological limits
  is_valid <- is_valid & (rr_segment >= min_rr & rr_segment <= max_rr)

  return(list(
    is_valid = is_valid,
    cleaned_rr = rr_segment[is_valid]
  ))
}

#' Moving Average RR Interval Validation
#'
#' Validates RR intervals using a moving average filter, flagging potentially
#' erroneous intervals.
#'
#' @param rr_segment A numeric vector of RR intervals (in milliseconds).
#' @param is_valid A logical vector indicating which intervals are valid initially. Defaults to all TRUE.
#' @param window_size An integer specifying the size of the moving window (must
#'   be odd). Defaults to 7.
#' @param threshold A numeric value representing the maximum allowed
#'   proportional deviation from the moving median. Defaults to 0.2 (20%).
#' @param centered_window A logical value indicating whether the moving window should be centered. Defaults to FALSE.
#'
#' @return A list containing two elements:
#'   \item{rr_segment}{A numeric vector containing only the valid RR intervals.}
#'
#' @details
#'    **Moving Average Filter:** A moving median is calculated for each RR
#'    interval using a window of size `window_size`. The current RR interval
#'    is excluded from the median calculation.  The proportional deviation is
#'    calculated. If this deviation exceeds the `threshold`, the interval is
#'    marked as invalid. Only *a priori* valid intervals within the window are
#'    used. If all intervals within the window (excluding current) are
#'    invalid, the validity remains unchanged.
#'
#' The moving window is centered. At the boundaries, the window is truncated.
#'
#' @examples
#' # Sample RR interval data (in seconds)
#' rr_data <- c(
#'   0.8, 0.9, 0.7, 1.5, 0.85, 0.95, 0.88, 0.92, 0.8,
#'   4.0
#' )
#'
#' # Validate the RR intervals
#' result <- moving_average_rr_validation(rr_data)
#'
#' # Print the results
#' print(result$is_valid)
#' print(result$cleaned_rr)
#'
#' # Example with pre-existing invalid intervals
#' is_valid_initial <- c(
#'   TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
#'   TRUE, TRUE, TRUE, TRUE
#' )
#' result2 <- moving_average_rr_validation(rr_data,
#'   is_valid = is_valid_initial,
#'   window_size = 5
#' )
#' print(result2)
#'
#' @importFrom stats median
#' @export
moving_average_rr_validation <- function(
  rr_segment,
  is_valid = rep(TRUE, length(rr_segment)),
  window_size = 7,
  threshold = 0.2,
  centered_window = FALSE
) {
  # Input Validation Checks
  validate_rr(rr_segment)
  validate_validity(is_valid, rr_segment)

  if (length(rr_segment) == 0) {
    return(list(
      is_valid = logical(0),
      cleaned_rr = numeric(0)
    ))
  }
  # window_size checks
  if (
    !is.numeric(window_size) ||
      length(window_size) != 1 ||
      !is.finite(window_size) ||
      as.integer(window_size) != window_size ||
      window_size <= 0
  ) {
    stop("`window_size` must be a single, finite, positive, odd integer.")
  }

  # Only require odd window size for centered windows
  if (centered_window && window_size %% 2 == 0) {
    stop("`window_size` must be a single, finite, positive, odd integer.")
  }

  if (window_size > length(rr_segment)) {
    warning(
      "`window_size` is larger than the length of `rr_segment`.
            The effective window size will be truncated."
    )
    current_window_size <- length(rr_segment)
  } else {
    current_window_size <- window_size
  }

  # threshold checks
  if (
    !is.numeric(threshold) ||
      length(threshold) != 1 ||
      !is.finite(threshold)
  ) {
    stop("`threshold` must be a single finite numeric value.")
  }
  if (threshold < 0 || threshold > 1) {
    stop("`threshold` must be between 0 and 1 (inclusive).")
  }

  n <- length(rr_segment)

  for (i in seq_len(n)) {
    if (!is_valid[i]) {
      next
    } # Skip if already marked as invalid

    # Calculate moving average and deviation
    if (centered_window) {
      half_window <- (current_window_size - 1) / 2
      lower_bound <- max(1, i - half_window)
      upper_bound <- min(n, i + half_window)
    } else {
      lower_bound <- max(1, i - current_window_size)
      upper_bound <- min(n, i)
    }

    # Extend window to include enough valid data points, in case we removed some
    # minus one for the i-th item we won't use to compute the reference value
    valid_count <- sum(is_valid[lower_bound:upper_bound]) - 1
    if (valid_count == 0) {
      next
    }

    lower_bound_extended <- lower_bound
    upper_bound_extended <- upper_bound
    while (
      valid_count < current_window_size &&
        (lower_bound_extended > 1 ||
          (upper_bound_extended < n && centered_window))
    ) {
      # Try extending lower bound first
      if (lower_bound_extended > 1) {
        lower_bound_extended <- lower_bound_extended - 1
        if (is_valid[lower_bound_extended]) valid_count <- valid_count + 1
      }

      # If lower bound cannot be extended, extend upper bound
      if (
        valid_count < current_window_size &&
          (upper_bound_extended < n && centered_window)
      ) {
        upper_bound_extended <- upper_bound_extended + 1
        if (is_valid[upper_bound_extended]) valid_count <- valid_count + 1
      }
    }

    if (lower_bound_extended <= upper_bound_extended) {
      valid_window_data <- rr_segment[
        lower_bound_extended:upper_bound_extended
      ][
        is_valid[lower_bound_extended:upper_bound_extended]
      ]
      # Remove the current interval for MAD calculation to prevent zero deviations.
      current_rr_index <- which(valid_window_data == rr_segment[i])[1]
      if (!is.na(current_rr_index)) {
        valid_window_data <- valid_window_data[-current_rr_index]
      }
      if (length(valid_window_data) > 0) {
        ma <- median(valid_window_data)
        deviation <- abs(rr_segment[i] - ma) / ma
        is_valid[i] <- is_valid[i] & (deviation <= threshold)
      }
    }
  }

  return(list(
    is_valid = is_valid,
    cleaned_rr = rr_segment[is_valid]
  ))
}

#' Full RR Interval Processing Pipeline
#'
#' This function performs a complete RR interval processing pipeline,
#' including artifact detection, physiological plausibility checks, and
#' moving average validation.
#'
#' @param rr_segment A numeric vector of RR intervals (in milliseconds).
#' @param min_rr The minimum acceptable RR interval (in milliseconds).
#'   Defaults to 272 ms (corresponding to a maximum heart rate of ~220 bpm).
#' @param max_rr The maximum acceptable RR interval (in milliseconds).
#'   Defaults to 2000 ms (corresponding to a minimum heart rate of 30 bpm).
#' @param window_size An integer specifying the size of the moving window
#'   (must be odd) for the moving average validation. Defaults to 7.
#' @param threshold A numeric value representing the maximum allowed
#'   proportional deviation from the moving median in the moving average
#'   validation. Defaults to 0.2 (20%).
#' @param centered_window A logical value indicating whether to use centered
#'   window for moving average validation. Defaults to FALSE.
#'
#' @return A list containing two elements:
#'   \item{is_valid}{A logical vector of the same length as `rr_segment`,
#'     where `TRUE` indicates a valid RR interval and `FALSE` indicates an
#'     invalid interval after all processing steps.}
#'   \item{cleaned_rr}{A numeric vector containing only the valid RR
#'     intervals after all processing steps.}
#'
#' @details This function sequentially applies the following validation steps:
#'   1. **Artifact Detection:** `rr_validate_measure_artifacts` is used
#'      to flag common measurement artifacts (e.g., values of 65.535 from
#'      some Garmin watches).
#'   2. **Physiological Validation:** `rr_validate_physiological` is used
#'      to flag intervals outside the specified `min_rr` and `max_rr` limits.
#'   3. **Moving Average Validation:** `moving_average_rr_validation` is
#'     used to flag intervals that deviate significantly from the local
#'     moving median, using the specified `window_size` and `threshold`.
#' The function uses a 'running result' approach, where the `is_valid` vector
#' from each step is passed as input to the next. This ensures that invalid
#' intervals identified in earlier steps are not considered in subsequent
#' validation steps.
#'
#' @examples
#' # Sample RR interval data (in milliseconds)
#' rr_data <- c(
#'   65.535, 500, 250, 750, 800, 1500, 820, 780,
#'   850, 3000, 800, 830, 790
#' )
#'
#' # Process the RR intervals
#' result <- rr_full_phase_processing(rr_data)
#'
#' # Print the results
#' print(result$is_valid)
#' print(result$cleaned_rr)
#'
#' # Example with custom parameters
#' rr_data2 <- c(
#'   65535, 600, 300, 700, 900,
#'   2200, 750
#' )
#' result2 <- rr_full_phase_processing(
#'   rr_data2,
#'   min_rr = 400,
#'   max_rr = 1800,
#'   window_size = 5,
#'   threshold = 0.15
#' )
#' print(result2)
#'
#' @export
rr_full_phase_processing <- function(
  rr_segment,
  min_rr = 272,
  max_rr = 2000,
  window_size = 7,
  threshold = 0.2,
  centered_window = FALSE
) {
  validate_rr(rr_segment)

  if (length(rr_segment) == 0) {
    return(list(
      is_valid = logical(0),
      cleaned_rr = numeric(0)
    ))
  }

  # Step 1: Artifact detection
  result1 <- rr_validate_measure_artifacts(rr_segment = rr_segment)

  # Step 2: Physiological validation
  result2 <- rr_validate_physiological(
    rr_segment = rr_segment,
    is_valid = result1$is_valid,
    min_rr = min_rr,
    max_rr = max_rr
  )

  # Step 3: Moving average validation
  result3 <- moving_average_rr_validation(
    rr_segment = rr_segment,
    is_valid = result2$is_valid,
    window_size = window_size,
    threshold = threshold,
    centered_window = centered_window
  )

  return(result3)
}
