#' Detect and Classify Artefacts in Heart Rate Variability Time Series
#'
#' This function implements the robust algorithm for heart rate variability (HRV)
#' time series artefact correction described in Lipponen & Tarvainen (2019).
#' It detects and classifies artefacts such as extra, missed, misaligned, and
#' ectopic beats in RR interval time series data.
#'
#' @param data A `dplyr::tibble` with at least one column:
#'   * `time`: A numeric vector representing RR intervals (time since the last
#'     beat) in seconds.
#' @param alpha A numeric value representing the scaling factor for threshold
#'   calculation.  Defaults to 5.2 as suggested in the paper.
#' @param c1 Constant for ectopic beat detection boundary. Defaults to 0.13.
#' @param c2 Constant for ectopic beat detection boundary. Defaults to 0.17.
#' @param qd_window Window size for quartile deviation calculation. Defaults to 91.
#'
#' @return A `dplyr::tibble` with the following columns:
#'   * `time`: The original RR intervals (in seconds).
#'   * `classification`: A character vector indicating the classification of each
#'     RR interval.  (Same possible values as before)
#'
#' @details
#' The algorithm uses time-varying thresholds based on the distribution of
#' successive RR-interval differences (`dRRs`) and differences between
#' individual RR intervals and a median RR interval (`mRRs`).  It implements
#' a decision algorithm (see Figure 1 in the paper) to classify beats into
#' different artefact types.  Missing beats are handled specially:  a new row
#' is *inserted* into the tibble, with the `time` value representing the estimated
#' time of the missed beat (half of the "long" interval), and the
#' `classification` set to `"missed"`. This insertion ensures that subsequent
#' calculations depending on the number of beats can be performed correctly.
#' Long, short and ectopic beat are not corrected in this function. The original
#' time series including all kind of original beats is returned.
#'
#' @examples
#' library(dplyr)
#' library(zoo)
#'
#' # Create some sample data with a few artefacts
#' set.seed(123)
#' rr_data <- tibble(time = cumsum(rnorm(100, mean = 0.8, sd = 0.1)))
#' rr_data$time[50] <- rr_data$time[50] + 1.0 # Missed beat
#' rr_data$time[70] <- rr_data$time[70] - 0.4 # Extra beat
#'
#' # Detect and classify the artefacts
#' rr_data_classified <- classify_hrv_artefacts_lipponen(rr_data)
#'
#' # Print the rows with artefacts
#' print(rr_data_classified %>% filter(classification != "normal"))
#'
#' @seealso [stats::quantile()] for quartile calculations, [zoo::rollapply()]
#'  for rolling window calculations.
#'
#' @export
#'
#' @references
#' Jukka A. Lipponen & Mika P. Tarvainen (2019) A robust algorithm for heart
#' rate variability time series artefact correction using novel beat
#' classification, Journal of Medical Engineering & Technology, 43:3, 173-181,
#' DOI: 10.1080/03091902.2019.1640306
#'
#' @importFrom stats quantile
#' @importFrom zoo rollapply rollmedian
classify_hrv_artefacts_lipponen <- function(
  data,
  alpha = 5.2,
  c1 = 0.13,
  c2 = 0.17,
  qd_window = 91
) {
  # Input validation (Same as before)
  if (!inherits(data, "tbl_df")) {
    stop("Input 'data' must be a dplyr::tibble.")
  }
  if (!("time" %in% colnames(data))) {
    stop("Input tibble 'data' must contain a 'time' column.")
  }
  if (!is.numeric(data$time)) {
    stop("The 'time' column must be numeric.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0) {
    stop("'alpha' must be a positive numeric value.")
  }

  # --- Helper Functions ---

  # Quartile Deviation (QD)
  qd <- function(x) {
    if (all(is.na(x))) {
      return(0)
    }
    (stats::quantile(x, 0.75, na.rm = TRUE) -
      stats::quantile(x, 0.25, na.rm = TRUE)) /
      2
  }

  rr <- data$time
  n <- length(rr)

  # 1. Calculate dRRs and mRRs
  drrs <- c(0, diff(rr))
  medrr <- runmed(rr, k = 11, endrule = "median") # zoo::rollmedian(rr, k = 11, fill = NA, align = "center")
  mrrs <- rr - medrr
  mrrs <- ifelse(mrrs < 0, 2 * mrrs, mrrs) # Scale mRRs

  # 2. Calculate time-varying thresholds (Th1 and Th2)
  th1 <- unname(
    alpha *
      zoo::rollapply(
        abs(drrs),
        width = qd_window,
        FUN = qd,
        fill = NA,
        align = "center",
        partial = TRUE
      )
  )
  th2 <- unname(
    alpha *
      zoo::rollapply(
        abs(mrrs),
        width = qd_window,
        FUN = qd,
        fill = NA,
        align = "center",
        partial = TRUE
      )
  )

  # 3. Normalize dRRs and mRRs
  drr <- drrs / (th1 + 1e-6) # Use smaller epsilon for better sensitivity
  mrr <- mrrs / (th2 + 1e-6)

  # 4. Decision Algorithm (using vectorized operations)
  data <- data %>%
    dplyr::mutate(classification = "normal")

  # --- Long/Short beat detection (Subspace S2) + Missed/Extra ---
  s21 <- drr
  s22 <- zoo::rollapply(
    data = drr,
    width = 3,
    FUN = function(x) {
      first_val <- x[1]
      if (is.na(x[1])) {
        return(NA)
      } else if (first_val >= 0) {
        return(min(x, na.rm = TRUE))
      } else {
        return(max(x, na.rm = TRUE))
      }
    },
    fill = NA,
    align = "left",
    partial = TRUE
  )

  long_short_condition <- unname(
    (abs(drr) > 1 &
      ((s21 > 1 & s22 < -1) |
        (s21 < -1 & s22 > 1))) |
      abs(mrr) > 3
  )

  data <- data %>%
    dplyr::mutate(
      classification = ifelse(
        long_short_condition & classification == "normal",
        ifelse(drr > 0, "long", "short"),
        classification
      )
    )

  # Missed beat detection
  missed_condition <- unname(
    data$classification == "long" & (abs(rr / 2 - medrr) < th2)
  )
  data <- data %>%
    dplyr::mutate(
      classification = ifelse(
        missed_condition & classification %in% c("long", "normal"),
        "missed",
        classification
      )
    )

  # Extra beat detection
  extra_condition <- unname(
    data$classification == "short" &
      c(FALSE, (abs(rr[-n] + rr[-1] - medrr[-n]) < th2[-n]))
  )
  data <- data %>%
    dplyr::mutate(
      classification = ifelse(
        extra_condition & classification %in% c("short", "normal"),
        "extra",
        classification
      )
    )

  ## --- Ectopic beat detection (Subspace S1) - Applied after missed/extra ---
  s11 <- drr
  # For ectopic detection, s12 should represent the following beat in sequence
  # This implements the PNP (Premature-Normal-Post) and NPN patterns
  s12 <- c(drr[-1], NA) # Shift drr by one position to get next value

  # Ectopic detection for patterns not already classified as missed/extra
  ectopic_condition <- unname(
    (s11 > 1 & s12 < -1 & s12 < (-c1 * s11 + c2)) |
      (s11 < -1 & s12 > 1 & s12 > (-c1 * s11 - c2))
  )
  data <- data %>%
    dplyr::mutate(
      classification = ifelse(
        ectopic_condition & classification %in% c("long", "short", "normal"),
        "ectopic",
        classification
      )
    )

  return(data)
}

#' Correct Heart Rate Variability Artefacts
#'
#' This function takes the output of `detect_hrv_artefacts` (a tibble with RR
#' intervals and artefact classifications) and performs the actual artefact
#' correction, including:
#'
#' *   **Interpolation:** Ectopic, long, and short beats are corrected by
#'     replacing the identified intervals with values obtained via cubic spline
#'     interpolation.
#' *   **Removal:** Extra beats are removed.
#' *   **Insertion:**  Rows are inserted for missed beats, with the time
#'      value set to half the duration of the long interval.
#'
#' @param data A `dplyr::tibble` returned by
#'   `detect_hrv_artefacts`, containing 'time' and 'classification' columns.
#'
#' @return A `dplyr::tibble` with the following columns:
#'   * `time`: The *corrected* RR intervals (in milliseconds).
#'   * `classification`:  The updated classification column. Intervals that
#'     were corrected by interpolation will have the classification
#'      `"interpolated"`. Inserted "missed" beats will have the classification
#'     `"missed"`. Removed beats will not be present.
#'
#' @seealso [classify_hrv_artefacts_lipponen()], [signal::interp1()]
#'
#' @export
#' @importFrom signal interp1
#' @importFrom dplyr %>% mutate bind_rows filter
#' @importFrom stats runmed sd spline
#' @importFrom methods is
correct_hrv_artefacts_lipponen <- function(data) {
  if (
    !inherits(data, "tbl_df") ||
      !all(c("time", "classification") %in% colnames(data))
  ) {
    stop(
      "Input 'data' must be a tibble with 'time' and 'classification' columns (output of detect_hrv_artefacts)."
    )
  }
  data$original_time <- data$time
  data$correction <- "none"
  rr <- data$time
  n <- length(rr)

  # --- 1. Interpolation (Ectopic, Long, Short) ---
  interp_indices <- which(
    data$classification %in% c("ectopic", "long", "short")
  )
  if (length(interp_indices) > 0) {
    # Use cubic spline interpolation, handling edge cases with padding
    not_interp_indices <- setdiff(1:nrow(data), interp_indices)
    if (length(not_interp_indices) < 2) {
      warning(
        "Less than two non-interpolated beats. Returning the original data."
      )
      return(data)
    }

    # TODO add spline interpolation with not-a-knot end conditions for transition and
    # PCHIP for ≤3-beat artifacts, spline for longer gaps for standing
    if (any(interp_indices <= 3) | any(interp_indices >= n - 2)) {
      rr_padded <- c(
        rep(median(rr[1:10]), 3),
        rr,
        rep(median(rr[(n - 10):n]), 3)
      )
      interp_indices_padded <- interp_indices + 3
      not_interp_padded <- setdiff(1:length(rr_padded), interp_indices_padded)
      rr_padded[interp_indices_padded] <- signal::interp1(
        x = not_interp_padded,
        y = rr_padded[not_interp_padded],
        xi = interp_indices_padded,
        method = "pchip"
      )
      rr_corrected <- rr_padded[4:(length(rr_padded) - 3)]
    } else {
      rr_corrected <- rr
      rr_corrected[interp_indices] <- signal::interp1(
        x = not_interp_indices,
        y = rr[not_interp_indices],
        xi = interp_indices,
        method = "pchip"
      )
    }

    data$time[interp_indices] <- rr_corrected[interp_indices]
    data$correction[interp_indices] <- "interpolated"
  }

  # --- 2. Remove Extra Beats ---
  data <- data %>%
    dplyr::filter(classification != "extra")

  # --- 3. Insert Missed Beats ---
  missed_indices <- which(data$classification == "missed")

  if (length(missed_indices) > 0) {
    for (i in rev(missed_indices)) {
      new_time <- data$time[i] / 2
      new_row <- dplyr::tibble(
        time = new_time,
        classification = "missed",
        correction = "halved"
      )
      data$time[i] <- new_time
      data <- dplyr::bind_rows(
        data[1:(i - 1), , drop = FALSE],
        new_row,
        data[i:nrow(data), , drop = FALSE]
      )
    }
  }

  return(data)
}

#' Calculate RMSSD with Artefact Correction
#'
#' A convenience function that combines `detect_hrv_artefacts` and
#' `correct_hrv_beats` to perform both artefact detection/classification and
#' correction in a single step, and then calculate the RMSSD.
#'
#' @param data A `dplyr::tibble` with a 'time' column (RR intervals in ms).
#' @param ...  Arguments to be passed to `detect_hrv_artefacts` (e.g.,
#'   `alpha`, `c1`, `c2`).
#'
#' @return A list with the following components
#'    * `rmssd_values`: The RMSSD value calculated from the *corrected* RR
#'      intervals.
#'    *  `classified_data`: The data with beat classification
#'    *  `corrected_data`: The data with beat correction
#'
#' @export
#' @importFrom dplyr %>%
calculate_hrv_rmssd <- function(data, ...) {
  # Detect and classify artefacts
  classified_data <- classify_hrv_artefacts_lipponen(data, ...)

  # Correct the artefacts
  corrected_data <- correct_hrv_artefacts_lipponen(classified_data)

  # Calculate RMSSD on the *corrected* and *normal* beats
  rmssd_values <- corrected_data %>%
    dplyr::filter(classification %in% c("normal", "missed")) %>% # Include "missed" (inserted)
    dplyr::summarise(rmssd = sqrt(mean(diff(time)^2))) %>%
    dplyr::pull(rmssd)

  return(list(
    rmssd_values = rmssd_values,
    classified_data = classified_data,
    corrected_data = corrected_data
  ))
}

#' Calculate RMSSD with Phase-Specific Artefact Correction (Orthostatic Test)
#'
#' @param data A `dplyr::tibble` with a 'time' column (RR intervals).  Assumes
#'   the first 3 minutes are lying and the next 3 minutes are standing.
#' @param secondary_threshold_percent_lying Numeric, percentage change
#'   threshold for secondary filtering during the lying phase (default: 20).
#' @param secondary_threshold_percent_standing Numeric, percentage change
#'   threshold for secondary filtering during the standing phase (default: 20).
#' @param transition_exclusion_time Numeric, duration (in seconds) to exclude
#'   around the transition (default: 45).  Increased default.
#' @param initial_stabilization_time Numeric, duration (in seconds) to exclude
#'   at the beginning of the lying phase (default: 60).
#' @param min_segment_length Numeric, minimum segment length in seconds
#'   (default: 30).
#' @param min_segment_beats Integer, minimum number of beats in a segment
#' (default: 30)
#' @param ... other parameters passed to `detect_hrv_artefacts`
#'
#' @return A list containing:
#'    * `rmssd_lying`: RMSSD for the lying phase (or NA).
#'    * `rmssd_standing`: RMSSD for the standing phase (or NA).
#'    * `rmssd_values`: A numeric vector of all segment RMSSD values.
#'    * `segment_lengths`: A numeric vector of segment lengths.
#'    * `segment_beat_counts`: An integer vector of segment beat counts.
#'    * `aggregated_rmssd`: Aggregated RMSSD (mean of a ll segments).
#'    * `n_segments`: Number of segments
#' @export
#' @importFrom dplyr %>% mutate lag filter lead case_when first
calculate_rmssd_orthostatic_enhanced <- function(
  data,
  secondary_threshold_percent_lying = 20,
  secondary_threshold_percent_standing = 20,
  transition_exclusion_time = 45,
  initial_stabilization_time = 60,
  min_segment_length = 30,
  min_segment_beats = 30,
  ...
) {
  # --- 1. Initial Artefact Detection (L&T) ---
  classified_data <- classify_hrv_artefacts_lipponen(data, ...)

  # --- 2. Define Phases and Exclusion Periods ---
  total_time <- sum(data$time)
  # Assume the first 3 mins are lying and the rest is standing, even if total time is more than 6 minutes
  lying_end_time <- min(180, total_time / 2)
  standing_start_time <- lying_end_time

  transition_start <- standing_start_time - transition_exclusion_time / 2
  transition_end <- standing_start_time + transition_exclusion_time / 2

  # --- 3. Refined Artefact Removal (Phase-Specific) ---
  classified_data <- classified_data %>%
    dplyr::mutate(
      cumulative_time = cumsum(time),
      phase = dplyr::case_when(
        cumulative_time <= lying_end_time ~ "lying",
        cumulative_time >= standing_start_time ~ "standing",
        TRUE ~ "transition"
      ),
      # Flag artefacts and neighbors
      artefact_or_neighbor = classification != "normal" |
        dplyr::lag(classification, default = "normal") != "normal" |
        dplyr::lead(classification, default = "normal") != "normal",
      # Calculate phase-specific percentage change
      rr_diff = abs(time - dplyr::lag(time, default = first(time))),
      percent_change = rr_diff / dplyr::lag(time, default = first(time))
    )

  # Apply phase-specific thresholds and exclusions
  filtered_data <- classified_data %>%
    dplyr::filter(
      !artefact_or_neighbor, # Remove artefacts and neighbors
      !(phase == "transition"), # Remove transition phase
      !(phase == "lying" & cumulative_time <= initial_stabilization_time), # Remove initial stabilization
      (phase == "lying" &
        (percent_change <= secondary_threshold_percent_lying / 100 |
          is.na(percent_change))) |
        (phase == "standing" &
          (percent_change <= secondary_threshold_percent_standing / 100 |
            is.na(percent_change)))
    )

  # --- 4. Segment-Based RMSSD Calculation (per phase) ---
  calculate_rmssd_segments <- function(rr_data, min_seg_length, min_seg_beats) {
    rr_clean <- rr_data
    n_clean <- length(rr_clean)

    if (n_clean == 0) {
      return(list(
        rmssd_values = NA,
        segment_lengths = NA,
        segment_beat_counts = NA,
        aggregated_rmssd = NA,
        n_segments = 0
      ))
    }

    # Identify segments (gaps between removed artefacts)
    segment_indices <- c(
      1,
      which(diff(rr_data) > (1.5 * median(rr_data))) + 1,
      n_clean + 1
    ) # Indices of segment starts.

    rmssd_values <- numeric(length(segment_indices) - 1)
    segment_lengths <- numeric(length(segment_indices) - 1)
    segment_beat_counts <- integer(length(segment_indices) - 1)

    valid_segments <- 0

    for (i in 1:(length(segment_indices) - 1)) {
      start_index <- segment_indices[i]
      end_index <- segment_indices[i + 1] - 1

      segment_rr <- rr_clean[start_index:end_index]
      segment_length <- sum(segment_rr)
      segment_beat_count <- length(segment_rr)

      # Check minimum length requirements
      length_ok <- segment_length >= min_seg_length
      beats_ok <- segment_beat_count >= min_seg_beats

      if (length_ok && beats_ok) {
        rmssd_values[i] <- sqrt(mean(diff(segment_rr)^2))
        segment_lengths[i] <- segment_length
        segment_beat_counts[i] <- segment_beat_count
        valid_segments <- valid_segments + 1
      } else {
        rmssd_values[i] <- NA
        segment_lengths[i] <- NA
        segment_beat_counts[i] <- NA
      }
    }
    rmssd_values <- rmssd_values[!is.na(rmssd_values)]
    segment_lengths <- segment_lengths[!is.na(segment_lengths)]
    segment_beat_counts <- segment_beat_counts[!is.na(segment_beat_counts)]

    if (valid_segments > 0) {
      aggregated_rmssd <- mean(rmssd_values)
    } else {
      aggregated_rmssd <- NA
    }
    return(list(
      rmssd_values = rmssd_values,
      segment_lengths = segment_lengths,
      segment_beat_counts = segment_beat_counts,
      aggregated_rmssd = aggregated_rmssd,
      n_segments = valid_segments
    ))
  }

  lying_rr <- filtered_data$time[filtered_data$phase == "lying"]
  standing_rr <- filtered_data$time[filtered_data$phase == "standing"]

  lying_results <- calculate_rmssd_segments(
    lying_rr,
    min_segment_length,
    min_segment_beats
  )
  standing_results <- calculate_rmssd_segments(
    standing_rr,
    min_segment_length,
    min_segment_beats
  )

  # --- 5. Combine Results ---
  all_rmssd_values <- c(
    lying_results$rmssd_values,
    standing_results$rmssd_values
  )
  all_segment_lengths <- c(
    lying_results$segment_lengths,
    standing_results$segment_lengths
  )
  all_segment_beat_counts <- c(
    lying_results$segment_beat_counts,
    standing_results$segment_beat_counts
  )

  if (length(all_rmssd_values) > 0) {
    aggregated_rmssd <- mean(all_rmssd_values)
  } else {
    aggregated_rmssd <- NA
  }

  return(list(
    rmssd_lying = ifelse(
      length(lying_results$rmssd_values) > 0,
      mean(lying_results$rmssd_values),
      NA
    ),
    rmssd_standing = ifelse(
      length(standing_results$rmssd_values) > 0,
      mean(standing_results$rmssd_values),
      NA
    ),
    rmssd_values = all_rmssd_values,
    segment_lengths = all_segment_lengths,
    segment_beat_counts = all_segment_beat_counts,
    aggregated_rmssd = aggregated_rmssd,
    n_segments = lying_results$n_segments + standing_results$n_segments
  ))
}
