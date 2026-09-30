#' Validate FIT Object
#'
#' This is a placeholder for the actual validation function.  In a real
#' package, this function would contain checks to ensure that \code{fit_object}
#' is a valid object of the expected type (likely a list or environment
#' resulting from \code{\link[FITfileR]{readFitFile}}).
#'
#' @param fit_object The object to validate.
#' @keywords internal
validate_fit_object <- function(fit_object) {
  if (!inherits(fit_object, "FitFile")) {
    stop("fit_object must be a FitFile object")
  }
}


#' Extract HR records from FIT object
#'
#' @description
#' Extracts heart rate records with timestamps, handling both list and data
#' frame formats. Timestamps enable elapsed-time (protocol) windowing rather
#' than row-number slicing.
#'
#' @param fit_object An object of class FitFile
#' @return A data frame with `timestamp` and `heart_rate` columns. If the
#'   record carries no timestamps, a one-sample-per-second sequence is used.
#' @keywords internal
#' @importFrom FITfileR records
get_hr_records <- function(fit_object) {
  validate_fit_object(fit_object)

  records <- FITfileR::records(fit_object)

  # Validate records exist
  if (
    is.null(records) ||
      (is.data.frame(records) && nrow(records) == 0) ||
      (is.list(records) && length(records) == 0)
  ) {
    stop("No heart rate records found in FIT file")
  }

  if (inherits(records, "list")) {
    # Find record with most data points
    max_rows_idx <- which.max(vapply(records, nrow, integer(1)))
    records <- records[[max_rows_idx]]
  }

  # Validate heart rate data
  if (is.null(records$heart_rate) || length(records$heart_rate) == 0) {
    stop("No heart rate data found in records")
  }

  if (is.null(records$timestamp)) {
    # Fall back to a one-sample-per-second timeline
    records$timestamp <- seq_along(records$heart_rate)
  }

  data.frame(
    timestamp = records$timestamp,
    heart_rate = as.numeric(records$heart_rate)
  )
}

#' Extract HR data from FIT object
#'
#' @description
#' Extracts heart rate data handling both list and data frame formats
#'
#' @param fit_object An object of class FitFile
#' @return Numeric vector of heart rate values
#' @keywords internal
#' @importFrom FITfileR records
get_HR <- function(fit_object) {
  hr_records <- get_hr_records(fit_object)
  HR <- hr_records$heart_rate

  # Check for missing/NA values
  na_count <- sum(is.na(HR))
  if (na_count > 0) {
    warning(sprintf("Found %d missing heart rate values", na_count))
  }

  # Validate physiological ranges (30-220 bpm)
  invalid_hr <- HR < 30 | HR > 220
  if (any(invalid_hr, na.rm = TRUE)) {
    warning(sprintf(
      "Found %d physiologically improbable heart rate values (< 30 or > 220 bpm)",
      sum(invalid_hr, na.rm = TRUE)
    ))
  }

  # Ensure minimum data points
  if (length(HR) < 30) {
    # At least 30 seconds of data
    warning("Insufficient heart rate data points")
  }

  return(HR)
}

#' Safely read a FIT file
#'
#' Reads a FIT file with proper error handling and validation. This function ensures
#' consistent error handling across the package when reading FIT files.
#'
#' @param file_path Character string specifying the path to the FIT file
#'
#' @return An object of class FitFile
#'
#' @examples
#' \dontrun{
#' fit_object <- read_fit_file("path/to/file.fit")
#' }
#'
#' @keywords internal
read_fit_file <- function(file_path) {
  # Enhanced input validation
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("file_path must be a single character string")
  }

  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  # Validate file extension
  if (!grepl("\\.fit$", file_path, ignore.case = TRUE)) {
    stop("File must have .fit extension: ", file_path)
  }

  # Check file size
  if (file.size(file_path) == 0) {
    stop("File is empty: ", file_path)
  }

  # Check file permissions
  if (!file.access(file_path, mode = 4) == 0) {
    stop("File is not readable: ", file_path)
  }

  tryCatch(
    FITfileR::readFitFile(file_path),
    error = function(e) {
      stop(sprintf("Error reading FIT file: %s", conditionMessage(e)))
    }
  )
}

#' Extract session metadata from FIT object
#'
#' Extracts and processes session-related metadata from a FIT file object,
#' including date, week number, and time of day classification.
#'
#' @param fit_object An object of class FitFile
#'
#' @return A list containing:
#'   \itemize{
#'     \item date (Date): The session date
#'     \item week (numeric): Week number of the year
#'     \item time_of_day (character): Classification as either "Morning" (4-13h)
#'           or "Evening"
#'   }
#'
#' @details
#' Time of day is classified as "Morning" for measurements taken between
#' 04:00 and 13:00, and "Evening" otherwise. This classification is particularly
#' relevant for HRV measurements which can vary significantly between morning
#' and evening readings.
#'
#' @keywords internal
extract_session_data <- function(fit_object) {
  # Input validation
  validate_fit_object(fit_object)

  # Get session data with validation
  session <- FITfileR::getMessagesByType(fit_object, "session")
  if (is.null(session) || length(session) == 0) {
    stop("No session data found in FIT file")
  }

  # Validate timestamp presence
  if (is.null(session$timestamp)) {
    stop("No timestamp found in session data")
  }

  # Validate timestamp format
  if (!inherits(session$timestamp, c("POSIXct", "POSIXt"))) {
    stop("Invalid timestamp format in session data")
  }

  date <- as.Date(session$timestamp)
  week <- as.numeric(strftime(session$timestamp, format = "%W"))

  # Validate date conversion
  if (is.na(date)) {
    stop("Failed to convert timestamp to date")
  }

  # Validate week number
  if (is.na(week) || week < 0 || week > 53) {
    stop("Invalid week number calculated from timestamp")
  }

  hour <- as.numeric(strftime(session$timestamp, format = "%H"))
  if (is.na(hour)) {
    stop("Failed to extract hour from timestamp")
  }

  time_of_day <- if (hour >= 4 && hour < 13) "Morning" else "Evening"

  list(
    date = date,
    week = week,
    time_of_day = time_of_day,
    duration = session$total_elapsed_time
  )
}

#' Names of the analysis arguments defining cache identity
#'
#' Internal helper listing the analysis arguments that make up the cache
#' configuration identity. Both [process_fit_file()] and
#' [process_fit_directory()] build their config digest from exactly these
#' names, pinned in one place so the two cannot drift apart.
#'
#' @return Character vector of argument names
#' @keywords internal
hrv_analysis_config_args <- function() {
  c(
    "standing_time",
    "transition_time",
    "laying_time",
    "min_rr",
    "max_rr",
    "window_size",
    "threshold",
    "centered_transition",
    "centered_window",
    "warmup",
    "sport_name",
    "min_quality_threshold",
    "correction_method"
  )
}

#' Compute the analysis configuration identity from a function frame
#'
#' Internal helper extracting the analysis arguments listed by
#' [hrv_analysis_config_args()] from the calling frame and hashing them
#' together with the package version.
#'
#' @param env Environment (usually `environment()` of the caller)
#' @return Character string with the configuration digest
#' @keywords internal
compute_analysis_config_id <- function(env) {
  config_args <- as.list(env)[hrv_analysis_config_args()]
  config_args$package_version <- as.character(
    utils::packageVersion("hrvester")
  )
  compute_config_id(config_args)
}

#' Process a single FIT file
#'
#' @description
#' Calculates HRV metrics from a FIT file, integrating data extraction,
#' RR interval processing, and HRV calculation with error handling.
#'
#' One elapsed-time protocol (in seconds) drives both HR and RR analyses:
#' the first `warmup` seconds are discarded; the laying phase ends at
#' `laying_time`; the transition band is centred on `laying_time` when
#' `centered_transition = TRUE` and the standing phase starts at
#' `laying_time + transition_time / 2` (or `laying_time + transition_time`
#' otherwise). HR windows are selected by record timestamp, so recordings
#' that are not sampled at exactly 1 Hz are handled correctly.
#'
#' Phase quality assessment uses the raw (pre-correction) artifact burden so
#' that correction cannot make a poor recording appear clean.
#'
#' @param file_path Path to FIT file
#' @param standing_time Time in seconds to consider as standing
#' @param transition_time Time in seconds to consider as transition
#' @param laying_time Time in seconds to consider as laying
#' @param min_rr Minimum RR interval in milliseconds
#' @param max_rr Maximum RR interval in milliseconds
#' @param window_size Window size for moving average calculation
#' @param threshold Threshold for artifact detection
#' @param centered_window Logical indicating whether the moving window should
#'   be centered. Defaults to FALSE.
#' @param centered_transition Logical indicating whether the transition time
#'   should be split into laying and standing times. FALSE, if transition time
#'   is only taken from the laying phase.
#' @param warmup Time in seconds from the start that should be discarded from
#'   both HR and RR analyses
#' @return Tibble containing HRV metrics
#' @param sport_name Name of the sport for the fit file. Used as a filter.
#' @param min_quality_threshold Minimum quality threshold (0-1) for data to be processed.
#'   Data below this quality threshold will be discarded. Default is 0.0 (no filtering).
#' @param correction_method Character string specifying the artifact correction
#'   method. Options are: "linear", "cubic", "lipponen", "none". Default is "linear".
#' @param config_id Cache provenance. Digest of the effective analysis
#'   configuration, computed from the other arguments when NULL (the default).
#'   Passed by [process_fit_directory()] so cached entries can be invalidated
#'   when the analysis configuration changes.
#' @export
process_fit_file <- function(
  file_path,
  standing_time = 180,
  transition_time = 20,
  laying_time = 180,
  min_rr = 272,
  max_rr = 2000,
  window_size = 7,
  threshold = 0.2,
  centered_transition = TRUE,
  centered_window = FALSE,
  warmup = 70,
  sport_name = "OST",
  min_quality_threshold = 0.0,
  correction_method = "linear",
  config_id = NULL
) {
  # Validate min_quality_threshold parameter
  if (
    !is.numeric(min_quality_threshold) || length(min_quality_threshold) != 1
  ) {
    stop("min_quality_threshold must be numeric")
  }

  if (min_quality_threshold < 0 || min_quality_threshold > 1) {
    stop("min_quality_threshold must be between 0 and 1")
  }

  # Validate correction_method parameter
  valid_methods <- c("linear", "cubic", "lipponen", "none")
  if (!is.character(correction_method) || length(correction_method) != 1) {
    stop("correction_method must be a single character string")
  }
  if (!correction_method %in% valid_methods) {
    stop(
      "correction_method must be one of: ",
      paste(valid_methods, collapse = ", ")
    )
  }

  # Provenance recorded with the result so caches can detect configuration
  # and content changes
  file_digest <- unname(tools::md5sum(file_path))
  if (is.null(config_id)) {
    config_id <- compute_analysis_config_id(environment())
  }

  fit_object <- read_fit_file(file_path = file_path)
  session <- extract_session_data(fit_object = fit_object)

  # Check if sport message type exists and filter by sport name
  sport_messages <- tryCatch(
    {
      FITfileR::getMessagesByType(fit_object, "sport")
    },
    error = function(e) {
      # If sport message type doesn't exist, assume it's valid (skip sport filtering)
      data.frame(name = sport_name)
    }
  )

  if (
    !is.null(sport_messages) &&
      nrow(sport_messages) > 0 &&
      sport_messages$name[1] != sport_name
  ) {
    return(
      result <- create_empty_result(
        file_path = file_path,
        session_date = session$date,
        week = session$week,
        time_of_day = session$time_of_day,
        file_digest = file_digest,
        config_id = config_id
      )
    )
  }

  hr_records <- get_hr_records(fit_object)
  hr_elapsed <- as.numeric(
    hr_records$timestamp - hr_records$timestamp[1],
    units = "secs"
  )

  # One protocol, in elapsed seconds, for both HR and RR analyses
  standing_start <- if (centered_transition) {
    laying_time + transition_time / 2
  } else {
    laying_time + transition_time
  }

  hr_in_window <- function(from, to) {
    idx <- hr_elapsed >= from & hr_elapsed < to
    hr_records$heart_rate[idx]
  }

  hr_window_mean <- function(from, to) {
    values <- hr_in_window(from, to)
    if (length(values[!is.na(values)]) == 0) {
      return(NA_real_)
    }
    round(mean(values, na.rm = TRUE), 2)
  }

  # Calculate metrics from timestamp-selected HR windows
  resting_hr_values <- hr_in_window(warmup, laying_time)
  resting_hr <- calculate_resting_hr(
    resting_hr_values,
    method = "lowest_sustained"
  )

  hrr_idx <- hr_elapsed >= standing_start &
    hr_elapsed < standing_start + 60
  hrr_metrics <- calculate_hrr(
    hr_records$heart_rate[hrr_idx],
    resting_hr,
    times = hr_elapsed[hrr_idx] - standing_start
  )

  # Extract RR data with quality metrics using specified correction method
  rr_intervals <- extract_rr_data(
    fit_object,
    correction_method = correction_method
  )

  # Capture raw-series provenance before phase splitting (dplyr may drop
  # custom attributes)
  raw_rr <- attr(rr_intervals, "raw_rr")
  raw_artifact_indices <- attr(rr_intervals, "raw_artifact_indices")
  threshold_used <- attr(rr_intervals, "threshold_used")
  if (is.null(raw_rr)) {
    raw_rr <- rr_intervals$time
    raw_artifact_indices <- integer(0)
  }

  rr_intervals <- split_rr_phases(
    rr_intervals,
    session,
    laying_time = laying_time,
    transition_time = transition_time,
    standing_time = standing_time,
    centered_transition = centered_transition
  )

  # Discard the warmup period from the laying phase
  rr_intervals <- rr_intervals %>%
    dplyr::filter(!(phase == "laying" & .data$elapsed_time <= warmup))

  laying_data <- rr_full_phase_processing(
    rr_segment = dplyr::filter(rr_intervals, phase == "laying")$time,
    min_rr = min_rr,
    max_rr = max_rr,
    window_size = window_size,
    threshold = threshold,
    centered_window = centered_window
  )

  laying_hrv <- calculate_hrv(laying_data$cleaned_rr)

  transition_data <- rr_full_phase_processing(
    rr_segment = dplyr::filter(rr_intervals, phase == "transition")$time,
    min_rr = min_rr,
    max_rr = max_rr,
    window_size = window_size,
    threshold = threshold,
    centered_window = centered_window
  )

  transitioning_hrv <- calculate_hrv(transition_data$cleaned_rr)

  standing_data <- rr_full_phase_processing(
    rr_segment = dplyr::filter(rr_intervals, phase == "standing")$time,
    min_rr = min_rr,
    max_rr = max_rr,
    window_size = window_size,
    threshold = threshold,
    centered_window = centered_window
  )

  standing_hrv <- calculate_hrv(standing_data$cleaned_rr)

  # Phase quality is assessed on the raw (pre-correction) beat series using
  # the artifact indices detected before correction, so interpolation cannot
  # hide the original artifact burden
  raw_elapsed <- cumsum(raw_rr) / 1000
  raw_phase <- if (centered_transition) {
    dplyr::case_when(
      raw_elapsed <= laying_time - transition_time / 2 ~ "laying",
      raw_elapsed <= laying_time + transition_time / 2 ~ "transition",
      .default = "standing"
    )
  } else {
    dplyr::case_when(
      raw_elapsed <= laying_time ~ "laying",
      raw_elapsed <= laying_time + transition_time ~ "transition",
      .default = "standing"
    )
  }

  phase_quality <- function(phase_name) {
    phase_mask <- raw_phase == phase_name
    if (phase_name == "laying") {
      # The warmup period is excluded from processing and from quality
      phase_mask <- phase_mask & raw_elapsed > warmup
    }
    phase_indices <- which(phase_mask)
    artifact_positions <- match(
      raw_artifact_indices[raw_artifact_indices %in% phase_indices],
      phase_indices
    )
    calculate_rr_quality(
      rr_intervals = raw_rr[phase_mask],
      artifacts_detected = artifact_positions,
      correction_metadata = list(
        threshold_used = threshold_used,
        method = correction_method
      )
    )
  }

  laying_quality <- phase_quality("laying")
  standing_quality <- phase_quality("standing")

  # Check quality thresholds - convert signal quality index to 0-1 scale for
  # comparison. NA (unassessable) never passes the gate.
  laying_quality_score <- laying_quality$signal_quality_index / 100
  standing_quality_score <- standing_quality$signal_quality_index / 100

  quality_ok <- is.finite(laying_quality_score) &&
    is.finite(standing_quality_score) &&
    laying_quality_score >= min_quality_threshold &&
    standing_quality_score >= min_quality_threshold

  # Apply quality filtering - both phases must meet minimum threshold
  if (!quality_ok) {
    message(sprintf(
      "File %s discarded due to low quality: laying=%.2f, standing=%.2f (threshold=%.2f)",
      basename(file_path),
      laying_quality_score,
      standing_quality_score,
      min_quality_threshold
    ))
    result <- create_empty_result(
      file_path = file_path,
      session_date = session$date,
      week = session$week,
      time_of_day = session$time_of_day,
      file_digest = file_digest,
      config_id = config_id
    )
  } else if (
    length(laying_data$is_valid) >= 2 && length(standing_data$is_valid) >= 2
  ) {
    # Process if we have enough data and quality is acceptable
    standing_max_hr <- hr_in_window(standing_start, standing_start + 40)
    result <- tibble::tibble(
      source_file = file_path,
      date = as.character(session$date),
      week = session$week,
      time_of_day = session$time_of_day,
      laying_rmssd = laying_hrv$rmssd,
      laying_sdnn = laying_hrv$sdnn,
      laying_hr = hr_window_mean(warmup, laying_time),
      laying_resting_hr = resting_hr,
      standing_rmssd = standing_hrv$rmssd,
      standing_sdnn = standing_hrv$sdnn,
      standing_hr = hr_window_mean(standing_start + 40, standing_start + 160),
      standing_max_hr = if (length(standing_max_hr[!is.na(standing_max_hr)]) >
        0) {
        max(standing_max_hr, na.rm = TRUE)
      } else {
        NA_real_
      },
      hrr_60s = hrr_metrics$hrr_60s,
      hrr_relative = hrr_metrics$hrr_relative,
      orthostatic_rise = hrr_metrics$orthostatic_rise,
      package_version = as.character(utils::packageVersion("hrvester")),
      activity = FITfileR::getMessagesByType(fit_object, "sport")$name,
      # Quality metrics for laying phase
      laying_artifact_percentage = laying_quality$artifact_percentage,
      laying_signal_quality_index = laying_quality$signal_quality_index,
      laying_data_completeness = laying_quality$data_completeness,
      laying_quality_grade = laying_quality$quality_grade,
      # Quality metrics for standing phase
      standing_artifact_percentage = standing_quality$artifact_percentage,
      standing_signal_quality_index = standing_quality$signal_quality_index,
      standing_data_completeness = standing_quality$data_completeness,
      standing_quality_grade = standing_quality$quality_grade,
      # Provenance used for cache invalidation
      file_digest = file_digest,
      config_id = config_id
    )
  } else {
    result <- create_empty_result(
      file_path = file_path,
      session_date = session$date,
      week = session$week,
      time_of_day = session$time_of_day,
      file_digest = file_digest,
      config_id = config_id
    )
  }
  return(result)
}

#' Create empty result row
#'
#' @description
#' Helper function to create empty result row with NA values
#'
#' @param file_path Source file path
#' @param session_date Date of measurement
#' @param week Week number
#' @param time_of_day Time of day
#' @param file_digest MD5 digest of the source file contents
#' @param config_id Analysis configuration identity
#' @return Tibble with NA values
#' @keywords internal
create_empty_result <- function(
  file_path,
  session_date,
  week,
  time_of_day,
  file_digest = NA_character_,
  config_id = NA_character_
) {
  tibble::tibble(
    source_file = file_path,
    date = as.character(session_date),
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
    activity = NA_character_,
    # Quality metrics for laying phase
    laying_artifact_percentage = NA_real_,
    laying_signal_quality_index = NA_real_,
    laying_data_completeness = NA_real_,
    laying_quality_grade = NA_character_,
    # Quality metrics for standing phase
    standing_artifact_percentage = NA_real_,
    standing_signal_quality_index = NA_real_,
    standing_data_completeness = NA_real_,
    standing_quality_grade = NA_character_,
    # Provenance used for cache invalidation
    file_digest = file_digest,
    config_id = config_id
  )
}


#' Process directory of FIT files with caching
#'
#' Processes multiple FIT files from a specified directory, utilizing caching to
#' avoid reprocessing unchanged files. This function efficiently processes new or updated files in a directory, leveraging a cache to skip already processed files.
#'
#' A cached entry is reused only when the package version, the effective
#' analysis configuration (protocol windows, filtering thresholds, correction
#' method, quality threshold, sport filter) and the source file contents all
#' match the current run. Overwritten files and configuration changes are
#' therefore reprocessed automatically; entries lacking provenance
#' information (legacy caches) are reprocessed as well.
#'
#' @param dir_path The directory path containing FIT files to process.
#' @param cache_file Path to the cache file for storing processed data. Defaults to "hrv_cache.csv" within the directory.
#' @param standing_time Time in seconds to consider as standing. Default is 180 seconds.
#' @param transition_time Time in seconds to consider as transition. Default is 20 seconds.
#' @param laying_time Time in seconds to consider as laying. Default is 180 seconds.
#' @param min_rr Minimum RR interval in milliseconds. Default is 272 ms.
#' @param max_rr Maximum RR interval in milliseconds. Default is 2000 ms.
#' @param window_size Window size for moving average calculation. Default is 7.
#' @param threshold Threshold for artifact detection. Default is 0.2.
#' @param centered_transition Logical indicating whether to center transition phases. Default is TRUE.
#' @param centered_window Logical indicating whether to use centered window for processing. Default is FALSE.
#' @param warmup Warmup time in seconds to exclude from beginning. Default is 70.
#' @param clear_cache Logical indicating whether to clear existing cache. Default is FALSE.
#' @param sport_name Character string specifying the sport name filter. Default is "OST".
#' @param min_quality_threshold Minimum quality threshold (0-1) for data to be processed.
#'   Data below this quality threshold will be discarded. Default is 0.0 (no filtering).
#' @param correction_method Character string specifying the artifact correction
#'   method. Options are: "linear", "cubic", "lipponen", "none". Default is "linear".
#'
#' @return A tibble containing HRV metrics for all processed FIT files
#' @importFrom dplyr desc
#' @export
process_fit_directory <- function(
  dir_path,
  cache_file = file.path(dir_path, "hrv_cache.csv"),
  standing_time = 180,
  transition_time = 20,
  laying_time = 180,
  min_rr = 272,
  max_rr = 2000,
  window_size = 7,
  threshold = 0.2,
  centered_transition = TRUE,
  centered_window = FALSE,
  warmup = 70,
  clear_cache = FALSE,
  sport_name = "OST",
  min_quality_threshold = 0.0,
  correction_method = "linear"
) {
  # Validate min_quality_threshold parameter
  if (
    !is.numeric(min_quality_threshold) || length(min_quality_threshold) != 1
  ) {
    stop("min_quality_threshold must be numeric")
  }

  if (min_quality_threshold < 0 || min_quality_threshold > 1) {
    stop("min_quality_threshold must be between 0 and 1")
  }

  # Validate correction_method parameter
  valid_methods <- c("linear", "cubic", "lipponen", "none")
  if (!is.character(correction_method) || length(correction_method) != 1) {
    stop("correction_method must be a single character string")
  }
  if (!correction_method %in% valid_methods) {
    stop(
      "correction_method must be one of: ",
      paste(valid_methods, collapse = ", ")
    )
  }

  # Validate inputs
  validate_inputs(dir_path, cache_file, clear_cache)

  # Get list of FIT files
  fit_files <- list.files(dir_path, pattern = "\\.fit$", full.names = TRUE)
  if (length(fit_files) == 0) {
    warning("No FIT files found in directory")
    return(cache_definition())
  }

  # Load or initialize cache
  cached_data <- if (file.exists(cache_file) && !clear_cache) {
    load_cache(cache_file = cache_file)
  } else {
    cache_definition()
  }

  # Identity of the effective analysis configuration: cache entries are only
  # reusable when the configuration, package version and file contents match
  config_id <- compute_analysis_config_id(environment())

  # Find files to process
  new_files <- setdiff(fit_files, cached_data$source_file)

  # An entry is outdated when its configuration identity is missing (legacy
  # caches) or different, or when the source file contents changed. Entries
  # whose files no longer exist are kept unchanged.
  entry_outdated <- function(entry) {
    if (is.na(entry$config_id) || entry$config_id != config_id) {
      return(TRUE)
    }
    if (!file.exists(entry$source_file)) {
      return(FALSE)
    }
    !identical(unname(tools::md5sum(entry$source_file)), entry$file_digest)
  }

  outdated_entries <- character(0)
  if (nrow(cached_data) > 0) {
    outdated <- vapply(
      seq_len(nrow(cached_data)),
      function(i) entry_outdated(cached_data[i, ]),
      logical(1)
    )
    # Only reprocess entries whose files are still present
    outdated_entries <- cached_data$source_file[
      outdated & cached_data$source_file %in% fit_files
    ]
  }

  files_to_process <- unique(c(new_files, outdated_entries))

  if (length(files_to_process) > 0) {
    # Process files
    message(sprintf("Processing %d files...", length(files_to_process)))

    new_data <- furrr::future_map_dfr(
      files_to_process,
      function(file_path) {
        result <- process_fit_file(
          file_path,
          standing_time = standing_time,
          transition_time = transition_time,
          laying_time = laying_time,
          min_rr = min_rr,
          max_rr = max_rr,
          window_size = window_size,
          threshold = threshold,
          centered_transition = centered_transition,
          centered_window = centered_window,
          warmup = warmup,
          sport_name = sport_name,
          min_quality_threshold = min_quality_threshold,
          correction_method = correction_method,
          config_id = config_id
        )
        return(result)
      },
      .options = furrr::furrr_options(seed = TRUE)
    )

    # Remove outdated entries and combine data
    cached_data <- cached_data %>%
      dplyr::filter(!source_file %in% outdated_entries)

    all_data <- dplyr::bind_rows(cached_data, new_data) %>%
      dplyr::arrange(date, desc(time_of_day))

    # Save updated cache atomically: a failed write leaves the previous
    # cache file untouched
    safe_file_operation(
      save_cache_atomic,
      data = all_data,
      cache_file = cache_file
    )

    return(all_data)
  } else {
    message("No new or outdated files to process")
    return(cached_data)
  }
}
