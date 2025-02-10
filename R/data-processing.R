#' Validate FIT file object
#' @keywords internal
validate_fit_object <- function(fit_object) {
  if (!inherits(fit_object, "FitFile")) {
    stop("fit_object must be a FitFile object")
  }
}

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
    transition_buffer = 20) {
  # Input validation with descriptive errors
  validate_fit_object(fit_object)

  # Validate numeric parameters
  if (!is.numeric(filter_factor) || filter_factor <= 0 || filter_factor >= 1) {
    stop("filter_factor must be between 0 and 1")
  }
  if (!is.numeric(laying_time) || laying_time <= 30) {
    stop("laying_time must be at least 30 seconds")
  }
  if (!is.numeric(standing_time) || standing_time <= 30) {
    stop("standing_time must be at least 30 seconds")
  }
  if (!is.numeric(transition_buffer) || transition_buffer < 0) {
    stop("transition_buffer must be non-negative")
  }

  # Validate transition buffer relative to measurement times
  if (transition_buffer >= min(laying_time, standing_time) / 2) {
    warning("transition_buffer must be less than half of the shortest measurement phase")
  }

  # Validate reasonable ratio between laying and standing times
  time_ratio <- laying_time / standing_time
  if (time_ratio < 0.5 || time_ratio > 2) {
    warning("laying_time and standing_time should be within a factor of 2 of each other")
  }

  # Check for HRV data availability
  hrv_data_check <- FITfileR::hrv(fit_object)
  if (is.null(hrv_data_check) || nrow(hrv_data_check) == 0) {
    warning("No HRV data found in FIT file")
    return(list(laying = numeric(), standing = numeric(), quality_metrics = numeric()))
  }

  # Extract raw RR intervals
  hrv_data <- dplyr::filter(hrv_data_check, .data$time < 3) # remove the random 65.5 values
  if (nrow(hrv_data) == 0) {
    warning("Empty HRV data in FIT file")
    return(list(laying = numeric(), standing = numeric(), quality_metrics = numeric()))
  }

  # Check data density
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

  phase_factors <- list(
    laying = filter_factor * 0.7,
    transition = filter_factor * 2,
    standing = filter_factor * 1
  )

  factors <- case_when(
    cumsum(rr_intervals) <= laying_time ~ phase_factors$laying,
    cumsum(rr_intervals) <= laying_time + transition_buffer ~ phase_factors$transition,
    .default = phase_factors$standing
  )

  kept_rr <- numeric(length(rr_intervals))

  for (i in 2:length(rr_intervals)) {
    reference <- case_when(
      sum(rr_intervals[1:i]) < 180 ~ median(
        rr_intervals[1:which.max(cumsum(rr_intervals) > 180)]
      ),
      .default = median(
        rr_intervals[
          (which.max(cumsum(rr_intervals) > 180) + 1):length(rr_intervals)
        ]
      )
    )
    if (rr_intervals[i] > reference * 1 - factors[i] &&
      rr_intervals[i] < reference * 1 + factors[i]) {
      kept_rr[i] <- TRUE
    } else {
      kept_rr[i] <- FALSE
    }
  }

  # Calculate position indices with transition buffer
  total_time <- sum(rr_intervals)
  laying_end <- which.max(cumsum(rr_intervals) > 180)
  standing_start <- which.max(cumsum(rr_intervals) > 180 + transition_buffer)
  laying_intervals <- rr_intervals[
    1:laying_end
  ][
    as.logical(kept_rr)[
      1:laying_end
    ]
  ]
  standing_intervals <- rr_intervals[standing_start:length(rr_intervals)][
    as.logical(kept_rr)[standing_start:length(rr_intervals)]
  ]

  # Validate minimum data points per phase
  min_required_points <- 30 # At least 30 beats per phase
  if (length(laying_intervals) < min_required_points ||
    length(standing_intervals) < min_required_points) {
      warning("Insufficient number of valid intervals in one or both phases")
      return(list(laying = numeric(), standing = numeric()))
  }

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

#' @keywords internal

#' @importFrom FITfileR records
get_HR <- function(fit_object) {
  validate_fit_object(fit_object)

  HR <- FITfileR::records(fit_object)

  # Validate records exist
  if (is.null(HR) || (is.data.frame(HR) && nrow(HR) == 0) ||
    (is.list(HR) && length(HR) == 0)) {
    stop("No heart rate records found in FIT file")
  }

  if (inherits(HR, "list")) {
    # Find record with most data points
    max_rows_idx <- which.max(sapply(HR, nrow))
    HR <- HR[[max_rows_idx]]$heart_rate
  } else {
    HR <- HR$heart_rate
  }

  # Validate heart rate data
  if (is.null(HR) || length(HR) == 0) {
    stop("No heart rate data found in records")
  }

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

  # Limit to first 360 seconds
  HR <- HR[1:min(360, length(HR))]

  # Ensure minimum data points
  if (length(HR) < 30) { # At least 30 seconds of data
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

  date <- clock::as_date(session$timestamp)
  week <- as.numeric(strftime(session$timestamp, format = "%W"))

  # Validate date conversion
  if (is.na(date)) {
    stop("Failed to convert timestamp to date")
  }

  # Validate week number
  if (is.na(week) || week < 0 || week > 53) {
    stop("Invalid week number calculated from timestamp")
  }

  hour <- clock::get_hour(session$timestamp)
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
  if (!is.numeric(filter_factor) || length(filter_factor) != 1 ||
    filter_factor < 0.1 || filter_factor > 0.3) {
    stop("filter_factor must be a single numeric value between 0.1 and 0.3")
  }

  result <- tryCatch(
    {
      fit_object <- read_fit_file(file_path = file_path)
      hr_data <- get_HR(fit_object)

      # Calculate metrics
      resting_hr <- calculate_resting_hr(
        hr_data[30:180],
        method = "lowest_sustained"
      )

      hrr_metrics <- calculate_hrr(hr_data[181:240], resting_hr)
      rr_data <- extract_rr_data(fit_object, filter_factor)

      # Get session data
      session <- extract_session_data(fit_object = fit_object)

      # Process if we have enough data
      if (length(rr_data$laying) >= 2 && length(rr_data$standing) >= 2) {
        laying <- calculate_hrv(rr_data$laying)
        standing <- calculate_hrv(rr_data$standing)

        tibble::tibble(
          source_file = file_path,
          date = as.character(session$date),
          week = session$week,
          time_of_day = session$time_of_day,
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


#' Process directory of FIT files with caching
#'
#' Processes multiple FIT files from a specified directory, utilizing caching to
#' avoid reprocessing unchanged files. This function is designed to be efficient
#' by only processing new or updated files since the last run.
#'
#' @param dir_path The directory path containing FIT files to process
#' @param cache_file Path to the cache file for storing processed data
#' @param filter_factor Numeric value (0-1) used to filter RR intervals
#' @param clear_cache Logical indicating whether to clear existing cache
#'
#' @return A tibble containing HRV metrics for all processed FIT files
#' @export
process_fit_directory <- function(
    dir_path,
    cache_file = file.path(dir_path, "hrv_cache.csv"),
    filter_factor = 0.175,
    clear_cache = FALSE) {
  # Validate inputs
  validate_inputs(dir_path, cache_file, filter_factor, clear_cache)

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

  # Find files to process
  new_files <- setdiff(fit_files, cached_data$source_file)
  outdated_entries <- cached_data %>%
    dplyr::filter(
      package_version != utils::packageVersion("hrvester") |
        RR_filter != filter_factor
    ) %>%
    dplyr::pull(source_file)

  files_to_process <- unique(c(new_files, outdated_entries))

  if (length(files_to_process) > 0) {
    # Process files
    message(sprintf("Processing %d files...", length(files_to_process)))
    p <- progressr::progressor(along = files_to_process)

    new_data <- furrr::future_map_dfr(
      files_to_process,
      function(file_path) {
        result <- process_fit_file(file_path, filter_factor = filter_factor)
        p()
        return(result)
      },
      .options = furrr::furrr_options(seed = TRUE)
    )

    # Remove outdated entries and combine data
    cached_data <- cached_data %>%
      dplyr::filter(!source_file %in% outdated_entries)

    all_data <- dplyr::bind_rows(cached_data, new_data) %>%
      dplyr::arrange(date, desc(time_of_day))

    # Save updated cache atomically
    temp_file <- tempfile(fileext = ".csv")
    safe_file_operation(
      readr::write_csv2,
      all_data,
      temp_file
    )
    safe_file_operation(
      file.copy,
      temp_file,
      cache_file
    )

    return(all_data)
  } else {
    message("No new or outdated files to process")
    return(cached_data)
  }
}
