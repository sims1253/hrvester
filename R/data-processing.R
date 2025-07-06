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

#' Extract RR Interval Data from a FIT File Object
#'
#' Extracts RR interval data (HRV data) from a FIT file object.  This function
#' is designed for situations where you expect the FIT file to contain HRV data.
#' If no HRV data is found, it issues a warning and returns an empty numeric
#' vector.
#'
#' @param fit_object A FIT file object, typically created by
#'   \code{\link[FITfileR]{readFitFile}}.  The object should be the result of
#'   reading a FIT file that is expected to contain HRV (RR interval) data.
#'
#' @return A data frame containing the RR data, obtained directly from
#'  \code{\link[FITfileR]{hrv}}. If no HRV data is found, the function
#'  returns an empty numeric vector (\code{numeric(0)}).  Unlike a previous
#'  version, this version *does not* perform any filtering on the returned HRV
#'  data.
#'
#' @details
#' The function performs the following steps:
#' 1.  **Input Validation:** It validates the \code{fit_object} using a
#'     \code{\link{validate_fit_object}} function (assumed to be defined
#'     elsewhere in your package).
#' 2.  **HRV Data Extraction:** It attempts to extract HRV data using
#'     \code{\link[FITfileR]{hrv}}.
#' 3.  **HRV Data Check:** It checks if the extracted HRV data is \code{NULL} or
#'     empty (has zero rows).
#' 4.  **Return Value:**
#'     *   If HRV data is found and is not empty, the function returns the
#'         data frame returned by \code{\link[FITfileR]{hrv}}.
#'     *   If HRV data is \code{NULL} or empty, the function issues a warning
#'         message ("No HRV data found in FIT file") and returns an empty
#'         numeric vector (\code{numeric(0)}).
#'
#' @examples
#' \dontrun{
#' # Assuming you have a FIT file named "activity.fit" that *should*
#' # contain HRV data.
#' fit_file <- FITfileR::readFitFile("activity.fit")
#' rr_data <- extract_rr_data(fit_file)
#'
#' # If HRV data was present:
#' # rr_data will be a data.frame (the result of FITfileR::hrv()).
#'
#' # If no HRV data was found:
#' # rr_data will be numeric(0).
#' }
#'
#' @seealso \code{\link[FITfileR]{hrv}}, \code{\link[FITfileR]{readFitFile}}
#' @importFrom FITfileR hrv
#' @export
extract_rr_data <- function(
    fit_object) {
  # Input validation with descriptive errors
  validate_fit_object(fit_object)

  # Check for HRV data availability
  hrv_data <- FITfileR::hrv(fit_object)
  if (is.null(hrv_data) || nrow(hrv_data) == 0) {
    warning("No HRV data found in FIT file")
    return(tibble(time = numeric()))
  }

  return(hrv_data)
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
#' Calculates HRV metrics from a FIT file, integrating data extraction,
#' RR interval processing, and HRV calculation with error handling.
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
#' @param warmup Time in seconds from the start that should be discarded
#' @return Tibble containing HRV metrics
#' @param sport_name Name of the sport for the fit file. Used as a filter.
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
    sport_name = "OST") {
  fit_object <- read_fit_file(file_path = file_path)
  session <- extract_session_data(fit_object = fit_object)

  if (FITfileR::getMessagesByType(fit_object, "sport")$name != sport_name) {
    return(result <- create_empty_result(
      file_path = file_path,
      session_date = session$date,
      week = session$week,
      time_of_day = session$time_of_day
    ))
  }

  hr_data <- get_HR(fit_object)

  # Calculate metrics
  resting_hr <- calculate_resting_hr(
    hr_data[30:180],
    method = "lowest_sustained"
  )

  hrr_metrics <- calculate_hrr(hr_data[181:240], resting_hr)

  rr_intervals <- extract_rr_data(fit_object)

  rr_intervals$time <- rr_intervals$time * 1000

  rr_intervals <- split_rr_phases(
    rr_intervals,
    session,
    laying_time = laying_time,
    transition_time = transition_time,
    standing_time = standing_time,
    centered_transition = centered_transition
  )

  laying_data <- rr_full_phase_processing(
    rr_segment = dplyr::filter(rr_intervals, phase == "laying")$time,
    min_rr = min_rr,
    max_rr = max_rr,
    window_size = 5,
    threshold = 0.2,
    centered_window = FALSE
  )

  laying_hrv <- calculate_hrv(corrected_data$time)
  laying_hrv

  # Debugging leftover
  rr_intervals %>%
    filter(phase == "laying") %>%
    ggplot(aes(x = elapsed_time, y = time, color = is_valid)) +
    geom_point() +
    ggtitle(calculate_hrv(laying_data$cleaned_rr)$rmssd) +
    coord_cartesian(ylim = c(0, 1500))

  transition_data <- rr_full_phase_processing(
    rr_segment = dplyr::filter(rr_intervals, phase == "transition")$time,
    is_valid = dplyr::filter(rr_intervals, phase == "transition")$is_valid,
    min_rr = min_rr,
    max_rr = max_rr,
    window_size = window_size,
    threshold = 0.17,
    centered_window = centered_window
  )
  rr_intervals$is_valid[
    min(which(rr_intervals$phase == "transition")):max(which(rr_intervals$phase == "transition"))
  ] <- transition_data$is_valid

  transitioning_hrv <- calculate_hrv(transition_data$cleaned_rr)

  standing_data <- rr_full_phase_processing(
    rr_segment = dplyr::filter(rr_intervals, phase == "standing")$time,
    is_valid = dplyr::filter(rr_intervals, phase == "standing")$is_valid,
    min_rr = min_rr,
    max_rr = max_rr,
    window_size = window_size,
    threshold = 0.2,
    centered_window = centered_window
  )
  rr_intervals$is_valid[
    min(which(rr_intervals$phase == "standing")):max(which(rr_intervals$phase == "standing"))
  ] <- standing_data$is_valid
  transitioning_hrv <- calculate_hrv(standing_data$cleaned_rr)

  standing_hrv <- calculate_hrv(standing_data$cleaned_rr)

  # Process if we have enough data
  if (length(laying_data$is_valid) >= 2 && length(standing_data$is_valid) >= 2) {
    result <- tibble::tibble(
      source_file = file_path,
      date = as.character(session$date),
      week = session$week,
      time_of_day = session$time_of_day,
      laying_rmssd = laying_hrv$rmssd,
      laying_sdnn = laying_hrv$sdnn,
      laying_hr = round(mean(hr_data[30:150], na.rm = TRUE), 2),
      laying_resting_hr = resting_hr,
      standing_rmssd = standing_hrv$rmssd,
      standing_sdnn = standing_hrv$sdnn,
      standing_hr = round(mean(hr_data[220:330], na.rm = TRUE), 2),
      standing_max_hr = max(hr_data[181:220], na.rm = TRUE),
      hrr_60s = hrr_metrics$hrr_60s,
      hrr_relative = hrr_metrics$hrr_relative,
      orthostatic_rise = hrr_metrics$orthostatic_rise,
      package_version = as.character(utils::packageVersion("hrvester")),
      activity = FITfileR::getMessagesByType(fit_object, "sport")$name
    )
  } else {
    result <- create_empty_result(
      file_path = file_path,
      session_date = session$date,
      week = session$week,
      time_of_day = session$time_of_day
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
#' @return Tibble with NA values
#' @keywords internal
create_empty_result <- function(file_path, session_date, week, time_of_day) {
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
    activity = NA_character_
  )
}


#' Process directory of FIT files with caching
#'
#' Processes multiple FIT files from a specified directory, utilizing caching to
#' avoid reprocessing unchanged files. This function efficiently processes new or updated files in a directory, leveraging a cache to skip already processed files.
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
#' @param clear_cache Logical indicating whether to clear existing cache. Default is FALSE.
#'
#' @return A tibble containing HRV metrics for all processed FIT files
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
    sport_name = "OST") {
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

  # Find files to process
  new_files <- setdiff(fit_files, cached_data$source_file)
  outdated_entries <- cached_data %>%
    dplyr::filter(
      package_version != utils::packageVersion("hrvester")
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
          sport_name = sport_name
        )
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
    safe_file_operation(
      readr::write_csv,
      x = all_data,
      file = cache_file
    )

    return(all_data)
  } else {
    message("No new or outdated files to process")
    return(cached_data)
  }
}
