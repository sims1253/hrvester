#' Cache Definition
#'
#' Defines the structure of the cache data. This function creates a tibble
#' containing all necessary columns for storing HRV metrics.
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item source_file (character): Path to the source FIT file
#'     \item date (character): Date of measurement
#'     \item week (numeric): Week number
#'     \item time_of_day (character): Time of day ("Morning" or "Evening")
#'     \item laying_rmssd (numeric): RMSSD during laying position
#'     \item laying_sdnn (numeric): SDNN during laying position
#'     \item laying_hr (numeric): Mean heart rate during laying position
#'     \item laying_resting_hr (numeric): Resting heart rate
#'     \item standing_rmssd (numeric): RMSSD during standing position
#'     \item standing_sdnn (numeric): SDNN during standing position
#'     \item standing_hr (numeric): Mean heart rate during standing position
#'     \item standing_max_hr (numeric): Maximum heart rate during standing
#'     \item package_version (character): Version of the package
#'     \item activity (character): Type of activity
#'   }
#' @export
cache_definition <- function() {
  tibble::tibble(
    source_file = character(),
    date = character(),
    week = numeric(),
    time_of_day = character(),
    laying_rmssd = numeric(),
    laying_sdnn = numeric(),
    laying_hr = numeric(),
    laying_resting_hr = numeric(),
    standing_rmssd = numeric(),
    standing_sdnn = numeric(),
    standing_hr = numeric(),
    standing_max_hr = numeric(),
    package_version = character(),
    activity = character(),
    hrr_60s = numeric(),
    hrr_relative = numeric(),
    orthostatic_rise = numeric()
  )
}

#' Validate cache structure
#'
#' Internal function to validate the structure of loaded cache data
#'
#' @param cache_data Dataframe to validate
#' @return TRUE if valid, throws error if invalid
#' @keywords internal
validate_cache_structure <- function(cache_data) {
  expected_cols <- colnames(cache_definition())

  if (!all(expected_cols %in% colnames(cache_data))) {
    missing_cols <- setdiff(expected_cols, colnames(cache_data))
    stop(sprintf(
      "Invalid cache structure. Missing columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Validate column types more robustly
  template <- cache_definition()
  for (col in names(template)) {
    expected_type <- class(template[[col]])
    actual_type <- class(cache_data[[col]])

    # For character columns, ensure they are character type
    if (expected_type == "character" && !is.character(cache_data[[col]])) {
      stop(sprintf(
        "Invalid column type for %s: expected character, got %s",
        col, actual_type[1]
      ))
    }

    # For numeric columns, ensure they are numeric
    if (expected_type == "numeric" && !is.numeric(cache_data[[col]])) {
      stop(sprintf(
        "Invalid column type for %s: expected numeric, got %s",
        col, actual_type[1]
      ))
    }
  }

  return(TRUE)
}

#' Validate input parameters
#'
#' Internal function to validate input parameters for process_fit_directory
#'
#' @param dir_path Directory path
#' @param cache_file Cache file path
#' @param clear_cache Clear cache flag
#' @return TRUE if valid, throws error if invalid
#' @keywords internal
validate_inputs <- function(dir_path, cache_file, clear_cache) {
  if (!dir.exists(dir_path)) {
    stop(sprintf("Directory '%s' does not exist", dir_path))
  }

  if (!is.character(cache_file) || length(cache_file) != 1) {
    stop("cache_file must be a single character string")
  }

  if (!is.logical(clear_cache) || length(clear_cache) != 1) {
    stop("clear_cache must be a single logical value (TRUE/FALSE)")
  }

  # Check write permissions for cache directory
  cache_dir <- dirname(cache_file)
  if (!dir.exists(cache_dir)) {
    tryCatch(
      dir.create(cache_dir, recursive = TRUE),
      error = function(e) {
        stop(sprintf("Cannot create cache directory: %s", conditionMessage(e)))
      }
    )
  } else if (!file.access(cache_dir, mode = 2) == 0) {
    stop(sprintf("No write permission for cache directory: %s", cache_dir))
  }

  return(TRUE)
}

#' Safe file operations wrapper
#'
#' Internal function to safely perform file operations with proper error handling
#'
#' @param operation Function to perform file operation
#' @param ... Arguments to pass to operation
#' @return Result of operation or throws error
#' @keywords internal
safe_file_operation <- function(operation, ...) {
  tryCatch(
    {
      operation(...)
    },
    error = function(e) {
      stop(sprintf(
        "File operation failed: %s",
        conditionMessage(e)
      ))
    }
  )
}

#' @keywords internal
load_cache <- function(cache_file) {
  tryCatch(
    {
      # Get column types from cache_definition
      template <- cache_definition()
      col_types <- readr::cols(.default = readr::col_guess())

      # Explicitly set column types based on template
      for (col_name in names(template)) {
        col_types[[col_name]] <- switch(class(template[[col_name]]),
          "character" = readr::col_character(),
          "numeric" = readr::col_double(),
          readr::col_guess()
        )
      }

      # First check if file is valid CSV
      if (length(readLines(cache_file)) < 2) { # Need at least header + one row
        warning("Invalid cache file structure, creating new cache")
        return(cache_definition())
      }

      # Attempt to read the file
      data <- readr::read_csv(
        cache_file,
        show_col_types = FALSE,
        col_types = col_types
      )

      # Check if we have all required columns
      if (!setequal(names(cache_definition()), names(data))) {
        warning("Cache file missing required columns, creating new cache")
        return(cache_definition())
      }

      # Ensure date is character
      data$date <- as.character(data$date)

      validate_cache_structure(data)
      data
    },
    error = function(e) {
      warning(sprintf("Invalid cache file, creating new cache: %s", conditionMessage(e)))
      cache_definition()
    }
  )
}
