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
#'     \item laying_artifact_percentage (numeric): Artifact percentage in laying phase
#'     \item laying_signal_quality_index (numeric): Signal quality index for laying phase
#'     \item laying_data_completeness (numeric): Data completeness for laying phase
#'     \item laying_quality_grade (character): Quality grade for laying phase (A/B/C/D/F)
#'     \item standing_artifact_percentage (numeric): Artifact percentage in standing phase
#'     \item standing_signal_quality_index (numeric): Signal quality index for standing phase
#'     \item standing_data_completeness (numeric): Data completeness for standing phase
#'     \item standing_quality_grade (character): Quality grade for standing phase (A/B/C/D/F)
#'     \item file_digest (character): MD5 digest of the source file contents
#'     \item config_id (character): Digest of the analysis configuration the
#'       entry was produced with
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
    orthostatic_rise = numeric(),
    # Quality metrics for laying phase
    laying_artifact_percentage = numeric(),
    laying_signal_quality_index = numeric(),
    laying_data_completeness = numeric(),
    laying_quality_grade = character(),
    # Quality metrics for standing phase
    standing_artifact_percentage = numeric(),
    standing_signal_quality_index = numeric(),
    standing_data_completeness = numeric(),
    standing_quality_grade = character(),
    # Provenance: entries are only reused when both digests match
    file_digest = character(),
    config_id = character()
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
        col,
        actual_type[1]
      ))
    }

    # For numeric columns, ensure they are numeric
    if (expected_type == "numeric" && !is.numeric(cache_data[[col]])) {
      stop(sprintf(
        "Invalid column type for %s: expected numeric, got %s",
        col,
        actual_type[1]
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

#' Hash a string
#'
#' Internal helper producing a stable MD5 digest of a character string
#' without requiring an external dependency.
#'
#' @param x Character string to hash
#' @return Character string with the MD5 digest
#' @keywords internal
hash_string <- function(x) {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(enc2utf8(x), tmp, useBytes = TRUE)
  unname(tools::md5sum(tmp))
}

#' Compute the analysis configuration identity
#'
#' Internal helper summarizing the effective analysis configuration (protocol
#' windows, filtering thresholds, correction method and package version) into
#' a single digest. Cache entries are only reused when this identity matches,
#' so changing any analysis argument invalidates the affected results.
#'
#' @param config Named list of configuration values
#' @return Character string with the configuration digest
#' @keywords internal
compute_config_id <- function(config) {
  config <- config[order(names(config))]
  fields <- vapply(
    names(config),
    function(nm) {
      paste0(nm, "=", paste(format(config[[nm]]), collapse = ","))
    },
    character(1)
  )
  hash_string(paste(fields, collapse = "|"))
}

#' Save the cache atomically
#'
#' Internal helper writing the cache to a temporary file in the destination
#' directory and replacing the destination only after the write succeeded.
#' A failed or interrupted write leaves the previous cache untouched.
#'
#' @param data Data frame to write
#' @param cache_file Path of the cache file to replace
#' @return Invisible TRUE, or an error if the cache could not be replaced
#' @keywords internal
save_cache_atomic <- function(data, cache_file) {
  tmp <- tempfile(pattern = ".hrv-cache-", tmpdir = dirname(cache_file))
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)

  readr::write_csv(data, tmp)

  if (!file.rename(tmp, cache_file)) {
    # Windows cannot rename over an existing destination; fall back to a
    # backup swap that still restores the previous cache on failure
    backup <- paste0(cache_file, ".bak")
    if (!file.rename(cache_file, backup) || !file.rename(tmp, cache_file)) {
      if (
        file.exists(backup) &&
          !file.exists(cache_file) &&
          !file.rename(backup, cache_file)
      ) {
        stop("Failed to restore previous cache file: ", cache_file)
      }
      stop("Failed to replace cache file atomically: ", cache_file)
    }
    unlink(backup)
  }

  invisible(TRUE)
}

#' @keywords internal
load_cache <- function(cache_file) {
  tryCatch(
    {
      # Get column types from cache_definition
      template <- cache_definition()

      # Explicitly set column types based on template. The collectors must
      # live inside the cols() specification so that, for example, an
      # all-NA column round-trips as the declared type instead of being
      # guessed into an incompatible one. Only columns present in the file
      # get a collector so legacy files do not trigger parser warnings.
      collectors <- lapply(
        template,
        function(col) {
          if (is.character(col)) {
            readr::col_character()
          } else {
            readr::col_double()
          }
        }
      )

      # First check if file has any content; a header-only file is a valid
      # empty cache
      if (length(readLines(cache_file)) < 1) {
        warning("Invalid cache file structure, creating new cache")
        return(cache_definition())
      }

      header <- names(
        readr::read_csv(cache_file, n_max = 0, show_col_types = FALSE)
      )
      collectors <- collectors[names(collectors) %in% header]
      col_types <- do.call(readr::cols, collectors)

      # Attempt to read the file
      data <- readr::read_csv(
        cache_file,
        show_col_types = FALSE,
        col_types = col_types
      )

      # Backfill provenance columns missing in legacy caches so those rows
      # are kept but flagged for reprocessing
      for (col_name in setdiff(names(template), names(data))) {
        data[[col_name]] <- if (is.character(template[[col_name]])) {
          NA_character_
        } else {
          NA_real_
        }
      }

      # Check if we have all required columns
      if (!all(names(template) %in% names(data))) {
        warning("Cache file missing required columns, creating new cache")
        return(cache_definition())
      }

      # Ensure date is character
      data$date <- as.character(data$date)

      validate_cache_structure(data)
      data
    },
    error = function(e) {
      warning(sprintf(
        "Invalid cache file, creating new cache: %s",
        conditionMessage(e)
      ))
      cache_definition()
    }
  )
}
