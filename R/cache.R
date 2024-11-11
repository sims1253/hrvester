cache_definition <- function(){
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
    RR_filter = numeric(),
    activity = character()
  )
}


#' Process directory of FIT files with caching
#'
#' @param dir_path Directory containing FIT files
#' @param cache_file Path to cache file
#' @return tibble of HRV metrics
#' @examples
#' \dontrun{
#' # Process all FIT files in a directory
#' metrics <- process_fit_directory("path/to/fit/files")
#'
#' # Use custom cache location
#' metrics <- process_fit_directory(
#'   "path/to/fit/files",
#'   cache_file = "custom_cache.csv"
#' )
#' }
#' @export
#' @importFrom dplyr '%>%'
process_fit_directory <- function(
    dir_path,
    cache_file = paste0(dir_path, "/", "hrv_cache.csv"),
    filter_factor = 0.175,
    clear_cache = FALSE) {
  # Create cache directory if needed
  cache_dir <- dirname(cache_file)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Get list of FIT files
  fit_files <- list.files(dir_path, pattern = "\\.fit$", full.names = TRUE)

  # Load or initialize cache
  cached_data <- if (
    file.exists(cache_file) & !clear_cache) {
    read.csv2(cache_file)
  } else {
    cache_definition()
  }

  # Find new files to process
  new_files <- setdiff(fit_files, cached_data$source_file)
  outdated_entries <- cached_data %>%
    dplyr::filter(package_version != utils::packageVersion("ostdashr") |
      RR_filter != filter_factor) %>%
    dplyr::pull(source_file)

  if (length(new_files) > 0) {
    message(
      sprintf("Reprocessing %d new files", length(new_files))
    )
  }
  if (length(outdated_entries) > 0) {
    message(
      sprintf("Reprocessing %d files due to package updates or changed filter_factor", length(outdated_entries))
    )
  }


  if (length(new_files) > 0 || length(outdated_entries) > 0) {
    # Process new and outdated files
    files_to_process <- unique(c(new_files, outdated_entries))
    p = progressr::progressor(along = files_to_process)
    new_data <- furrr::future_map_dfr(
      files_to_process,
      function(file_path) {
        result = process_fit_file(file_path, filter_factor = filter_factor)
        p()
        return(result)
      }
    ) %>%
      dplyr::filter(!is.null(.))

    # Remove outdated entries from cached_data before combining
    cached_data <- cached_data %>%
      dplyr::filter(!source_file %in% outdated_entries)

    # Combine and sort data
    all_data <- dplyr::bind_rows(cached_data, new_data)

    if (nrow(all_data) > 0) {
      all_data <- all_data %>%
        dplyr::arrange(date, desc(time_of_day))

      # Write to temporary file first
      temp_file <- tempfile(fileext = ".csv")
      write.csv2(all_data, file = temp_file, row.names = FALSE)

      # Then move to final location
      file.copy(temp_file, cache_file, overwrite = TRUE)
      unlink(temp_file)
    }

    all_data
  } else {
    cached_data
  }
}
