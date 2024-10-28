#' Process directory of FIT files with caching
#'
#' @param dir_path Directory containing FIT files
#' @param cache_file Path to cache file
#' @return Tibble of HRV metrics
#' @examples
#' \dontrun{
#' # Process all FIT files in a directory
#' metrics <- process_fit_directory("path/to/fit/files")
#'
#' # Use custom cache location
#' metrics <- process_fit_directory(
#'   "path/to/fit/files",
#'   cache_file = "custom_cache.parquet"
#' )
#' }
#' @export
process_fit_directory <- function(dir_path, cache_file = "hrv_cache.parquet") {
  # Get all FIT files
  fit_files <- list.files(dir_path, pattern = "\\.fit$", full.names = TRUE)
  
  # Read cache if it exists
  cached_data <- if (file.exists(cache_file)) {
    arrow::read_parquet(cache_file)
  } else {
    tibble()
  }
  
  # Process only new files
  new_files <- setdiff(fit_files, cached_data$file_path)
  
  if (length(new_files) > 0) {
    # Process new files in parallel
    new_data <- furrr::future_map_dfr(
      new_files,
      process_fit_file,
      .progress = TRUE
    ) %>% filter(!is.null(.))
    
    # Combine and save
    all_data <- bind_rows(cached_data, new_data) %>%
      arrange(date, time_of_day)
    
    arrow::write_parquet(all_data, cache_file)
    all_data
  } else {
    cached_data
  }
}
