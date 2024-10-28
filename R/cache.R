#' Process directory of FIT files with caching
#' @param dir_path Directory containing FIT files
#' @param cache_file Path to cache file
#' @return Tibble of all HRV metrics
#' @export
process_fit_directory <- function(dir_path, cache_file = "hrv_cache.parquet") {
  # Get all FIT files
  fit_files <- list.files(dir_path, pattern = "\\.fit$", full.names = TRUE)
  
  # Read existing cache if available
  cached_data <- if(file.exists(cache_file)) {
    arrow::read_parquet(cache_file)
  } else {
    tibble()
  }
  
  # Find new files
  processed_files <- cached_data$file_path
  new_files <- setdiff(fit_files, processed_files)
  
  # Process new files
  if(length(new_files) > 0) {
    new_data <- map_dfr(new_files, process_fit_file) %>%
      mutate(file_path = new_files)
    
    # Combine and save
    all_data <- bind_rows(cached_data, new_data)
    arrow::write_parquet(all_data, cache_file)
    
    all_data
  } else {
    cached_data
  }
}
