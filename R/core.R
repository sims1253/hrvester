#' Calculate HRV metrics from RR intervals
#' @param rr_intervals Numeric vector of RR intervals
#' @return Named list of HRV metrics (RMSSD, SDNN)
#' @keywords internal
calculate_hrv <- function(rr_intervals) {
  list(
    rmssd = calc_rmssd(rr_intervals),
    sdnn = calc_sdnn(rr_intervals)
  )
}

#' Process single FIT file
#' @param file_path Path to FIT file
#' @return Tibble with HRV metrics
#' @export
process_fit_file <- function(file_path) {
  fit <- FITfileR::readFitFile(file_path)
  
  # Extract RR intervals for laying and standing positions
  rr_data <- extract_rr_intervals(fit)
  
  # Calculate metrics for each position
  laying_metrics <- calculate_hrv(rr_data$laying)
  standing_metrics <- calculate_hrv(rr_data$standing)
  
  # Return as single row tibble
  tibble(
    date = as.Date(fit$header$time_created),
    time_of_day = get_time_of_day(fit$header$time_created),
    laying_rmssd = laying_metrics$rmssd,
    laying_sdnn = laying_metrics$sdnn,
    standing_rmssd = standing_metrics$rmssd,
    standing_sdnn = standing_metrics$sdnn
  )
}
