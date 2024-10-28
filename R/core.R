#' Extract RR intervals from FIT file
#'
#' @param fit_object FITfileR object
#' @return List of laying and standing RR intervals in ms
#' @keywords internal
extract_rr_intervals <- function(fit_object) {
  # Get all records at once instead of filtering multiple times
  records <- FITfileR::records(fit_object)
  
  if (is.null(records) || nrow(records) == 0) {
    return(list(laying = numeric(), standing = numeric()))
  }
  
  # Extract all RR intervals at once
  rr_data <- records[records$message == "hrv" & !is.na(records$time), ]
  
  if (nrow(rr_data) == 0) {
    return(list(laying = numeric(), standing = numeric()))
  }
  
  # Convert timestamps once
  timestamps <- as.numeric(rr_data$timestamp)
  start_time <- min(timestamps)
  
  # Define windows (in seconds)
  laying_end <- start_time + 180    # First 3 minutes
  standing_start <- laying_end + 30  # After 30s transition
  
  # Efficient subsetting
  list(
    laying = rr_data$time[timestamps <= laying_end],
    standing = rr_data$time[timestamps >= standing_start]
  )
}

#' Calculate HRV metrics from RR intervals
#'
#' @param rr_intervals Numeric vector of RR intervals in milliseconds
#' @return Named list containing:
#'   \item{rmssd}{Root Mean Square of Successive Differences}
#'   \item{sdnn}{Standard Deviation of NN intervals}
#'   \item{mean_hr}{Mean Heart Rate in BPM}
#' @keywords internal
calculate_hrv <- function(rr_intervals) {
  if (length(rr_intervals) < 2) {
    return(list(
      rmssd = NA_real_,
      sdnn = NA_real_,
      mean_hr = NA_real_
    ))
  }
  
  # Pre-calculate differences for RMSSD
  diffs <- diff(rr_intervals)
  
  list(
    rmssd = sqrt(mean(diffs^2, na.rm = TRUE)),
    sdnn = sd(rr_intervals, na.rm = TRUE),
    mean_hr = 60000 / mean(rr_intervals, na.rm = TRUE)
  )
}

#' Process a single FIT file to extract HRV metrics
#'
#' @param file_path Path to FIT file
#' @return Tibble row containing:
#'   \item{file_path}{Path to processed file}
#'   \item{date}{Date of measurement}
#'   \item{time_of_day}{Morning or Evening}
#'   \item{laying_rmssd}{RMSSD during laying position}
#'   \item{laying_sdnn}{SDNN during laying position}
#'   \item{laying_hr}{Mean HR during laying position}
#'   \item{standing_rmssd}{RMSSD during standing position}
#'   \item{standing_sdnn}{SDNN during standing position}
#'   \item{standing_hr}{Mean HR during standing position}
#' @return NULL if processing fails
#' @export
process_fit_file <- function(file_path) {
  tryCatch({
    # Read file once
    fit_object <- FITfileR::readFitFile(file_path)
    
    # Extract RR intervals
    rr_data <- extract_rr_intervals(fit_object)
    
    # Only process if we have enough data
    if (length(rr_data$laying) >= 2 && length(rr_data$standing) >= 2) {
      # Get timestamp once
      timestamp <- fit_object$header$time_created
      
      # Calculate metrics
      laying <- calculate_hrv(rr_data$laying)
      standing <- calculate_hrv(rr_data$standing)
      
      # Create single row efficiently
      tibble(
        file_path = file_path,
        date = as.Date(timestamp),
        time_of_day = get_time_of_day(timestamp),
        laying_rmssd = laying$rmssd,
        laying_sdnn = laying$sdnn,
        laying_hr = laying$mean_hr,
        standing_rmssd = standing$rmssd,
        standing_sdnn = standing$sdnn,
        standing_hr = standing$mean_hr
      )
    } else {
      NULL
    }
  }, error = function(e) NULL)
}
