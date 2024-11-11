#' Extract RR intervals from FIT file
#'
#' @param fit_object FITfileR object
#' @param filter_factor Used to filter RR values that are 1.x times smaller or
#'   larger than the last one as suspected false readings.
#' @return List of laying and standing RR intervals in ms
#' @keywords internal
extract_rr_intervals <- function(fit_object,
                                 filter_factor,
                                 laying_time = 180,
                                 standing_time = 180) {
  # Get HRV data directly using FITfileR methods
  if ("hrv" %in% FITfileR::listMessageTypes(fit_object)) {
    # hopefully time between heartbeats is below 2 seconds.
    hrv_data <- dplyr::filter(FITfileR::hrv(fit_object), time < 2)$time
  } else {
    return(list(laying = numeric(), standing = numeric()))
  }

  # Pre-calculate differences for RMSSD
  RR <- c(hrv_data[1])
  for (i in 2:length(hrv_data)) {
    if (hrv_data[i - 1] * (1 - filter_factor) < hrv_data[i] &
      hrv_data[i - 1] * (1 + filter_factor) > hrv_data[i]) {
      RR <- append(RR, hrv_data[i])
    }
  }

  # Get heartbeat timestamps
  timestamps <- cumsum(RR)

  # Return RR intervals for each position
  return(
    list(
      laying = RR[1:which.max(timestamps > 180) - 1],
      standing = RR[
        which.max(timestamps > 180):which.max(timestamps > 360) - 1
      ]
    )
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

  return(
    list(
      rmssd = round(
        sqrt(mean(diff(1000 * rr_intervals)^2, na.rm = TRUE)),
        digits = 2
      ),
      sdnn = round(sd(1000 * rr_intervals, na.rm = TRUE), digits = 2)
    )
  )
}

#' Extract HR data from \code{FitFile} object
#'
#' @param fit_object An object of class \code{FitFile}, eg.
#'   from \code{\link[FITfileR::readFitFile]{FITfileR::readFitFile}}
#'
#' @return A vector containing up to the first three minutes of heart rate data.
#' @export
get_HR <- function(fit_object) {
  HR <- FITfileR::records(fit_object)
  if (all(class(HR) == "list")) {
    HR <- HR[[which.max(sapply(HR, nrow))]]$heart_rate
    HR <- HR[1:min(360, length(HR))]
  } else {
    HR <- HR$heart_rate[1:min(360, nrow(HR))]
  }
  return(HR)
}

#' Process a single FIT file to extract HRV metrics
#'
#' @param file_path Path to FIT file
#' @return Tibble row containing:
#'   \item{source_file}{Path to processed file}
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
process_fit_file <- function(file_path, filter_factor) {
  tryCatch(
    {
      # Read file once
      fit_object <- FITfileR::readFitFile(file_path)

      # Extract RR intervals
      rr_data <- extract_rr_intervals(fit_object = fit_object,
                                      filter_factor = filter_factor)
      hr_data <- get_HR(fit_object = fit_object)

      # Only process if we have enough data
      if (length(rr_data$laying) >= 2 && length(rr_data$standing) >= 2) {
        # Get times once
        date <- clock::as_date(FITfileR::getMessagesByType(fit_object, "session")$timestamp)
        week <- as.numeric(strftime(FITfileR::getMessagesByType(fit_object, "session")$timestamp, format = "%W"))
        time_of_day <- ifelse(
          (clock::get_hour(FITfileR::getMessagesByType(fit_object, "session")$timestamp) > 4) &
            (clock::get_hour(FITfileR::getMessagesByType(fit_object, "session")$timestamp) < 13),
          "Morning",
          "Evening"
        )

        # Calculate metrics
        laying <- calculate_hrv(rr_data$laying)
        standing <- calculate_hrv(rr_data$standing)

        # Create single row efficiently
        tibble::tibble(
          source_file = file_path,
          date = as.character(date),
          week = week,
          time_of_day = time_of_day,
          laying_rmssd = laying$rmssd,
          laying_sdnn = laying$sdnn,
          laying_hr = round(mean(hr_data[30:150], na.rm = TRUE), digits = 2),
          laying_resting_hr = round(mean(hr_data[120:170], na.rm = TRUE), digits = 0),
          standing_rmssd = standing$rmssd,
          standing_sdnn = standing$sdnn,
          standing_hr = round(mean(hr_data[220:330], na.rm = TRUE), digits = 2),
          standing_max_hr = max(hr_data[181:220]),
          package_version = as.character(packageVersion("ostdashr")),
          RR_filter = filter_factor,
          activity = FITfileR::getMessagesByType(fit_object, "sport")$name
        )
      } else {
        tibble::tibble(
          source_file = file_path,
          date = as.character(date),
          week = week,
          time_of_day = time_of_day,
          laying_rmssd = NA,
          laying_sdnn = NA,
          laying_hr = NA,
          laying_resting_hr = NA,
          standing_rmssd = NA,
          standing_sdnn = NA,
          standing_hr = NA,
          standing_max_hr = NA,
          package_version = as.character(packageVersion("ostdashr")),
          RR_filter = filter_factor,
          activity = FITfileR::getMessagesByType(fit_object, "sport")$name
        ) # TODO change this to do HR without HRV probably?
      }
    },
    error =  function(e) tibble::tibble(
      source_file = file_path,
      date = NA,
      week = NA,
      time_of_day = NA,
      laying_rmssd = NA,
      laying_sdnn = NA,
      laying_hr = NA,
      laying_resting_hr = NA,
      standing_rmssd = NA,
      standing_sdnn = NA,
      standing_hr = NA,
      standing_max_hr = NA,
      package_version = as.character(packageVersion("ostdashr")),
      RR_filter = filter_factor,
      activity = NA
    )
  )
}
