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

#' Extract HRV data from \code{FitFile} object
#'
#' @param fit_object An object of class \code{FitFile}, eg.
#'   from \code{\link[FITfileR::readFitFile]{FITfileR::readFitFile}}
#'
#' @return A vector containing up to the first three minutes of heart rate
#'   variability data.
#' @export
get_HRV <- function(fit_object) {
  HRV <- dplyr::filter(FITfileR::hrv(fit_object), time < 2)$time
  return(HRV)
}

#' Calculate RR from HRV data.
#'
#' @param HRV A vector of HRV data (e.g. from \link{get_HRV})
#' @param filter_factor Used to filter RR values that are 1.x times smaller or
#'   larger than the last one as suspected false readings.
#'
#' @return A named list containing the full RR data and suspected RR data for
#'   standing and lying down.
#' @export
get_RR <- function(HRV, filter_factor) {
  RR <- c(HRV[1])
  for (i in 2:length(HRV)) {
    if (HRV[i - 1] * (1 - filter_factor) < HRV[i] & HRV[i - 1] * (1 + filter_factor) > HRV[i]) {
      RR <- append(RR, HRV[i])
    }
  }
  RR <- data.frame(RR = RR, time = cumsum(RR))

  RR$RR_ms <- RR$RR * 1000
  RR$successive_differences <- numeric(length = nrow(RR))
  for (i in 1:nrow(RR) - 1) {
    RR$successive_differences[i] <- RR$RR_ms[i + 1] - RR$RR_ms[i]
  }

  return(list(
    RR = RR,
    laying_RR = RR[1:which.max(RR$time > 180) - 1, ],
    standing_RR = RR[which.max(RR$time > 180):nrow(RR), ]
  ))
}

#' Calculate HRV summaries for an orthostatic test
#'
#' @param fit_object An object of class \code{FitFile}, eg.
#'   from \code{\link[FITfileR::readFitFile]{FITfileR::readFitFile}}
#' @param filter_factor Used to filter RR values that are 1.x times smaller or
#'   larger than the last one as suspected false readings.
#'
#' @return A named list of HRV summarie metrics.
#' @export
#'
#' @importFrom FITfileR records hrv getMessagesByType
#' @importFrom dplyr filter
#' @importFrom clock as_date add_days get_hour
hrv_metrics <- function(fit_object, filter_factor = 0.25) {
  HR <- get_HR(fit_object = fit_object)
  HRV <- get_HRV(fit_object = fit_object)
  RR <- get_RR(HRV = HRV, filter_factor)

  metrics <- list(
    laying_SDNN = round(sd(RR$laying_RR$RR_ms), digits = 2),
    laying_rMSSD = round(sqrt(mean(RR$laying_RR$successive_differences^2))),
    laying_mean_hr = round(mean(HR[30:150], na.rm = TRUE), digits = 2),
    laying_resting_hr = round(mean(HR[120:170], na.rm = TRUE), digits = 0),
    standing_SDNN = round(sd(RR$standing_RR$RR_ms), digits = 2),
    standing_rMSSD = round(sqrt(mean(RR$standing_RR$successive_differences^2))),
    standing_mean_hr = round(mean(HR[220:330], na.rm = TRUE), digits = 2),
    standing_max_hr = max(HR[181:220]),
    date = clock::as_date(getMessagesByType(fit_object, "session")$timestamp),
    week = as.numeric(strftime(getMessagesByType(fit_object, "session")$timestamp, format = "%W"))
  )
  metrics$standing_time_to_mean_hr <- min(
    which(HR <= metrics$standing_mean_hr)[
      which(HR <= metrics$standing_mean_hr) > (180 + which.max(HR[181:220]))
    ]
  ) -
    (180 + which.max(HR[181:220]))

  metrics$morning <- ifelse(
    (get_hour(getMessagesByType(fit_object, "session")$timestamp) > 4) &
      (get_hour(getMessagesByType(fit_object, "session")$timestamp) < 13),
    "Morning",
    "Evening"
  )


  if (get_hour(getMessagesByType(fit_object, "session")$timestamp) < 4) {
    metrics$date <- add_days(metrics$date, -1)
  }

  return(metrics)
}


#' Plot HRV summaries for a single orthostatic test fit file.
#'
#' @param fit_file_path Path to the fit file
#' @param base "HR" if x axis should be heart rate or "RR" of x axis should be RR.
#' @param filter_factor Used to filter RR values that are 1.x times smaller or
#'   larger than the last one as suspected false readings.
#'
#' @return A ggplot2 plot object.
#' @export
#'
#' @importFrom FITfileR readFitFile
#' @importFrom dplyr '%>%'
#' @importFrom ggplot2 ggplot geom_line geom_vline scale_x_continuous annotate aes xlab theme_bw ggtitle
hrv_plot <- function(fit_file_path, base = "RR", filter_factor = 0.25) {
  fit_object <- readFitFile(fit_file_path)

  metrics <- hrv_metrics(
    fit_object = fit_object,
    filter_factor = filter_factor
  )

  HR <- get_HR(fit_object = fit_object)
  RR <- get_RR(HRV = get_HRV(fit_object = fit_object), filter_factor = filter_factor)
  standing_RR <- RR$standing_RR
  laying_RR <- RR$laying_RR
  RR <- RR$RR

  if (base == "RR") {
    p <- RR %>%
      ggplot(aes(x = time, y = RR)) +
      geom_line() +
      geom_vline(xintercept = 180, linetype = "dashed", color = "red") +
      scale_x_continuous(limits = c(0, 360), breaks = c(0, 60, 120, 180, 240, 300, 360), expand = c(0, 0)) +
      annotate("text",
        color = "red", x = 80, y = mean(standing_RR$RR),
        label = paste0("SDNN(ms): ", metrics$laying_SDNN)
      ) +
      annotate("text",
        color = "red", x = 80, y = mean(standing_RR$RR) - 0.04,
        label = paste0("rMSSD(ms): ", metrics$laying_rMSSD)
      ) +
      annotate("text",
        color = "red", x = 80, y = mean(standing_RR$RR) - 0.08,
        label = paste0("resting HR(bpm): ", metrics$laying_resting_hr)
      ) +
      annotate("text",
        color = "red", x = 280, y = mean(laying_RR$RR) + 0.1,
        label = paste0("SDNN(ms): ", metrics$standing_SDNN)
      ) +
      annotate("text",
        color = "red", x = 280, y = mean(laying_RR$RR) + 0.06,
        label = paste0("rMSSD(ms): ", metrics$standing_rMSSD)
      ) +
      annotate("text",
        color = "red", x = 280, y = mean(laying_RR$RR) + 0.02,
        label = paste0("mean HR(bpm): ", metrics$standing_mean_hr)
      ) +
      annotate("text",
        color = "red", x = 280, y = mean(laying_RR$RR) - 0.02,
        label = paste0("time to mean HR(s): ", metrics$standing_time_to_mean_hr)
      ) +
      annotate("text", color = "red", x = 218, y = min(RR$RR), label = paste0("max HR(bpm): ", max(HR))) +
      xlab("Time (s)") +
      ggtitle(metrics$morning) +
      theme_bw(base_size = 12)
  } else if (base == "HR") {
    p <- data.frame(HR = get_HR(fit_object = fit_object)) %>%
      ggplot(aes(y = HR, x = seq_along(HR))) +
      geom_line() +
      geom_vline(xintercept = 180, linetype = "dashed", color = "red") +
      scale_x_continuous(limits = c(0, 360), breaks = c(0, 60, 120, 180, 240, 300, 360), expand = c(0, 0)) +
      annotate("text",
        color = "red", x = 80, y = mean(HR[180:length(HR)]) + 15,
        label = paste0("SDNN(ms): ", metrics$laying_SDNN)
      ) +
      annotate("text",
        color = "red", x = 80, y = mean(HR[180:length(HR)]) + 11,
        label = paste0("rMSSD(ms): ", metrics$laying_rMSSD)
      ) +
      annotate("text",
        color = "red", x = 80, y = mean(HR[180:length(HR)]) + 7,
        label = paste0("mean HR(bpm): ", metrics$laying_mean_hr)
      ) +
      annotate("text",
        color = "red", x = 280, y = mean(HR[1:180]) + 5,
        label = paste0("SDNN(ms): ", metrics$standing_SDNN)
      ) +
      annotate("text",
        color = "red", x = 280, y = mean(HR[1:180]) + 1,
        label = paste0("rMSSD(ms): ", metrics$standing_rMSSD)
      ) +
      annotate("text",
        color = "red", x = 280, y = mean(HR[1:180]) - 3,
        label = paste0("mean HR(bpm): ", metrics$standing_mean_hr)
      ) +
      annotate("text",
        color = "red", x = 280, y = mean(HR[1:180]) - 7,
        label = paste0("time to mean HR(s): ", metrics$standing_time_to_mean_hr)
      ) +
      annotate("text", color = "red", x = 218, y = max(HR[10:length(HR)]), label = paste0("max HR(bpm): ", max(HR))) +
      xlab("Time (s)") +
      ggtitle(metrics$morning) +
      theme_bw(base_size = 12)
  } else {
    stop("No proper base given. Must be 'RR' or 'HR'")
  }
  return(p)
}

#' Cache HRV metrics from fit files
#' 
#' @param fit_dir Path of directory containing fit files
#' @param cache_file Path to save the parquet cache file
#' @return A data frame of cached metrics
#' @importFrom arrow read_parquet write_parquet
cache_hrv_metrics <- function(fit_dir, cache_file = "hrv_metrics_cache.parquet") {
  tests <- list.files(fit_dir, pattern = ".fit", full.names = TRUE)
  
  # Read existing cache if it exists
  if (file.exists(cache_file)) {
    cached_metrics <- arrow::read_parquet(cache_file)
    cached_files <- cached_metrics$file_path
    new_files <- tests[!tests %in% cached_files]
  } else {
    cached_metrics <- NULL
    new_files <- tests
  }
  
  if (length(new_files) > 0) {
    # Process new files in parallel
    if (requireNamespace("parallel", quietly = TRUE)) {
      num_cores <- parallel::detectCores() - 1
      cl <- parallel::makeCluster(num_cores)
      parallel::clusterExport(cl, c("hrv_metrics", "get_HR", "get_HRV", "get_RR"))
      
      new_metrics <- as_tibble(
        do.call(
          bind_rows,
          parallel::parLapply(cl, new_files,
            function(x, ...) {
              metrics <- hrv_metrics(FITfileR::readFitFile(x), ...)
              metrics$file_path <- x
              return(metrics)
            },
            filter_factor = 0.175
          )
        )
      )
      
      parallel::stopCluster(cl)
    } else {
      new_metrics <- as_tibble(
        do.call(
          bind_rows,
          lapply(new_files,
            function(x, ...) {
              metrics <- hrv_metrics(FITfileR::readFitFile(x), ...)
              metrics$file_path <- x
              return(metrics)
            },
            filter_factor = 0.175
          )
        )
      )
    }
    
    # Combine with existing cache
    if (!is.null(cached_metrics)) {
      metrics <- bind_rows(cached_metrics, new_metrics)
    } else {
      metrics <- new_metrics
    }
    
    # Save updated cache
    arrow::write_parquet(metrics, cache_file)
  } else {
    metrics <- cached_metrics
  }
  
  return(metrics)
}

#' Trend plots of HRV summaries for orthostatic tests
#'
#' @param fit_dir Path of directory containing fit files
#' @param cache_file Path to the cache file (optional)
#' @param just_rssme Logical, whether to show only rMSSD metrics
#' @return A ggplot2 plot object
#' @export
hrv_trend_plot <- function(fit_dir, cache_file = "hrv_metrics_cache.parquet", just_rssme = FALSE) {
  # Get metrics from cache or process files
  metrics <- cache_hrv_metrics(fit_dir, cache_file)
  
  # Rest of the function remains the same, starting from the pivot_longer...
  metrics_long <- metrics %>%
    tidyr::pivot_longer(
      !c(date, week, morning, file_path),
      names_to = c("position", "metric"),
      names_pattern = "(.+?)_(.*)",
      values_to = "value"
    )
  
  # ... rest of the existing plotting code ...
}

#' Validate FIT file object
#' @param fit_object Object to validate
#' @return TRUE if valid, throws error if invalid
validate_fit_object <- function(fit_object) {
  if (!inherits(fit_object, "FitFile")) {
    stop("Input must be a FitFile object")
  }
  if (is.null(FITfileR::records(fit_object))) {
    stop("FIT file contains no records")
  }
  return(TRUE)
}

#' Validate numeric vector
#' @param x Vector to validate
#' @param name Name of vector for error message
#' @return TRUE if valid, throws error if invalid
validate_numeric_vector <- function(x, name) {
  if (!is.numeric(x)) {
    stop(sprintf("%s must be numeric", name))
  }
  if (length(x) == 0) {
    stop(sprintf("%s cannot be empty", name))
  }
  if (any(is.na(x))) {
    warning(sprintf("NAs found in %s", name))
  }
  return(TRUE)
}

#' Default HRV analysis configuration
#' @export
default_hrv_config <- function() {
  list(
    filter_factor = 0.25,
    laying_window = c(120, 170),  # Window for laying HR calculation
    standing_window = c(220, 330), # Window for standing HR calculation
    max_hr_window = c(181, 220),   # Window for max HR calculation
    morning_hours = c(4, 13),      # Define morning hours
    cache_filename = "hrv_metrics_cache.parquet"
  )
}

#' Setup logging
#' @importFrom logger layout_glue_generator setup_logger
setup_hrv_logging <- function() {
  logger::setup_logger(
    layout = logger::layout_glue_generator(
      format = "{level} [{time}] {msg}"
    )
  )
}

# Use in functions:
logger::log_info("Processing {length(new_files)} new files")
logger::log_debug("Calculated metrics for {fit_file_path}")

#' Create HRV metrics object
#' @export
new_hrv_metrics <- function(metrics_list) {
  structure(
    metrics_list,
    class = c("hrv_metrics", "list")
  )
}

#' Print method for HRV metrics
#' @export
print.hrv_metrics <- function(x, ...) {
  cat("HRV Metrics for", format(x$date), "\n")
  cat("Morning:", x$morning, "\n")
  cat("\nLaying metrics:\n")
  cat("  SDNN:", x$laying_SDNN, "ms\n")
  cat("  rMSSD:", x$laying_rMSSD, "ms\n")
  cat("  Mean HR:", x$laying_mean_hr, "bpm\n")
  # ... etc
}

#' Clean HRV metrics data
#' @param metrics Data frame of HRV metrics
#' @return Cleaned metrics
clean_hrv_metrics <- function(metrics) {
  metrics %>%
    # Remove obvious outliers
    filter(
      laying_SDNN < 300,
      standing_SDNN < 300,
      laying_mean_hr > 30,
      laying_mean_hr < 100,
      standing_mean_hr > 40,
      standing_mean_hr < 120
    ) %>%
    # Handle missing values
    mutate(across(
      where(is.numeric),
      ~ifelse(is.infinite(.), NA, .)
    )) %>%
    # Add any derived columns
    mutate(
      hrv_ratio = standing_rMSSD / laying_rMSSD,
      hr_response = standing_max_hr - laying_resting_hr
    )
}

#' Analyze HRV from directory of FIT files
#' @export
analyze_hrv <- function(fit_dir, config = default_hrv_config()) {
  setup_hrv_logging()
  
  logger::log_info("Starting HRV analysis for {fit_dir}")
  
  # Get metrics
  metrics <- cache_hrv_metrics(
    fit_dir = fit_dir,
    cache_file = config$cache_filename
  ) %>%
    clean_hrv_metrics()
  
  # Generate plots
  plots <- list(
    trend = hrv_trend_plot(metrics),
    rmssd = hrv_trend_plot(metrics, just_rssme = TRUE)
  )
  
  # Return results
  structure(
    list(
      metrics = metrics,
      plots = plots,
      config = config
    ),
    class = "hrv_analysis"
  )
}
