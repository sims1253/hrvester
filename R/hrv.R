#' Extract HR data from \code{FitFile} object
#'
#' @param fit_object An object of class \code{FitFile}, eg.
#'   from \code{\link[FITfileR::readFitFile]{FITfileR::readFitFile}}
#'
#' @return A vector containing up to the first three minutes of heart rate data.
#' @export
get_HR <- function(fit_object) {
  HR <- records(fit_object)
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
  HRV <- filter(hrv(fit_object), time < 2)$time
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
    laying_mean_hr = round(mean(HR[1:180], na.rm = TRUE), digits = 2),
    standing_SDNN = round(sd(RR$standing_RR$RR_ms), digits = 2),
    standing_rMSSD = round(sqrt(mean(RR$standing_RR$successive_differences^2))),
    standing_mean_hr = round(mean(HR[181:length(HR)], na.rm = TRUE), digits = 2),
    standing_max_hr = max(HR),
    date = as_date(getMessagesByType(fit_object, "session")$timestamp),
    week = as.numeric(strftime(getMessagesByType(fit_object, "session")$timestamp, format = "%W"))
  )
  metrics$standing_time_to_mean_hr <- min(
    which(HR <= metrics$standing_mean_hr)[
      which(HR <= metrics$standing_mean_hr) > which.max(HR)
    ]
  ) -
    which.max(HR)

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
        label = paste0("mean HR(bpm): ", metrics$laying_mean_hr)
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

#' Trend plots of HRV summaries for orthostatic tests
#'
#' @param fit_dir Path of directory containing fit files.
#'
#' @return A ggplot2 plot object.
#' @export
#' @importFrom dplyr '%>%' group_by summarise filter across bind_rows as_tibble
#' @importFrom ggplot2 ggplot geom_point geom_line geom_hline facet_grid theme_bw aes
hrv_trend_plot <- function(fit_dir, just_rssme = FALSE) {
  tests <- list.files(fit_dir, pattern = ".fit", full.names = TRUE)

  metrics <- as_tibble(
    do.call(
      bind_rows,
      lapply(tests,
        function(x, ...) hrv_metrics(FITfileR::readFitFile(x), ...),
        filter_factor = 0.175
      )
    )
  ) %>%
    tidyr::pivot_longer(
      !c(date, week, morning),
      names_to = c("position", "metric"),
      names_pattern = "(.+?)_(.*)",
      values_to = "value"
    )

  overall_means <- as.data.frame(metrics) %>%
    group_by(across(c("morning", "position", "metric"))) %>%
    summarise(mean = mean(value))

  weekly_means <- as.data.frame(metrics) %>%
    group_by(across(c("morning", "position", "metric", "week"))) %>%
    summarise(mean = mean(value))

  metrics$weekly_mean <- rep(0, nrow(metrics))
  for (i in 1:nrow(metrics)) {
    metrics$weekly_mean[[i]] <- filter(
      weekly_means,
      morning == metrics$morning[[i]],
      position == metrics$position[[i]],
      metric == metrics$metric[[i]],
      week == metrics$week[[i]]
    )$mean
  }

  if (just_rssme) {
    metrics %>%
      filter(
        metric == "rMSSD",
        morning == "Morning"
      ) %>%
      ggplot(aes(x = date, y = value, color = position)) +
      geom_point() +
      geom_line() +
      geom_line(aes(y = weekly_mean), linetype = "dashed") +
      geom_hline(
        data = filter(
          overall_means,
          morning == "Morning",
          metric == "rMSSD"
        ),
        aes(yintercept = mean, color = position),
        linetype = "dotted"
      ) +
      ylab("rMSSD") +
      # facet_grid(metric ~ morning, scales = "free_y") +
      theme_bw()
  } else {
    metrics %>%
      ggplot(aes(x = date, y = value, color = position)) +
      geom_point() +
      geom_line() +
      geom_line(aes(y = weekly_mean), linetype = "dashed") +
      geom_hline(
        data = overall_means,
        aes(yintercept = mean, color = position),
        linetype = "dotted"
      ) +
      facet_grid(metric ~ morning, scales = "free_y") +
      theme_bw()
  }
}
