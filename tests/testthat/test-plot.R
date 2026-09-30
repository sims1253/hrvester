library(mockery)
library(FITfileR)
library(testthat)

trend_metrics <- function(quality_grade = "A") {
  base <- hrvester:::create_empty_result(
    "x.fit", as.Date("2026-01-01"), 1, "Morning"
  )
  base$laying_rmssd <- 50
  base$laying_sdnn <- 60
  base$laying_hr <- 60
  base$laying_resting_hr <- 58
  base$standing_rmssd <- 30
  base$standing_sdnn <- 40
  base$standing_hr <- 80
  base$standing_max_hr <- 95
  base$hrr_60s <- 25
  base$hrr_relative <- 50
  base$orthostatic_rise <- 20
  base$activity <- "OST"
  base$laying_quality_grade <- quality_grade
  base$standing_quality_grade <- quality_grade

  dates <- as.character(as.Date("2026-01-01") + 0:13)
  dplyr::bind_rows(lapply(dates, function(d) {
    base$date <- d
    base
  }))
}

test_that("hrv_trend_plot consumes current pipeline output (R14)", {
  metrics <- trend_metrics()

  # Character quality grades no longer collide with numeric metrics
  p_all <- expect_no_warning(hrv_trend_plot(metrics))
  expect_s3_class(p_all, "ggplot")

  # just_rssme mode also works with quality-grade columns present
  p_rssme <- expect_no_warning(hrv_trend_plot(metrics, just_rssme = TRUE))
  expect_s3_class(p_rssme, "ggplot")

  # Missing quality grades (NA) are handled
  p_na <- expect_no_warning(hrv_trend_plot(trend_metrics(quality_grade = NA_character_)))
  expect_s3_class(p_na, "ggplot")
})

test_that("hrv_trend_plot keeps provenance columns out of the facets (R14)", {
  metrics <- trend_metrics()
  p <- hrv_trend_plot(metrics)
  # Quality grades and provenance must not appear as pivoted metrics
  expect_false("quality_grade" %in% unique(p$data$metric))
  expect_false("file_digest" %in% names(p$data))
})

test_that("hrv_plot builds an RR plot from the extraction output (R14)", {
  skip_if_not_installed("FITfileR")

  tmp <- tempfile("hrvtest")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  fit_file <- file.path(tmp, "test1.fit")

  set.seed(7)
  hrv_s <- rep(0.8, 450) + rnorm(450, 0, 0.01)
  hr <- c(rep(60, 180), rep(85, 180))
  saveRDS(
    list(
      hrv = data.frame(time = hrv_s),
      record = data.frame(
        timestamp = seq.POSIXt(
          as.POSIXct("2025-01-01 08:00:00"),
          by = 1,
          length.out = 360
        ),
        heart_rate = hr
      ),
      session = data.frame(
        timestamp = as.POSIXct("2025-01-01 08:00:00"),
        total_elapsed_time = 360
      ),
      sport = data.frame(name = "OST")
    ),
    fit_file
  )

  with_mocked_bindings(
    readFitFile = function(path) {
      structure(readRDS(path), class = "FitFile")
    },
    hrv = function(fit_object) fit_object$hrv,
    records = function(fit_object) fit_object$record,
    getMessagesByType = function(fit_object, type) {
      if (type == "session") fit_object$session
      else if (type == "sport") fit_object$sport
    },
    .package = "FITfileR",
    {
      p <- expect_no_warning(hrv_plot(fit_file, base = "RR"))
      expect_s3_class(p, "ggplot")

      # The RR line layer contains the full beat series with a
      # seconds-based elapsed timeline
      line_data <- ggplot2::layer_data(p, 1)
      expect_true(nrow(line_data) >= 400)
      expect_true(max(line_data$x, na.rm = TRUE) > 300) # seconds, not ms
      expect_true(max(line_data$x, na.rm = TRUE) <= 400)
    }
  )
})
