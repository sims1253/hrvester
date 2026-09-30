test_that("analyze_readiness returns correct status", {
  # Create sample current metrics
  current_metrics <- data.frame(
    laying_rmssd = 50,
    laying_resting_hr = 60,
    orthostatic_rise = 20
  )

  # Create sample baseline metrics
  baseline_metrics <- data.frame(
    laying_rmssd = c(45, 47, 48, 47, 52, 48, 44),
    laying_resting_hr = c(65, 63, 62, 60, 58, 57, 60),
    orthostatic_rise = c(15, 18, 20, 22, 25, 28, 30)
  )

  # Check status
  expect_equal(
    analyze_readiness(current_metrics, baseline_metrics)$status,
    "FRESH"
  )

  # Create sample baseline metrics
  baseline_metrics <- data.frame(
    laying_rmssd = c(45, 47, 48, 50, 52, 48, 50),
    laying_resting_hr = c(65, 63, 62, 60, 58, 57, 60),
    orthostatic_rise = c(15, 18, 20, 22, 25, 28, 30)
  )

  # Check status
  expect_equal(
    analyze_readiness(current_metrics, baseline_metrics)$status,
    "NORMAL"
  )

  # Create sample baseline metrics
  baseline_metrics <- data.frame(
    laying_rmssd = c(55, 55, 55, 55, 55, 55, 55),
    laying_resting_hr = c(65, 63, 62, 60, 58, 57, 60),
    orthostatic_rise = c(15, 18, 20, 22, 25, 28, 30)
  )

  # Check status
  expect_equal(
    analyze_readiness(current_metrics, baseline_metrics)$status,
    "CAUTION"
  )

  # Create sample baseline metrics
  baseline_metrics <- data.frame(
    laying_rmssd = c(60, 60, 60, 60, 60, 60, 60),
    laying_resting_hr = c(65, 63, 62, 60, 58, 57, 60),
    orthostatic_rise = c(15, 18, 20, 22, 25, 28, 30)
  )

  # Check status
  expect_equal(
    analyze_readiness(current_metrics, baseline_metrics)$status,
    "WARNING"
  )
})

test_that("analyze_readiness handles edge cases", {
  # Test with minimal baseline data
  current_metrics <- data.frame(
    laying_rmssd = 40,
    laying_resting_hr = 70,
    orthostatic_rise = 10
  )

  baseline_metrics <- data.frame(
    laying_rmssd = rep(40, 7),
    laying_resting_hr = rep(70, 7),
    orthostatic_rise = rep(10, 7)
  )

  result <- analyze_readiness(current_metrics, baseline_metrics)

  expect_equal(result$status, "NORMAL")
})

test_that("analyze_readiness validates input correctly", {
  # Test invalid current metrics
  expect_error(analyze_readiness(data.frame(), baseline_metrics = data.frame()))

  # Test invalid baseline metrics
  expect_warning(analyze_readiness(
    current_metrics = data.frame(
      laying_rmssd = 50,
      laying_resting_hr = 60,
      orthostatic_rise = 20
    ),
    baseline_metrics = data.frame(
      laying_rmssd = 1:2,
      laying_resting_hr = 1:2,
      orthostatic_rise = 1:2
    )
  ))
})

test_that("calculate_neural_recovery produces expected scores", {
  # Create test data with enough history for moving averages
  dates <- seq(as.Date("2025-01-01"), by = "day", length.out = 14)
  test_data <- data.frame(
    date = dates,
    laying_rmssd = c(rep(50, 7), 50, 45, 55, 40, 60, 48, 52), # Last 7 days vary
    laying_resting_hr = c(rep(60, 7), 60, 65, 58, 70, 62, 59, 61),
    standing_hr = c(rep(85, 7), 85, 90, 80, 95, 88, 83, 86),
    hrr_60s = c(rep(25, 7), 25, 20, 15, 10, 22, 24, 23)
  )

  # Run calculation
  result <- calculate_neural_recovery(test_data)

  # Test output structure
  expect_true(all(
    c(
      "rmssd_score",
      "ortho_score",
      "hrr_score",
      "neural_recovery_score",
      "recovery_status"
    ) %in%
      names(result)
  ))

  # Test most recent day's scores are within expected ranges
  recent_result <- result[nrow(result), ]
  expect_true(recent_result$rmssd_score >= 0 && recent_result$rmssd_score <= 40)
  expect_true(recent_result$ortho_score >= 0 && recent_result$ortho_score <= 30)
  expect_true(recent_result$hrr_score >= 0 && recent_result$hrr_score <= 30)
  expect_true(
    recent_result$neural_recovery_score >= 0 &&
      recent_result$neural_recovery_score <= 100
  )

  # Test recovery status classification
  expect_true(
    recent_result$recovery_status %in%
      c("Fresh", "Good", "Normal", "Reduced", "Low")
  )
})

test_that("calculate_neural_recovery handles edge cases", {
  # Create baseline data for moving averages
  baseline_dates <- seq(as.Date("2025-01-01"), by = "day", length.out = 7)
  baseline_data <- data.frame(
    date = baseline_dates,
    laying_rmssd = rep(50, 7),
    laying_resting_hr = rep(60, 7),
    standing_hr = rep(85, 7),
    hrr_60s = rep(25, 7)
  )

  # Test with minimum values
  min_dates <- seq(as.Date("2025-01-08"), by = "day", length.out = 3)
  min_data <- rbind(
    baseline_data,
    data.frame(
      date = min_dates,
      laying_rmssd = rep(1, 3),
      laying_resting_hr = rep(40, 3),
      standing_hr = rep(40, 3),
      hrr_60s = rep(1, 3)
    )
  )

  min_result <- calculate_neural_recovery(min_data)
  expect_true(min_result$neural_recovery_score[nrow(min_result)] >= 0)
  expect_equal(min_result$recovery_status[nrow(min_result)], "Low")

  # Test with maximum values
  max_dates <- seq(as.Date("2025-01-08"), by = "day", length.out = 3)
  max_data <- rbind(
    baseline_data,
    data.frame(
      date = max_dates,
      laying_rmssd = rep(200, 3),
      laying_resting_hr = rep(100, 3),
      standing_hr = rep(120, 3),
      hrr_60s = rep(40, 3)
    )
  )

  max_result <- calculate_neural_recovery(max_data)
  expect_true(max_result$neural_recovery_score[nrow(max_result)] <= 100)
  expect_equal(max_result$recovery_status[nrow(max_result)], "Fresh")
})

test_that("calculate_neural_recovery validates input correctly", {
  # Test missing columns
  invalid_data <- data.frame(
    date = as.Date("2025-01-01"),
    laying_rmssd = 50,
    laying_resting_hr = 60
  )

  expect_error(calculate_neural_recovery(invalid_data))

  # Test negative values
  negative_data <- data.frame(
    date = as.Date("2025-01-01"),
    laying_rmssd = -50,
    laying_resting_hr = 60,
    standing_hr = 85,
    hrr_60s = 25
  )

  expect_error(calculate_neural_recovery(negative_data))

  # Test negative window
  test_data <- data.frame(
    date = seq(as.Date("2025-01-01"), by = "day", length.out = 14),
    laying_rmssd = c(rep(50, 7), 50, 45, 55, 40, 60, 48, 52), # Last 7 days vary
    laying_resting_hr = c(rep(60, 7), 60, 65, 58, 70, 62, 59, 61),
    standing_hr = c(rep(85, 7), 85, 90, 80, 95, 88, 83, 86),
    hrr_60s = c(rep(25, 7), 25, 20, 15, 10, 22, 24, 23)
  )

  expect_error(calculate_neural_recovery(test_data, window_size = -7))
})

test_that("training_recommendations provides appropriate advice", {
  # Test different score ranges
  expect_equal(
    training_recommendations(85, "BJJ")$status,
    "Fresh"
  )

  expect_equal(
    training_recommendations(75, "BJJ")$status,
    "Good"
  )

  expect_equal(
    training_recommendations(60, "BJJ")$status,
    "Normal"
  )

  expect_equal(
    training_recommendations(45, "BJJ")$status,
    "Reduced"
  )

  expect_equal(
    training_recommendations(30, "BJJ")$status,
    "Low"
  )

  # Test BJJ-specific recommendations
  bjj_rec <- training_recommendations(85, "BJJ")
  expect_true(!is.null(bjj_rec$bjj_specific))

  # Test strength-specific recommendations
  strength_rec <- training_recommendations(85, "STRENGTH")
  expect_true(is.null(strength_rec$bjj_specific))
})

test_that("training_recommendations validates input", {
  # Test invalid scores
  expect_error(training_recommendations(-10, "BJJ"))
  expect_error(training_recommendations(110, "BJJ"))
  expect_error(training_recommendations("invalid", "BJJ"))

  # Test invalid training type
  expect_error(training_recommendations(85, "INVALID"))
  expect_error(training_recommendations(85, 123))
})

test_that("calculate_trend_direction works correctly", {
  # Test increasing trend
  increasing_values <- c(1, 2, 3, 4, 5)
  expect_equal(
    calculate_trend_direction(increasing_values),
    "Strong Increase"
  )

  # Test decreasing trend
  decreasing_values <- c(5, 4, 3, 2, 1)
  expect_equal(
    calculate_trend_direction(decreasing_values),
    "Strong Decrease"
  )

  # Test stable trend
  stable_values <- c(3, 3.1, 2.9, 3, 3.1)
  expect_equal(
    calculate_trend_direction(stable_values),
    "Stable"
  )

  # Test input validation
  expect_error(calculate_trend_direction(numeric(0)))
  expect_error(calculate_trend_direction(c(1, NA, NA)))
})

test_that("generate_daily_report produces expected format", {
  # Create test data with all required fields and history
  dates <- seq(as.Date("2025-01-01"), by = "day", length.out = 14)
  test_data <- data.frame(
    date = dates,
    laying_rmssd = c(rep(50, 7), 50, 45, 55, 40, 60, 48, 52),
    laying_resting_hr = c(rep(60, 7), 60, 65, 58, 70, 62, 59, 61),
    standing_hr = c(rep(85, 7), 85, 90, 80, 95, 88, 83, 86),
    orthostatic_rise = c(rep(20, 7), 22, 25, 18, 28, 24, 21, 23),
    hrr_60s = c(rep(25, 7), 25, 20, 15, 10, 22, 24, 23),
    time_of_day = rep("Morning", 14)
  )

  # Generate report

  report <- generate_daily_report(test_data)

  # Test report structure
  expect_true(grepl("HRV Status Report for", report))
  expect_true(grepl("Current Metrics:", report))
  expect_true(grepl("Recommendations:", report))
  expect_true(grepl("7-Day Trends:", report))

  # Test input validation
  expect_error(generate_daily_report(test_data[1:7, ]))

  # Test with missing required columns
  invalid_data <- test_data[, !names(test_data) %in% c("orthostatic_rise")]
  expect_warning(expect_error(generate_daily_report(invalid_data)))
})

# ============== Review regression tests (R13, R16) ==============

test_that("generate_daily_report accepts pipeline output: character dates and fractional HR (R13)", {
  # process_fit_file()/cache emit dates as character; resting HR may be
  # fractional, which %d formatting cannot print
  d <- data.frame(
    date = as.character(as.Date("2026-01-01") + 0:7),
    laying_rmssd = 50,
    laying_resting_hr = 60.5,
    orthostatic_rise = 20,
    standing_hr = 80,
    hrr_60s = 25,
    time_of_day = "Morning"
  )

  report <- expect_no_warning(generate_daily_report(d))
  expect_true(grepl("HRV Status Report for", report))
  expect_true(grepl("60.5", report)) # fractional HR rendered, not an error

  # Mixed Date and character inputs both work
  d2 <- d
  d2$date <- as.Date(d2$date)
  expect_no_error(generate_daily_report(d2))

  # Invalid dates are rejected explicitly
  d3 <- d
  d3$date[1] <- "not-a-date"
  expect_error(generate_daily_report(d3), "invalid dates")
})

test_that("generate_daily_report requires a usable baseline (R13)", {
  d <- data.frame(
    date = as.character(as.Date("2026-01-01") + 0:7),
    laying_rmssd = c(rep(NA_real_, 7), 50),
    laying_resting_hr = 60,
    orthostatic_rise = 20,
    standing_hr = 80,
    hrr_60s = 25,
    time_of_day = "Morning"
  )
  # Eight rows exist but no usable baseline day within the last 7 days
  expect_error(
    generate_daily_report(d),
    "At least two usable baseline measurements are required"
  )

  # A single usable baseline day also fails: the 7-day trend needs two
  # points
  d2 <- d
  d2$laying_rmssd[2] <- 50
  expect_error(
    generate_daily_report(d2),
    "At least two usable baseline measurements are required"
  )
})

test_that("calculate_neural_recovery reports insufficient data instead of Low (R16)", {
  d <- data.frame(
    date = as.Date("2026-01-01") + 0:13,
    laying_rmssd = 50,
    laying_resting_hr = 60,
    standing_hr = 75,
    hrr_60s = 25
  )
  x <- calculate_neural_recovery(d)

  # Startup rows without a usable moving average
  expect_true(all(is.na(x$neural_recovery_score[1:4])))
  expect_true(all(x$recovery_status[1:4] == "Insufficient data"))

  # Once the baseline exists, real scores are classified
  expect_false(is.na(x$neural_recovery_score[14]))
  expect_true(x$recovery_status[14] %in%
    c("Fresh", "Good", "Normal", "Reduced", "Low"))

  # A missing component never yields a complete-looking score
  d$standing_hr[14] <- NA
  x2 <- calculate_neural_recovery(d)
  expect_true(is.na(x2$neural_recovery_score[14]))
  expect_equal(x2$recovery_status[14], "Insufficient data")

  d3 <- d
  d3$standing_hr[14] <- 75
  d3$hrr_60s[14] <- NA
  x3 <- calculate_neural_recovery(d3)
  expect_true(is.na(x3$neural_recovery_score[14]))
  expect_equal(x3$recovery_status[14], "Insufficient data")
})

test_that("training_recommendations handles NA scores explicitly (R16)", {
  rec <- training_recommendations(NA)
  expect_equal(rec$status, "Insufficient data")
  expect_false(is.null(rec$focus))
  expect_true(is.na(rec$score))

  # Numeric out-of-range values still error
  expect_error(training_recommendations(150))
  expect_error(training_recommendations("not numeric"))
})

test_that("analyze_readiness flags missing baselines explicitly (R16)", {
  current <- data.frame(
    laying_rmssd = 50,
    laying_resting_hr = 60,
    orthostatic_rise = 20
  )
  baseline <- data.frame(
    laying_rmssd = c(rep(50, 6), NA),
    laying_resting_hr = rep(60, 7),
    orthostatic_rise = rep(20, 7)
  )
  result <- analyze_readiness(current, baseline)
  expect_equal(result$status, "NORMAL")

  # A wholly NA baseline component produces an explicit status, not a
  # silent WARNING and not an error
  baseline_na <- data.frame(
    laying_rmssd = rep(NA_real_, 7),
    laying_resting_hr = rep(60, 7),
    orthostatic_rise = rep(20, 7)
  )
  result_na <- analyze_readiness(current, baseline_na)
  expect_equal(result_na$status, "INSUFFICIENT_DATA")
})

test_that("generate_daily_report gate matches the trend input with duplicate days (R13)", {
  # Two rows per date (Morning/Evening): the last 7 rows can carry fewer
  # usable values than the window as a whole (analyze_readiness warns about
  # the 14-row baseline, which is expected here)
  mk <- function(rmssd) data.frame(
    date = rep(as.character(as.Date("2026-01-01") + 0:7), each = 2),
    laying_rmssd = rmssd,
    laying_resting_hr = 60,
    orthostatic_rise = 20,
    standing_hr = 80,
    hrr_60s = 25,
    time_of_day = rep(c("Morning", "Evening"), 8)
  )

  # Exactly one usable baseline value overall: the gate must reject
  d <- mk(c(50, rep(NA_real_, 13), 55, NA))
  expect_error(
    suppressWarnings(generate_daily_report(d)),
    "At least two usable baseline measurements are required"
  )

  # Two usable values early in the window: a naive tail(7) would hold none,
  # the NA-trimmed gate accepts
  d2 <- mk(c(50, 52, rep(NA_real_, 12), 55, NA))
  expect_no_error(suppressWarnings(generate_daily_report(d2)))
})
