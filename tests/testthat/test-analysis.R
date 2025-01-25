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
  expect_equal(analyze_readiness(current_metrics, baseline_metrics)$status, "FRESH")

  # Create sample baseline metrics
  baseline_metrics <- data.frame(
    laying_rmssd = c(45, 47, 48, 50, 52, 48, 50),
    laying_resting_hr = c(65, 63, 62, 60, 58, 57, 60),
    orthostatic_rise = c(15, 18, 20, 22, 25, 28, 30)
  )

  # Check status
  expect_equal(analyze_readiness(current_metrics, baseline_metrics)$status, "NORMAL")

  # Create sample baseline metrics
  baseline_metrics <- data.frame(
    laying_rmssd = c(55, 55, 55, 55, 55, 55, 55),
    laying_resting_hr = c(65, 63, 62, 60, 58, 57, 60),
    orthostatic_rise = c(15, 18, 20, 22, 25, 28, 30)
  )

  # Check status
  expect_equal(analyze_readiness(current_metrics, baseline_metrics)$status, "CAUTION")

  # Create sample baseline metrics
  baseline_metrics <- data.frame(
    laying_rmssd = c(60, 60, 60, 60, 60, 60, 60),
    laying_resting_hr = c(65, 63, 62, 60, 58, 57, 60),
    orthostatic_rise = c(15, 18, 20, 22, 25, 28, 30)
  )

  # Check status
  expect_equal(analyze_readiness(current_metrics, baseline_metrics)$status, "WARNING")
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
  expect_true(all(c(
    "rmssd_score", "ortho_score", "hrr_score",
    "neural_recovery_score", "recovery_status"
  ) %in% names(result)))

  # Test most recent day's scores are within expected ranges
  recent_result <- result[nrow(result), ]
  expect_true(recent_result$rmssd_score >= 0 && recent_result$rmssd_score <= 40)
  expect_true(recent_result$ortho_score >= 0 && recent_result$ortho_score <= 30)
  expect_true(recent_result$hrr_score >= 0 && recent_result$hrr_score <= 30)
  expect_true(recent_result$neural_recovery_score >= 0 && recent_result$neural_recovery_score <= 100)

  # Test recovery status classification
  expect_true(recent_result$recovery_status %in%
    c("Fresh", "Good", "Normal", "Reduced", "Low"))
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
