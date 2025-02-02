test_that("calculate_hrv handles normal case correctly", {
  # Create simulated RR intervals with known properties
  set.seed(123)
  rr_intervals <- c(1.0, 1.1, 0.9, 1.2, 0.8, 1.0, 1.1, 0.9)

  result <- calculate_hrv(rr_intervals)

  # Test structure
  expect_type(result, "list")
  expect_named(result, c("rmssd", "sdnn"))

  # Test calculations
  # We can calculate expected values manually for comparison
  rr_ms <- rr_intervals * 1000
  expected_rmssd <- sqrt(mean(diff(rr_ms)^2))
  expected_sdnn <- sd(rr_ms)

  expect_equal(result$rmssd, round(expected_rmssd, 2))
  expect_equal(result$sdnn, round(expected_sdnn, 2))
})

test_that("calculate_hrv handles edge cases", {
  # Test with single value
  expect_equal(
    calculate_hrv(1.0),
    list(rmssd = NA_real_, sdnn = NA_real_)
  )

  # Test with empty vector
  expect_equal(
    calculate_hrv(numeric(0)),
    list(rmssd = NA_real_, sdnn = NA_real_)
  )

  # Test with NA values
  rr_with_na <- c(1.0, NA, 0.9, 1.1)
  result <- calculate_hrv(rr_with_na)
  expect_false(is.na(result$rmssd))
  expect_false(is.na(result$sdnn))
})

test_that("calculate_robust_ma works correctly", {
  # Test normal case
  x <- 1:10
  result <- calculate_robust_ma(x, window = 3)
  expect_equal(length(result), length(x))
  expect_equal(result[3], mean(1:3))

  # Test with missing values
  x_with_na <- c(1, 2, NA, 4, 5)
  result <- calculate_robust_ma(x_with_na, window = 4)
  expect_false(is.na(result[4])) # Should still calculate despite NA

  # Test minimum fraction requirement
  result <- calculate_robust_ma(x_with_na, window = 3, min_fraction = 0.9)
  expect_true(is.na(result[3])) # Should be NA due to high min_fraction
})

test_that("calculate_moving_averages processes data correctly", {
  # Create test data
  test_data <- data.frame(
    date = as.character(seq.Date(from = Sys.Date(), by = "day", length.out = 10)),
    laying_rmssd = rnorm(10, mean = 50, sd = 5),
    laying_resting_hr = rnorm(10, mean = 60, sd = 3),
    standing_hr = rnorm(10, mean = 80, sd = 5)
  )

  result <- calculate_moving_averages(test_data, window_size = 3)

  # Check structure
  expect_true(all(c(
    "rmssd_ma", "resting_hr_ma", "standing_hr_ma",
    "rmssd_change", "hr_change"
  ) %in% names(result)))

  # Check calculations
  expect_equal(nrow(result), nrow(test_data))
  expect_true(all(!is.na(result$rmssd_ma[3:10]))) # First 2 should be NA
})

test_that("calculate_resting_hr methods work correctly", {
  set.seed(1)
  # Create test heart rate data
  hr_data <- rnorm(60, mean = 65)

  # Test different methods
  last_30s <- calculate_resting_hr(hr_data, method = "last_30s")
  min_30s <- calculate_resting_hr(hr_data, method = "min_30s")
  lowest_sustained <- calculate_resting_hr(hr_data, method = "lowest_sustained")

  # Check results
  expect_true(all(!is.na(c(last_30s, min_30s, lowest_sustained))))
  expect_true(min_30s <= last_30s) # Min should be lowest

  # Test with unstable data
  unstable_hr <- rep(c(65, 55), 30)
  expect_warning(
    calculate_resting_hr(unstable_hr, method = "lowest_sustained"),
    "No stable windows found. Falling back to min_30s method."
  )
})

test_that("calculate_hrr handles recovery calculations correctly", {
  # Create test data
  standing_hr <- c(rep(100, 20), seq(100, 80, length.out = 40))
  baseline_hr <- 60

  result <- calculate_hrr(standing_hr, baseline_hr)

  # Check structure
  expect_named(result, c("hrr_60s", "hrr_relative", "orthostatic_rise"))

  # Check calculations
  expect_equal(result$orthostatic_rise, 100 - baseline_hr)
  expect_true(result$hrr_60s > 0)
  expect_true(result$hrr_relative >= 0 && result$hrr_relative <= 100)
})
