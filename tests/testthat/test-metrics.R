test_that("calculate_hrv handles normal case correctly", {
  # Create simulated RR intervals with known properties
  set.seed(123)
  rr_intervals <- c(1000, 1100, 900, 1200, 800, 1000, 1100, 900)

  result <- calculate_hrv(rr_intervals)

  # Test structure
  expect_type(result, "list")
  expect_named(result, c("rmssd", "sdnn"))

  # Test calculations
  # We can calculate expected values manually for comparison
  expected_rmssd <- sqrt(mean(diff(rr_intervals)^2))
  expected_sdnn <- sd(rr_intervals)

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
    date = as.character(seq.Date(
      from = Sys.Date(),
      by = "day",
      length.out = 10
    )),
    laying_rmssd = rnorm(10, mean = 50, sd = 5),
    laying_resting_hr = rnorm(10, mean = 60, sd = 3),
    standing_hr = rnorm(10, mean = 80, sd = 5)
  )

  result <- calculate_moving_averages(test_data, window_size = 3)

  # Check structure
  expect_true(all(
    c(
      "rmssd_ma",
      "resting_hr_ma",
      "standing_hr_ma",
      "rmssd_change",
      "hr_change"
    ) %in%
      names(result)
  ))

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

# ============== Review regression tests (R08) ==============

test_that("calculate_hrv treats milliseconds as the canonical unit (R08)", {
  # Extraction output is milliseconds; input and output units match
  out_ms <- calculate_hrv(rep(800, 100))
  expect_true(out_ms$rmssd >= 0)

  # Known-value check: successive differences of exactly 20 ms give RMSSD 20
  beats <- seq(800, 1200, by = 20) # in milliseconds
  expect_equal(calculate_hrv(beats)$rmssd, 20)
  expect_equal(calculate_hrv(beats / 1000)$rmssd, 0.02) # seconds in, seconds out
})

test_that("calculate_hrr selects peak and 60 s values by timestamp (R08/R02)", {
  times <- 0:60
  hr <- c(rep(60, 5), 100, rep(80, 24), rep(85, 30), 70)

  # Peak must come from the first 20 s; the 60 s value is the last sample
  # within 60 s (70 bpm at t = 60), not the 60th element
  res <- calculate_hrr(hr, baseline_hr = 60, times = times)
  expect_equal(res$orthostatic_rise, 40)
  expect_equal(res$hrr_60s, 30)
  expect_equal(res$hrr_relative, 75) # (100 - 70) / (100 - 60) * 100

  # Insufficient time coverage returns NA explicitly
  res_short <- calculate_hrr(hr[1:10], baseline_hr = 60, times = times[1:10])
  expect_true(is.na(res_short$hrr_60s))
})
