library(testthat)
library(tibble)

# Helper function to create simulated RR intervals
create_test_rr <- function(n_intervals, mean_rr = 800, variation = 0.1, 
                          anomaly_rate = 0, anomaly_multiplier = 2.0) {
  rr <- rnorm(n_intervals, mean = mean_rr, sd = variation * mean_rr)
  if (anomaly_rate > 0) {
    n_anomalies <- floor(n_intervals * anomaly_rate)
    anomaly_positions <- sample(n_intervals, n_anomalies)
    rr[anomaly_positions] <- rr[anomaly_positions] * anomaly_multiplier
  }
  pmax(rr, 100)  # Ensure positive values
}

# Test filter_physiological_rr function
test_that("filter_physiological_rr filters correctly", {
  rr_data <- c(100, 300, 800, 900, 1000, 2500, 3000)  # Mix of valid and invalid
  
  result <- filter_physiological_rr(rr_data, min_rr = 272, max_rr = 2000)
  
  expect_type(result, "logical")
  expect_length(result, length(rr_data))
  expect_equal(result, c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("filter_physiological_rr handles edge cases", {
  # Empty input
  expect_equal(filter_physiological_rr(numeric(0)), logical(0))
  
  # All valid
  rr_data <- c(800, 900, 1000)
  result <- filter_physiological_rr(rr_data)
  expect_true(all(result))
  
  # All invalid
  rr_data <- c(100, 200, 3000)
  result <- filter_physiological_rr(rr_data)
  expect_false(any(result))
  
  # With NA values
  rr_data <- c(800, NA, 900, 1000)
  result <- filter_physiological_rr(rr_data)
  expect_equal(result, c(TRUE, FALSE, TRUE, TRUE))
})

test_that("filter_physiological_rr validates parameters", {
  rr_data <- c(800, 900, 1000)
  
  # Non-numeric input
  expect_error(
    filter_physiological_rr("not_numeric"),
    "rr_intervals must be numeric"
  )
  
  # Invalid min_rr
  expect_error(
    filter_physiological_rr(rr_data, min_rr = -100),
    "min_rr must be a single positive number"
  )
  
  # Invalid max_rr
  expect_error(
    filter_physiological_rr(rr_data, max_rr = 0),
    "max_rr must be a single positive number"
  )
  
  # min_rr >= max_rr
  expect_error(
    filter_physiological_rr(rr_data, min_rr = 1000, max_rr = 500),
    "min_rr must be less than max_rr"
  )
})

# Test detect_rr_artifacts function
test_that("detect_rr_artifacts identifies artifacts correctly", {
  # Create data with a clear artifact
  rr_data <- c(800, 850, 820, 400, 830, 810, 840)  # 400 is an artifact
  
  result <- detect_rr_artifacts(rr_data, window_size = 5, threshold = 0.2)
  
  expect_type(result, "logical")
  expect_length(result, length(rr_data))
  expect_false(result[4])  # The artifact should be detected
})

test_that("detect_rr_artifacts handles edge cases", {
  # Empty input
  expect_equal(detect_rr_artifacts(numeric(0)), logical(0))
  
  # Single value
  result <- detect_rr_artifacts(800)
  expect_equal(result, TRUE)
  
  # Small dataset (less than window size)
  rr_data <- c(800, 850, 820)
  result <- detect_rr_artifacts(rr_data, window_size = 5)
  expect_true(all(result))
  
  # Data with NA values
  rr_data <- c(800, NA, 850, 820)
  result <- detect_rr_artifacts(rr_data, window_size = 3)
  expect_equal(result[2], FALSE)  # NA should be marked as invalid
})

test_that("detect_rr_artifacts validates parameters", {
  rr_data <- c(800, 850, 820, 830, 810)
  
  # Non-numeric input
  expect_error(
    detect_rr_artifacts("not_numeric"),
    "rr_intervals must be numeric"
  )
  
  # Invalid window_size
  expect_error(
    detect_rr_artifacts(rr_data, window_size = 0),
    "window_size must be a positive integer"
  )
  
  # Invalid threshold
  expect_error(
    detect_rr_artifacts(rr_data, threshold = -0.1),
    "threshold must be a number between 0 and 1"
  )
  
  expect_error(
    detect_rr_artifacts(rr_data, threshold = 1.5),
    "threshold must be a number between 0 and 1"
  )
  
  # Invalid centered_window
  expect_error(
    detect_rr_artifacts(rr_data, centered_window = "invalid"),
    "centered_window must be a single logical value"
  )
})

test_that("detect_rr_artifacts centered vs non-centered windows", {
  # Create data with an artifact
  rr_data <- c(800, 850, 820, 400, 830, 810, 840)
  
  result_centered <- detect_rr_artifacts(rr_data, window_size = 5, centered_window = TRUE)
  result_backward <- detect_rr_artifacts(rr_data, window_size = 5, centered_window = FALSE)
  
  expect_type(result_centered, "logical")
  expect_type(result_backward, "logical")
  expect_length(result_centered, length(rr_data))
  expect_length(result_backward, length(rr_data))
  
  # Both should detect the artifact at position 4
  expect_false(result_centered[4])
  expect_false(result_backward[4])
})

# Test preprocess_rr_intervals function
test_that("preprocess_rr_intervals applies complete pipeline", {
  # Create test data with physiological outliers and artifacts
  rr_data <- c(100, 800, 850, 400, 900, 2500, 820, 830)
  
  result <- preprocess_rr_intervals(rr_data, min_rr = 272, max_rr = 2000)
  
  expect_type(result, "list")
  expect_named(result, c("cleaned_rr", "valid_mask", "n_removed_physiological", 
                        "n_removed_artifacts", "n_original", "n_final"))
  
  expect_equal(result$n_original, 8)
  expect_equal(result$n_removed_physiological, 2)  # 100 and 2500
  expect_true(result$n_removed_artifacts >= 0)
  expect_equal(length(result$cleaned_rr), result$n_final)
  expect_equal(length(result$valid_mask), result$n_original)
})

test_that("preprocess_rr_intervals handles empty input", {
  result <- preprocess_rr_intervals(numeric(0))
  
  expect_equal(result$n_original, 0)
  expect_equal(result$n_final, 0)
  expect_equal(result$n_removed_physiological, 0)
  expect_equal(result$n_removed_artifacts, 0)
  expect_equal(length(result$cleaned_rr), 0)
  expect_equal(length(result$valid_mask), 0)
})

test_that("preprocess_rr_intervals validates input", {
  expect_error(
    preprocess_rr_intervals("not_numeric"),
    "rr_intervals must be numeric"
  )
})

test_that("preprocess_rr_intervals preserves valid data", {
  # Create data with only valid intervals
  rr_data <- c(800, 850, 820, 830, 810, 840, 825)
  
  result <- preprocess_rr_intervals(rr_data)
  
  expect_equal(result$n_removed_physiological, 0)
  expect_equal(result$n_removed_artifacts, 0)
  expect_equal(result$n_final, result$n_original)
  expect_equal(result$cleaned_rr, rr_data)
  expect_true(all(result$valid_mask))
})

test_that("preprocess_rr_intervals diagnostic information is accurate", {
  # Create data with known issues
  rr_data <- c(100, 800, 850, 400, 900, 2500, 820, 830)  # 2 physiological, 1 potential artifact
  
  result <- preprocess_rr_intervals(rr_data, min_rr = 272, max_rr = 2000)
  
  # Check that diagnostic counts are reasonable
  expect_equal(result$n_original, 8)
  expect_equal(result$n_removed_physiological, 2)  # 100 and 2500
  expect_equal(result$n_removed_physiological + result$n_removed_artifacts + result$n_final, 
               result$n_original)
})