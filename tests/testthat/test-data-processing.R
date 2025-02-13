library(mockery)
library(FITfileR)
library(dplyr)
library(testthat)

# Helper function to create temporary directory (simplified)
create_test_environment <- function(hrv_data1 = NULL, hr_data1 = NULL, hrv_data2 = NULL, hr_data2 = NULL) {
  temp_dir <- tempfile("hrvtest")
  dir.create(temp_dir)

  # Create mock FIT file data for test1.fit
  fit_data1 <- create_mock_fit_data(hrv_data = hrv_data1, hr_data = hr_data1, session_time = as.POSIXct("2025-01-01 08:00:00"))
  saveRDS(fit_data1, file.path(temp_dir, "test1.fit")) # Save as RDS, not FIT

  # Create mock FIT file data for test2.fit
  fit_data2 <- create_mock_fit_data(hrv_data = hrv_data2, hr_data = hr_data2, session_time = as.POSIXct("2025-01-02 08:00:00"))
  saveRDS(fit_data2, file.path(temp_dir, "test2.fit")) # Save as RDS, not FIT

  return(temp_dir)
}

create_mock_fit_data <- function(
    hrv_data = NULL,
    hr_data = NULL,
    session_time = Sys.time(),
    sport_name = "Orthostatic") {
  list(
    hrv = if (!is.null(hrv_data)) data.frame(time = hrv_data) else data.frame(),
    record = if (!is.null(hr_data)) {
      data.frame(
        timestamp = seq.POSIXt(from = session_time, by = 1, length.out = length(hr_data)),
        heart_rate = hr_data
      )
    } else {
      data.frame()
    },
    session = data.frame(
      timestamp = session_time,
      total_elapsed_time = if (!is.null(hr_data)) length(hr_data) else 0,
      stringsAsFactors = FALSE
    ),
    sport = data.frame(name = sport_name, stringsAsFactors = FALSE)
  )
}

# Helper function to create simulated RR intervals (kept as before)
create_simulated_rr <- function(
    n_intervals,
    mean_rr = 1.0,
    variation = 0.1,
    anomaly_rate = 0) {
  rr <- rnorm(n_intervals, mean = mean_rr, sd = variation * mean_rr)
  if (anomaly_rate > 0) {
    n_anomalies <- floor(n_intervals * anomaly_rate)
    anomaly_positions <- sample(n_intervals, n_anomalies)
    rr[anomaly_positions] <- rr[anomaly_positions] * runif(n_anomalies, min = 1.5, max = 2.0)
  }
  pmax(rr, 0.1) # Ensure positive
}


test_that("extract_rr_data handles normal case correctly", {
  set.seed(1)
  laying_rr <- create_simulated_rr(180, mean_rr = 1.0, variation = 0.1)
  standing_rr <- create_simulated_rr(180, mean_rr = 0.8, variation = 0.05)
  hrv_data <- c(laying_rr, standing_rr)

  # Create a *list* representing the FitFile object's data
  fit_object <- list(data = list(hrv = data.frame(time = hrv_data)))

  local_mocked_bindings(
    "validate_fit_object" = function(x) TRUE
  )
  local_mocked_bindings(
    hrv = function(object) object$data$hrv, # Empty list
    .package = "FITfileR"
  )

  result <- extract_rr_data(fit_object)

  expect_type(result, "list")
  expect_named(result, c("time"))
  expect_true(length(result$time) > 0)
})

test_that("extract_rr_data handles missing data correctly", {
  # Create an empty list to represent a FitFile with no HRV data
  fit_object <- list(data = list(hrv = data.frame()))

  local_mocked_bindings(
    hrv = function(object) object$data$hrv, # Empty list
    .package = "FITfileR"
  )
  local_mocked_bindings(
    "validate_fit_object" = function(x) TRUE
  )

  # No mocking needed, we're directly testing the function's behavior
  suppressWarnings(result <- extract_rr_data(fit_object)) # Expect a warning

  expect_type(result, "list")
  expect_length(result$time, 0)
})


test_that("extract_rr_data validates inputs correctly", {
  # Create valid base data
  valid_data <- create_simulated_rr(360, mean_rr = 1.0, variation = 0.1)

  # Test invalid object -- passing a simple list WITHOUT a 'data' element
  expect_error(
    extract_rr_data(list()),
    "fit_object must be a FitFile object"
  )

  local_mocked_bindings(
    hrv = function(object) object$data$hrv, # Empty list
    .package = "FITfileR"
  )
  local_mocked_bindings(
    "validate_fit_object" = function(x) TRUE
  )
  # Passing a list with a data element, but with invalid content. hrv is expected.
  expect_warning(
    extract_rr_data(list(data = list(something_else = 1))),
    "No HRV data found in FIT file"
  )
})


test_that("process_fit_file handles file processing correctly", {
  expect_error(
    process_fit_file("empty.fit"),
    "File does not exist: empty.fit"
  )
})


test_that("create_empty_result creates proper structure", {
  result <- create_empty_result("test.fit", Sys.Date(), 1, "Morning")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_type(result$laying_rmssd, "double")
  expect_type(result$laying_sdnn, "double")
  expect_type(result$activity, "character")
  expect_true(all(is.na(result$laying_rmssd)))
  expect_true(all(is.na(result$standing_hr)))
})

test_that("process_fit_directory handles file system errors", {
  # Test with non-existent directory
  expect_error(
    process_fit_directory("/nonexistent/directory"),
    "Directory.*does not exist"
  )

  # Test with unwriteable directory. Only run on Unix-like systems.
  if (.Platform$OS.type == "unix") {
    temp_dir <- create_test_environment()
    on.exit(unlink(temp_dir, recursive = TRUE))

    cache_dir <- file.path(temp_dir, "readonly")
    dir.create(cache_dir)
    Sys.chmod(cache_dir, mode = "444") # Read-only

    expect_error(
      process_fit_directory(cache_dir),
      "No write permission for cache directory"
    )
  }
})

test_that("process_fit_directory validates input parameters", {
  temp_dir <- create_test_environment()
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Test invalid clear_cache
  expect_error(
    process_fit_directory(temp_dir, clear_cache = "invalid"),
    "clear_cache must be a single logical value"
  )
})
