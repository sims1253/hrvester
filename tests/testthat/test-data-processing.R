library(mockery)
library(FITfileR)
library(dplyr)
library(testthat)

# Helper function to create temporary directory (simplified)
create_test_environment <- function(
  hrv_data1 = NULL,
  hr_data1 = NULL,
  hrv_data2 = NULL,
  hr_data2 = NULL
) {
  temp_dir <- tempfile("hrvtest")
  dir.create(temp_dir)

  # Create mock FIT file data for test1.fit
  fit_data1 <- create_mock_fit_data(
    hrv_data = hrv_data1,
    hr_data = hr_data1,
    session_time = as.POSIXct("2025-01-01 08:00:00")
  )
  saveRDS(fit_data1, file.path(temp_dir, "test1.fit")) # Save as RDS, not FIT

  # Create mock FIT file data for test2.fit
  fit_data2 <- create_mock_fit_data(
    hrv_data = hrv_data2,
    hr_data = hr_data2,
    session_time = as.POSIXct("2025-01-02 08:00:00")
  )
  saveRDS(fit_data2, file.path(temp_dir, "test2.fit")) # Save as RDS, not FIT

  return(temp_dir)
}

create_mock_fit_data <- function(
  hrv_data = NULL,
  hr_data = NULL,
  session_time = Sys.time(),
  sport_name = "Orthostatic"
) {
  list(
    hrv = if (!is.null(hrv_data)) data.frame(time = hrv_data) else data.frame(),
    record = if (!is.null(hr_data)) {
      data.frame(
        timestamp = seq.POSIXt(
          from = session_time,
          by = 1,
          length.out = length(hr_data)
        ),
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
  anomaly_rate = 0
) {
  rr <- rnorm(n_intervals, mean = mean_rr, sd = variation * mean_rr)
  if (anomaly_rate > 0) {
    n_anomalies <- floor(n_intervals * anomaly_rate)
    anomaly_positions <- sample(n_intervals, n_anomalies)
    rr[anomaly_positions] <- rr[anomaly_positions] *
      runif(n_anomalies, min = 1.5, max = 2.0)
  }
  pmax(rr, 0.1) # Ensure positive
}


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

# Tests for quality threshold functionality (Task 1.4)
test_that("process_fit_file accepts min_quality_threshold parameter", {
  # Test that the function accepts the new parameter without error
  expect_error(
    process_fit_file("nonexistent.fit", min_quality_threshold = 0.7),
    "File does not exist: nonexistent.fit" # Should fail on file existence, not parameter
  )

  # Test parameter validation
  expect_error(
    process_fit_file("nonexistent.fit", min_quality_threshold = "invalid"),
    "min_quality_threshold must be numeric"
  )

  expect_error(
    process_fit_file("nonexistent.fit", min_quality_threshold = -0.1),
    "min_quality_threshold must be between 0 and 1"
  )

  expect_error(
    process_fit_file("nonexistent.fit", min_quality_threshold = 1.5),
    "min_quality_threshold must be between 0 and 1"
  )
})

test_that("process_fit_directory accepts min_quality_threshold parameter", {
  # Create empty directory for parameter validation test
  temp_dir <- tempfile("hrvtest")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Should accept the parameter and handle empty directory gracefully
  expect_no_error({
    result <- suppressWarnings(process_fit_directory(
      temp_dir,
      min_quality_threshold = 0.8
    ))
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0) # No files to process
  })

  # Test parameter validation
  expect_error(
    process_fit_directory(temp_dir, min_quality_threshold = "invalid"),
    "min_quality_threshold must be numeric"
  )

  expect_error(
    process_fit_directory(temp_dir, min_quality_threshold = 2.0),
    "min_quality_threshold must be between 0 and 1"
  )
})

# =================== Task 1.4: Quality Filtering Integration Tests ================

test_that("process_fit_file includes quality metrics in output", {
  # Skip if FITfileR is not available
  skip_if_not_installed("FITfileR")

  # Create test data with some artifacts
  test_hrv <- create_simulated_rr(
    300,
    mean_rr = 0.8,
    variation = 0.1,
    anomaly_rate = 0.05
  )
  test_hr <- rep(60, 360) # 6 minutes of HR data

  temp_dir <- create_test_environment(hrv_data1 = test_hrv, hr_data1 = test_hr)
  on.exit(unlink(temp_dir, recursive = TRUE))

  fit_file <- file.path(temp_dir, "test1.fit")

  # Mock all the necessary FITfileR functions for this test
  with_mocked_bindings(
    readFitFile = function(path) {
      mock_data <- readRDS(path)
      structure(mock_data, class = "FitFile")
    },
    hrv = function(fit_object) {
      fit_object$hrv
    },
    records = function(fit_object) {
      fit_object$record
    },
    getMessagesByType = function(fit_object, type) {
      if (type == "session") {
        return(fit_object$session)
      } else if (type == "sport") {
        return(fit_object$sport)
      }
    },
    .package = "FITfileR",
    {
      # Test with low quality threshold (should accept)
      result_low_threshold <- process_fit_file(
        fit_file,
        min_quality_threshold = 0.0,
        sport_name = "Orthostatic"
      )

      # Should include quality metrics columns
      expected_quality_cols <- c(
        "laying_artifact_percentage",
        "laying_signal_quality_index",
        "laying_data_completeness",
        "laying_quality_grade",
        "standing_artifact_percentage",
        "standing_signal_quality_index",
        "standing_data_completeness",
        "standing_quality_grade"
      )

      expect_true(all(expected_quality_cols %in% names(result_low_threshold)))

      # Quality metrics should have valid values (not NA)
      expect_true(!is.na(result_low_threshold$laying_signal_quality_index))
      expect_true(!is.na(result_low_threshold$standing_signal_quality_index))
      expect_true(
        result_low_threshold$laying_quality_grade %in%
          c("A", "B", "C", "D", "F")
      )
      expect_true(
        result_low_threshold$standing_quality_grade %in%
          c("A", "B", "C", "D", "F")
      )
    }
  )
})

test_that("process_fit_file accepts quality threshold parameter with mocked functions", {
  # Skip if FITfileR is not available
  skip_if_not_installed("FITfileR")

  # This test focuses on verifying that the mocking works and parameters are accepted
  # Create minimal test data
  good_quality_hrv <- rep(800, 300) # 300 intervals of 800ms each (75 bpm)
  test_hr <- rep(60, 360)

  temp_dir <- create_test_environment(
    hrv_data1 = good_quality_hrv,
    hr_data1 = test_hr
  )
  on.exit(unlink(temp_dir, recursive = TRUE))

  fit_file <- file.path(temp_dir, "test1.fit")

  # Test that mocking works correctly (this was the main issue we fixed)
  expect_no_error({
    with_mocked_bindings(
      readFitFile = function(path) {
        mock_data <- readRDS(path)
        structure(mock_data, class = "FitFile")
      },
      hrv = function(fit_object) {
        fit_object$hrv
      },
      records = function(fit_object) {
        fit_object$record
      },
      getMessagesByType = function(fit_object, type) {
        if (type == "session") {
          return(fit_object$session)
        } else if (type == "sport") {
          return(fit_object$sport)
        }
      },
      .package = "FITfileR",
      {
        # Test that quality threshold parameter is accepted without error
        # This verifies the parameter validation and mocking both work
        result <- suppressWarnings(process_fit_file(
          fit_file,
          min_quality_threshold = 0.5,
          sport_name = "Orthostatic"
        ))

        # Should return a result (even if NA values due to insufficient data)
        expect_true(is.data.frame(result))
        expect_true("laying_signal_quality_index" %in% names(result))
        expect_true("standing_signal_quality_index" %in% names(result))
        expect_true(
          "min_quality_threshold" %in% names(formals(process_fit_file))
        )
      }
    )
  })
})

test_that("process_fit_file validates min_quality_threshold parameter", {
  # Test parameter validation without needing actual files

  # Create minimal mock file for testing
  temp_file <- tempfile(fileext = ".fit")
  writeLines("mock", temp_file)
  on.exit(unlink(temp_file))

  # Valid values should be accepted (tested implicitly through function signature)
  expect_error(
    process_fit_file(temp_file, min_quality_threshold = "invalid"),
    "min_quality_threshold must be numeric"
  )

  expect_error(
    process_fit_file(temp_file, min_quality_threshold = -0.1),
    "min_quality_threshold must be between 0 and 1"
  )

  expect_error(
    process_fit_file(temp_file, min_quality_threshold = 1.5),
    "min_quality_threshold must be between 0 and 1"
  )
})

test_that("create_empty_result includes quality metrics columns", {
  # Test the updated create_empty_result function
  empty_result <- create_empty_result(
    file_path = "/test/path.fit",
    session_date = as.Date("2025-01-01"),
    week = 1,
    time_of_day = "Morning"
  )

  # Should include all quality metrics columns
  expected_quality_cols <- c(
    "laying_artifact_percentage",
    "laying_signal_quality_index",
    "laying_data_completeness",
    "laying_quality_grade",
    "standing_artifact_percentage",
    "standing_signal_quality_index",
    "standing_data_completeness",
    "standing_quality_grade"
  )

  expect_true(all(expected_quality_cols %in% names(empty_result)))

  # Quality metrics should be NA in empty result
  expect_true(is.na(empty_result$laying_signal_quality_index))
  expect_true(is.na(empty_result$standing_signal_quality_index))
  expect_true(is.na(empty_result$laying_quality_grade))
  expect_true(is.na(empty_result$standing_quality_grade))
})
