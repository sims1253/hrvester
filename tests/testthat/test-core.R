library(testthat)
library(mockery)
library(FITfileR)
library(dplyr)

# Define all required generic functions
setGeneric("hrv", function(object) standardGeneric("hrv"))
setGeneric("records", function(object) standardGeneric("records"))
setGeneric("getMessagesByType", function(object, type) standardGeneric("getMessagesByType"))
setGeneric("listMessageTypes", function(object) standardGeneric("listMessageTypes"))
setGeneric("messages", function(object) standardGeneric("messages"))
setGeneric("globalMessageNumber", function(object) standardGeneric("globalMessageNumber"))

# Define the FitFile class
setClass("FitFile",
  slots = c(
    data = "list",
    messages = "list"
  ),
  prototype = list(
    data = list(),
    messages = list()
  )
)

# Define Message class for FIT messages
setClass("Message",
  slots = c(
    number = "integer",
    data = "list"
  ),
  prototype = list(
    number = 0L,
    data = list()
  )
)

# Define methods for the mock FitFile class
setMethod("hrv", "FitFile", function(object) {
  object@data$hrv
})

setMethod("records", "FitFile", function(object) {
  object@data$record
})

setMethod("getMessagesByType", "FitFile", function(object, type) {
  object@data[[type]]
})

setMethod("listMessageTypes", "FitFile", function(object) {
  # Ensure we only process Message objects
  messages <- Filter(is, object@messages, inherits, "Message")
  names(messages)[unlist(vapply(messages, function(x) x@number, integer(1)))]
})

setMethod("messages", "FitFile", function(object) {
  # Ensure we only process Message objects
  message_list <- object@messages
  # Filter out non-Message objects
  message_list <- Filter(is, message_list, inherits, "Message")
  # Return the filtered list of Message objects
  return(message_list)
})

setMethod("globalMessageNumber", "Message", function(object) {
  object@number
})

create_mock_fit_data <- function(
    hrv_data = NULL,
    hr_data = NULL,
    session_time = Sys.time(),
    sport_name = "Test Activity") {
  # Create the data structure
  data_list <- list(
    hrv = if (!is.null(hrv_data)) {
      data.frame(time = hrv_data)
    } else {
      data.frame()
    },
    record = if (!is.null(hr_data)) {
      data.frame(
        timestamp = seq.POSIXt(from = Sys.time(), by = 1, length.out = length(hr_data)),
        heart_rate = hr_data
      )
    } else {
      data.frame()
    },
    session = data.frame(
      timestamp = session_time,
      stringsAsFactors = FALSE
    ),
    sport = data.frame(
      name = sport_name,
      stringsAsFactors = FALSE
    )
  )

  # Create Message objects for each message type
  messages_list <- list()
  if (!is.null(hrv_data)) {
    messages_list[["hrv"]] <- new("Message", number = 1L, data = data_list[["hrv"]])
  } else {
    messages_list[["hrv"]] <- new("Message", number = 1L, data = data.frame())
  }
  if (!is.null(hr_data)) {
    messages_list[["record"]] <- new("Message", number = 2L, data = data_list[["record"]])
  } else {
    messages_list[["record"]] <- new("Message", number = 2L, data = data.frame())
  }
  messages_list[["session"]] <- new("Message", number = 3L, data = data_list[["session"]])
  messages_list[["sport"]] <- new("Message", number = 4L, data = data_list[["sport"]])

  # Create a proper FitFile object
  fit_file <- new("FitFile",
    data = data_list,
    messages = messages_list
  )

  return(fit_file)
}

# Helper function to create simulated RR intervals
create_simulated_rr <- function(
    n_intervals,
    mean_rr = 1.0,
    variation = 0.1,
    anomaly_rate = 0) {
  # Generate base intervals with normal variation
  rr <- rnorm(n_intervals, mean = mean_rr, sd = variation * mean_rr)

  # Add anomalies if specified
  if (anomaly_rate > 0) {
    n_anomalies <- floor(n_intervals * anomaly_rate)
    anomaly_positions <- sample(n_intervals, n_anomalies)
    rr[anomaly_positions] <- rr[anomaly_positions] * runif(
      n_anomalies,
      min = 1.5,
      max = 2.0
    )
  }

  # Ensure all intervals are positive
  pmax(rr, 0.1)
}

test_that("extract_rr_data handles normal case correctly", {
  set.seed(1)
  # Create simulated RR intervals for a 6-minute test
  # Laying (3 min): Higher variability, slower heart rate
  laying_rr <- create_simulated_rr(180, mean_rr = 1.0, variation = 0.1)
  # Standing (3 min): Lower variability, faster heart rate
  standing_rr <- create_simulated_rr(180, mean_rr = 0.8, variation = 0.05)

  # Combine phases
  hrv_data <- c(laying_rr, standing_rr)

  # Create mock FIT object
  fit_object <- create_mock_fit_data(hrv_data = hrv_data)

  # Mock FITfileR::hrv to directly return the hrv data
  testthat::local_mocked_bindings(
    hrv = function(object) {
      return(object@data$hrv)
    },
    .package = "FITfileR"
  )

  # Extract intervals
  result <- extract_rr_data(
    fit_object,
    filter_factor = 0.16,
    laying_time = 180,
    standing_time = 180
  )

  # Test structure
  expect_type(result, "list")
  expect_named(result, c("laying", "standing", "quality_metrics"))

  # Test data presence
  expect_true(length(result$laying) > 0)
  expect_true(length(result$standing) > 0)

  # Test phase separation
  expect_true(mean(result$laying) > mean(result$standing))
  expect_true(sd(result$laying) > sd(result$standing))
})

test_that("extract_rr_data handles missing data correctly", {
  # Test with empty data
  fit_object <- create_mock_fit_data()
  expect_warning(result <- extract_rr_data(
    fit_object,
    filter_factor = 0.175
  ))

  expect_type(result, "list")
  expect_named(result, c("laying", "standing", "quality_metrics"))
  expect_length(result$laying, 0)
  expect_length(result$standing, 0)
})

test_that("extract_rr_data filters outliers effectively", {
  set.seed(1)
  # Create data with known outliers
  rr_with_outliers <- create_simulated_rr(
    360,
    mean_rr = 1.0,
    variation = 0.1,
    anomaly_rate = 0.1
  )

  fit_object <- create_mock_fit_data(hrv_data = rr_with_outliers)

  testthat::local_mocked_bindings(
    hrv = function(object) {
      return(object@data$hrv)
    },
    .package = "FITfileR"
  )

  expect_warning(result <- extract_rr_data(
    fit_object,
    filter_factor = 0.175
  ))

  # Check that outliers were removed
  expect_true(sd(result$laying) < sd(rr_with_outliers[1:180]))
  expect_true(sd(result$standing) < sd(rr_with_outliers[181:360]))
})

test_that("extract_rr_data handles transition period correctly", {
  # Create data with clear transition period
  laying_rr <- create_simulated_rr(160, mean_rr = 1.0, variation = 0.1)
  transition_rr <- create_simulated_rr(40, mean_rr = 0.7, variation = 0.2)
  standing_rr <- create_simulated_rr(160, mean_rr = 0.8, variation = 0.05)

  hrv_data <- c(laying_rr, transition_rr, standing_rr)

  fit_object <- create_mock_fit_data(hrv_data = hrv_data)

  testthat::local_mocked_bindings(
    hrv = function(object) {
      return(object@data$hrv)
    },
    .package = "FITfileR"
  )

  expect_warning(result <- extract_rr_data(
    fit_object,
    filter_factor = 0.175,
    transition_buffer = 20
  ))

  # Verify transition period was excluded
  expect_true(all(result$laying < max(laying_rr) * 1.1))
  expect_true(all(result$standing > min(standing_rr) * 0.9))
})

test_that("extract_rr_data validates inputs correctly", {
  # Create valid base data
  valid_data <- create_simulated_rr(360, mean_rr = 1.0, variation = 0.1)
  fit_object <- create_mock_fit_data(hrv_data = valid_data)

  # Test invalid filter_factor
  expect_error(
    extract_rr_data(fit_object, filter_factor = -0.1),
    "filter_factor must be between 0 and 1"
  )
  expect_error(
    extract_rr_data(fit_object, filter_factor = 1.1),
    "filter_factor must be between 0 and 1"
  )

  # Test invalid times
  expect_error(
    extract_rr_data(fit_object, laying_time = -10),
    "laying_time must be positive"
  )
  expect_error(
    extract_rr_data(fit_object, standing_time = -10),
    "standing_time must be positive"
  )

  # Test invalid object
  expect_error(
    extract_rr_data(list(), filter_factor = 0.175),
    "fit_object must be a FitFile object"
  )
})

test_that("extract_rr_data handles physiological quality checks", {
  # Create data with physiologically impossible values
  set.seed(1)
  impossible_laying <- create_simulated_rr(1000, mean_rr = 3.0, variation = 0.1) # HR ~ 20 bpm
  impossible_standing <- create_simulated_rr(1000, mean_rr = 0.3, variation = 0.1) # HR ~ 200 bpm

  hrv_data <- c(impossible_laying, impossible_standing)

  fit_object <- create_mock_fit_data(hrv_data = hrv_data)

  testthat::local_mocked_bindings(
    hrv = function(object) {
      return(object@data$hrv)
    },
    .package = "FITfileR"
  )

  # Should generate warnings about physiologically impossible values
  expect_warning(
    extract_rr_data(fit_object, filter_factor = 0.175),
    "Unusual .* range detected"
  )
})

test_that("extract_rr_data handles data density requirements", {
  # Create sparse data (missing beats)
  sparse_data <- create_simulated_rr(100) # Much fewer intervals than expected

  fit_object <- create_mock_fit_data(hrv_data = sparse_data)

  testthat::local_mocked_bindings(
    hrv = function(object) {
      return(object@data$hrv)
    },
    .package = "FITfileR"
  )

  # Should warn about insufficient data density
  expect_warning(
    extract_rr_data(fit_object, filter_factor = 0.175),
    "Insufficient data density"
  )
})

test_that("extract_rr_data produces consistent phase-specific filtering", {
  # Create data with different variability in each phase
  laying_rr <- create_simulated_rr(180, mean_rr = 1.0, variation = 0.15)
  standing_rr <- create_simulated_rr(180, mean_rr = 0.8, variation = 0.05)

  hrv_data <- c(laying_rr, standing_rr)

  fit_object <- create_mock_fit_data(hrv_data = hrv_data)

  testthat::local_mocked_bindings(
    hrv = function(object) {
      return(object@data$hrv)
    },
    .package = "FITfileR"
  )

  result <- extract_rr_data(fit_object, filter_factor = 0.175)

  # Check that phase-specific filtering was applied correctly
  expect_true(sd(result$laying) / mean(result$laying) >
    sd(result$standing) / mean(result$standing))
})


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

test_that("process_fit_file handles file processing correctly", {
  # Create a temporary test file
  temp_file <- tempfile(fileext = ".fit")
  file.create(temp_file)

  # Test file existence check
  expect_error(
    process_fit_file("nonexistent.fit", 0.175),
    "File does not exist"
  )

  # Test with valid file but invalid content
  result <- process_fit_file(temp_file, 0.175)
  expect_true(all(is.na(result)))

  # Clean up
  unlink(temp_file)
})

test_that("create_empty_result creates proper structure", {
  result <- create_empty_result(
    "test.fit",
    Sys.Date(),
    1,
    "Morning",
    0.175
  )

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)

  # Check that required columns exist and have correct types
  expect_type(result$laying_rmssd, "double")
  expect_type(result$laying_sdnn, "double")
  expect_type(result$activity, "character")

  # Check that numeric columns are NA
  expect_true(all(is.na(result$laying_rmssd)))
  expect_true(all(is.na(result$standing_hr)))
})
