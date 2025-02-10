library(mockery)
library(FITfileR)
library(dplyr)

# Helper function to create temporary directory with test files
create_test_environment <- function() {
  # Create temporary directory
  temp_dir <- tempfile("hrvtest")
  dir.create(temp_dir)

  # Create some dummy FIT files
  file.create(file.path(temp_dir, "test1.fit"))
  file.create(file.path(temp_dir, "test2.fit"))

  return(temp_dir)
}


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

  # Mock FITfileR::hrv to directly return the hrv data
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

  result <- extract_rr_data(
    fit_object,
    filter_factor = 0.175,
    transition_buffer = 20
  )

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
    "laying_time must be at least 30 seconds"
  )
  expect_error(
    extract_rr_data(fit_object, standing_time = -10),
    "standing_time must be at least 30 seconds"
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

test_that("process_fit_file handles file processing correctly", {
  # Create a temporary test file
  temp_file <- tempfile(fileext = ".fit")
  file.create(temp_file)

  # Test file existence check
  expect_equal(
    create_empty_result("nonexistent.fit", NA, NA, NA, 0.175),
    process_fit_file("nonexistent.fit", 0.175)
  )

  # Test with valid file but invalid content
  result <- process_fit_file(temp_file, 0.175)
  expect_true(all(is.na(subset(result, select = -c(source_file, package_version, RR_filter)))))

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

# Tests for process_fit_directory function
test_that("process_fit_directory handles new files correctly", {
  # Setup test environment
  temp_dir <- create_test_environment()
  on.exit(unlink(temp_dir, recursive = TRUE))

  cache_file <- file.path(temp_dir, "cache.csv")

  # Test processing new files
  result <- process_fit_directory(temp_dir, cache_file)

  # Check that cache file was created
  expect_true(file.exists(cache_file))

  # Check result structure
  expect_s3_class(result, "tbl_df")
})

test_that("process_fit_directory handles existing cache correctly", {
  temp_dir <- create_test_environment()
  on.exit(unlink(temp_dir, recursive = TRUE))

  cache_file <- file.path(temp_dir, "cache.csv")

  # Create initial cache using cache_definition
  initial_cache <- cache_definition() %>%
    add_row(
      source_file = file.path(temp_dir, "test1.fit"),
      date = format(Sys.Date(), "%Y-%m-%d"), # Ensure date is formatted string
      week = as.numeric(format(Sys.Date(), "%V")),
      time_of_day = "Morning",
      laying_rmssd = 50,
      laying_sdnn = 40,
      laying_hr = 60,
      laying_resting_hr = 55,
      standing_rmssd = 30,
      standing_sdnn = 25,
      standing_hr = 80,
      standing_max_hr = 90,
      package_version = as.character(packageVersion("hrvester")),
      RR_filter = 0.175,
      activity = "Orthostatic",
      hrr_60s = 20,
      hrr_relative = 0.5,
      orthostatic_rise = 20
    )

  # Write the cache file
  readr::write_csv2(
    initial_cache,
    cache_file,
    col_names = TRUE,
    append = FALSE
  )

  # Load and verify cache
  result <- process_fit_directory(temp_dir, cache_file)

  # Test specific checks
  expect_equal(nrow(result), 2)
  expect_true(is.character(result$date)) # Explicitly check date type

  # Verify all data types match cache_definition
  template <- cache_definition()
  for (col_name in names(template)) {
    expect_equal(
      class(result[[col_name]]),
      class(template[[col_name]]),
      info = sprintf("Column %s has incorrect type", col_name)
    )
  }
})

test_that("process_fit_directory handles clear_cache parameter", {
  temp_dir <- create_test_environment()
  on.exit(unlink(temp_dir, recursive = TRUE))

  cache_file <- file.path(temp_dir, "cache.csv")

  # Create initial cache
  initial_cache <- cache_definition() %>%
    add_row(
      source_file = file.path(temp_dir, "test1.fit"),
      date = as.character(Sys.Date()),
      week = as.numeric(format(Sys.Date(), "%V")),
      time_of_day = "Morning",
      laying_rmssd = 50,
      laying_sdnn = 40,
      laying_hr = 60,
      laying_resting_hr = 55,
      standing_rmssd = 30,
      standing_sdnn = 25,
      standing_hr = 80,
      standing_max_hr = 90,
      package_version = as.character(packageVersion("hrvester")),
      RR_filter = 0.175,
      activity = "Orthostatic",
      hrr_60s = 20,
      hrr_relative = 0.5,
      orthostatic_rise = 20
    )

  readr::write_csv2(initial_cache, cache_file)

  result <- process_fit_directory(temp_dir, cache_file, clear_cache = TRUE)

  expect_equal(nrow(result), 2) # Changed expectation to 2 since it processes both test files
})

test_that("process_fit_directory handles invalid cache file", {
  temp_dir <- create_test_environment()
  on.exit(unlink(temp_dir, recursive = TRUE))

  cache_file <- file.path(temp_dir, "cache.csv")

  # Test with completely invalid content
  writeLines("invalid,cache,file", cache_file)
  expect_warning(
    process_fit_directory(temp_dir, cache_file),
    "Invalid cache file structure" # Exactly match the warning message
  )

  # Test with missing required columns
  writeLines("some,random,columns\n1,2,3", cache_file)
  expect_warning(
    process_fit_directory(temp_dir, cache_file),
    "Cache file missing required columns, creating new cache"
  )

  # Test with empty file
  writeLines("", cache_file)
  expect_warning(
    process_fit_directory(temp_dir, cache_file),
    "Invalid cache file structure"
  )

  # Verify that a new, valid cache is created in all cases
  suppressWarnings({ # Suppress expected warning for this verification
    result <- process_fit_directory(temp_dir, cache_file)
  })
  expect_true(inherits(result, "tbl_df"))
  expect_equal(names(result), names(cache_definition()))
  expect_true(is.character(result$date))
})

test_that("process_fit_directory handles file system errors", {
  # Test with non-existent directory
  expect_error(
    process_fit_directory("/nonexistent/directory"),
    "Directory.*does not exist"
  )

  # Test with unwriteable directory
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

  # Test invalid filter_factor
  expect_error(
    process_fit_directory(temp_dir, filter_factor = -1),
    "filter_factor must be a number between 0 and 1"
  )

  expect_error(
    process_fit_directory(temp_dir, filter_factor = "invalid"),
    "filter_factor must be a number between 0 and 1"
  )

  # Test invalid clear_cache
  expect_error(
    process_fit_directory(temp_dir, clear_cache = "invalid"),
    "clear_cache must be a single logical value"
  )
})

test_that("process_fit_directory handles package version changes", {
  temp_dir <- create_test_environment()
  on.exit(unlink(temp_dir, recursive = TRUE))

  cache_file <- file.path(temp_dir, "cache.csv")

  initial_cache <- cache_definition() %>%
    add_row(
      source_file = file.path(temp_dir, "test1.fit"),
      date = as.character(Sys.Date()),
      week = as.numeric(format(Sys.Date(), "%V")),
      time_of_day = "Morning",
      laying_rmssd = 50,
      laying_sdnn = 40,
      laying_hr = 60,
      laying_resting_hr = 55,
      standing_rmssd = 30,
      standing_sdnn = 25,
      standing_hr = 80,
      standing_max_hr = 90,
      package_version = "0.0.1",
      RR_filter = 0.175,
      activity = "Orthostatic",
      hrr_60s = 20,
      hrr_relative = 0.5,
      orthostatic_rise = 20
    )

  readr::write_csv2(
    initial_cache,
    cache_file,
    col_names = TRUE,
    append = FALSE
  )

  result <- process_fit_directory(temp_dir, cache_file)
  expect_match(result$package_version[1], as.character(packageVersion("hrvester")), fixed = TRUE)
})
