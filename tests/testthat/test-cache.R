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

# Tests for cache_definition function
test_that("cache_definition creates correct structure", {
  cache <- cache_definition()

  # Check that it's a tibble
  expect_s3_class(cache, "tbl_df")

  # Check for required columns
  expected_cols <- c(
    "source_file", "date", "week", "time_of_day",
    "laying_rmssd", "laying_sdnn", "laying_hr",
    "laying_resting_hr", "standing_rmssd", "standing_sdnn",
    "standing_hr", "standing_max_hr", "package_version",
    "RR_filter", "activity"
  )

  expect_true(all(expected_cols %in% colnames(cache)))

  # Check column types
  expect_type(cache$source_file, "character")
  expect_type(cache$date, "character")
  expect_type(cache$week, "double")
  expect_type(cache$time_of_day, "character")
  expect_type(cache$laying_rmssd, "double")
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
      activity = "Orthostatic"
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
      activity = "Orthostatic"
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
      activity = "Orthostatic"
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
