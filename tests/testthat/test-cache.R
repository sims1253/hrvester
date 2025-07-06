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
    "activity"
  )

  expect_true(all(expected_cols %in% colnames(cache)))

  # Check column types
  expect_type(cache$source_file, "character")
  expect_type(cache$date, "character")
  expect_type(cache$week, "double")
  expect_type(cache$time_of_day, "character")
  expect_type(cache$laying_rmssd, "double")
})
