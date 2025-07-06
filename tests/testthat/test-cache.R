# Tests for cache_definition function
test_that("cache_definition creates correct structure", {
  cache <- cache_definition()

  # Check that it's a tibble
  expect_s3_class(cache, "tbl_df")

  # Check for required columns
  expected_cols <- c(
    "source_file",
    "date",
    "week",
    "time_of_day",
    "laying_rmssd",
    "laying_sdnn",
    "laying_hr",
    "laying_resting_hr",
    "standing_rmssd",
    "standing_sdnn",
    "standing_hr",
    "standing_max_hr",
    "package_version",
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

# Test that cache supports quality metrics columns
test_that("cache_definition includes quality metrics columns", {
  cache <- cache_definition()

  # Check for quality metrics columns (should be added in Task 1.4)
  quality_cols <- c(
    "laying_artifact_percentage",
    "laying_signal_quality_index",
    "laying_data_completeness",
    "laying_quality_grade",
    "standing_artifact_percentage",
    "standing_signal_quality_index",
    "standing_data_completeness",
    "standing_quality_grade"
  )

  # These columns should be present after Task 1.4 implementation
  for (col in quality_cols) {
    expect_true(
      col %in% colnames(cache),
      info = paste("Missing quality metric column:", col)
    )
  }

  # Check quality column types
  if ("laying_artifact_percentage" %in% colnames(cache)) {
    expect_type(cache$laying_artifact_percentage, "double")
    expect_type(cache$laying_signal_quality_index, "double")
    expect_type(cache$laying_data_completeness, "double")
    expect_type(cache$laying_quality_grade, "character")
  }
})
