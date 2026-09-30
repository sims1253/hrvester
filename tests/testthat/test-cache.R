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

# ============== Review regression tests (R12, R19) ==============

test_that("load_cache round-trips all-NA result rows with correct types (R12)", {
  row <- hrvester:::create_empty_result(
    "example.fit", as.Date("2026-01-01"), 0, "Morning"
  )
  path <- tempfile(fileext = ".csv")
  readr::write_csv(row, path)

  restored <- hrvester:::load_cache(path)
  expect_equal(nrow(restored), 1)
  # Nullable numeric and character columns keep their declared types
  expect_type(restored$laying_rmssd, "double")
  expect_type(restored$date, "character")
  expect_type(restored$laying_quality_grade, "character")
  expect_true(is.na(restored$laying_rmssd))
  expect_equal(restored$source_file, "example.fit")
})

test_that("load_cache round-trips a populated row and a mixture (R12)", {
  populated <- hrvester:::create_empty_result(
    "full.fit", as.Date("2026-01-02"), 1, "Evening"
  )
  populated$laying_rmssd <- 42.5
  populated$laying_quality_grade <- "A"
  empty_row <- hrvester:::create_empty_result(
    "empty.fit", as.Date("2026-01-03"), 1, "Morning"
  )
  path <- tempfile(fileext = ".csv")
  readr::write_csv(dplyr::bind_rows(populated, empty_row), path)

  restored <- hrvester:::load_cache(path)
  expect_equal(nrow(restored), 2)
  expect_equal(restored$laying_rmssd[1], 42.5)
  expect_true(is.na(restored$laying_rmssd[2]))
  expect_equal(restored$laying_quality_grade[1], "A")
  expect_true(is.na(restored$laying_quality_grade[2]))
})

test_that("load_cache accepts a header-only file as a valid empty cache (R12)", {
  # A header-only file matching the current schema loads as empty
  path2 <- tempfile(fileext = ".csv")
  readr::write_csv(hrvester::cache_definition(), path2)
  restored <- expect_no_warning(hrvester:::load_cache(path2))
  expect_equal(nrow(restored), 0)
})

test_that("load_cache backfills provenance columns for legacy caches (R11)", {
  # Simulate a legacy cache without file_digest/config_id
  legacy <- hrvester::cache_definition()
  legacy <- dplyr::bind_rows(legacy, hrvester:::create_empty_result(
    "legacy.fit", as.Date("2026-01-01"), 1, "Morning"
  ))
  legacy <- dplyr::select(legacy, !dplyr::any_of(c("file_digest", "config_id")))
  path <- tempfile(fileext = ".csv")
  readr::write_csv(legacy, path)

  restored <- expect_no_warning(hrvester:::load_cache(path))
  expect_equal(nrow(restored), 1)
  expect_true(all(c("file_digest", "config_id") %in% names(restored)))
  # Missing provenance flags the entry for reprocessing
  expect_true(is.na(restored$config_id))
})

test_that("save_cache_atomic preserves the previous cache on write failure (R19)", {
  skip_on_os("windows")

  cache_dir <- tempfile("hrvcache")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  cache_file <- file.path(cache_dir, "cache.csv")

  good <- hrvester::cache_definition()
  good <- dplyr::bind_rows(good, hrvester:::create_empty_result(
    "good.fit", as.Date("2026-01-01"), 1, "Morning"
  ))
  readr::write_csv(good, cache_file)
  original_bytes <- readBin(cache_file, "raw", file.size(cache_file))

  # A read-only cache directory makes the temporary write fail; the
  # previous cache must remain byte-for-byte unchanged
  Sys.chmod(cache_dir, "555")
  on.exit(Sys.chmod(cache_dir, "755"), add = TRUE)
  if (file.access(cache_dir, 2) == 0) {
    skip("Cannot make the cache directory read-only (running as root?)")
  }

  expect_error(
    hrvester:::safe_file_operation(
      hrvester:::save_cache_atomic,
      data = good,
      cache_file = cache_file
    ),
    "File operation failed"
  )

  Sys.chmod(cache_dir, "755")
  after_bytes <- readBin(cache_file, "raw", file.size(cache_file))
  expect_identical(after_bytes, original_bytes)
  # No temporary files left behind
  leftovers <- list.files(cache_dir, pattern = "^\\.hrv-cache-")
  expect_equal(leftovers, character(0))
})

test_that("save_cache_atomic publishes the complete new cache on success (R19)", {
  cache_file <- tempfile(fileext = ".csv")
  readr::write_csv(hrvester::cache_definition(), cache_file)

  new_data <- hrvester::cache_definition()
  new_data <- dplyr::bind_rows(new_data, hrvester:::create_empty_result(
    "new.fit", as.Date("2026-02-01"), 5, "Evening"
  ))
  expect_no_error(hrvester:::save_cache_atomic(new_data, cache_file))

  restored <- hrvester:::load_cache(cache_file)
  expect_equal(nrow(restored), 1)
  expect_equal(restored$source_file, "new.fit")
})
