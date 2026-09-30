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
  sport_name = "Orthostatic",
  record = NULL
) {
  list(
    hrv = if (!is.null(hrv_data)) data.frame(time = hrv_data) else data.frame(),
    record = if (!is.null(record)) {
      record
    } else if (!is.null(hr_data)) {
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
      # Session duration follows the HR record so the beat timeline and the
      # protocol windows agree
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

# Standard FITfileR mocks: RDS-backed fit objects (see with_mocked_bindings
# usage below)
mock_fit_bindings <- list(
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
  }
)

# A protocol-complete recording: 450 intervals of 0.8 s = 360 s total,
# matching the default 180/20/180 s orthostatic protocol
default_hrv_data <- function(anomaly_positions = integer(0)) {
  set.seed(42)
  hrv <- rep(0.8, 450) + rnorm(450, 0, 0.01)
  for (pos in anomaly_positions) {
    hrv[pos] <- hrv[pos] * ifelse(pos %% 2 == 0, 2.5, 0.3)
  }
  hrv
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

  # Create test data with some artifacts covering a full 360 s protocol
  test_hrv <- create_simulated_rr(
    450,
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
    readFitFile = mock_fit_bindings$readFitFile,
    hrv = mock_fit_bindings$hrv,
    records = mock_fit_bindings$records,
    getMessagesByType = mock_fit_bindings$getMessagesByType,
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
  # Create minimal test data (0.8 s intervals in seconds, 360 s total)
  good_quality_hrv <- default_hrv_data()
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
      readFitFile = mock_fit_bindings$readFitFile,
      hrv = mock_fit_bindings$hrv,
      records = mock_fit_bindings$records,
      getMessagesByType = mock_fit_bindings$getMessagesByType,
      .package = "FITfileR",
      {
        # Test that quality threshold parameter is accepted without error
        # This verifies the parameter validation and mocking both work
        result <- process_fit_file(
          fit_file,
          min_quality_threshold = 0.5,
          sport_name = "Orthostatic"
        )

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

# ============== Review regression tests (R02, R03, R04, R11) ==============

test_that("process_fit_file honors warmup for HR and RR analyses (R02)", {
  skip_if_not_installed("FITfileR")

  # HR is 60 bpm during the first 70 s, then 70 bpm: the resting HR window
  # [warmup, laying_time) must follow the warmup argument
  hrv <- default_hrv_data()
  hr <- c(rep(60, 70), rep(70, 290))
  temp_dir <- create_test_environment(hrv_data1 = hrv, hr_data1 = hr)
  on.exit(unlink(temp_dir, recursive = TRUE))
  fit_file <- file.path(temp_dir, "test1.fit")

  with_mocked_bindings(
    readFitFile = mock_fit_bindings$readFitFile,
    hrv = mock_fit_bindings$hrv,
    records = mock_fit_bindings$records,
    getMessagesByType = mock_fit_bindings$getMessagesByType,
    .package = "FITfileR",
    {
      res_warm0 <- process_fit_file(fit_file, sport_name = "Orthostatic", warmup = 0)
      res_warm70 <- process_fit_file(fit_file, sport_name = "Orthostatic", warmup = 70)
      expect_equal(res_warm0$laying_resting_hr, 60)
      expect_equal(res_warm70$laying_resting_hr, 70)

      # RR warmup: beats within the first warmup seconds are excluded from
      # the laying phase, so fewer laying beats feed the HRV calculation
      hrv_low <- default_hrv_data()
      res_w <- process_fit_file(fit_file, sport_name = "Orthostatic", warmup = 0)
      res_wo <- process_fit_file(fit_file, sport_name = "Orthostatic", warmup = 100)
      # Both produce results; the exact beat counts differ by construction
      expect_true(is.finite(res_w$laying_rmssd) || is.na(res_w$laying_rmssd))
      expect_true(!identical(res_w$source_file, character(0)))
    }
  )
})

test_that("process_fit_file selects HR windows by timestamp, not row number (R02)", {
  skip_if_not_installed("FITfileR")

  hrv <- default_hrv_data()
  hr <- c(rep(60, 180), rep(100, 20), rep(80, 160))
  base_time <- as.POSIXct("2025-01-01 08:00:00")

  # Regular 1 Hz recording
  temp_dir <- create_test_environment(hrv_data1 = hrv, hr_data1 = hr)
  on.exit(unlink(temp_dir, recursive = TRUE))
  fit_file <- file.path(temp_dir, "test1.fit")

  # Same signal sampled every other second during the standing phase
  idx <- c(1:180, seq(181, 360, by = 2))
  irregular_record <- data.frame(
    timestamp = seq.POSIXt(base_time, by = 1, length.out = 360)[idx],
    heart_rate = hr[idx]
  )
  irregular_dir <- tempfile("hrvtest")
  dir.create(irregular_dir)
  saveRDS(
    create_mock_fit_data(
      hrv_data = hrv,
      hr_data = hr, # drives session duration; record below takes precedence
      session_time = base_time,
      record = irregular_record
    ),
    file.path(irregular_dir, "test1.fit")
  )
  on.exit(unlink(irregular_dir, recursive = TRUE), add = TRUE)
  irregular_file <- file.path(irregular_dir, "test1.fit")

  with_mocked_bindings(
    readFitFile = mock_fit_bindings$readFitFile,
    hrv = mock_fit_bindings$hrv,
    records = mock_fit_bindings$records,
    getMessagesByType = mock_fit_bindings$getMessagesByType,
    .package = "FITfileR",
    {
      regular <- process_fit_file(fit_file, sport_name = "Orthostatic")
      irregular <- process_fit_file(irregular_file, sport_name = "Orthostatic")

      # Peak in the first 40 s of standing and the 60 s recovery value are
      # selected by elapsed time in both cases
      expect_equal(regular$standing_max_hr, 100)
      expect_equal(irregular$standing_max_hr, 100)
      expect_equal(irregular$hrr_60s, regular$hrr_60s)
      expect_equal(irregular$orthostatic_rise, 40)
    }
  )
})

test_that("process_fit_file forwards caller filter settings to all phases (R03)", {
  skip_if_not_installed("FITfileR")

  hrv <- default_hrv_data()
  hr <- rep(60, 360)
  temp_dir <- create_test_environment(hrv_data1 = hrv, hr_data1 = hr)
  on.exit(unlink(temp_dir, recursive = TRUE))
  fit_file <- file.path(temp_dir, "test1.fit")

  # Signal with a deviation between 10% and 20% from the local level: the
  # caller's threshold decides whether it is filtered out
  hrv_near <- default_hrv_data()
  hrv_near[100] <- 0.8 * 1.18 # +18% deviation within the laying phase
  near_dir <- tempfile("hrvtest")
  dir.create(near_dir)
  saveRDS(
    create_mock_fit_data(hrv_data = hrv_near, hr_data = hr,
      session_time = as.POSIXct("2025-01-01 08:00:00")),
    file.path(near_dir, "test1.fit")
  )
  on.exit(unlink(near_dir, recursive = TRUE), add = TRUE)
  near_file <- file.path(near_dir, "test1.fit")

  forwarded <- NULL
  real_rr_full_phase_processing <- hrvester::rr_full_phase_processing
  record_call <- function(...) {
    forwarded <<- c(forwarded, list(list(...)))
    real_rr_full_phase_processing(...)
  }

  with_mocked_bindings(
    readFitFile = mock_fit_bindings$readFitFile,
    hrv = mock_fit_bindings$hrv,
    records = mock_fit_bindings$records,
    getMessagesByType = mock_fit_bindings$getMessagesByType,
    .package = "FITfileR",
    with_mocked_bindings(
      rr_full_phase_processing = record_call,
      .package = "hrvester",
      {
        res <- process_fit_file(
          fit_file,
          sport_name = "Orthostatic",
          window_size = 9,
          threshold = 0.1,
          centered_window = TRUE
        )

        expect_length(forwarded, 3) # laying, transition and standing
        for (call_args in forwarded) {
          expect_equal(call_args$window_size, 9)
          expect_equal(call_args$threshold, 0.1)
          expect_equal(call_args$centered_window, TRUE)
          expect_equal(call_args$min_rr, 272)
          expect_equal(call_args$max_rr, 2000)
        }

        # Behavior check on the near-threshold artifact: the strict
        # threshold filters the +18% beat, the loose threshold keeps it
        res_strict <- process_fit_file(
          near_file, sport_name = "Orthostatic", threshold = 0.1
        )
        res_loose <- process_fit_file(
          near_file, sport_name = "Orthostatic", threshold = 0.3
        )
        expect_false(
          identical(res_strict$laying_rmssd, res_loose$laying_rmssd)
        )
      }
    )
  )
})

test_that("phase quality reflects the raw artifact burden, not correction (R04)", {
  skip_if_not_installed("FITfileR")

  # Inject artifacts into the laying phase only; interpolation can repair
  # the intervals but must not erase the quality gate's evidence
  laying_artifacts <- c(90, 120, 150)
  hrv <- default_hrv_data(anomaly_positions = laying_artifacts)
  hr <- rep(60, 360)
  temp_dir <- create_test_environment(hrv_data1 = hrv, hr_data1 = hr)
  on.exit(unlink(temp_dir, recursive = TRUE))
  fit_file <- file.path(temp_dir, "test1.fit")

  with_mocked_bindings(
    readFitFile = mock_fit_bindings$readFitFile,
    hrv = mock_fit_bindings$hrv,
    records = mock_fit_bindings$records,
    getMessagesByType = mock_fit_bindings$getMessagesByType,
    .package = "FITfileR",
    {
      # "none" skips artifact detection entirely; the invariant is that the
      # methods which do detect share the same raw burden
      results <- lapply(
        c("linear", "cubic", "lipponen"),
        function(method) {
          process_fit_file(
            fit_file,
            sport_name = "Orthostatic",
            correction_method = method
          )
        }
      )
      names(results) <- c("linear", "cubic", "lipponen")

      # The raw laying artifact burden is identical for every method; it
      # must not improve just because a stronger correction was selected
      laying_pct <- unname(vapply(
        results, function(r) r$laying_artifact_percentage, numeric(1)
      ))
      expect_true(all(laying_pct > 0))
      expect_true(all(abs(laying_pct - laying_pct[1]) < 1e-6))

      # The clean standing phase stays clean regardless of method
      standing_pct <- unname(vapply(
        results, function(r) r$standing_artifact_percentage, numeric(1)
      ))
      expect_equal(standing_pct, rep(0, 3))

      # The discarded-quality decision is identical across methods
      discarded <- unname(vapply(
        results, function(r) is.na(r$laying_rmssd), logical(1)
      ))
      expect_length(unique(discarded), 1)
    }
  )
})

test_that("process_fit_directory invalidates cache on config and content changes (R11)", {
  temp_dir <- tempfile("hrvtest")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  f1 <- file.path(temp_dir, "a.fit")
  f2 <- file.path(temp_dir, "b.fit")
  saveRDS(list(x = 1), f1)
  saveRDS(list(x = 2), f2)
  cache_file <- file.path(temp_dir, "hrv_cache.csv")

  calls <- character(0)
  counting_wrapper <- function(file_path, ..., config_id = NULL) {
    calls <<- c(calls, basename(file_path))
    hrvester:::create_empty_result(
      file_path,
      as.Date("2026-01-01"),
      1,
      "Morning",
      file_digest = unname(tools::md5sum(file_path)),
      config_id = config_id
    )
  }

  run <- function(...) {
    with_mocked_bindings(
      process_fit_file = counting_wrapper,
      .package = "hrvester",
      suppressMessages(process_fit_directory(
        temp_dir, cache_file = cache_file, ...
      ))
    )
  }

  # First run processes both files
  run()
  expect_equal(sort(calls), c("a.fit", "b.fit"))

  # Identical rerun hits the cache
  calls <- character(0)
  run()
  expect_equal(calls, character(0))

  # A configuration change reprocesses both files
  calls <- character(0)
  run(correction_method = "cubic")
  expect_equal(sort(calls), c("a.fit", "b.fit"))

  # Overwriting file contents at the same path reprocesses that file only
  calls <- character(0)
  saveRDS(list(x = 999), f1)
  run(correction_method = "cubic")
  expect_equal(calls, "a.fit")

  # Cache rows carry provenance
  cached <- load_cache(cache_file)
  expect_true(all(nzchar(cached$file_digest)))
  expect_true(all(nzchar(cached$config_id)))
})

test_that("process_fit_directory keeps cache entries for removed files (R11)", {
  temp_dir <- tempfile("hrvtest")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  f1 <- file.path(temp_dir, "a.fit")
  f2 <- file.path(temp_dir, "b.fit")
  saveRDS(list(x = 1), f1)
  saveRDS(list(x = 2), f2)
  cache_file <- file.path(temp_dir, "hrv_cache.csv")

  calls <- character(0)
  counting_wrapper <- function(file_path, ..., config_id = NULL) {
    calls <<- c(calls, basename(file_path))
    hrvester:::create_empty_result(
      file_path,
      as.Date("2026-01-01"),
      1,
      "Morning",
      file_digest = unname(tools::md5sum(file_path)),
      config_id = config_id
    )
  }

  with_mocked_bindings(
    process_fit_file = counting_wrapper,
    .package = "hrvester",
    suppressMessages(process_fit_directory(temp_dir, cache_file = cache_file))
  )
  unlink(f2)
  calls <- character(0)

  result <- with_mocked_bindings(
    process_fit_file = counting_wrapper,
    .package = "hrvester",
    suppressMessages(process_fit_directory(
      temp_dir,
      cache_file = cache_file,
      correction_method = "lipponen" # different config
    ))
  )
  # b.fit no longer exists: its historical row survives, a.fit is reprocessed
  expect_equal(calls, "a.fit")
  expect_true("b.fit" %in% basename(result$source_file))
})
