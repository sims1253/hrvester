check_phase_transitions <- function(
  result,
  laying_time,
  transition_time,
  duration,
  centered = TRUE
) {
  # 1. Check for correct order of phases (laying -> transition -> standing)
  phases <- unique(result$phase)
  expect_equal(
    phases,
    c("laying", "transition", "standing"),
    info = "Phases are not in the correct order"
  )

  # 2. Phase boundaries in elapsed seconds (interval-end convention)
  laying_end <- if (centered) {
    laying_time - transition_time / 2
  } else {
    laying_time
  }
  transition_end <- if (centered) {
    laying_time + transition_time / 2
  } else {
    laying_time + transition_time
  }

  # 3. Every beat is in the phase its interval-end time implies
  expect_true(
    all(result$phase[result$elapsed_time <= laying_end] == "laying"),
    info = "Beats at or before the laying boundary must be laying"
  )
  expect_true(
    all(
      result$phase[
        result$elapsed_time > laying_end & result$elapsed_time <= transition_end
      ] == "transition"
    ),
    info = "Beats within the transition band must be transition"
  )
  expect_true(
    all(result$phase[result$elapsed_time > transition_end] == "standing"),
    info = "Beats after the transition band must be standing"
  )
}

test_that("split_rr_phases splits data correctly (valid inputs)", {
  # 100 one-second beats: elapsed time is 1, 2, ..., 100 s
  rr_intervals <- data.frame(time = rep(1, 100))
  session_info <- list(duration = 100)
  laying_time <- 30
  transition_time <- 20
  standing_time <- 50

  result <- split_rr_phases(
    rr_intervals,
    session_info,
    laying_time,
    transition_time,
    standing_time,
    time_unit = "seconds"
  )

  expect_true(is.data.frame(result))
  expect_true("phase" %in% colnames(result))
  # Independent expectations for 1 s beats: centered boundaries at 20 s and
  # 40 s elapsed time
  expect_equal(sum(result$phase == "laying"), 20)
  expect_equal(sum(result$phase == "transition"), 20)
  expect_equal(sum(result$phase == "standing"), 60)
  check_phase_transitions(
    result,
    laying_time,
    transition_time,
    session_info$duration
  )
})

test_that("split_rr_phases uses beat times, not uniform spacing (R01)", {
  # 60 s at 60 bpm, then 60 s at 120 bpm: heart rate changes between phases
  rr <- data.frame(time = c(rep(1000, 60), rep(500, 120)))

  result <- split_rr_phases(
    rr,
    list(duration = 120),
    laying_time = 60,
    transition_time = 0,
    standing_time = 60,
    centered_transition = FALSE
  )

  # The 60th interval ends at 60 s and belongs to laying; the remaining 120
  # faster intervals are standing. Uniform stretching would misassign 30.
  expect_equal(sum(result$phase == "laying"), 60)
  expect_equal(sum(result$phase == "standing"), 120)
  expect_equal(result$elapsed_time[60], 60)
  expect_equal(result$elapsed_time[61], 60.5)
})

test_that("split_rr_phases converts milliseconds to seconds by default", {
  result <- split_rr_phases(
    data.frame(time = c(1000, 1000, 500)),
    list(duration = 2.5),
    laying_time = 2,
    transition_time = 0,
    standing_time = 0.5,
    centered_transition = FALSE
  )
  expect_equal(result$elapsed_time, c(1, 2, 2.5))
  expect_equal(result$phase, c("laying", "laying", "standing"))
})

test_that("split_rr_phases warns on session duration mismatch", {
  # Beats only sum to 50 s although the session claims 100 s
  expect_warning(
    result <- split_rr_phases(
      data.frame(time = rep(1, 50)),
      list(duration = 100),
      30,
      20,
      50,
      time_unit = "seconds"
    ),
    "deviates from session_info\\$duration"
  )
  # The beat-derived timeline is used regardless
  expect_equal(result$elapsed_time[50], 50)
})

test_that("split_rr_phases handles empty rr_intervals", {
  suppressWarnings(
    result <- split_rr_phases(
      data.frame(time = numeric()),
      list(duration = 100),
      30,
      20,
      50
    )
  )
  expect_true(is.data.frame(result))
  expect_true("phase" %in% colnames(result))
  expect_true(nrow(result) == 0)
  expect_true(is.character(result$phase))
})

test_that("split_rr_phases throws errors for invalid inputs", {
  # Invalid rr_intervals
  expect_error(
    split_rr_phases("not a df", list(duration = 100), 30, 20, 50),
    "rr_intervals must be a data frame"
  )
  expect_error(
    split_rr_phases(
      data.frame(no_time = 1:10),
      list(duration = 100),
      30,
      20,
      50
    ),
    "rr_intervals must contain a 'time' column"
  )

  # Invalid session_info
  expect_error(
    split_rr_phases(data.frame(time = 1:10), "not a list", 30, 20, 50),
    "session_info must be a list"
  )
  expect_error(
    split_rr_phases(data.frame(time = 1:10), list(), 30, 20, 50),
    "session_info must be a list containing a 'duration' element"
  )
  expect_error(
    split_rr_phases(data.frame(time = 1:10), list(duration = "a"), 30, 20, 50),
    "session_info\\$duration must be a positive numeric value"
  )
  expect_error(
    split_rr_phases(data.frame(time = 1:10), list(duration = -10), 30, 20, 50),
    "session_info\\$duration must be a positive numeric value"
  )

  # Invalid laying_time, transition_time, and standing_time
  invalid_times <- list(
    laying_time = list("a", -1),
    transition_time = list("a", -1),
    standing_time = list("a", -1)
  )
  purrr::walk(
    names(invalid_times),
    ~ {
      args <- list(
        rr_intervals = data.frame(time = rep(1, 10)),
        session_info = list(duration = 100),
        laying_time = 30,
        transition_time = 20,
        standing_time = 50,
        time_unit = "seconds"
      )
      outer_x <- .x
      purrr::walk(
        invalid_times[[.x]],
        ~ {
          args[[outer_x]] <- .
          expect_error(do.call(split_rr_phases, args))
        }
      )
    }
  )

  expect_error(
    split_rr_phases(
      data.frame(time = rep(1, 10)),
      list(duration = 100),
      50,
      20,
      51,
      time_unit = "seconds"
    ),
    "cannot exceed"
  )

  expect_error(
    split_rr_phases(
      data.frame(time = rep(1, 10)),
      list(duration = 100),
      50,
      20,
      10,
      centered_transition = FALSE,
      time_unit = "seconds"
    ),
    "transition_time must not exceed the standing_time."
  )

  expect_error(
    split_rr_phases(
      data.frame(time = rep(1, 10)),
      list(duration = 100),
      30,
      20,
      50,
      time_unit = "hours"
    ),
    "'arg' should be one of"
  )
})

test_that("split_rr_phases handles edge cases", {
  rr_intervals <- data.frame(time = rep(1, 100))
  session_info <- list(duration = 100)

  # laying = duration
  result2 <- split_rr_phases(
    rr_intervals,
    session_info,
    100,
    0,
    0,
    time_unit = "seconds"
  )
  expect_true(all(result2$phase == "laying"))

  # laying/transition = 0
  result3 <- split_rr_phases(
    rr_intervals,
    session_info,
    0,
    0,
    50,
    time_unit = "seconds"
  )
  expect_true(all(result3$phase == "standing"))
})


test_that("split_rr_phases handles non-centered transitions and exact boundaries", {
  # Non-centered: transition taken from the laying phase only
  result <- split_rr_phases(
    data.frame(time = rep(1, 100)),
    list(duration = 100),
    laying_time = 30,
    transition_time = 20,
    standing_time = 50,
    centered_transition = FALSE,
    time_unit = "seconds"
  )
  expect_equal(sum(result$phase == "laying"), 30)
  expect_equal(sum(result$phase == "transition"), 20)
  expect_equal(sum(result$phase == "standing"), 50)
})

test_that("split_rr_phases does not alter the beat intervals themselves", {
  # Phase assignment must not stretch or redistribute interval values;
  # elapsed time is exactly the cumulative sum of the input intervals
  times <- c(rep(1000, 30), 1500, rep(1000, 29))
  p <- split_rr_phases(
    data.frame(time = times),
    list(duration = 60.5),
    laying_time = 30,
    transition_time = 0,
    standing_time = 30.5,
    centered_transition = FALSE
  )
  expect_equal(p$time, times)
  expect_equal(p$elapsed_time, cumsum(times) / 1000)
})

test_that("validate_rr throws errors for invalid inputs", {
  expect_error(
    validate_rr("not numeric"),
    "`rr_segment` must be a numeric vector."
  )
  expect_error(
    validate_rr(c(1, 2, NA)),
    "`rr_segment` cannot contain NA values."
  )
  expect_error(
    validate_rr(c(1, 2, -1)),
    "`rr_segment` must contain only positive values."
  )
})

test_that("validate_rr does not throw errors for valid inputs", {
  expect_no_error(validate_rr(c(1, 2, 3)))
  expect_no_error(validate_rr(1:100))
})

test_that("validate_validity throws errors for invalid inputs", {
  rr_segment <- c(1, 2, 3)
  expect_error(
    validate_validity("not logical", rr_segment),
    "`is_valid` must be a logical vector."
  )
  expect_error(
    validate_validity(c(TRUE, FALSE), rr_segment),
    "`is_valid` must have the same length as `rr_segment`."
  )
  expect_error(
    validate_validity(c(TRUE, FALSE, NA), rr_segment),
    "`is_valid` cannot contain NA values."
  )
})

test_that("validate_validity does not throw errors for valid inputs", {
  expect_no_error(validate_validity(c(TRUE, TRUE, TRUE), c(1, 2, 3)))
  expect_no_error(validate_validity(rep(TRUE, 100), 1:100))
})

test_that("rr_validate_measure_artifacts works correctly", {
  # Test case 1: Basic test with no artifacts.
  rr1 <- c(800, 900, 700, 850)
  result1 <- rr_validate_measure_artifacts(rr1)
  expect_equal(result1$is_valid, rep(TRUE, length(rr1)))
  expect_equal(result1$cleaned_rr, rr1)

  # Test case 2: Test with some artifacts and pre-existing invalid intervals.
  rr2 <- c(800, 65.535, 700, 65.535, 850)
  is_valid2 <- c(TRUE, TRUE, FALSE, TRUE, TRUE)
  result2 <- rr_validate_measure_artifacts(rr2, is_valid = is_valid2)
  expect_equal(result2$is_valid, c(TRUE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(result2$cleaned_rr, c(800, 850))

  # Test case 3: Test with all artifacts.
  rr3 <- c(65.535, 65.535, 65.535)
  result3 <- rr_validate_measure_artifacts(rr3)
  expect_equal(result3$is_valid, c(FALSE, FALSE, FALSE))
  expect_equal(length(result3$cleaned_rr), 0)
  expect_type(result3$cleaned_rr, "double")

  # Test case 4: Empty input.
  rr4 <- numeric(0)
  result4 <- rr_validate_measure_artifacts(rr4)
  expect_equal(result4$is_valid, logical(0))
  expect_equal(result4$cleaned_rr, numeric(0))

  # Test case 5: Only one valid value, no artifacts
  rr5 <- c(1200)
  result5 <- rr_validate_measure_artifacts(rr5)
  expect_equal(result5$is_valid, TRUE)
  expect_equal(result5$cleaned_rr, 1200)

  # Test case 6: Only one value, which is an artifact.
  rr6 <- c(65.535)
  result6 <- rr_validate_measure_artifacts(rr6)
  expect_equal(result6$is_valid, FALSE)
  expect_equal(length(result6$cleaned_rr), 0)
})

test_that("rr_validate_measure_artifacts handles input errors correctly", {
  # rr_segment errors:
  expect_error(
    rr_validate_measure_artifacts(c("a", "b")),
    "`rr_segment` must be a numeric vector."
  )
  expect_error(
    rr_validate_measure_artifacts(c(-1, 1)),
    "`rr_segment` must contain only positive values."
  )

  # is_valid errors
  expect_error(
    rr_validate_measure_artifacts(c(1, 2), is_valid = c(1, 2)),
    "`is_valid` must be a logical vector."
  )
  expect_error(
    rr_validate_measure_artifacts(c(1, 2), is_valid = c(TRUE)),
    "`is_valid` must have the same length as `rr_segment`."
  )
})

test_that("rr_validate_physiological works correctly", {
  # Test case 1: Basic test with a mix of valid and invalid values.
  rr1 <- c(250, 500, 1000, 1800, 2500)
  result1 <- rr_validate_physiological(rr1)
  expect_equal(result1$is_valid, c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_equal(result1$cleaned_rr, c(500, 1000, 1800))

  # Test case 2: Test with pre-existing invalid intervals.
  rr2 <- c(250, 500, 1000, 1800, 2500)
  is_valid2 <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  result2 <- rr_validate_physiological(rr2, is_valid = is_valid2)
  expect_equal(result2$is_valid, c(FALSE, FALSE, TRUE, TRUE, FALSE))

  # Test case 3: Empty input.
  result3 <- rr_validate_physiological(numeric(0))
  expect_equal(result3$is_valid, logical(0))
  expect_equal(result3$cleaned_rr, numeric(0))

  # Test case 4: All intervals equal to min_rr
  rr4 <- c(300, 300, 300)
  result4 <- rr_validate_physiological(rr4)
  expect_equal(result4$is_valid, rep(TRUE, 3))

  # Test case 5: All intervals equal to max_rr
  rr5 <- c(2000, 2000, 2000)
  result5 <- rr_validate_physiological(rr5)
  expect_equal(result5$is_valid, rep(TRUE, 3))
})

test_that("rr_validate_physiological handles input errors correctly", {
  # Input validation tests (assuming validate_rr and validate_validity are tested separately)
  # rr_segment errors:
  expect_error(
    rr_validate_physiological(c("a", "b")),
    "`rr_segment` must be a numeric vector."
  )
  expect_error(
    rr_validate_physiological(c(-1, 1)),
    "`rr_segment` must contain only positive values."
  )

  # is_valid errors
  expect_error(
    rr_validate_physiological(c(1, 2), is_valid = c(1, 2)),
    "`is_valid` must be a logical vector."
  )
  expect_error(
    rr_validate_physiological(c(1, 2), is_valid = c(TRUE)),
    "`is_valid` must have the same length as `rr_segment`."
  )

  # min_rr errors
  expect_error(
    rr_validate_physiological(c(1, 2), min_rr = "a"),
    "`min_rr` must be a single, finite, positive value."
  )
  expect_error(
    rr_validate_physiological(c(1, 2), min_rr = c(100, 200)),
    "`min_rr` must be a single, finite, positive value."
  )
  expect_error(
    rr_validate_physiological(c(1, 2), min_rr = Inf),
    "`min_rr` must be a single, finite, positive value."
  )
  expect_error(
    rr_validate_physiological(c(1, 2), min_rr = -1),
    "`min_rr` must be a single, finite, positive value."
  )
  expect_error(
    rr_validate_physiological(c(1, 2), min_rr = 0),
    "`min_rr` must be a single, finite, positive value."
  )

  # max_rr errors
  expect_error(
    rr_validate_physiological(c(1, 2), max_rr = "a"),
    "`max_rr` must be a single, finite, positive value."
  )
  expect_error(
    rr_validate_physiological(c(1, 2), max_rr = c(1000, 2000)),
    "`max_rr` must be a single, finite, positive value."
  )
  expect_error(
    rr_validate_physiological(c(1, 2), max_rr = Inf),
    "`max_rr` must be a single, finite, positive value."
  )
  expect_error(
    rr_validate_physiological(c(1, 2), max_rr = -1),
    "`max_rr` must be a single, finite, positive value."
  )
  expect_error(
    rr_validate_physiological(c(1, 2), max_rr = 0),
    "`max_rr` must be a single, finite, positive value."
  )
  expect_error(
    rr_validate_physiological(c(1, 2), min_rr = 1000, max_rr = 500),
    "`max_rr` must be greater than `min_rr`."
  )
})

test_that("moving_average_rr_validation works correctly", {
  # Test case 1: Basic test with a small RR segment and default parameters.
  rr1 <- c(800, 900, 700, 1500, 850)
  result1 <- moving_average_rr_validation(rr1, window_size = 5)
  expect_type(result1, "list")
  expect_named(result1, c("is_valid", "cleaned_rr"))
  expect_equal(length(result1$is_valid), length(rr1))
  expect_equal(result1$is_valid, c(TRUE, TRUE, TRUE, FALSE, TRUE))
  expect_equal(result1$cleaned_rr, rr1[result1$is_valid])

  # Test case 2: Test with pre-existing invalid intervals.
  rr2 <- c(800, 900, 700, 1500, 850)
  is_valid2 <- c(TRUE, FALSE, TRUE, TRUE, TRUE)
  result2 <- moving_average_rr_validation(
    rr2,
    is_valid = is_valid2,
    window_size = 5
  )
  expect_equal(result2$is_valid, c(TRUE, FALSE, TRUE, FALSE, TRUE))

  # Test case 3: Test with a different window size.
  rr3 <- c(800, 900, 700, 1500, 850, 900, 800)
  result3 <- moving_average_rr_validation(rr3, window_size = 3)
  expect_equal(result3$is_valid, c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE))

  # Test case 4: Test with a different threshold.
  rr4 <- c(800, 900, 700, 1500, 850)
  result4 <- moving_average_rr_validation(rr4, threshold = 0.5, window_size = 5)
  expect_equal(result4$is_valid, c(TRUE, TRUE, TRUE, FALSE, TRUE))

  # Test case 5: Test with a single valid interval (edge case).
  rr5 <- c(1000)
  result5 <- moving_average_rr_validation(rr5, window_size = 1)
  expect_equal(result5$is_valid, TRUE)
  expect_equal(result5$cleaned_rr, 1000)

  # Test case 7: Test with all intervals invalid (except one).
  rr6 <- c(800, 900, 5700, 1500, 3850)
  is_valid6 <- c(FALSE, FALSE, TRUE, FALSE, FALSE)
  result6 <- moving_average_rr_validation(
    rr6,
    is_valid = is_valid6,
    window_size = 5
  )
  expect_equal(result6$is_valid, is_valid6)

  # Test case 9: Test with identical values (to ensure no division by zero).
  rr7 <- c(1000, 1000, 1000, 1000, 1000)
  result7 <- moving_average_rr_validation(rr7, window_size = 5)
  expect_equal(result7$is_valid, rep(TRUE, 5))
})

test_that("moving_average_rr_validation handles input errors correctly", {
  # rr_segment errors:
  expect_error(
    moving_average_rr_validation(c("a", "b")),
    "`rr_segment` must be a numeric vector."
  )
  expect_error(
    moving_average_rr_validation(c(-1, 1)),
    "`rr_segment` must contain only positive values."
  )

  # is_valid errors
  expect_error(
    moving_average_rr_validation(c(1, 2), is_valid = c(1, 2)),
    "`is_valid` must be a logical vector."
  )
  expect_error(
    moving_average_rr_validation(c(1, 2), is_valid = c(TRUE)),
    "`is_valid` must have the same length as `rr_segment`."
  )
  # window_size errors
  expect_error(
    moving_average_rr_validation(c(1, 2), window_size = "a"),
    "`window_size` must be a single, finite, positive, odd integer.",
    fixed = TRUE
  )
  expect_error(
    moving_average_rr_validation(c(1, 2), window_size = c(1, 3)),
    "`window_size` must be a single, finite, positive, odd integer.",
    fixed = TRUE
  )
  expect_error(
    moving_average_rr_validation(c(1, 2), window_size = Inf),
    "`window_size` must be a single, finite, positive, odd integer.",
    fixed = TRUE
  )
  expect_error(
    moving_average_rr_validation(c(1, 2), window_size = 2.5),
    "`window_size` must be a single, finite, positive, odd integer.",
    fixed = TRUE
  )
  expect_error(
    moving_average_rr_validation(c(1, 2), window_size = -1),
    "`window_size` must be a single, finite, positive, odd integer.",
    fixed = TRUE
  )
  # Even window size should error only for centered windows
  expect_error(
    moving_average_rr_validation(
      c(1, 2),
      window_size = 2,
      centered_window = TRUE
    ),
    "`window_size` must be a single, finite, positive, odd integer.",
    fixed = TRUE
  )
  expect_warning(
    moving_average_rr_validation(c(1, 2), window_size = 5),
    "`window_size` is larger than the length of `rr_segment`.",
    fixed = TRUE
  )

  # threshold errors
  expect_error(
    moving_average_rr_validation(c(1, 2), threshold = "a", window_size = 1),
    "`threshold` must be a single finite numeric value.",
    fixed = TRUE
  )
  expect_error(
    moving_average_rr_validation(
      c(1, 2),
      threshold = c(0.1, 0.2),
      window_size = 1
    ),
    "`threshold` must be a single finite numeric value.",
    fixed = TRUE
  )
  expect_error(
    moving_average_rr_validation(c(1, 2), threshold = Inf, window_size = 1),
    "`threshold` must be a single finite numeric value.",
    fixed = TRUE
  )
  expect_error(
    moving_average_rr_validation(c(1, 2), threshold = -0.1, window_size = 1),
    "`threshold` must be between 0 and 1 (inclusive).",
    fixed = TRUE
  )
  expect_error(
    moving_average_rr_validation(c(1, 2), threshold = 1.1, window_size = 1),
    "`threshold` must be between 0 and 1 (inclusive).",
    fixed = TRUE
  )
})

test_that("rr_full_phase_processing works correctly with valid inputs", {
  rr_data <- c(65.535, 500, 300, 750, 800, 1500, 820, 780, 850, 2000, 800)
  result <- rr_full_phase_processing(rr_data)

  # Basic checks on output type and structure
  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("is_valid", "cleaned_rr"))
  expect_type(result$is_valid, "logical")
  expect_type(result$cleaned_rr, "double")
  expect_length(result$is_valid, length(rr_data))
  expect_length(result$cleaned_rr, sum(result$is_valid))

  # Check against a known result (manually calculated for this specific input)
  expected_is_valid <- c(
    FALSE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    TRUE,
    FALSE
  )
  expect_equal(result$is_valid, expected_is_valid)
  expect_equal(result$cleaned_rr, c(500, 2000))
})

test_that("rr_full_phase_processing handles custom parameters", {
  rr_data <- c(65.535, 700, 600, 750, 800, 1500, 820, 780, 850, 2000, 800)
  result <- rr_full_phase_processing(
    rr_data,
    min_rr = 400,
    max_rr = 1800,
    window_size = 5,
    threshold = 0.1
  )

  # Basic checks (as above)
  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("is_valid", "cleaned_rr"))
  expect_length(result$is_valid, length(rr_data))

  # Check against a *different* known result (using the custom parameters)
  expected_is_valid <- c(
    FALSE,
    TRUE,
    FALSE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    TRUE,
    FALSE,
    FALSE,
    TRUE
  )
  expect_equal(result$is_valid, expected_is_valid)
})

test_that("rr_full_phase_processing handles empty input", {
  suppressWarnings(result <- rr_full_phase_processing(numeric(0)))
  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("is_valid", "cleaned_rr"))
  expect_length(result$is_valid, 0)
  expect_length(result$cleaned_rr, 0)
})

test_that("rr_full_phase_processing handles all invalid input (all 65.535)", {
  rr_data <- rep(65.535, 5)
  result <- suppressWarnings(rr_full_phase_processing(rr_data, window_size = 1))
  expect_true(all(!result$is_valid)) # All should be invalid
  expect_length(result$cleaned_rr, 0)
})

test_that("rr_full_phase_processing handles input with only one valid value", {
  rr_data <- c(65.535, 65.535, 800, 65.535)
  result <- rr_full_phase_processing(rr_data, window_size = 1)
  expect_equal(sum(result$is_valid), 1)
  expect_equal(result$cleaned_rr, 800)
})

test_that("rr_full_phase_processing works with various window sizes", {
  rr_data <- c(
    0.8,
    0.9,
    0.7,
    1.5,
    0.85,
    0.95,
    0.88,
    0.92,
    0.8,
    4.0
  ) *
    1000

  # Test with a smaller window size
  result_small <- rr_full_phase_processing(rr_data, window_size = 3)
  expect_type(result_small$is_valid, "logical")

  # Test with the default window size
  result_default <- rr_full_phase_processing(rr_data)
  expect_type(result_default$is_valid, "logical")
})

test_that("rr_full_phase_processing works with various thresholds", {
  rr_data <- c(
    0.8,
    0.9,
    0.7,
    1.5,
    0.85,
    0.95,
    0.88,
    0.92,
    0.8,
    4.0
  ) *
    1000

  # Test with a smaller threshold
  result_small_thresh <- rr_full_phase_processing(rr_data, threshold = 0.1)
  expect_type(result_small_thresh$is_valid, "logical")

  # Test with a larger threshold
  result_large_thresh <- rr_full_phase_processing(rr_data, threshold = 0.3)
  expect_type(result_large_thresh$is_valid, "logical")
})
