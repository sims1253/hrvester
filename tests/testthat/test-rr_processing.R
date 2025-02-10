check_phase_transitions <- function(result, laying_time, transition_time, duration) {
  # 1. Check for correct order of phases (laying -> transition -> standing)
  phases <- unique(result$phase)
  expect_equal(phases, c("laying", "transition", "standing"), info = "Phases are not in the correct order")

  # 2. Find transition points (where the phase changes)
  transition_points <- which(diff(as.numeric(factor(result$phase))) != 0)

  # 3. Check approximate locations of transitions, with tolerance
  if (length(transition_points) >= 1) {
    # Laying -> Transition
    expected_laying_end <- laying_time / (duration / nrow(result))
    expect_equal(transition_points[1], expected_laying_end, tolerance = 1, info = "Laying -> Transition transition point is incorrect")
  }

  if (length(transition_points) >= 2) {
    # Transition -> Standing
    expected_transition_end <- (laying_time + transition_time) / (duration / nrow(result))
    expect_equal(transition_points[2], expected_transition_end, tolerance = 1, info = "Transition -> Standing transition point is incorrect")
  }

  if (length(transition_points) < 2 || length(transition_points) > 2) {
    warning("Unexpected number of transition points") # Handles the edge cases
  }
}

test_that("split_rr_phases splits data correctly (valid inputs)", {
  rr_intervals <- data.frame(time = 1:100)
  session_info <- list(duration = 100)
  laying_time <- 30
  transition_time <- 20
  standing_time <- 50

  result <- split_rr_phases(rr_intervals, session_info, laying_time, transition_time, standing_time)

  expect_true(is.data.frame(result))
  expect_true("phase" %in% colnames(result))
  check_phase_transitions(result, laying_time, transition_time, session_info$duration)
})

test_that("split_rr_phases handles empty rr_intervals", {
  suppressWarnings(result <- split_rr_phases(data.frame(time = numeric()), list(duration = 100), 30, 20, 50))
  expect_true(is.data.frame(result))
  expect_true("phase" %in% colnames(result))
  expect_true(nrow(result) == 0)
  expect_true(is.character(result$phase))
})

test_that("split_rr_phases throws errors for invalid inputs", {
  # Invalid rr_intervals
  expect_error(split_rr_phases("not a df", list(duration = 100), 30, 20, 50), "rr_intervals must be a data frame")
  expect_error(split_rr_phases(data.frame(no_time = 1:10), list(duration = 100), 30, 20, 50), "rr_intervals must contain a 'time' column")

  # Invalid session_info
  expect_error(split_rr_phases(data.frame(time = 1:10), "not a list", 30, 20, 50), "session_info must be a list")
  expect_error(split_rr_phases(data.frame(time = 1:10), list(), 30, 20, 50), "session_info must be a list containing a 'duration' element")
  expect_error(split_rr_phases(data.frame(time = 1:10), list(duration = "a"), 30, 20, 50), "session_info\\$duration must be a positive numeric value")
  expect_error(split_rr_phases(data.frame(time = 1:10), list(duration = -10), 30, 20, 50), "session_info\\$duration must be a positive numeric value")

  # Invalid laying_time, transition_time, and standing_time
  invalid_times <- list(
    laying_time = list("a", -1),
    transition_time = list("a", -1),
    standing_time = list("a", -1)
  )
  purrr::walk(names(invalid_times), ~ {
    args <- list(rr_intervals = data.frame(time = 1:10), session_info = list(duration = 100), laying_time = 30, transition_time = 20, standing_time = 50)
    outer_x <- .x
    purrr::walk(invalid_times[[.x]], ~ {
      args[[outer_x]] <- .
      expect_error(do.call(split_rr_phases, args))
    })
  })

  expect_error(
    split_rr_phases(data.frame(time = 1:10), list(duration = 100), 50, 20, 51),
    "The sum of laying_time, and standing_time cannot exceed session_info\\$duration."
  )
  expect_error(
    split_rr_phases(data.frame(time = 1:10), list(duration = 100), 50, 20, 10),
    "transition_time must not exceed the standing_time."
  )
})

test_that("split_rr_phases handles edge cases", {
  rr_intervals <- data.frame(time = 1:100)
  session_info <- list(duration = 100)

  # laying = duration
  result2 <- split_rr_phases(rr_intervals, session_info, 100, 0, 0)
  expect_true(all(result2$phase == "laying"))

  # laying/transition = 0
  result3 <- split_rr_phases(rr_intervals, session_info, 0, 0, 50)
  expect_true(all(result3$phase == "standing"))
})


test_that("split_rr_phases handles different row counts and non-integer durations", {
  rr_intervals <- data.frame(time = 1:50)
  session_info <- list(duration = 73.5) # Non Integer Duration
  laying_time <- 30
  transition_time <- 20
  standing_time <- 43.5
  result <- split_rr_phases(rr_intervals, session_info, laying_time, transition_time, standing_time)
  expect_true(nrow(result) == 50)
  expect_equal(result$elapsed_time[1], 73.5 / 50) # Check first elapsed time
  expect_equal(result$elapsed_time[50], 73.5) # last time is the total duration
  check_phase_transitions(result, laying_time, transition_time, session_info$duration) # Check transitions
})
