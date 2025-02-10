#' Split RR Intervals Into Orthostatic Test Phases
#'
#' This function splits RR intervals into the orthostatic test phases
#' (laying, transition, and standing) based on specified time thresholds.
#'
#' @param rr_intervals A data frame containing RR interval data.  Must include
#'   a column named `time`.  The function will add an `elapsed_time` column
#'   and a `phase` column.
#' @param session_info A list containing session information.  Must include a
#'   numeric element named `duration` representing the total duration of the
#'   recording (in the same units as `laying_time`, `transition_time`, and
#'   `standing_time`).
#' @param laying_time Numeric. The duration of the laying phase (e.g., in
#'   seconds). Must be non-negative.
#' @param transition_time Numeric. The duration of the transition phase (e.g.,
#'   in seconds). Must be non-negative.
#' @param standing_time Numeric. The duration of the standing phase (e.g., in
#'   seconds). Must be non-negative.
#'
#' @return A data frame with the same data as `rr_intervals`, but with two
#'   added columns:
#'   \item{elapsed_time}{The cumulative time from the start of the recording.}
#'   \item{phase}{A character string indicating the phase: "laying",
#'   "transition", or "standing".}
#'
#' @details The sum of `laying_time` and `standing_time` cannot exceed
#'   `session_info$duration`. If `rr_intervals` is an empty data frame, a
#'   warning is issued, and an empty data frame with a `phase` column is
#'   returned.
#'
#' @export
#'
#' @examples
#' # Example with valid inputs
#' rr_data <- data.frame(time = 1:100)
#' session_data <- list(duration = 100)
#' result <- split_rr_phases(rr_data, session_data, 30, 20, 50)
#' head(result)
#'
#' # Example with an empty rr_intervals data frame
#' rr_empty <- data.frame(time = numeric())
#' session_data <- list(duration = 60)
#' result_empty <- split_rr_phases(rr_empty, session_data, 20, 10, 30)
#' print(result_empty)
#'
#' \dontrun{
#' # Example with invalid input (sum of times exceeds duration)
#' rr_data <- data.frame(time = 1:100)
#' session_data <- list(duration = 100)
#' # This will throw an error
#' result_error <- split_rr_phases(rr_data, session_data, 50, 60, 70)
#' }
split_rr_phases <- function(
    rr_intervals,
    session_info,
    laying_time = 180,
    transition_time = 20,
    standing_time = 180) {
  # rr_intervals checks
  if (!is.data.frame(rr_intervals)) {
    stop("rr_intervals must be a data frame.")
  }
  if (!("time" %in% colnames(rr_intervals))) {
    stop("rr_intervals must contain a 'time' column.")
  }
  if (nrow(rr_intervals) == 0) {
    warning("rr_intervals is an empty data frame. An empty data.frame with phase column will be returned.")
    rr_intervals$phase <- character() # Add the 'phase' column
    return(rr_intervals)
  }

  # session_info checks
  if (!is.list(session_info) || is.null(session_info$duration)) {
    stop("session_info must be a list containing a 'duration' element.")
  }
  if (!is.numeric(session_info$duration) || session_info$duration <= 0) {
    stop("session_info$duration must be a positive numeric value")
  }

  # laying_time checks
  if (!is.numeric(laying_time) || length(laying_time) != 1 || laying_time < 0) {
    stop("laying_time must be a non-negative numeric value.")
  }

  # transition_time checks
  if (!is.numeric(transition_time) || length(transition_time) != 1 || transition_time < 0) {
    stop("transition_time must be a non-negative numeric value.")
  }

  # standing_time checks
  if (!is.numeric(standing_time) || length(standing_time) != 1 || standing_time < 0) {
    stop("standing_time must be a non-negative numeric value.")
  }

  # Time duration checks
  if (laying_time + standing_time > session_info$duration) {
    stop("The sum of laying_time, and standing_time cannot exceed session_info$duration.")
  }
  if (transition_time > standing_time) {
    stop("transition_time must not exceed the standing_time.")
  }


  rr_intervals$elapsed_time <- seq(
    from = session_info$duration / nrow(rr_intervals),
    to = session_info$duration,
    by = session_info$duration / nrow(rr_intervals)
  )

  rr_intervals <- rr_intervals %>%
    mutate(phase = case_when(
      elapsed_time <= laying_time ~ "laying",
      elapsed_time <= laying_time + transition_time ~ "transition",
      .default = "standing"
    ))
  return(rr_intervals)
}
