#' Split RR Intervals Into Orthostatic Test Phases
#'
#' This function splits RR intervals into the orthostatic test phases based on time thresholds.
#'
#' @param rr_intervals A data frame containing RR interval data with a 'time' column.
#' @param session_info Session information (currently unused).  Consider removing this parameter.
#' @param laying_time The time threshold (in the same units as the 'time' column in `rr_intervals`) for the laying phase.
#' @param transition_time The duration of the transition phase (in the same units as the 'time' column in `rr_intervals`).
#'
#' @return A data frame with an added 'phase' column indicating the phase ("laying", "transition", or "standing").
#' @export
split_rr_phases <- function(
    rr_intervals,
    session_info,
    laying_time,
    transition_time) {
  rr_intervals$rr <- rr_intervals$time
  rr_intervals$time <- seq(
    from = session_info$duration / nrow(rr_intervals),
    to = session_info$duration,
    by = session_info$duration / nrow(rr_intervals)
  )
  rr_intervals <- rr_intervals %>%
    mutate(phase = case_when(
      time <= laying_time ~ "laying",
      time <= laying_time + transition_time ~ "transition",
      .default = "standing"
    ))
  return(rr_intervals)
}
