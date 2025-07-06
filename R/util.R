roll_fun <- function(data, fun, window = median, center = TRUE) {
  n <- length(data)
  result <- numeric(n)

  for (i in seq_len(n)) {
    start_idx <- max(1, i - window + 1)
    window_data <- x[start_idx:i]

    result[i] <- if (sum(!is.na(window_data)) >= min_obs) {
      mean(window_data, na.rm = TRUE)
    } else {
      NA_real_
    }
  }

  return(result)
}
