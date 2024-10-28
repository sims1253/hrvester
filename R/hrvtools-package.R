#' ostdashr: Simple Heart Rate Variability Analysis
#'
#' @description
#' Provides tools for analyzing Heart Rate Variability (HRV) from FIT files.
#' Focuses on orthostatic test analysis (laying to standing measurements).
#'
#' @section Main Functions:
#' - process_fit_directory(): Process multiple FIT files with caching
#' - process_fit_file(): Process a single FIT file
#' - plot_hrv_trend(): Create trend plots of HRV metrics
#'
#' @importFrom dplyr %>% filter arrange bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs theme_minimal facet_wrap
#' @importFrom furrr future_map_dfr
#'
#' @keywords internal
"_PACKAGE"
