#' Run HRV Processing Test Suite on FIT Files
#'
#' This function processes one or more FIT files using `process_fit_file`,
#' allowing for batch testing with specified parameters. It captures key HRV
#' metrics and processing parameters for each file, along with any errors
#' encountered.
#'
#' @param file_paths A character vector of full paths to `.fit` files, or a
#'   single path to a directory containing `.fit` files.
#' @param output_file Optional. Path to a CSV file where results will be saved.
#'   If `NULL` (default), results are returned as a tibble.
#' @param sport_name Character. The name of the sport to filter for in the FIT
#'   files. Default is "OST". Passed to `process_fit_file`.
#' @param laying_time Numeric. Duration of the laying phase in seconds.
#'   Default 180. Passed to `process_fit_file`.
#' @param transition_time Numeric. Duration of the transition phase in seconds.
#'   Default 20. Passed to `process_fit_file`.
#' @param standing_time Numeric. Duration of the standing phase in seconds.
#'   Default 180. Passed to `process_fit_file`.
#' @param warmup Numeric. Initial duration in seconds to discard as warmup.
#'   Default 70. Passed to `process_fit_file`.
#' @param min_rr Numeric. Minimum RR interval in milliseconds. Default 272.
#'   Passed to `process_fit_file`.
#' @param max_rr Numeric. Maximum RR interval in milliseconds. Default 2000.
#'   Passed to `process_fit_file`.
#' @param pff_window_size Numeric. Window size for the moving average validation
#'   in `process_fit_file`. Default 7. Passed to `window_size` argument of
#'   `process_fit_file`.
#' @param pff_threshold_stand Numeric. Threshold for the moving average validation
#'   for the standing phase in `process_fit_file`. Default 0.2. Passed to
#'   `threshold_stand` argument of `process_fit_file`.
#' @param pff_threshold_lay_trans Numeric. Threshold for the moving average validation
#'   for the laying and transition phases in `process_fit_file`. Default 0.17. Passed to
#'   `threshold_lay_trans` argument of `process_fit_file`.
#' @param pff_centered_window Logical. Whether the moving window should be
#'   centered in `process_fit_file`. Default `FALSE`. Passed to
#'   `centered_window` argument of `process_fit_file`.
#' @param pff_centered_transition Logical. Whether the transition phase should
#'   be centered around the time boundary in `process_fit_file`. Default `TRUE`.
#'   Passed to `centered_transition` argument of `process_fit_file`.
#'
#' @return A tibble containing a summary of processing results for each file,
#'   including input parameters, key HRV metrics, and any error messages.
#'   If `output_file` is specified, this tibble is also saved as a CSV file.
#'
#' @examples
#' \dontrun{
#' # Assuming you have FIT files in a directory "path/to/fit_files"
#' # And process_fit_file is loaded
#' results <- run_hrv_test_suite(file_paths = "path/to/fit_files",
#'                               output_file = "results.csv")
#' print(results)
#'
#' # Processing specific files
#' specific_files <- c("path/to/file1.fit", "path/to/file2.fit")
#' results_specific <- run_hrv_test_suite(file_paths = specific_files)
#' }
#'
run_hrv_test_suite <- function(
    file_paths,
    output_file = NULL,
    sport_name = "OST",
    laying_time = 180,
    transition_time = 20,
    standing_time = 180,
    warmup = 70,
    min_rr = 272,
    max_rr = 2000,
    pff_window_size = 7,
    pff_threshold_stand = 0.2, # Renamed from pff_threshold
    pff_threshold_lay_trans = 0.17, # New parameter
    pff_centered_window = FALSE,
    pff_centered_transition = TRUE) {

  # Validate file_paths and resolve to a list of .fit files
  actual_file_paths <- c()
  if (length(file_paths) == 1 && dir.exists(file_paths)) {
    actual_file_paths <- list.files(
      path = file_paths,
      pattern = "\\.fit$",
      full.names = TRUE,
      recursive = FALSE
    )
    if (length(actual_file_paths) == 0) {
      warning("No .fit files found in the directory: ", file_paths)
      return(tibble::tibble()) # Return empty tibble
    }
  } else if (is.character(file_paths)) {
    actual_file_paths <- file_paths[file.exists(file_paths) & grepl("\\.fit$", file_paths, ignore.case = TRUE)]
    non_existent_files <- setdiff(file_paths, actual_file_paths)
    if (length(non_existent_files) > 0) {
      warning("The following files do not exist or are not .fit files and will be skipped: ",
              paste(non_existent_files, collapse = ", "))
    }
    if (length(actual_file_paths) == 0) {
      warning("No valid .fit files found in the provided paths.")
      return(tibble::tibble()) # Return empty tibble
    }
  } else {
    stop("file_paths must be a character vector of file paths or a single directory path.")
  }

  all_summaries <- list()

  for (file_path in actual_file_paths) {
    current_summary <- NULL
    tryCatch({
      result_pff <- process_fit_file(
        file_path = file_path,
        sport_name = sport_name,
        laying_time = laying_time,
        transition_time = transition_time,
        standing_time = standing_time,
        warmup = warmup,
        min_rr = min_rr,
        max_rr = max_rr,
        window_size = pff_window_size,
        threshold_stand = pff_threshold_stand,     # Pass to renamed parameter
        threshold_lay_trans = pff_threshold_lay_trans, # Pass new parameter
        centered_window = pff_centered_window,
        centered_transition = pff_centered_transition
      )

      # Check if result_pff is a tibble and has rows
      if (tibble::is_tibble(result_pff) && nrow(result_pff) > 0) {
        # Check if essential metrics are present and not all NA
        # (e.g. if sport_name filter caused empty useful data)
        if ("laying_rmssd" %in% names(result_pff) && !all(is.na(result_pff$laying_rmssd))) {
          current_summary <- tibble::tibble(
            source_file = utils::basename(file_path),
            date = result_pff$date[1], # Assuming date is same for all rows if multiple
            param_pff_window_size = pff_window_size,
            param_pff_threshold_stand = pff_threshold_stand, # Renamed column
            param_pff_threshold_laying_transition = pff_threshold_lay_trans, # Use new parameter value
            param_pff_centered_window = pff_centered_window,
            param_pff_centered_transition = pff_centered_transition,
            laying_rmssd = result_pff$laying_rmssd[1],
            standing_rmssd = result_pff$standing_rmssd[1],
            laying_sdnn = result_pff$laying_sdnn[1],
            standing_sdnn = result_pff$standing_sdnn[1],
            error_message = NA_character_
          )
        } else { # Processed, but essential HRV metrics might be NA (e.g., wrong sport)
           date_val <- if("date" %in% names(result_pff)) result_pff$date[1] else NA_character_
           current_summary <- tibble::tibble(
            source_file = utils::basename(file_path),
            date = date_val,
            param_pff_window_size = pff_window_size,
            param_pff_threshold_stand = pff_threshold_stand, # Renamed column
            param_pff_threshold_laying_transition = pff_threshold_lay_trans, # Use new parameter value
            param_pff_centered_window = pff_centered_window,
            param_pff_centered_transition = pff_centered_transition,
            laying_rmssd = NA_real_,
            standing_rmssd = NA_real_,
            laying_sdnn = NA_real_,
            standing_sdnn = NA_real_,
            error_message = "Processed but no primary HRV metrics calculated (e.g., wrong sport, or insufficient data for phases)"
          )
        }
      } else { # process_fit_file returned empty tibble (e.g. file error before HRV calc)
        current_summary <- tibble::tibble(
          source_file = utils::basename(file_path),
          date = NA_character_,
          param_pff_window_size = pff_window_size,
          param_pff_threshold_stand = pff_threshold_stand, # Renamed column
          param_pff_threshold_laying_transition = pff_threshold_lay_trans, # Use new parameter value
          param_pff_centered_window = pff_centered_window,
          param_pff_centered_transition = pff_centered_transition,
          laying_rmssd = NA_real_,
          standing_rmssd = NA_real_,
          laying_sdnn = NA_real_,
          standing_sdnn = NA_real_,
          error_message = "File not processed by process_fit_file or result was empty"
        )
      }
    }, error = function(e) {
      current_summary <<- tibble::tibble( # Use <<- to assign to current_summary in outer scope
        source_file = utils::basename(file_path),
        date = NA_character_,
        param_pff_window_size = pff_window_size,
        param_pff_threshold_stand = pff_threshold_stand, # Renamed column
        param_pff_threshold_laying_transition = pff_threshold_lay_trans, # Use new parameter value
        param_pff_centered_window = pff_centered_window,
        param_pff_centered_transition = pff_centered_transition,
        laying_rmssd = NA_real_,
        standing_rmssd = NA_real_,
        laying_sdnn = NA_real_,
        standing_sdnn = NA_real_,
        error_message = as.character(e$message)
      )
    })
    all_summaries[[length(all_summaries) + 1]] <- current_summary
  }

  if (length(all_summaries) == 0) {
    final_summary_tibble <- tibble::tibble( # Define empty structure if no files processed
        source_file = character(), date = character(),
        param_pff_window_size = numeric(), param_pff_threshold_stand = numeric(), # Renamed column
        param_pff_threshold_laying_transition = numeric(), # Column reflects new parameter
        param_pff_centered_window = logical(), param_pff_centered_transition = logical(),
        laying_rmssd = numeric(), standing_rmssd = numeric(),
        laying_sdnn = numeric(), standing_sdnn = numeric(),
        error_message = character()
    )
  } else {
    final_summary_tibble <- dplyr::bind_rows(all_summaries)
  }


  if (!is.null(output_file)) {
    if (!is.character(output_file) || length(output_file) != 1) {
      warning("output_file path must be a single character string. Results not saved to CSV.")
    } else {
      tryCatch({
        readr::write_csv(final_summary_tibble, output_file)
      }, error = function(e) {
        warning("Error writing results to CSV file: ", output_file, "\n", e$message)
      })
    }
  }

  return(final_summary_tibble)
}

# Needed functions (assuming process_fit_file is in the global environment or sourced)
# library(tibble)
# library(dplyr)
# library(readr)
# library(utils) # for basename
# source("R/data-processing.R") # If process_fit_file is not part of a loaded package
# source("R/hrv_analysis.R") # For calculate_hrv etc.
# source("R/rr_processing.R") # For rr_full_phase_processing etc.
# source("R/utils.R") # For create_empty_result (though not directly used by run_hrv_test_suite)
# source("R/plotting_functions.R") # Not used here
# source("R/globals.R") # Not used here
# source("R/zzz.R") # Not used here

# Note: For this to run, `process_fit_file` and its dependencies must be available.
# If this script is part of a package, dependencies are handled by NAMESPACE.
# If run as a standalone script, dependent scripts must be sourced or functions loaded.
# Explicit package::function calls are used for tibble, dplyr, readr, utils.
# `process_fit_file` is assumed to be available.


#' Find Stable HRV Processing Parameters
#'
#' Evaluates different sets of processing parameters for HRV analysis by
#' perturbing each parameter and observing the stability of RMSSD values.
#'
#' @param fit_files_directory Character string. Path to the directory containing
#'   FIT files to be processed.
#' @param param_grid A tibble or data.frame where each row represents a core
#'   set of parameters to evaluate. Expected columns: `pff_window_size`,
#'   `pff_threshold_lay_trans`, `pff_threshold_stand`, `pff_centered_window`.
#' @param output_csv_path Optional character string. Path to save the results
#'   as a CSV file.
#' @param sport_name Character. Sport name to filter by in `process_fit_file`.
#'   Default is "OST".
#' @param laying_time Numeric. Duration of laying phase. Default 180.
#' @param transition_time Numeric. Duration of transition phase. Default 20.
#' @param standing_time Numeric. Duration of standing phase. Default 180.
#' @param warmup Numeric. Warmup duration. Default 70.
#' @param min_rr Numeric. Minimum RR interval. Default 272.
#' @param max_rr Numeric. Maximum RR interval. Default 2000.
#' @param pff_centered_transition Logical. For `process_fit_file`. Default TRUE.
#' @param window_size_perturbation Numeric. Amount to perturb window size by
#'   (e.g., current +/- this value). Default 2.
#' @param threshold_perturbation Numeric. Amount to perturb threshold values by.
#'   Default 0.02.
#'
#' @return A tibble, sorted by `overall_sensitivity` (ascending) and then by
#'   `num_valid_files_core` (descending). Each row corresponds to a core
#'   parameter set from `param_grid` and includes its average RMSSD values for
#'   laying and standing phases, the number of valid files processed, and
#'   calculated sensitivity metrics (`sensitivity_lay`, `sensitivity_stand`,
#'   `overall_sensitivity`).
#'
#' @details
#' The function iterates through each parameter set in `param_grid`. For each set:
#' 1. It runs `run_hrv_test_suite` with the core parameters.
#' 2. It then perturbs each of the four key parameters (`pff_window_size`,
#'    `pff_threshold_lay_trans`, `pff_threshold_stand`, `pff_centered_window`)
#'    one at a time, creating "neighbor" parameter sets.
#'    - `pff_window_size` is perturbed by `+/- window_size_perturbation`.
#'      Only valid odd positive window sizes are tested.
#'    - Thresholds are perturbed by `+/- threshold_perturbation`.
#'      Only valid thresholds (e.g., >0.01 and <1.0) are tested.
#'    - `pff_centered_window` is flipped (TRUE to FALSE or vice-versa).
#' 3. For each neighbor set, `run_hrv_test_suite` is called.
#' 4. The absolute difference in average RMSSD (laying and standing) between
#'    the core run and each neighbor run is calculated.
#' 5. `sensitivity_lay` and `sensitivity_stand` are the mean of these
#'    absolute differences for laying and standing phases, respectively.
#' 6. `overall_sensitivity` is the sum of `sensitivity_lay` and
#'    `sensitivity_stand`. If core parameters yield no valid files or if no
#'    valid neighbor comparisons can be made, sensitivity might be `NA` or `Inf`.
#'
#' The goal is to identify parameter sets that produce consistent RMSSD values
#' even when the parameters are slightly changed, indicating robustness.
#'
#' @export
#' @importFrom dplyr arrange bind_rows filter desc
#' @importFrom readr write_csv
#' @importFrom tibble tibble is_tibble
#' @importFrom utils tail
#'
find_stable_hrv_parameters <- function(
  fit_files_directory,
  param_grid,
  output_csv_path = NULL,
  sport_name = "OST",
  laying_time = 180,
  transition_time = 20,
  standing_time = 180,
  warmup = 70,
  min_rr = 272,
  max_rr = 2000,
  pff_centered_transition = TRUE,
  window_size_perturbation = 2,
  threshold_perturbation = 0.02
) {

  if (!is.data.frame(param_grid)) {
    stop("param_grid must be a data.frame or tibble.")
  }
  expected_cols <- c("pff_window_size", "pff_threshold_lay_trans", "pff_threshold_stand", "pff_centered_window")
  if (!all(expected_cols %in% names(param_grid))) {
    stop(paste("param_grid must contain columns:", paste(expected_cols, collapse = ", ")))
  }

  all_param_evaluations <- list()

  message(sprintf("Starting evaluation of %d core parameter sets.", nrow(param_grid)))

  # Helper function to run suite and get mean RMSSDs
  .run_and_get_avg_rmssd <- function(current_run_params,
                                     fit_dir = fit_files_directory,
                                     s_name = sport_name,
                                     l_time = laying_time,
                                     t_time = transition_time,
                                     s_time = standing_time,
                                     w_up = warmup,
                                     min_r = min_rr,
                                     max_r = max_rr,
                                     pff_c_trans = pff_centered_transition) {
    # Ensure current_run_params is a list or data.frame for consistent access
    if (!is.list(current_run_params) && !is.data.frame(current_run_params)) {
        stop(".run_and_get_avg_rmssd: current_run_params must be a list or data.frame")
    }

    results_run <- run_hrv_test_suite(
      file_paths = fit_dir,
      sport_name = s_name,
      laying_time = l_time,
      transition_time = t_time,
      standing_time = s_time,
      warmup = w_up,
      min_rr = min_r,
      max_rr = max_r,
      pff_window_size = current_run_params$pff_window_size,
      pff_threshold_stand = current_run_params$pff_threshold_stand,
      pff_threshold_lay_trans = current_run_params$pff_threshold_lay_trans,
      pff_centered_window = current_run_params$pff_centered_window,
      pff_centered_transition = pff_c_trans,
      output_file = NULL
    )

    valid_results <- results_run %>%
      dplyr::filter(is.na(error_message) & !is.na(laying_rmssd) & !is.na(standing_rmssd))

    if (nrow(valid_results) == 0) {
      return(list(avg_lay = NA_real_, avg_stand = NA_real_, num_valid = 0))
    } else {
      return(list(
        avg_lay = mean(valid_results$laying_rmssd, na.rm = TRUE),
        avg_stand = mean(valid_results$standing_rmssd, na.rm = TRUE),
        num_valid = nrow(valid_results)
      ))
    }
  }


  for (i in 1:nrow(param_grid)) {
    core_params_row <- param_grid[i, ]
    message(sprintf("Processing core parameter set %d of %d: ws=%d, th_lt=%.2f, th_s=%.2f, cw=%s",
                    i, nrow(param_grid),
                    core_params_row$pff_window_size,
                    core_params_row$pff_threshold_lay_trans,
                    core_params_row$pff_threshold_stand,
                    as.character(core_params_row$pff_centered_window)))

    core_run_output <- .run_and_get_avg_rmssd(core_params_row)
    avg_rmssd_core_lay <- core_run_output$avg_lay
    avg_rmssd_core_stand <- core_run_output$avg_stand
    num_valid_files_core <- core_run_output$num_valid

    if (num_valid_files_core == 0 || is.na(avg_rmssd_core_lay) || is.na(avg_rmssd_core_stand)) {
      message("  Core parameters resulted in no valid files or NA RMSSD. Assigning Inf sensitivity.")
      all_param_evaluations[[length(all_param_evaluations) + 1]] <- tibble::tibble(
        pff_window_size = core_params_row$pff_window_size,
        pff_threshold_lay_trans = core_params_row$pff_threshold_lay_trans,
        pff_threshold_stand = core_params_row$pff_threshold_stand,
        pff_centered_window = core_params_row$pff_centered_window,
        avg_rmssd_core_lay = NA_real_,
        avg_rmssd_core_stand = NA_real_,
        num_valid_files_core = num_valid_files_core, # Could be 0
        sensitivity_lay = Inf,
        sensitivity_stand = Inf,
        overall_sensitivity = Inf
      )
      next
    }

    neighbor_diffs_lay <- c()
    neighbor_diffs_stand <- c()

    # Perturb pff_window_size
    ws_neighbors_values <- c(core_params_row$pff_window_size - window_size_perturbation,
                             core_params_row$pff_window_size + window_size_perturbation)
    for (ws_val in ws_neighbors_values) {
      if (ws_val > 0 && ws_val %% 2 != 0) { # Must be positive and odd
        params_neighbor <- core_params_row
        params_neighbor$pff_window_size <- ws_val
        neighbor_run_output <- .run_and_get_avg_rmssd(params_neighbor)
        if (neighbor_run_output$num_valid > 0 && !is.na(neighbor_run_output$avg_lay) && !is.na(neighbor_run_output$avg_stand)) {
          neighbor_diffs_lay <- c(neighbor_diffs_lay, abs(neighbor_run_output$avg_lay - avg_rmssd_core_lay))
          neighbor_diffs_stand <- c(neighbor_diffs_stand, abs(neighbor_run_output$avg_stand - avg_rmssd_core_stand))
        }
      }
    }

    # Perturb pff_threshold_lay_trans
    th_lt_neighbors_values <- c(core_params_row$pff_threshold_lay_trans - threshold_perturbation,
                                core_params_row$pff_threshold_lay_trans + threshold_perturbation)
    for (th_lt_val in th_lt_neighbors_values) {
      if (th_lt_val > 0.01 && th_lt_val < 1.0) { # Valid threshold range
        params_neighbor <- core_params_row
        params_neighbor$pff_threshold_lay_trans <- th_lt_val
        neighbor_run_output <- .run_and_get_avg_rmssd(params_neighbor)
         if (neighbor_run_output$num_valid > 0 && !is.na(neighbor_run_output$avg_lay) && !is.na(neighbor_run_output$avg_stand)) {
          neighbor_diffs_lay <- c(neighbor_diffs_lay, abs(neighbor_run_output$avg_lay - avg_rmssd_core_lay))
          neighbor_diffs_stand <- c(neighbor_diffs_stand, abs(neighbor_run_output$avg_stand - avg_rmssd_core_stand))
        }
      }
    }

    # Perturb pff_threshold_stand
    th_s_neighbors_values <- c(core_params_row$pff_threshold_stand - threshold_perturbation,
                               core_params_row$pff_threshold_stand + threshold_perturbation)
    for (th_s_val in th_s_neighbors_values) {
      if (th_s_val > 0.01 && th_s_val < 1.0) { # Valid threshold range
        params_neighbor <- core_params_row
        params_neighbor$pff_threshold_stand <- th_s_val
        neighbor_run_output <- .run_and_get_avg_rmssd(params_neighbor)
        if (neighbor_run_output$num_valid > 0 && !is.na(neighbor_run_output$avg_lay) && !is.na(neighbor_run_output$avg_stand)) {
          neighbor_diffs_lay <- c(neighbor_diffs_lay, abs(neighbor_run_output$avg_lay - avg_rmssd_core_lay))
          neighbor_diffs_stand <- c(neighbor_diffs_stand, abs(neighbor_run_output$avg_stand - avg_rmssd_core_stand))
        }
      }
    }

    # Perturb pff_centered_window
    params_neighbor <- core_params_row
    params_neighbor$pff_centered_window <- !core_params_row$pff_centered_window
    neighbor_run_output <- .run_and_get_avg_rmssd(params_neighbor)
    if (neighbor_run_output$num_valid > 0 && !is.na(neighbor_run_output$avg_lay) && !is.na(neighbor_run_output$avg_stand)) {
      neighbor_diffs_lay <- c(neighbor_diffs_lay, abs(neighbor_run_output$avg_lay - avg_rmssd_core_lay))
      neighbor_diffs_stand <- c(neighbor_diffs_stand, abs(neighbor_run_output$avg_stand - avg_rmssd_core_stand))
    }

    sensitivity_lay <- if (length(neighbor_diffs_lay) > 0) mean(neighbor_diffs_lay, na.rm = TRUE) else NA_real_
    sensitivity_stand <- if (length(neighbor_diffs_stand) > 0) mean(neighbor_diffs_stand, na.rm = TRUE) else NA_real_

    overall_sensitivity <- if (is.na(sensitivity_lay) || is.na(sensitivity_stand) || length(neighbor_diffs_lay) == 0 || length(neighbor_diffs_stand) == 0) {
      Inf # If any sensitivity is NA or no valid neighbor comparisons, overall is Inf
    } else {
      sensitivity_lay + sensitivity_stand
    }

    current_eval_summary <- tibble::tibble(
      pff_window_size = core_params_row$pff_window_size,
      pff_threshold_lay_trans = core_params_row$pff_threshold_lay_trans,
      pff_threshold_stand = core_params_row$pff_threshold_stand,
      pff_centered_window = core_params_row$pff_centered_window,
      avg_rmssd_core_lay = avg_rmssd_core_lay,
      avg_rmssd_core_stand = avg_rmssd_core_stand,
      num_valid_files_core = num_valid_files_core,
      sensitivity_lay = sensitivity_lay,
      sensitivity_stand = sensitivity_stand,
      overall_sensitivity = overall_sensitivity
    )
    all_param_evaluations[[length(all_param_evaluations) + 1]] <- current_eval_summary
  }

  message("Finished processing all core parameter sets.")

  if (length(all_param_evaluations) == 0) {
    warning("No parameter sets were evaluated.")
    final_results_tibble <- tibble::tibble(
        pff_window_size = integer(),
        pff_threshold_lay_trans = numeric(),
        pff_threshold_stand = numeric(),
        pff_centered_window = logical(),
        avg_rmssd_core_lay = numeric(),
        avg_rmssd_core_stand = numeric(),
        num_valid_files_core = integer(),
        sensitivity_lay = numeric(),
        sensitivity_stand = numeric(),
        overall_sensitivity = numeric()
    )
  } else {
    final_results_tibble <- dplyr::bind_rows(all_param_evaluations)
    final_results_tibble <- final_results_tibble %>%
      dplyr::arrange(overall_sensitivity, dplyr::desc(num_valid_files_core))
  }

  if (!is.null(output_csv_path)) {
    if (!is.character(output_csv_path) || length(output_csv_path) != 1) {
      warning("output_csv_path must be a single character string. Results not saved to CSV.")
    } else {
      tryCatch({
        readr::write_csv(final_results_tibble, output_csv_path)
        message(sprintf("Results saved to %s", output_csv_path))
      }, error = function(e) {
        warning("Error writing results to CSV file: ", output_csv_path, "\n", e$message)
      })
    }
  }

  return(final_results_tibble)
}
