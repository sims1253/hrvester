% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cache.R
\name{process_fit_directory}
\alias{process_fit_directory}
\title{Process directory of FIT files with caching}
\usage{
process_fit_directory(
  dir_path,
  cache_file = file.path(dir_path, "hrv_cache.csv"),
  filter_factor = 0.175,
  clear_cache = FALSE
)
}
\arguments{
\item{dir_path}{The directory path containing FIT files to process}

\item{cache_file}{Path to the cache file for storing processed data}

\item{filter_factor}{Numeric value (0-1) used to filter RR intervals}

\item{clear_cache}{Logical indicating whether to clear existing cache}
}
\value{
A tibble containing HRV metrics for all processed FIT files
}
\description{
Processes multiple FIT files from a specified directory, utilizing caching to
avoid reprocessing unchanged files. This function is designed to be efficient
by only processing new or updated files since the last run.
}
