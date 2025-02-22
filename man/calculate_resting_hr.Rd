% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core.R
\name{calculate_resting_hr}
\alias{calculate_resting_hr}
\title{Calculate resting heart rate}
\usage{
calculate_resting_hr(
  hr_data,
  method = "lowest_sustained",
  stability_threshold = 3,
  window_size = 30
)
}
\arguments{
\item{hr_data}{Vector of heart rate values}

\item{method}{Calculation method to use}

\item{stability_threshold}{Allowable HR variation}

\item{window_size}{Window size in samples}
}
\value{
Numeric resting heart rate value
}
\description{
Estimates resting heart rate using various methods
}
