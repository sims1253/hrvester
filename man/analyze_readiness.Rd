% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{analyze_readiness}
\alias{analyze_readiness}
\title{Analyze HRV readiness status}
\usage{
analyze_readiness(current_metrics, baseline_metrics)
}
\arguments{
\item{current_metrics}{A dataframe containing the current day's HRV metrics.
Must contain exactly one row.
Should include:
\itemize{
\item laying_rmssd (numeric): Current day's RMSSD during laying position
\item laying_resting_hr (numeric): Current day's resting heart rate
\item orthostatic_rise (numeric): Current day's orthostatic response
}}

\item{baseline_metrics}{A dataframe containing the previous 7 days' HRV
metrics.
Should include:
\itemize{
\item laying_rmssd (numeric): Historical RMSSD values for baseline
calculation
\item laying_resting_hr (numeric): Historical resting heart rate values
\item orthostatic_rise (numeric): Historical orthostatic response values
}}
}
\value{
A list containing:
\itemize{
\item status (character): Training status
("FRESH", "NORMAL", "CAUTION", "WARNING")
\item recommendations (list): Training recommendations for BJJ,
strength, and cardio
\item flags (list): Additional warning flags for high fatigue, poor
recovery, and overreaching risk
\item metrics (list): Baseline values and deviation calculations
}
}
\description{
This function evaluates an athlete's readiness for training based on HRV
metrics.
It compares current day metrics against a 7-day rolling baseline and provides
a training status along with specific recommendations.
}
