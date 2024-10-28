# hrvtools

Simple R package for analyzing Heart Rate Variability (HRV) from FIT files.

## Installation

```r
remotes::install_github("sims1253/hrvtools")
```

## Usage

```r
library(hrvtools)
```

## Process a directory of FIT files (uses caching for speed)

```r
metrics <- process_fit_directory(
  dir_path = "path/to/fit/files",
cache_file = "hrv_cache.parquet"
)
```

## Create trend plots

```r
plot_hrv_trend(metrics, metric = "rmssd")
plot_hrv_trend(metrics, metric = "sdnn")
plot_hrv_trend(metrics, metric = "hr")
```

## Metrics

The package calculates:

- RMSSD (Root Mean Square of Successive Differences)
- SDNN (Standard Deviation of NN intervals)
- Mean Heart Rate

For both laying and standing positions.
