# ostdashr

Dashboard for orthostatic test data.

## Installation

```r
remotes::install_github("sims1253/ostdashr")
```

## Usage

```r
library(ostdashr)
```

## Process a directory of FIT files (uses caching for speed)

```r
metrics <- process_fit_directory(
  dir_path = "path/to/fit/files"
)
```

## Create plots

```r
library(patchwork)
metrics = process_fit_directory("path/to/all/your/fit/files")
hrv_plot("path/to/todays/file.fit") / 
hrv_trend_plot(metrics, just_rssme = TRUE) {
```
