# HRV Analysis Tools

Simple R package for analyzing Heart Rate Variability (HRV) from FIT files.

## Installation

```r
remotes::install_github("yourusername/hrvtools")
```

```r
library(hrvtools)
```

Analyze a directory of FIT files

```r
metrics <- cache_hrv_metrics("path/to/fit/files")
```

Create trend plot

```r
plot <- hrv_trend_plot(metrics)
```
