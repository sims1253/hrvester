
# hrvester <a href="https://sims1253.github.io/hrvester/"><img src="man/figures/logo.png" align="right" height="120" alt="hrvester website" /></a>

<!-- badges: start -->

[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R-CMD-check](https://github.com/sims1253/hrvester/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sims1253/hrvester/actions/workflows/R-CMD-check.yaml)
[![Tests](https://github.com/sims1253/hrvester/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/sims1253/hrvester/actions/workflows/test-coverage.yaml)
[![Codecov test
coverage](https://codecov.io/gh/sims1253/hrvester/graph/badge.svg)](https://app.codecov.io/gh/sims1253/hrvester)
[![GH-Pages](https://github.com/sims1253/hrvester/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/sims1253/hrvester/actions/workflows/pkgdown.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`hrvester` is designed to analyze Heart Rate Variability (HRV) data from
`.fit` files. It focuses on orthostatic tests (measuring HRV while
laying down and then standing up) to provide insights into an athlete’s
recovery and readiness for training.

The package implements research-based preprocessing methods including:

- Kubios-style artifact detection with time-varying thresholds
- Multiple correction methods: Linear interpolation, cubic spline, and
  Lipponen-Tarvainen algorithms
- Comprehensive quality assessment following clinical HRV standards
- Automated orthostatic test analysis with phase-specific processing
- Dashboard summary with training recommendations:

<img src="https://github.com/sims1253/hrvester/blob/master/example/hrv_dashboard.svg?raw=true" width="800" alt="HRV Dashboard Preview">

## What is HRV?

Heart Rate Variability (HRV) is the variation in time intervals between
heartbeats. It’s a non-invasive measure that reflects the activity of
your autonomic nervous system, and it can be used to assess your body’s
readiness for training, recovery status, and overall stress levels.

## Features

- Preprocessing Pipeline: Artifact correction methods including
  research-validated algorithms
  - Kubios-style Detection: Time-varying thresholds with dRR series
    analysis for artifact identification
  - Multiple Correction Methods: Linear interpolation, cubic spline, and
    Lipponen-Tarvainen algorithms (Lipponen & Tarvainen, 2019)
  - Quality Assessment: Quality metrics with A/B/C/D/F grading system
- Orthostatic Test Processing: Pipeline from raw `.fit` files to
  readiness scores, including:
  - RR Interval Preprocessing: Artifact detection, physiological
    plausibility checks, and correction methods
  - HRV Metric Calculation: Key HRV metrics like RMSSD and SDNN
    calculation
  - Resting Heart Rate Estimation: Algorithm to determine resting heart
    rate
  - Neural Recovery Score: Algorithm for athlete readiness assessment
    (Jamieson, 2011)
  - Trend Analysis: Moving averages and trend detection for monitoring
- Data Management:
  - Caching: Automatic caching system with quality metrics storage to
    avoid reprocessing
  - Quality Filtering: Configurable quality thresholds to exclude poor
    measurements
  - Batch Processing: Process directories of `.fit` files
- Visualization & Reporting:
  - Dashboards: HRV trend visualization with ggplot2
  - Training Recommendations: Insights based on HRV trends and readiness
    scores
  - Export Capabilities: Multiple output formats for integration with
    other tools

## Installation

You can install the development version of hrvester from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("sims1253/hrvester")
```

## Quick Start

Here’s a basic example showing how to process a single HRV orthostatic
test:

``` r
library(hrvester)

# Process a single FIT file with preprocessing
result <- process_fit_file("your_file.fit", 
                          correction_method = "linear",
                          min_quality_threshold = 0.7)

# View the results
result
```

## Preprocessing Methods

The package offers multiple artifact correction methods:

``` r
# Read a FIT file
fit_data <- FITfileR::readFitFile("your_file.fit")

# Extract RR intervals with different correction methods
rr_linear <- extract_rr_data(fit_data, correction_method = "linear")
rr_cubic <- extract_rr_data(fit_data, correction_method = "cubic") 
rr_lipponen <- extract_rr_data(fit_data, correction_method = "lipponen")
rr_none <- extract_rr_data(fit_data, correction_method = "none")

# Compare quality metrics
methods <- list("Linear" = rr_linear, "Cubic" = rr_cubic, 
                "Lipponen" = rr_lipponen, "None" = rr_none)

sapply(methods, function(x) attr(x, "quality_metrics")$quality_grade)
```

## Batch Processing

Process multiple files with quality filtering:

``` r
# Process an entire directory of FIT files
results <- process_fit_directory("fit_files/", 
                                min_quality_threshold = 0.7,
                                correction_method = "linear")

# View summary statistics
summary(results)

# Show quality distribution
table(results$laying_quality_grade)
```

## Advanced Usage

### Custom Quality Assessment

``` r
# Extract RR data and assess quality manually
rr_data <- extract_rr_data(fit_data, correction_method = "linear")
quality_metrics <- attr(rr_data, "quality_metrics")

# Check quality details
quality_metrics[c("artifact_percentage", "signal_quality_index", "quality_grade")]

# Check measurement quality
if(quality_metrics$signal_quality_index >= 90 && 
   quality_metrics$artifact_percentage < 5) {
  print("High-quality measurement")
} else {
  print("Consider re-measurement")
}
```

### Readiness Analysis

``` r
# Analyze readiness - get current and baseline data
current_metrics <- tail(results, 1)  # Most recent measurement
baseline_metrics <- head(tail(results, 8), 7)  # Previous 7 days

# Analyze readiness status
readiness <- analyze_readiness(current_metrics, baseline_metrics)

# View training recommendations
cat("Training status:", readiness$status)
readiness$recommendations
```

## Research Foundation

The preprocessing pipeline implements methods from the HRV literature:

- **Lipponen-Tarvainen**: Research-validated algorithm with \<2% HRV
  error (Lipponen & Tarvainen, 2019)
- **Linear Interpolation**: Standard linear interpolation method for
  artifact correction
- **Cubic Spline**: Kubios-compatible cubic spline interpolation method
- **Neural Recovery Score**: BioForce HRV methodology combining RMSSD,
  orthostatic response, and heart rate recovery components (Jamieson,
  2011)

## Citation

If using in research, please cite:

``` bibtex
@software{hrvester,
  title = {hrvester: Athlete-Centric HRV Analysis Toolkit},
  author = {Maximilian Scholz},
  year = {2024},
  url = {https://github.com/sims1253/hrvester}
}
```

## License

MIT License. See [LICENSE](LICENSE) for details.
