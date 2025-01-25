# hrvester - Athlete-Centric HRV Analysis Toolkit

[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R-CMD-check](https://github.com/sims1253/hrvester/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sims1253/hrvester/actions)
[![codecov](https://codecov.io/gh/sims1253/hrvester/graph/badge.svg?token=RUcQo5iwgn)](https://codecov.io/gh/sims1253/hrvester)

<img src="https://github.com/sims1253/hrvester/blob/master/example/hrv_dashboard.svg?raw=true" width="800" alt="HRV Dashboard Preview">

A high-performance toolkit for sports physiology analysis, providing:

- **Orthostatic Test Processing** - Full pipeline from FIT files to readiness scores
- **Neural Recovery Metrics** - BioForce-inspired composite scoring
- **BJJ-Specific Analytics** - Grappling-specific training recommendations
- **Automated Reporting** - Daily PDF/HTML reports with training guidance
- **Dashboard Visualization** - Interactive trend monitoring

## Features

- **Optimized Processing**  
  Multi-threaded FIT file analysis using `furrr`
- **Clinical Validation**  
  Implements peer-reviewed algorithms from:
  - BioForce HRV methodology
  - Firstbeat Technologies recovery analysis
  - European Journal of Applied Physiology standards
- **Advanced Caching**  
  Version-aware cache invalidation with automatic reprocessing
- **Sports Specific**  
  Specialized metrics for:
  - Brazilian Jiu-Jitsu (positional sparring load)
  - Strength training (volume/intensity ratios)
  - Endurance sports (HR recovery profiles)

## Installation

```r
# Install from GitHub
remotes::install_github("sims1253/hrvester")
```

## Quick Start

```r
# Process FIT files with parallel execution
metrics <- process_fit_directory(
  "path/to/fit/files",
  filter_factor = 0.175,  # Adaptive outlier rejection
  cache_file = "hrv_cache.csv"
)

# Generate daily readiness report
report <- generate_daily_report(metrics)
cat(report)

# Create dashboard
dashboard <- plot_hrv_dashboard(metrics)
dashboard
```

## Documentation

| Component              | Description                          |
|------------------------|--------------------------------------|
| `analyze_readiness()`  | Training readiness scoring           |
| `calculate_neural_recovery()` | Composite recovery metrics    |
| `plot_weekly_heatmap()`| Recovery status visualization        |
| `generate_training_recommendations()` | Sport-specific guidance |

Full function reference: [Reference Manual](man/)

## Citation

If using in research, please cite:

```bibtex
@software{hrvester,
  title = {hrvester: Athlete-Centric HRV Analysis Toolkit},
  author = {Maximilian Scholz},
  year = {2025},
  url = {https://github.com/sims1253/hrvester}
}
```
---
