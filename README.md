# hrvester <a href="https://sims1253.github.io/hrvester/"><img src="man/figures/logo.png" align="right" height="120" alt="hrvester website" /></a>

[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R-CMD-check](https://github.com/sims1253/hrvester/workflows/R-CMD-check/badge.svg)](https://github.com/sims1253/hrvester/actions)
[![Actions Status](https://github.com/sims1253/hrvester/workflows/Tests/badge.svg)](https://github.com/sims1253/hrvester/actions)
[![Codecov test coverage](https://codecov.io/gh/sims1253/hrvester/graph/badge.svg)](https://app.codecov.io/gh/sims1253/hrvester)

A toolkit for sports physiology analysis, providing:

- **Orthostatic Test Processing** - Full pipeline from FIT files to readiness scores
- **Neural Recovery Metrics** - BioForce-inspired composite scoring
- **BJJ-Specific Analytics** - Grappling-specific training recommendations
- **Automated Reporting** - Daily PDF/HTML reports with training guidance
- **Dashboard Visualization** - Interactive trend monitoring

<img src="https://github.com/sims1253/hrvester/blob/master/example/hrv_dashboard.svg?raw=true" width="800" alt="HRV Dashboard Preview">

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

## hrvester 1.0 Roadmap

### Phase 1: Core Reorganization
**Target: v0.4.0**

#### Code Restructuring
- [ ] Create new `data-processing.R`
  - [ ] Move all FIT file processing logic from `core.R`
  - [ ] Consolidate file reading operations
  - [ ] Add input validation

- [ ] Refactor `core.R`
  - [ ] Move core algorithms (HRV calculation, moving averages)
  - [ ] Remove redundant functions
  - [ ] Standardize parameter naming
  - [ ] Add algorithm documentation

- [ ] Update `analysis.R`
  - [ ] Review and consolidate analysis functions
  - [ ] Improve parameter validation
  - [ ] Add missing documentation

#### Testing Updates
- [ ] Update test files to match new structure
- [ ] Add tests for edge cases
- [ ] Verify test coverage remains high
- [ ] Unify cache and data templates for tests

### Phase 2: Error Handling Enhancement
**Target: v0.5.0**

#### Unify Data Representation
- [ ] Unify result and cache templates
- [ ] Document the data format

#### Error System Implementation
- [ ] Define standard error types
- [ ] Implement consistent error messages
- [ ] Add input validation across all public functions
- [ ] Improve error recovery mechanisms

#### Documentation
- [ ] Document error handling patterns
- [ ] Add error handling examples
- [ ] Update function documentation with error cases

#### Testing
- [ ] Add error condition tests
- [ ] Verify error message consistency
- [ ] Test recovery mechanisms

### Phase 3: API Finalization
**Target: v0.6.0**

#### Public API
- [ ] FInalize official public API
- [ ] Mark internal functions with appropriate naming
- [ ] Document API stability guarantees
- [ ] Create API documentation

#### Documentation Enhancement
- [ ] Complete function documentation
- [ ] Add examples for all public functions
- [ ] Create vignettes for common use cases
- [ ] Update README with clear installation/usage

#### Final Testing
- [ ] Complete test coverage for public API
- [ ] Add integration tests
- [ ] Performance testing

### Phase 4: Release Preparation
**Target: v1.0.0**

#### Final Review
- [ ] Code review of all components
- [ ] Documentation review
- [ ] Test coverage review
- [ ] Performance review

#### Release Preparation
- [ ] Update DESCRIPTION
- [ ] Update NEWS.md
- [ ] Version number update
- [ ] Final README review
- [ ] CRAN submission preparation

#### Post-Release
- [ ] Create release tags
- [ ] Update documentation website
- [ ] Announce release
- [ ] Create maintenance schedule

### Future Considerations (Post 1.0)
- Enhanced visualization features
- Additional caching optimizations (DuckDB?)
- Interactive dashboard elements
- Additional sport-specific metrics
- Performance optimizations
