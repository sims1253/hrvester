# hrvester 0.4.0

## Quality Assessment System

* **Quality grades** (A/B/C/D/F) for each measurement phase based on clinical HRV standards
* **Signal quality index** (0-100) calculated from artifact levels and data completeness
* **Automatic quality filtering** via `min_quality_threshold` parameter in `process_fit_file()` and `process_fit_directory()`
* **Research-grade thresholds** aligned with peer-reviewed HRV literature

## Enhanced Data Processing

* All processing functions now include comprehensive quality metrics in output
* Quality metrics stored in cache for efficient re-processing
* Improved error handling and validation throughout preprocessing pipeline

## New Functions

* Enhanced `process_fit_file()` and `process_fit_directory()` with quality filtering
* Quality assessment integrated into existing preprocessing functions

## Bug Fixes

* Fixed artifact detection edge cases in preprocessing pipeline
* Improved handling of missing or invalid RR interval data
* Enhanced robustness of heart rate recovery calculations

# hrvester 0.3.0

## Features

* Initial release with core HRV processing functionality
* Kubios-style artifact detection and correction
* Multiple correction methods (linear, cubic spline, Lipponen-Tarvainen)
* Orthostatic test analysis with phase-specific processing
* Caching system for efficient batch processing