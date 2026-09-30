# hrvester 0.5.0

This release fixes numerical and data-provenance defects identified in a full
source review of the processing pipeline, and tightens the unit and protocol
contracts. Results computed with earlier versions may change where the old
behavior was incorrect.

## Phase timing and protocol

* `split_rr_phases()` derives `elapsed_time` from the actual beat intervals
  (cumulative sum) instead of stretching beats uniformly to the session
  duration, so beats can no longer land in the wrong phase when heart rate
  changes between laying and standing. A `time_unit` argument
  (`"milliseconds"`, the pipeline default, or `"seconds"`) documents the
  conversion; a warning is issued when beat time and session duration
  disagree by more than 5%.
* `process_fit_file()` applies one elapsed-time protocol (in seconds) to both
  HR and RR analyses. The `warmup` argument is now honored, HR windows are
  selected by record timestamp rather than fixed row numbers (irregular
  sampling is handled), and the 360-sample truncation in `get_HR()` was
  removed.
* Caller-supplied `window_size`, `threshold` and `centered_window` are now
  forwarded to every phase in `process_fit_file()` instead of being silently
  overridden by hard-coded per-phase values.

## Correction semantics

* `correct_rr_cubic_spline()` computes the adaptive lower threshold relative
  to the median RR interval (`median_rr - 200` ms) instead of treating 200 ms
  as an absolute RR value, so substantially shortened but plausible intervals
  are detected.
* A cubic correction budget of zero now disables correction entirely
  (previously `1:0` indexing still corrected one beat), and
  `max_correction_rate` is validated as a single finite percentage.
* `correct_hrv_artefacts_lipponen()` merges the split interval when removing
  an extra beat and uses a zero-length-safe prefix when inserting missed
  beats, preserving the recording's elapsed duration.
* `calculate_hrv_rmssd()` computes RMSSD from the successfully corrected
  beats (including interpolated ones) instead of dropping them.
* `detect_rr_artifacts()` centered windows exclude only the current index,
  not all neighbors with an equal value, so repeated or quantized RR values
  are no longer falsely flagged.
* `correct_rr_lipponen_tarvainen()` renames the `rmssd_error` diagnostic to
  `relative_rmssd_change` (it measures change from the uncorrected input,
  not accuracy), guards a zero reference RMSSD, and accuracy claims were
  removed from the documentation.

## Quality assessment

* Phase quality in `process_fit_file()` is computed from the raw
  (pre-correction) artifact burden carried through provenance attributes, so
  correction can no longer make a poor recording pass the quality gate. The
  correction metadata records the actual method and threshold used.
* `calculate_rr_quality()` returns an explicit unassessable result (grade
  "F", NA scores) for empty or wholly unusable series instead of grading
  them "A".

## Units

* Milliseconds are documented as the canonical RR unit across the package.
  `calculate_hrv()` input/output units now match, and
  `calculate_rmssd_orthostatic_enhanced()` gained a `time_unit` argument and
  applies `transition_exclusion_time` for the first time (the transition
  window was previously labeled but never excluded). Segments are split on
  elapsed-time gaps so RMSSD is not computed across removed periods.

## Caching

* Cache entries carry a file-content digest and an analysis-configuration
  identity. Overwritten files, changed analysis arguments and package
  upgrades now invalidate the affected entries automatically; legacy caches
  without provenance are reprocessed. Entries for deleted files are kept.
* `load_cache()` installs its readr column specifications in the correct
  place, so all-NA result rows round-trip with their declared types instead
  of being rejected and wiped.
* The cache is written atomically (temporary file plus rename): a failed or
  interrupted write leaves the previous cache untouched.

## Reports, plots and recovery classification

* `generate_daily_report()` accepts character dates and fractional resting
  heart rates from the pipeline, and requires a usable baseline day instead
  of relying on row counts alone.
* `hrv_plot()` consumes the actual RR time series returned by
  `extract_rr_data()`; `hrv_trend_plot()` no longer pivots character quality
  grades together with numeric metrics.
* `calculate_neural_recovery()` reports missing baselines and missing
  components as an explicit "Insufficient data" status instead of "Low";
  `training_recommendations()` and `analyze_readiness()` handle undefined
  scores explicitly.
* `plot_weekly_heatmap()` uses locale-independent numeric weekday keys and
  ISO week-year (`%G-W%V`) week keys, so weekday tiles no longer vanish
  outside a German locale and dates around New Year stay grouped.

# hrvester 0.4.0

## Quality Assessment System

* Quality grades (A/B/C/D/F) for each measurement phase based on clinical HRV standards
* Signal quality index (0-100) calculated from artifact levels and data completeness
* Automatic quality filtering via `min_quality_threshold` parameter in `process_fit_file()` and `process_fit_directory()`
* Thresholds aligned with HRV literature

## Enhanced Data Processing

* Processing functions include quality metrics in output
* Quality metrics stored in cache for re-processing
* Improved error handling and validation in preprocessing pipeline

## New Functions

* Enhanced `process_fit_file()` and `process_fit_directory()` with quality filtering
* Quality assessment integrated into preprocessing functions

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