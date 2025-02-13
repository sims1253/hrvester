## hrvester 1.0 Roadmap

### Phase 1: Core Reorganization
**Target: v0.4.0**

#### Code Restructuring
- [x] Create new `data-processing.R`
  - [x] Move all FIT file processing logic from `core.R`
  - [x] Consolidate file reading operations
  - [x] Add input validation
  - [ ] Separate the HR filtering from get_HR's core functionality

- [ ] Refactor `core.R`
  - [ ] Move core algorithms (HRV calculation, moving averages)
  - [ ] Remove redundant functions
  - [ ] Standardize parameter naming
  - [ ] Add algorithm documentation

- [ ] Update `analysis.R`
  - [ ] Review and consolidate analysis functions
  - [ ] Improve parameter validation
  - [ ] Add missing documentation

- [x] Create new `rr_processing.R`
  - [x] Move all rr interval validation and preprocessing here
  - [x] Move all non-fit-file touching functions from extract_rr_data here 

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
