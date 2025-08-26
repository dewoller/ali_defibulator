# Comprehensive Testing Framework for Defibrillator Analysis Project

This document describes the comprehensive testing framework that has been implemented for your geospatial defibrillator analysis project.

## Overview

A complete testing framework has been established with three levels of testing:
- **Unit Tests**: Individual function testing with mocks
- **Integration Tests**: Component interaction testing  
- **End-to-End Tests**: Full pipeline execution testing

## Test Structure

### Unit Tests (`tests/testthat/test-*.R`)

1. **`test-find_closest_osrm_point.R`** - OSRM routing functions
2. **`test-point2isochrone.R`** - Isochrone generation with fallbacks
3. **`test-find_n_closest_mb.R`** - Spatial nearest neighbor operations
4. **`test-get_victoria_defib_ll.R`** - Web scraping and data parsing
5. **`test-summarise_one_sua.R`** - SUA demographic calculations
6. **`test-packages.R`** - Package loading and conflict resolution
7. **`test-st_singles_to_multi.R`** - Spatial geometry conversions
8. **`test-antijoin_within_distance.R`** - Distance-based filtering
9. **`test-utility_functions.R`** - Helper functions like %ni% operator

### Integration Tests

1. **`test-targets_integration.R`** - Targets pipeline component testing
2. **`test-helpers.R`** - Cross-function integration testing

### End-to-End Tests  

1. **`test-end_to_end.R`** - Full pipeline execution and data quality validation

## Test Data and Fixtures

### Mock Data Generation
- **`tests/fixtures/enhanced_mock_data.R`** - Comprehensive mock data generator
- **`tests/fixtures/create_mock_data.R`** - Basic mock data for existing tests
- **`tests/fixtures/data/`** - Generated mock datasets (GeoJSON, CSV, RDS)

The mock data includes:
- 500+ mock meshblocks with realistic demographics
- 50+ mock defibrillators with SJA classification
- 200+ mock VACAR incidents with dates and outcomes
- 100+ mock SA1 areas with proper spatial relationships
- Census data, SEIFA scores, and OSRM response mocking

## Test Configuration

**`tests/test_config.R`** provides:
- Test environment setup and teardown
- Configuration constants for data validation
- Custom test expectations for spatial data
- Mock data validators
- External service availability checks

## Test Runners

### Command Line Usage

```bash
# Run all tests
Rscript tests/run_tests.R

# Run only unit tests  
Rscript tests/run_tests.R unit

# Run with coverage report
Rscript tests/run_tests.R all --coverage

# Quick development tests
Rscript tests/run_tests.R --quick

# Create fresh mock data
Rscript tests/run_tests.R --create-mock-data
```

### Interactive Usage

```r
# Load the comprehensive test runner
source("tests/run_tests.R")

# Run all tests
run_comprehensive_tests()

# Run only unit tests
run_comprehensive_tests("unit")

# Run with coverage
run_comprehensive_tests(coverage = TRUE)

# Quick tests for development
run_quick_tests()
```

### Standard testthat Runner

```bash
# Traditional testthat approach
Rscript tests/testthat.R
```

## Continuous Integration

**`.github/workflows/test.yml`** provides:
- Multi-version R testing (4.3, 4.4)
- System dependency installation
- Spatial package support
- Coverage reporting
- Artifact uploads
- Lint checking

The CI workflow:
1. Installs system dependencies (GDAL, PROJ, GEOS, etc.)
2. Sets up R package dependencies
3. Creates mock test data
4. Runs unit and integration tests
5. Generates coverage reports
6. Uploads results as artifacts

## Key Features

### Comprehensive Coverage
- Tests every custom R function in `/R/` directory
- Tests all major targets pipeline components
- Validates spatial operations and data integrity
- Includes error handling and edge case testing

### Realistic Mock Data
- Spatially coherent test datasets
- Proper demographic distributions
- Realistic coordinate systems and projections
- Maintains referential integrity across datasets

### External Service Mocking
- OSRM routing service mocking
- Web scraping request mocking
- Handles external service unavailability gracefully

### Performance Testing
- Tests handle realistic data volumes (thousands of features)
- Includes timeout protection
- Memory usage considerations

### Data Quality Validation
- Coordinate validation (lat/lon bounds)
- Distance and duration reasonableness checks
- Demographic proportion validation  
- Spatial validity verification

## Running Tests

### Prerequisites

Ensure you have the required R packages:
```r
install.packages(c(
  "testthat", "devtools", "mockery", "covr",
  "sf", "dplyr", "purrr", "stringr", "tidyr",
  "units", "geosphere", "nngeo"
))
```

### Basic Test Run

```bash
cd /Users/dewoller/code/ali/defib
Rscript tests/testthat.R
```

### Development Workflow

1. **Make code changes**
2. **Run quick tests**: `run_quick_tests()` 
3. **Run full unit tests**: `run_comprehensive_tests("unit")`
4. **Run integration tests**: `run_comprehensive_tests("integration")`
5. **Run end-to-end tests**: `run_comprehensive_tests("end-to-end")`

## Test Categories and Skip Conditions

Tests automatically adapt to the environment:
- **CI Environment**: Skips slow tests and external service tests
- **Missing Dependencies**: Skips tests requiring unavailable packages
- **No Internet**: Skips web-dependent tests  
- **No OSRM Server**: Uses mocked OSRM responses

## Coverage and Reporting

The framework generates:
- **HTML Coverage Reports** (`tests/coverage.html`)
- **Test Result Summaries** (console output)
- **Mock Data Summaries** (`tests/fixtures/data/mock_data_summary.csv`)
- **CI Artifacts** (uploaded to GitHub Actions)

## Maintenance

### Adding New Tests

1. Create test file: `tests/testthat/test-your_function.R`
2. Follow existing patterns for mocking and validation
3. Add to appropriate category in `test_config.R`
4. Update `run_tests.R` if needed

### Updating Mock Data

1. Modify `tests/fixtures/enhanced_mock_data.R`
2. Regenerate with: `run_comprehensive_tests(create_mock_data = TRUE)`
3. Validate new data structure with existing tests

## Troubleshooting

### Common Issues

1. **Missing spatial dependencies**: Install system libraries (GDAL, PROJ, GEOS)
2. **OSRM connection errors**: Tests automatically use mocks when server unavailable
3. **Memory issues**: Reduce mock data sizes in `test_config.R`
4. **Slow tests**: Use `--quick` flag or set `SKIP_SLOW_TESTS=TRUE`

### Debug Mode

Run tests in debug mode:
```r
options(testthat.debug = TRUE)
run_comprehensive_tests(verbose = TRUE)
```

This comprehensive testing framework ensures your geospatial analysis pipeline is robust, reliable, and maintainable. All functions and data flows are thoroughly tested with realistic mock data and proper error handling.