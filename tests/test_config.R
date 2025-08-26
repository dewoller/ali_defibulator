# ABOUTME: Configuration and setup for the testing framework
# ABOUTME: Defines test environment settings, constants, and helper functions

# Test configuration constants
TEST_CONFIG <- list(
  # Spatial bounds for mock data (Melbourne metro area)
  BBOX = c(144.5, -38.0, 145.5, -37.0),
  
  # Test data sizes (reduced for CI performance)
  N_MESHBLOCKS = 200,
  N_DEFIBS = 30,
  N_VACAR = 100,
  N_SA1 = 40,
  
  # Distance thresholds for testing
  MIN_DISTANCE = 10,      # meters
  MAX_DISTANCE = 50000,   # meters (50km)
  MIN_DURATION = 10,      # seconds  
  MAX_DURATION = 3600,    # seconds (1 hour)
  
  # Population ranges for validation
  MIN_PERSON = 0,
  MAX_PERSON = 1000,
  
  # SEIFA score ranges
  MIN_SEIFA = 600,
  MAX_SEIFA = 1400,
  
  # Test timeouts
  SLOW_TEST_TIMEOUT = 30,  # seconds
  FAST_TEST_TIMEOUT = 5,   # seconds
  
  # File paths
  MOCK_DATA_DIR = "tests/fixtures/data",
  OUTPUT_DIR = "output",
  
  # Test categories
  UNIT_TESTS = c(
    "find_closest_osrm_point",
    "point2isochrone", 
    "summarise_one_sua",
    "get_victoria_defib_ll",
    "packages",
    "st_singles_to_multi",
    "antijoin_within_distance",
    "utility_functions"
  ),
  
  INTEGRATION_TESTS = c(
    "targets_integration",
    "helpers"
  ),
  
  E2E_TESTS = c(
    "end_to_end"
  ),
  
  # Skip conditions
  SKIP_SLOW_TESTS = Sys.getenv("SKIP_SLOW_TESTS", "FALSE") == "TRUE",
  SKIP_EXTERNAL_TESTS = Sys.getenv("SKIP_EXTERNAL_TESTS", "TRUE") == "TRUE",
  IS_CI = Sys.getenv("CI", "FALSE") == "TRUE",
  
  # Tolerance levels for numeric comparisons
  DISTANCE_TOLERANCE = 10,    # meters
  DURATION_TOLERANCE = 5,     # seconds
  PROPORTION_TOLERANCE = 0.01, # 1%
  COORDINATE_TOLERANCE = 0.0001, # ~10m at Melbourne latitude
  
  # Mocking settings
  MOCK_OSRM = TRUE,
  MOCK_WEB_REQUESTS = TRUE,
  USE_REAL_DATA = FALSE
)

# Helper function to check if external services are available
check_external_services <- function() {
  list(
    osrm_server = tryCatch({
      # Quick check if OSRM server is responding
      httr::GET("http://localhost:1234/", timeout(2))
      TRUE
    }, error = function(e) FALSE),
    
    internet = tryCatch({
      # Quick internet connectivity check
      httr::GET("https://httpbin.org/status/200", timeout(2))
      TRUE  
    }, error = function(e) FALSE)
  )
}

# Environment setup for tests
setup_test_environment <- function() {
  # Set test-specific options
  options(
    testthat.default_reporter = if (interactive()) "summary" else "progress",
    testthat.use_colours = TRUE,
    warn = 1  # Show warnings immediately
  )
  
  # Create necessary directories
  if (!dir.exists(TEST_CONFIG$MOCK_DATA_DIR)) {
    dir.create(TEST_CONFIG$MOCK_DATA_DIR, recursive = TRUE)
  }
  
  # Set environment variables for testing
  Sys.setenv(
    TESTTHAT = "true",
    R_TESTS = "true"
  )
  
  # Load required test libraries
  suppressPackageStartupMessages({
    library(testthat)
    if (requireNamespace("mockery", quietly = TRUE)) library(mockery)
    if (requireNamespace("sf", quietly = TRUE)) library(sf)
    if (requireNamespace("dplyr", quietly = TRUE)) library(dplyr)
  })
  
  invisible(TRUE)
}

# Cleanup after tests
cleanup_test_environment <- function() {
  # Clean up temporary files
  temp_files <- list.files(tempdir(), pattern = "^test_", full.names = TRUE)
  if (length(temp_files) > 0) {
    unlink(temp_files, recursive = TRUE)
  }
  
  # Reset options
  options(warn = 0)
  
  invisible(TRUE)
}

# Custom test helper functions
expect_valid_coordinates <- function(object) {
  act <- quasi_label(rlang::enquo(object), arg = "object")
  
  if (inherits(act$val, "sf")) {
    coords <- st_coordinates(act$val)
  } else if (is.matrix(act$val) && ncol(act$val) >= 2) {
    coords <- act$val
  } else {
    fail(sprintf("%s is not a valid coordinate object", act$lab))
    return(invisible(act$val))
  }
  
  # Check longitude bounds (-180 to 180)
  lon_valid <- all(coords[,1] >= -180 & coords[,1] <= 180, na.rm = TRUE)
  
  # Check latitude bounds (-90 to 90)  
  lat_valid <- all(coords[,2] >= -90 & coords[,2] <= 90, na.rm = TRUE)
  
  if (!lon_valid || !lat_valid) {
    fail(sprintf("%s contains invalid coordinates", act$lab))
  } else {
    succeed()
  }
  
  invisible(act$val)
}

expect_reasonable_distance <- function(object) {
  act <- quasi_label(rlang::enquo(object), arg = "object")
  
  distances <- as.numeric(act$val)
  
  if (any(distances < TEST_CONFIG$MIN_DISTANCE | distances > TEST_CONFIG$MAX_DISTANCE, na.rm = TRUE)) {
    fail(sprintf("%s contains unreasonable distances", act$lab))
  } else {
    succeed()
  }
  
  invisible(act$val)
}

expect_reasonable_duration <- function(object) {
  act <- quasi_label(rlang::enquo(object), arg = "object")
  
  durations <- as.numeric(act$val)
  
  if (any(durations < TEST_CONFIG$MIN_DURATION | durations > TEST_CONFIG$MAX_DURATION, na.rm = TRUE)) {
    fail(sprintf("%s contains unreasonable durations", act$lab)) 
  } else {
    succeed()
  }
  
  invisible(act$val)
}

# Mock data validators
validate_mock_meshblocks <- function(meshblocks) {
  expect_s3_class(meshblocks, "sf")
  expect_true("mb_code_2021" %in% names(meshblocks))
  expect_true("person" %in% names(meshblocks)) 
  expect_true("dwelling" %in% names(meshblocks))
  expect_true("sa1_code_2021" %in% names(meshblocks))
  expect_true(all(meshblocks$person >= 0))
  expect_true(all(meshblocks$dwelling >= 0))
  expect_valid_coordinates(meshblocks)
}

validate_mock_defibrillators <- function(defibrillators) {
  expect_s3_class(defibrillators, "sf")
  expect_true("sua_id" %in% names(defibrillators))
  expect_true("company" %in% names(defibrillators))
  expect_true("is_sja_defib" %in% names(defibrillators))
  expect_type(defibrillators$is_sja_defib, "logical")
  expect_valid_coordinates(defibrillators)
}

validate_mock_vacar <- function(vacar) {
  expect_s3_class(vacar, "sf")
  expect_true("va_internal_id" %in% names(vacar))
  expect_true("va_date" %in% names(vacar))
  expect_s3_class(vacar$va_date, "Date")
  expect_valid_coordinates(vacar)
}

# Initialize test environment when loaded
if (identical(Sys.getenv("TESTTHAT"), "true")) {
  setup_test_environment()
}