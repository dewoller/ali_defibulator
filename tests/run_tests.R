# ABOUTME: Comprehensive test runner script for the defibrillator analysis project
# ABOUTME: Executes all test suites with proper setup, reporting, and cleanup

library(testthat)
library(devtools)
library(here)
library(fs)

#' Run comprehensive test suite
#' @param test_level Character: "unit", "integration", "end-to-end", or "all"
#' @param coverage Logical: Whether to generate coverage report
#' @param verbose Logical: Whether to show detailed output
#' @param create_mock_data Logical: Whether to regenerate mock data
#' @return Test results summary
run_comprehensive_tests <- function(
  test_level = "all",
  coverage = FALSE,
  verbose = TRUE,
  create_mock_data = FALSE
) {
  
  cat("========================================\n")
  cat("DEFIBRILLATOR ANALYSIS PROJECT TESTS\n")
  cat("========================================\n\n")
  
  # Setup
  cat("Setting up test environment...\n")
  
  # Load all project functions
  tryCatch({
    devtools::load_all(quiet = !verbose)
    cat("✓ Project functions loaded\n")
  }, error = function(e) {
    cat("✗ Error loading project functions:", conditionMessage(e), "\n")
    return(FALSE)
  })
  
  # Create/update mock data if requested
  if (create_mock_data) {
    cat("Creating mock test data...\n")
    tryCatch({
      Sys.setenv(CREATE_MOCK_DATA = "TRUE")
      source(here("tests", "fixtures", "enhanced_mock_data.R"))
      cat("✓ Mock data created\n")
    }, error = function(e) {
      cat("✗ Error creating mock data:", conditionMessage(e), "\n")
    })
  }
  
  # Ensure test fixtures exist
  if (!dir_exists(here("tests", "fixtures", "data"))) {
    cat("Creating test fixtures...\n")
    source(here("tests", "fixtures", "create_mock_data.R"))
  }
  
  # Initialize results tracking
  results <- list()
  total_tests <- 0
  total_passed <- 0
  total_failed <- 0
  total_skipped <- 0
  
  # Helper function to run specific test files
  run_test_category <- function(pattern, category_name) {
    cat(paste0("\n", toupper(category_name), " TESTS\n"))
    cat(paste(rep("=", nchar(category_name) + 6), collapse = ""), "\n")
    
    test_files <- dir_ls(here("tests", "testthat"), regexp = pattern)
    
    if (length(test_files) == 0) {
      cat("No test files found matching pattern:", pattern, "\n")
      return(list(passed = 0, failed = 0, skipped = 0, total = 0))
    }
    
    cat("Running", length(test_files), "test files...\n\n")
    
    category_results <- test_dir(
      path = here("tests", "testthat"),
      filter = gsub(".*test-|\\.R$", "", basename(test_files)),
      reporter = if (verbose) "summary" else "silent",
      env = parent.frame(),
      stop_on_failure = FALSE
    )
    
    return(category_results)
  }
  
  # Run tests based on specified level
  if (test_level %in% c("unit", "all")) {
    # Unit tests - individual functions
    unit_patterns <- c(
      "test-find_.*\\.R$",
      "test-get_.*\\.R$", 
      "test-summarise_.*\\.R$",
      "test-point2isochrone\\.R$",
      "test-packages\\.R$",
      "test-st_singles_to_multi\\.R$",
      "test-antijoin_.*\\.R$",
      "test-utility_.*\\.R$"
    )
    
    for (pattern in unit_patterns) {
      results$unit <- run_test_category(pattern, "Unit")
    }
  }
  
  if (test_level %in% c("integration", "all")) {
    # Integration tests - component interactions
    results$integration <- run_test_category("test-targets_integration\\.R$", "Integration")
    results$helpers <- run_test_category("test-helpers\\.R$", "Helper Functions")
    results$existing_integration <- run_test_category("test-integration\\.R$", "Existing Integration")
  }
  
  if (test_level %in% c("end-to-end", "all")) {
    # End-to-end tests - full pipeline
    results$end_to_end <- run_test_category("test-end_to_end\\.R$", "End-to-End")
  }
  
  # Generate coverage report if requested
  if (coverage) {
    cat("\nGENERATING COVERAGE REPORT\n")
    cat("==========================\n")
    
    tryCatch({
      cov <- covr::package_coverage(
        path = here(),
        type = "tests",
        quiet = !verbose
      )
      
      # Save coverage report
      covr::report(cov, file = here("tests", "coverage.html"))
      cat("✓ Coverage report saved to tests/coverage.html\n")
      
      # Print coverage summary
      cat("Coverage Summary:\n")
      print(cov)
      
    }, error = function(e) {
      cat("✗ Error generating coverage report:", conditionMessage(e), "\n")
    })
  }
  
  # Summary report
  cat("\n\nTEST SUMMARY\n")
  cat("============\n")
  
  if (length(results) > 0) {
    for (category in names(results)) {
      if (!is.null(results[[category]])) {
        cat(sprintf("%-15s: %d tests\n", 
                   str_to_title(category), 
                   length(results[[category]])))
      }
    }
  }
  
  # Check for test failures and provide guidance
  cat("\nSTATUS: ")
  if (any(sapply(results, function(x) any(as.data.frame(x)$failed > 0, na.rm = TRUE)))) {
    cat("❌ Some tests failed\n")
    cat("\nNext steps:\n")
    cat("1. Check individual test output above\n")
    cat("2. Fix failing tests\n") 
    cat("3. Re-run tests with: Rscript tests/run_tests.R\n")
    return(FALSE)
  } else {
    cat("✅ All tests passed!\n")
    cat("\nYour code is ready for deployment.\n")
    return(TRUE)
  }
}

#' Quick test runner for development
run_quick_tests <- function() {
  cat("Running quick development tests...\n")
  
  # Load project
  devtools::load_all(quiet = TRUE)
  
  # Run just the core unit tests
  quick_tests <- c(
    "test-find_closest_osrm_point",
    "test-point2isochrone", 
    "test-summarise_one_sua",
    "test-utility_functions"
  )
  
  test_results <- test_dir(
    path = here("tests", "testthat"),
    filter = paste(quick_tests, collapse = "|"),
    reporter = "summary"
  )
  
  return(test_results)
}

# Command line interface
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  # Parse arguments
  test_level <- if (length(args) > 0) args[1] else "all"
  coverage <- "--coverage" %in% args
  verbose <- !"--quiet" %in% args
  create_mock <- "--create-mock-data" %in% args
  quick <- "--quick" %in% args
  
  if (quick) {
    result <- run_quick_tests()
  } else {
    result <- run_comprehensive_tests(
      test_level = test_level,
      coverage = coverage, 
      verbose = verbose,
      create_mock_data = create_mock
    )
  }
  
  # Exit with appropriate code
  if (isFALSE(result)) {
    quit(status = 1)
  } else {
    quit(status = 0) 
  }
}

# Interactive usage examples
if (interactive()) {
  cat("Interactive test runner loaded.\n")
  cat("Usage examples:\n")
  cat("  run_comprehensive_tests()                    # Run all tests\n")
  cat("  run_comprehensive_tests('unit')              # Run only unit tests\n")
  cat("  run_comprehensive_tests(coverage = TRUE)     # Include coverage\n")
  cat("  run_quick_tests()                           # Quick dev tests\n")
}