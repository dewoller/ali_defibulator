library(testthat)
library(devtools)

# Load test configuration and setup
source(file.path("tests", "test_config.R"))

# Load the source code from the project
load_all()

# Create mock data if needed
if (!dir.exists(TEST_CONFIG$MOCK_DATA_DIR) || 
    length(list.files(TEST_CONFIG$MOCK_DATA_DIR)) == 0) {
  cat("Creating mock test data...\n")
  source(file.path("tests", "fixtures", "enhanced_mock_data.R"))
}

# Run all tests with proper configuration
cat("Running comprehensive test suite...\n")
test_results <- test_dir(
  "tests/testthat/",
  reporter = "summary",
  stop_on_failure = FALSE
)

# Print summary
cat("\nTest Summary:\n")
cat("=============\n")
if (inherits(test_results, "testthat_results")) {
  as_tibble(test_results) %>%
    summarise(
      total_tests = n(),
      passed = sum(passed),
      failed = sum(failed), 
      skipped = sum(skipped),
      warnings = sum(warning)
    ) %>%
    print()
}

# Cleanup
cleanup_test_environment()
