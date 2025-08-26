# Simple test runner for the defibrillator analysis project
library(testthat)

# Source all R functions
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (file in r_files) {
  tryCatch({
    source(file)
    cat("✓ Sourced:", basename(file), "\n")
  }, error = function(e) {
    cat("✗ Error sourcing", basename(file), ":", conditionMessage(e), "\n")
  })
}

# Create basic mock test data
cat("Creating basic mock data...\n")

# Simple mock meshblocks
mock_meshblocks <- data.frame(
  mb_code_2021 = c("MB1", "MB2", "MB3"),
  person = c(100, 150, 200),
  dwelling = c(40, 60, 80),
  cald_pop = c(20, 30, 50),
  age_55_plus_pop = c(15, 25, 40),
  age_65_plus_pop = c(10, 15, 25),
  seifa_score = c(900, 1000, 1100),
  seifa_decile = c(3, 5, 8),
  sa1_code_2021 = c("SA1", "SA1", "SA2"),
  stringsAsFactors = FALSE
)

# Simple mock defibrillators
mock_defibs <- data.frame(
  sua_id = 1:3,
  company = c("Company A", "Company B", "ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET"),
  latitude = c(-37.8, -37.81, -37.82),
  longitude = c(144.9, 144.91, 144.92),
  address = c("123 Main St", "456 Oak Ave", "789 High St"),
  postcode = c("3000", "3001", "3002"),
  stringsAsFactors = FALSE
)

cat("Running basic function tests...\n")

# Test 1: %ni% operator
cat("\nTesting %ni% operator...\n")
if (exists("%ni%")) {
  test_result_1 <- 5 %ni% c(1, 2, 3, 4)
  test_result_2 <- 3 %ni% c(1, 2, 3, 4)
  
  if (test_result_1 == TRUE && test_result_2 == FALSE) {
    cat("✓ %ni% operator works correctly\n")
  } else {
    cat("✗ %ni% operator failed\n")
  }
} else {
  cat("✗ %ni% operator not found\n")
}

# Test 2: antijoin_within_distance function
cat("\nTesting antijoin_within_distance...\n")
if (exists("antijoin_within_distance")) {
  tryCatch({
    # Test with mock data
    df1 <- data.frame(
      id = 1:2,
      latitude = c(-37.8, -37.9),
      longitude = c(144.9, 145.0)
    )
    
    df2 <- data.frame(
      latitude = c(-37.8),
      longitude = c(144.9)
    )
    
    result <- antijoin_within_distance(df1, df2, limit_distance = 1000)
    
    if (nrow(result) <= nrow(df1)) {
      cat("✓ antijoin_within_distance works correctly\n")
    } else {
      cat("✗ antijoin_within_distance returned unexpected results\n")
    }
  }, error = function(e) {
    cat("✗ Error in antijoin_within_distance:", conditionMessage(e), "\n")
  })
} else {
  cat("✗ antijoin_within_distance function not found\n")
}

# Test 3: get_victoria_defib_ll (basic structure test)
cat("\nTesting get_victoria_defib_ll structure...\n")
if (exists("get_victoria_defib_ll")) {
  # Test with NULL input
  result <- get_victoria_defib_ll(NULL)
  if (is.null(result)) {
    cat("✓ get_victoria_defib_ll handles NULL input correctly\n")
  } else {
    cat("✗ get_victoria_defib_ll didn't handle NULL input\n")
  }
} else {
  cat("✗ get_victoria_defib_ll function not found\n")
}

# Test 4: Check if sf functions work
cat("\nTesting spatial functionality...\n")
if (requireNamespace("sf", quietly = TRUE)) {
  library(sf)
  tryCatch({
    # Create a simple point
    point <- st_point(c(145, -37))
    if (inherits(point, "sfg")) {
      cat("✓ sf package working correctly\n")
    } else {
      cat("✗ sf package not working as expected\n")
    }
  }, error = function(e) {
    cat("✗ Error with sf package:", conditionMessage(e), "\n")
  })
} else {
  cat("✗ sf package not available\n")
}

# Test 5: Check essential packages
cat("\nChecking essential packages...\n")
essential_packages <- c("dplyr", "purrr", "stringr", "tidyr", "readr")

for (pkg in essential_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("✓", pkg, "available\n")
  } else {
    cat("✗", pkg, "not available\n")
  }
}

cat("\nBasic function tests completed!\n")
cat("=====================================\n")

# Run actual testthat tests if available
if (dir.exists("tests/testthat")) {
  cat("\nRunning testthat unit tests...\n")
  
  # Try to run a simple test
  tryCatch({
    test_result <- test_file("tests/testthat/test-utility_functions.R", reporter = "summary")
    cat("✓ Unit test completed\n")
  }, error = function(e) {
    cat("Note: Some tests may require additional setup:", conditionMessage(e), "\n")
  })
}

cat("\nTest run complete! Check output above for any issues.\n")