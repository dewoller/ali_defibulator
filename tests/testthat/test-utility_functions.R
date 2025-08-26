# ABOUTME: Tests for utility functions like the %ni% operator and other helpers
# ABOUTME: Validates basic utility functions used throughout the project

library(testthat)

test_that("%ni% operator works correctly", {
  # Load the ni operator
  source(here::here("R/ni.R"))
  
  # Test basic functionality
  expect_true(5 %ni% c(1, 2, 3, 4))
  expect_false(3 %ni% c(1, 2, 3, 4))
  
  # Test with character vectors
  expect_true("apple" %ni% c("banana", "orange", "grape"))
  expect_false("banana" %ni% c("banana", "orange", "grape"))
  
  # Test with empty vectors
  expect_true(1 %ni% numeric(0))
  expect_true("test" %ni% character(0))
  
  # Test with NA values
  expect_true(NA %ni% c(1, 2, 3))
  expect_false(NA %ni% c(1, 2, NA))
})

test_that("%ni% is equivalent to negated %in%", {
  source(here::here("R/ni.R"))
  
  # Test equivalence with various data types
  test_vectors <- list(
    c(1, 2, 3, 4, 5),
    c("a", "b", "c", "d"),
    c(TRUE, FALSE, TRUE),
    c(1.1, 2.2, 3.3, 4.4)
  )
  
  search_sets <- list(
    c(2, 4, 6),
    c("b", "d", "f"),
    c(TRUE, TRUE),
    c(2.2, 4.4, 6.6)
  )
  
  for (i in seq_along(test_vectors)) {
    vec <- test_vectors[[i]]
    set <- search_sets[[i]]
    
    for (element in vec) {
      expect_equal(
        element %ni% set,
        !(element %in% set),
        info = paste("Failed for element", element, "in set", paste(set, collapse = ", "))
      )
    }
  }
})

test_that("%ni% handles edge cases", {
  source(here::here("R/ni.R"))
  
  # Test with NULL
  expect_error(NULL %ni% c(1, 2, 3))
  
  # Test with different data types
  expect_true(1 %ni% c("1", "2", "3"))  # No coercion
  expect_true("1" %ni% c(1, 2, 3))      # No coercion
  
  # Test with factors
  factor_vec <- factor(c("a", "b", "c"))
  expect_true("d" %ni% factor_vec)
  expect_false("a" %ni% factor_vec)
})