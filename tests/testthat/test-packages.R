# ABOUTME: Tests for package loading and conflict resolution functionality
# ABOUTME: Ensures all required packages load correctly with proper conflict handling

library(testthat)

test_that("packages can be sourced without errors", {
  # Test that the packages file loads successfully
  expect_no_error({
    source(here::here("R/packages.R"))
  })
})

test_that("required packages are loaded", {
  # Source the packages file
  source(here::here("R/packages.R"))
  
  # Check that key packages are available
  expect_true("sf" %in% loadedNamespaces())
  expect_true("dplyr" %in% loadedNamespaces()) 
  expect_true("targets" %in% loadedNamespaces())
  expect_true("purrr" %in% loadedNamespaces())
  expect_true("stringr" %in% loadedNamespaces())
})

test_that("spatial packages are correctly loaded", {
  source(here::here("R/packages.R"))
  
  # Check spatial analysis packages
  expect_true("sf" %in% loadedNamespaces())
  expect_true("units" %in% loadedNamespaces())
  
  # Test that sf functions work
  expect_no_error({
    point <- st_point(c(145, -37))
    expect_s3_class(point, "sfg")
  })
})

test_that("osrm package functions are available", {
  source(here::here("R/packages.R"))
  
  # Check if OSRM functions are available (even if server is down)
  expect_true(exists("osrmTable"))
  expect_true(exists("osrmIsochrone"))
})

test_that("data manipulation packages work correctly", {
  source(here::here("R/packages.R"))
  
  # Test dplyr operations
  test_data <- tibble(x = 1:3, y = 4:6)
  result <- test_data %>% 
    filter(x > 1) %>%
    mutate(z = x + y)
  
  expect_equal(nrow(result), 2)
  expect_true("z" %in% names(result))
})

test_that("targets integration works", {
  source(here::here("R/packages.R"))
  
  # Check targets functions are available
  expect_true(exists("tar_target"))
  expect_true(exists("tar_source"))
  expect_true(exists("tar_make"))
})