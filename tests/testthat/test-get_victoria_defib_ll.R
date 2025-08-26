# ABOUTME: Tests for Victorian defibrillator web scraping functions
# ABOUTME: Covers file download, HTML parsing, JSON extraction, and data transformation

library(testthat)
library(mockery)
library(jsonlite)
library(readr)
library(dplyr)
library(stringr)

test_that("get_victoria_defib_file handles valid downloads correctly", {
  skip_if_not_installed("mockery")
  
  # Mock fs::file_info to simulate file size checks
  mock_file_info <- mock(
    data.frame(size = 25000),  # First check: file exists and is large enough
    cycle = TRUE
  )
  
  # Mock fs::file_exists to return TRUE 
  mock_file_exists <- mock(TRUE, cycle = TRUE)
  
  with_mock(
    "fs::file_info" = mock_file_info,
    "fs::file_exists" = mock_file_exists,
    {
      result <- get_victoria_defib_file("http://test.com", "3000")
      expect_equal(result, "data/postcode/pc_3000.html")
    }
  )
})

test_that("get_victoria_defib_file retries on small files", {
  skip_if_not_installed("mockery")
  
  # Mock file size progression: small, then large
  mock_file_info <- mock(
    data.frame(size = 5000),   # Too small
    data.frame(size = 25000),  # Good size
    cycle = FALSE
  )
  
  mock_file_exists <- mock(FALSE, cycle = TRUE)
  mock_file_delete <- mock()
  mock_system <- mock()
  
  with_mock(
    "fs::file_info" = mock_file_info,
    "fs::file_exists" = mock_file_exists,
    "fs::file_delete" = mock_file_delete,
    "system" = mock_system,
    {
      result <- get_victoria_defib_file("http://test.com", "3000")
      expect_equal(result, "data/postcode/pc_3000.html")
    }
  )
})

test_that("get_victoria_defib_file returns NULL after max retries", {
  skip_if_not_installed("mockery")
  
  # Always return small file size
  mock_file_info <- mock(data.frame(size = 5000), cycle = TRUE)
  mock_file_exists <- mock(FALSE, cycle = TRUE)
  mock_file_delete <- mock()
  mock_system <- mock()
  
  with_mock(
    "fs::file_info" = mock_file_info,
    "fs::file_exists" = mock_file_exists,
    "fs::file_delete" = mock_file_delete,
    "system" = mock_system,
    {
      result <- get_victoria_defib_file("http://test.com", "3000")
      expect_null(result)
    }
  )
})

test_that("get_victoria_defib_ll parses JSON correctly", {
  skip_if_not_installed("mockery")
  
  # Create mock JSON data structure
  mock_json <- list(
    aeds = list(
      list(latitude = -37.8, longitude = 144.9),
      list(latitude = -37.9, longitude = 145.0)
    )
  )
  
  # Create mock HTML content
  mock_html <- paste0(
    'window.AVAEDAPP.public_results = ',
    jsonlite::toJSON(c(mock_json, list(
      latitude = -37.85,
      longitude = 144.95,
      company_name = "Test Company",
      street_no = "123",
      street_name = "Test",
      street_type = "St",
      suburb_name = "Melbourne",
      post_code = "3000",
      state = "VIC",
      unit_no = NA
    )), auto_unbox = TRUE),
    ';\nmore content'
  )
  
  mock_read_file <- mock(mock_html, cycle = TRUE)
  
  with_mock(
    "readr::read_file" = mock_read_file,
    {
      result <- get_victoria_defib_ll("dummy_file.html")
      
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 2)
      expect_true(all(c("lat", "lon", "company", "address", "postcode") %in% names(result)))
      expect_equal(result$lat[1], -37.8)
      expect_equal(result$lon[1], 144.9)
      expect_true(str_detect(result$address[1], "123 Test St"))
    }
  )
})

test_that("get_victoria_defib_ll handles NULL input", {
  result <- get_victoria_defib_ll(NULL)
  expect_null(result)
})

test_that("get_victoria_defib_ll handles empty JSON", {
  skip_if_not_installed("mockery")
  
  mock_html <- 'window.AVAEDAPP.public_results = [];\nmore content'
  mock_read_file <- mock(mock_html, cycle = TRUE)
  
  with_mock(
    "readr::read_file" = mock_read_file,
    {
      result <- get_victoria_defib_ll("dummy_file.html")
      expect_null(result)
    }
  )
})

test_that("get_victoria_defib_ll handles missing coordinates gracefully", {
  skip_if_not_installed("mockery")
  
  # JSON with missing aed coordinates but base coordinates available
  mock_json <- list(
    aeds = list(
      list(latitude = NA, longitude = NA)
    )
  )
  
  mock_html <- paste0(
    'window.AVAEDAPP.public_results = ',
    jsonlite::toJSON(c(mock_json, list(
      latitude = -37.85,
      longitude = 144.95,
      company_name = "Test Company",
      street_no = "123",
      street_name = "Test",
      street_type = "St", 
      suburb_name = "Melbourne",
      post_code = "3000",
      state = "VIC",
      unit_no = NA
    )), auto_unbox = TRUE),
    ';\nmore content'
  )
  
  mock_read_file <- mock(mock_html, cycle = TRUE)
  
  with_mock(
    "readr::read_file" = mock_read_file,
    {
      result <- get_victoria_defib_ll("dummy_file.html")
      
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 1)
      # Should fall back to base coordinates
      expect_equal(result$lat[1], -37.85)
      expect_equal(result$lon[1], 144.95)
    }
  )
})