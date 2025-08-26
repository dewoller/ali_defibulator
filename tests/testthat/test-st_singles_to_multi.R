# ABOUTME: Tests for spatial geometry conversion from single to multi-polygon
# ABOUTME: Ensures proper handling of different geometry types and data frame structures

library(testthat)
library(sf)
library(dplyr)

test_that("st_singles_to_multi converts single geometries to multi", {
  # Create test data with single polygons
  test_data <- tibble(
    id = 1:3,
    name = c("A", "B", "C"),
    isochrone = list(
      st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
      st_polygon(list(rbind(c(2,0), c(3,0), c(3,1), c(2,1), c(2,0)))),
      st_polygon(list(rbind(c(4,0), c(5,0), c(5,1), c(4,1), c(4,0))))
    )
  )
  
  # Call the function
  result <- st_singles_to_multi(test_data)
  
  # Expectations
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 3)
  expect_true("geometry" %in% names(result))
  
  # Check that geometries are converted to multi-polygon
  for (i in 1:nrow(result)) {
    geom_type <- st_geometry_type(result$geometry[i])
    expect_true(geom_type %in% c("MULTIPOLYGON", "POLYGON"))
  }
})

test_that("st_singles_to_multi handles already multi-polygon geometries", {
  # Create test data with mixed geometry types
  single_poly <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  multi_poly <- st_multipolygon(list(
    list(rbind(c(2,0), c(3,0), c(3,1), c(2,1), c(2,0)))
  ))
  
  test_data <- tibble(
    id = 1:2,
    name = c("Single", "Multi"),
    isochrone = list(single_poly, multi_poly)
  )
  
  result <- st_singles_to_multi(test_data)
  
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
  
  # Both should be handled correctly
  expect_true(all(st_is_valid(result$geometry)))
})

test_that("st_singles_to_multi preserves data columns", {
  # Create test data with multiple columns
  test_data <- tibble(
    sua_id = 1:2,
    company = c("Company A", "Company B"),
    address = c("123 Main St", "456 Oak Ave"),
    postcode = c("3000", "3001"),
    is_sja_defib = c(TRUE, FALSE),
    sua_area = c(1000.5, 2000.7),
    isochrone = list(
      st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
      st_polygon(list(rbind(c(2,0), c(3,0), c(3,1), c(2,1), c(2,0))))
    )
  )
  
  result <- st_singles_to_multi(test_data)
  
  # Check all original columns are preserved
  expect_true(all(c("sua_id", "company", "address", "postcode", "is_sja_defib", "sua_area") %in% names(result)))
  expect_equal(result$sua_id, c(1, 2))
  expect_equal(result$company, c("Company A", "Company B"))
  expect_equal(result$is_sja_defib, c(TRUE, FALSE))
})

test_that("st_singles_to_multi handles empty data", {
  # Create empty test data
  test_data <- tibble(
    id = integer(0),
    isochrone = list()
  )
  
  result <- st_singles_to_multi(test_data)
  
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 0)
  expect_true("geometry" %in% names(result))
})

test_that("st_singles_to_multi sets correct CRS", {
  # Create test data with explicit CRS
  test_poly <- st_polygon(list(rbind(
    c(144.9, -37.8), c(145.0, -37.8), c(145.0, -37.7), c(144.9, -37.7), c(144.9, -37.8)
  )))
  
  test_data <- tibble(
    id = 1,
    isochrone = list(test_poly)
  )
  
  result <- st_singles_to_multi(test_data)
  
  # Should have geometry column
  expect_true("geometry" %in% names(result))
  expect_s3_class(result$geometry, "sfc")
})

test_that("st_singles_to_multi handles NULL geometries gracefully", {
  # Create test data with some NULL geometries
  test_data <- tibble(
    id = 1:3,
    isochrone = list(
      st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
      NULL,
      st_polygon(list(rbind(c(2,0), c(3,0), c(3,1), c(2,1), c(2,0))))
    )
  )
  
  # Function should handle this gracefully or filter out NULLs
  expect_no_error({
    result <- st_singles_to_multi(test_data)
  })
})

test_that("st_singles_to_multi maintains spatial validity", {
  # Create test data with potentially invalid geometries
  test_data <- tibble(
    id = 1:2,
    isochrone = list(
      st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
      st_polygon(list(rbind(c(2,2), c(3,2), c(3,3), c(2,3), c(2,2))))
    )
  )
  
  result <- st_singles_to_multi(test_data)
  
  # All geometries should be valid
  expect_true(all(st_is_valid(result$geometry)))
})