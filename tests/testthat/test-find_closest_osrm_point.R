library(testthat)
library(sf)
library(dplyr)
library(tidyr)
library(mockery)

test_that("find_closest_osrm_point returns the closest point with correct structure", {
  skip_if_not_installed("mockery")
  
  # Mock the osrmTable function to avoid external dependencies
  mockery::stub(find_closest_osrm_point, "osrmTable", function(...) {
    list(
      durations = c(120, 300, 180),
      distances = c(200, 500, 300)
    )
  })
  
  # Create test data
  setA <- tibble(
    longitude = c(145.0, 145.1, 145.2),
    latitude = c(-37.7, -37.8, -37.9),
    id = 1:3
  )
  
  # Call the function
  result <- find_closest_osrm_point(145.3, -38.0, setA)
  
  # Expectations
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$id, 1)  # Should choose the row with minimum distance
  expect_equal(result$distance, 200)
  expect_equal(result$duration, 120)
})

test_that("find_closest_osrm_points_closest_n works with different n values", {
  skip_if_not_installed("mockery")
  
  # Create test data
  setA <- st_as_sf(
    tibble(
      longitude = runif(20, 144, 146),
      latitude = runif(20, -38, -37),
      id = 1:20
    ),
    coords = c("longitude", "latitude"),
    crs = 4326
  )
  
  setB <- st_as_sf(
    tibble(
      longitude = runif(5, 144, 146),
      latitude = runif(5, -38, -37),
      point_id = letters[1:5]
    ),
    coords = c("longitude", "latitude"),
    crs = 4326
  )
  
  # Mock the find_closest_osrm_point function
  mockery::stub(find_closest_osrm_points_closest_n, "find_closest_osrm_point", 
                function(lon, lat, setA) {
                  setA[1,] %>% 
                    mutate(duration = 120, distance = 200)
                })
  
  # Test with different n values
  result_n5 <- find_closest_osrm_points_closest_n(setB, setA, n = 5)
  result_n10 <- find_closest_osrm_points_closest_n(setB, setA, n = 10)
  
  # Expectations
  expect_equal(nrow(result_n5), nrow(setB))
  expect_equal(nrow(result_n10), nrow(setB))
  expect_true("duration" %in% names(result_n5))
  expect_true("distance" %in% names(result_n5))
})
