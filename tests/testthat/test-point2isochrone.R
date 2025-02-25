library(testthat)
library(sf)
library(mockery)

test_that("point2isochrone generates valid isochrone", {
  skip_if_not_installed("mockery")
  
  # Mock the osrmIsochrone function
  mockery::stub(point2isochrone, "osrmIsochrone", function(...) {
    # Create a simple polygon as a mock isochrone
    st_polygon(list(rbind(
      c(145.0, -37.7),
      c(145.1, -37.7),
      c(145.1, -37.8),
      c(145.0, -37.8),
      c(145.0, -37.7)
    ))) %>%
      st_sfc(crs = 4326)
  })
  
  # Call the function
  result <- point2isochrone(latitude = -37.75, longitude = 145.05)
  
  # Expectations
  expect_s3_class(result, "sfc")
  expect_true(st_is_valid(result))
})

test_that("point2isochrone fallback works when OSRM fails", {
  skip_if_not_installed("mockery")
  
  # Mock the osrmIsochrone function to fail
  mockery::stub(point2isochrone, "osrmIsochrone", function(...) {
    stop("OSRM error")
  })
  
  # Call the function
  result <- point2isochrone(
    latitude = -37.75, 
    longitude = 145.05,
    fallback_radius = 200
  )
  
  # Expectations
  expect_s3_class(result, "sfc")
  expect_true(st_is_valid(result))
  
  # Check the fallback radius (after transforming to a suitable CRS)
  area <- st_area(st_transform(result, 3112))
  expected_area <- pi * 200^2
  expect_equal(as.numeric(area), expected_area, tolerance = 0.1)
})
