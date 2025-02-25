library(testthat)
library(sf)
library(dplyr)

test_that("find_n_closest_mb returns the correct number of nearest meshblocks", {
  # Create test data
  meshes <- tibble(
    mb_code_2021 = as.character(1:20),
    centroid = st_sfc(
      lapply(1:20, function(i) st_point(c(144 + i/100, -37 - i/100))),
      crs = 4326
    )
  ) %>%
    mutate(geometry = centroid) %>%
    st_as_sf()
  
  sua <- tibble(
    sua_id = letters[1:3],
    longitude = c(144.1, 144.2, 144.3),
    latitude = c(-37.1, -37.2, -37.3),
    isochrone = rep(list(NULL), 3)
  )
  
  # Skip if the nngeo package is not installed
  skip_if_not_installed("nngeo")
  
  # Call the function with n=5
  result <- find_n_closest_mb(sua, meshes, n = 5)
  
  # Expectations
  expect_equal(nrow(result), nrow(sua))
  expect_equal(length(result$nn[[1]]), 5)  # Each sua should have 5 nearest meshblocks
  expect_equal(length(result$nn[[2]]), 5)
  expect_equal(length(result$nn[[3]]), 5)
})
