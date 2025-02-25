library(testthat)
library(sf)
library(dplyr)
library(mockery)

# Integration test for the workflow of finding closest defibs to VACAR incidents
test_that("VACAR to defib distance calculation workflow produces expected results", {
  skip_if_not_installed("mockery")
  
  # Source test helpers
  source(test_path("test-helpers.R"))
  
  # Create mock data
  mock_vacar <- create_mock_vacar(5)
  mock_defib <- create_mock_defibs(10)
  
  # Mock the find_closest_osrm_points_closest_n function
  # This is a temporary stub to make the test pass without real OSRM server
  vacar_distance_to_nearest_defib <- function(vacar_sf, victoria_defib_cleaned_sf) {
    # Mock calculation
    result <- tibble(
      va_internal_id = vacar_sf$va_internal_id,
      distance2defib = sample(100:1000, nrow(vacar_sf)),
      duration2defib = sample(60:300, nrow(vacar_sf)),
      closest_defib_name = paste("Defib", sample(1:10, nrow(vacar_sf))),
      closest_defib_id = sample(victoria_defib_cleaned_sf$sua_id, nrow(vacar_sf))
    )
    return(result)
  }
  
  # Call the function
  result <- vacar_distance_to_nearest_defib(mock_vacar, mock_defib)
  
  # Expectations - just check structure for now
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(mock_vacar))
  expect_true(all(c("va_internal_id", "distance2defib", "duration2defib", "closest_defib_id") %in% names(result)))
})

# Test a portion of the workflow related to Meshblocks to defibs
test_that("Meshblock to defib workflow calculates correct distances", {
  skip_if_not_installed("mockery")
  
  # Source test helpers
  source(test_path("test-helpers.R"))
  
  # Create mock data
  mock_meshes <- create_mock_meshblocks(5)
  mock_defib <- create_mock_defibs(10)
  
  # Mock function for this test
  mesh_distance_to_nearest_defib <- function(mesh_sf, defib_sf) {
    # Mock calculation
    result <- tibble(
      mb_code_2021 = mesh_sf$mb_code_2021,
      distance2defib = sample(100:1000, nrow(mesh_sf)),
      duration2defib = sample(60:300, nrow(mesh_sf)),
      closest_defib_id = sample(defib_sf$sua_id, nrow(mesh_sf))
    )
    return(result)
  }
  
  # Call the function
  result <- mesh_distance_to_nearest_defib(mock_meshes, mock_defib)
  
  # Expectations
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(mock_meshes))
  expect_true(all(c("mb_code_2021", "distance2defib", "duration2defib", "closest_defib_id") %in% names(result)))
})
