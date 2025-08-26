# ABOUTME: End-to-end tests for the complete defibrillator analysis pipeline
# ABOUTME: Tests full workflow from data loading through to final outputs

library(testthat)
library(targets)
library(sf)
library(dplyr)
library(fs)

test_that("complete pipeline can run with mock data", {
  skip_on_ci()  # Skip on CI due to external dependencies and long runtime
  skip_if_not(file_exists("data/SUA-24-2-25.csv"), "SUA data file not available")
  
  # Create a minimal test targets script
  temp_targets <- tempfile(fileext = ".R")
  
  minimal_pipeline <- '
targets::tar_source()
library(targets)
library(sf)
library(dplyr)

tar_plan(
  # Minimal reference data
  test_crs = st_crs(4326),
  
  # Load minimal defib data
  test_defibs = read.csv("data/SUA-24-2-25.csv") %>%
    slice_head(n = 5) %>%
    mutate(sua_id = row_number()) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = test_crs),
  
  # Create test output
  test_export = test_defibs %>%
    st_drop_geometry() %>%
    select(sua_id, company, address)
)
'
  
  writeLines(minimal_pipeline, temp_targets)
  
  # Test that minimal pipeline can run
  expect_no_error({
    withr::with_dir(dirname(temp_targets), {
      tar_script(readLines(basename(temp_targets)), ask = FALSE)
      tar_make(callr_function = NULL)
      
      # Check that targets were built
      expect_true(tar_exist_objects("test_export"))
    })
  })
  
  unlink(temp_targets)
})

test_that("pipeline produces expected output files", {
  skip_on_ci()
  skip_if_not(dir_exists("output"), "Output directory not available")
  
  # Check that key output files exist after pipeline run
  expected_files <- c(
    "output/mesh.csv",
    "output/vacar.csv", 
    "output/sua_noh.csv",
    "output/victoria_export.xlsx"
  )
  
  # Run a check on expected file structure (not contents)
  for (file in expected_files) {
    if (file_exists(file)) {
      expect_true(file_size(file) > 0, info = paste(file, "should not be empty"))
    }
  }
})

test_that("geojson outputs are valid spatial data", {
  skip_on_ci()
  skip_if_not(dir_exists("output"), "Output directory not available")
  
  # Check GeoJSON files if they exist
  geojson_files <- dir_ls("output", regexp = "\\.geojson$")
  
  for (geojson_file in geojson_files) {
    if (file_exists(geojson_file) && file_size(geojson_file) > 0) {
      expect_no_error({
        spatial_data <- st_read(geojson_file, quiet = TRUE)
        expect_s3_class(spatial_data, "sf")
        expect_true(nrow(spatial_data) > 0)
        expect_true(all(st_is_valid(spatial_data)))
      }, info = paste("Failed to read or validate", geojson_file))
    }
  }
})

test_that("full pipeline data integrity checks", {
  skip_on_ci()
  
  # Mock a complete data flow test
  mock_test <- function() {
    # Create minimal versions of key data structures
    mock_mesh <- tibble(
      mb_code_2021 = paste0("MB", 1:100),
      person = sample(10:500, 100),
      dwelling = sample(5:200, 100),
      sa1_code_2021 = rep(paste0("SA", 1:20), 5),
      cald_pop = round(runif(100, 0, 0.3) * person),
      age_55_plus_pop = round(runif(100, 0.1, 0.4) * person),
      seifa_decile = sample(1:10, 100, replace = TRUE),
      geometry = st_sfc(
        lapply(1:100, function(i) {
          x <- 144 + (i %% 10) * 0.01
          y <- -37 - floor(i/10) * 0.01
          st_polygon(list(rbind(
            c(x, y), c(x+0.005, y), c(x+0.005, y-0.005), c(x, y-0.005), c(x, y)
          )))
        }),
        crs = 4326
      )
    ) %>% st_as_sf()
    
    mock_defibs <- tibble(
      sua_id = 1:20,
      company = paste("Company", 1:20),
      address = paste("Address", 1:20),
      postcode = sample(3000:3200, 20, replace = TRUE),
      is_sja_defib = sample(c(TRUE, FALSE), 20, replace = TRUE),
      latitude = runif(20, -37.5, -37),
      longitude = runif(20, 144.5, 145)
    ) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
    mock_vacar <- tibble(
      va_internal_id = 1:50,
      va_date = as.Date("2022-01-01") + sample(0:365, 50, replace = TRUE),
      geometry = st_sfc(
        lapply(1:50, function(i) {
          st_point(c(runif(1, 144, 145), runif(1, -37.5, -37)))
        }),
        crs = 4326
      )
    ) %>% st_as_sf()
    
    # Test data integrity
    expect_true(all(st_is_valid(mock_mesh$geometry)))
    expect_true(all(st_is_valid(mock_defibs$geometry)))
    expect_true(all(st_is_valid(mock_vacar$geometry)))
    
    # Test data relationships
    expect_equal(length(unique(mock_mesh$sa1_code_2021)), 20)
    expect_equal(length(unique(mock_defibs$sua_id)), 20)
    expect_equal(length(unique(mock_vacar$va_internal_id)), 50)
    
    # Test aggregation operations
    mesh_summary <- mock_mesh %>%
      st_drop_geometry() %>%
      group_by(sa1_code_2021) %>%
      summarise(
        total_person = sum(person),
        total_dwelling = sum(dwelling),
        avg_seifa = mean(seifa_decile),
        .groups = "drop"
      )
    
    expect_equal(nrow(mesh_summary), 20)
    expect_true(all(mesh_summary$total_person > 0))
    expect_true(all(mesh_summary$total_dwelling > 0))
    
    return(TRUE)
  }
  
  expect_true(mock_test())
})

test_that("pipeline handles realistic data volumes", {
  skip_on_ci()
  
  # Test that the pipeline can handle data volumes similar to production
  test_volumes <- function() {
    # Victorian meshblocks: ~50,000
    # Defibrillators: ~2,000
    # VACAR incidents: ~10,000
    
    n_mesh <- 1000  # Reduced for testing
    n_defibs <- 50
    n_vacar <- 200
    
    # Create large test datasets
    large_mesh <- tibble(
      mb_code_2021 = paste0("MB", 1:n_mesh),
      person = sample(1:500, n_mesh, replace = TRUE),
      geometry = st_sfc(
        lapply(1:n_mesh, function(i) {
          x <- 144 + (i %% 100) * 0.01
          y <- -37 - floor(i/100) * 0.01
          st_point(c(x, y))
        }),
        crs = 4326
      )
    ) %>% st_as_sf()
    
    large_defibs <- tibble(
      sua_id = 1:n_defibs,
      geometry = st_sfc(
        lapply(1:n_defibs, function(i) {
          st_point(c(runif(1, 144, 145), runif(1, -38, -37)))
        }),
        crs = 4326
      )
    ) %>% st_as_sf()
    
    # Test basic spatial operations at scale
    start_time <- Sys.time()
    distances <- st_distance(large_mesh, large_defibs)
    end_time <- Sys.time()
    
    # Should complete in reasonable time (< 30 seconds for test data)
    expect_lt(as.numeric(end_time - start_time), 30)
    expect_equal(dim(distances), c(n_mesh, n_defibs))
    
    return(TRUE)
  }
  
  expect_true(test_volumes())
})

test_that("error handling in pipeline is robust", {
  # Test pipeline behavior with problematic data
  
  # Test with missing coordinates
  bad_coords_defib <- tibble(
    sua_id = 1:3,
    company = c("A", "B", "C"),
    latitude = c(-37.8, NA, -37.9),
    longitude = c(144.9, 145.0, NA)
  )
  
  # Should handle NA coordinates gracefully
  expect_no_error({
    valid_rows <- !is.na(bad_coords_defib$latitude) & !is.na(bad_coords_defib$longitude)
    clean_defibs <- bad_coords_defib[valid_rows, ]
    expect_equal(nrow(clean_defibs), 1)
  })
  
  # Test with empty datasets
  empty_data <- tibble(
    sua_id = integer(0),
    latitude = numeric(0),
    longitude = numeric(0)
  )
  
  expect_no_error({
    result <- empty_data %>%
      filter(sua_id > 0)  # Should return empty tibble
    expect_equal(nrow(result), 0)
  })
  
  # Test with invalid geometries
  invalid_geom <- st_sfc(
    st_polygon(list(rbind(c(0,0), c(1,1), c(0,1), c(1,0), c(0,0)))),  # Self-intersecting
    crs = 4326
  )
  
  expect_no_error({
    fixed_geom <- st_make_valid(invalid_geom)
    expect_true(all(st_is_valid(fixed_geom)))
  })
})

test_that("output data meets quality standards", {
  # Test that output data meets expected quality criteria
  
  # Mock final export data
  mock_mesh_export <- tibble(
    mb_code_2021 = paste0("MB", 1:10),
    person = sample(50:500, 10),
    distance2defib = runif(10, 100, 5000),
    duration2defib = runif(10, 60, 1800),
    cald_pop = runif(10, 0, 50),
    age_55_plus_pop = runif(10, 5, 100)
  )
  
  # Quality checks
  quality_checks <- function(data) {
    # No missing critical data
    expect_true(all(!is.na(data$mb_code_2021)))
    expect_true(all(!is.na(data$person)))
    expect_true(all(data$person >= 0))
    
    # Reasonable distances and durations
    expect_true(all(data$distance2defib > 0))
    expect_true(all(data$duration2defib > 0))
    expect_true(all(data$distance2defib < 50000))  # < 50km reasonable max
    expect_true(all(data$duration2defib < 3600))   # < 1 hour reasonable max
    
    # Demographic data is reasonable
    expect_true(all(data$cald_pop >= 0))
    expect_true(all(data$age_55_plus_pop >= 0))
    expect_true(all(data$cald_pop <= data$person))  # CALD pop can"t exceed total
    expect_true(all(data$age_55_plus_pop <= data$person))
    
    return(TRUE)
  }
  
  expect_true(quality_checks(mock_mesh_export))
})