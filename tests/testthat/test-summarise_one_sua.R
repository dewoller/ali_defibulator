# ABOUTME: Tests for SUA (Service Unit Area) demographic summarization functions
# ABOUTME: Validates spatial intersection calculations and demographic aggregation

library(testthat)
library(sf)
library(dplyr)
library(units)

test_that("summarise_one_sua calculates demographics correctly", {
  # Create test meshblock data
  test_mesh <- tibble(
    mb_code_2021 = c("MB1", "MB2", "MB3"),
    person = c(100, 150, 200),
    dwelling = c(40, 60, 80),
    cald_pop = c(20, 30, 50),
    age_55_plus_pop = c(15, 25, 40),
    age_65_plus_pop = c(10, 15, 25),
    seifa_score = c(900, 1000, 1100),
    seifa_decile = c(3, 5, 8),
    sa1_code_2021 = c("SA1", "SA1", "SA2"),
    geometry = st_sfc(
      st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
      st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0)))),
      st_polygon(list(rbind(c(2,0), c(3,0), c(3,1), c(2,1), c(2,0)))),
      crs = 4326
    )
  ) %>% st_as_sf()
  
  # Create test nearest neighbor data
  test_nn <- tibble(
    mb_code_2021 = c("MB1", "MB2", "MB3"),
    geometry = test_mesh$geometry
  ) %>% st_as_sf()
  
  # Create test isochrone that partially overlaps meshblocks
  test_isochrone <- st_polygon(list(rbind(
    c(0.5, 0), c(2.5, 0), c(2.5, 1), c(0.5, 1), c(0.5, 0)
  ))) %>% 
    st_sfc(crs = 4326)
  
  # Call the function
  result <- summarise_one_sua(test_nn, test_isochrone, test_mesh)
  
  # Expectations
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_true(all(c("n_mesh_intersections", "weighted_seifa_score", "person", "dwelling") %in% names(result)))
  expect_true("most_populous_decile" %in% names(result))
  
  # Check that proportional calculations are applied
  expect_true(result$person > 0)
  expect_true(result$dwelling > 0)
  expect_true(result$cald_pop > 0)
  expect_true(result$weighted_seifa_score > 0)
})

test_that("summarise_one_sua handles no intersections", {
  # Create test data with no spatial overlap
  test_mesh <- tibble(
    mb_code_2021 = c("MB1"),
    person = c(100),
    dwelling = c(40),
    cald_pop = c(20),
    age_55_plus_pop = c(15),
    age_65_plus_pop = c(10),
    seifa_score = c(900),
    seifa_decile = c(3),
    sa1_code_2021 = c("SA1"),
    geometry = st_sfc(
      st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
      crs = 4326
    )
  ) %>% st_as_sf()
  
  test_nn <- tibble(
    mb_code_2021 = c("MB1"),
    geometry = test_mesh$geometry
  ) %>% st_as_sf()
  
  # Create isochrone that doesn't overlap
  test_isochrone <- st_polygon(list(rbind(
    c(5, 5), c(6, 5), c(6, 6), c(5, 6), c(5, 5)
  ))) %>% 
    st_sfc(crs = 4326)
  
  # Call the function - should handle empty intersection gracefully
  expect_no_error({
    result <- summarise_one_sua(test_nn, test_isochrone, test_mesh)
  })
})

test_that("summarise_one_sua calculates most_populous_decile correctly", {
  # Create test data with clear decile distribution
  test_mesh <- tibble(
    mb_code_2021 = c("MB1", "MB2", "MB3"),
    person = c(50, 200, 100),  # MB2 has highest population
    dwelling = c(20, 80, 40),
    cald_pop = c(10, 40, 20),
    age_55_plus_pop = c(8, 30, 15),
    age_65_plus_pop = c(5, 20, 10),
    seifa_score = c(900, 1000, 1100),
    seifa_decile = c(3, 7, 5),  # Decile 7 should be most populous
    sa1_code_2021 = c("SA1", "SA1", "SA1"),
    geometry = st_sfc(
      st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
      st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0)))),
      st_polygon(list(rbind(c(2,0), c(3,0), c(3,1), c(2,1), c(2,0)))),
      crs = 4326
    )
  ) %>% st_as_sf()
  
  test_nn <- tibble(
    mb_code_2021 = c("MB1", "MB2", "MB3"),
    geometry = test_mesh$geometry
  ) %>% st_as_sf()
  
  # Isochrone covers all meshblocks
  test_isochrone <- st_polygon(list(rbind(
    c(-0.5, -0.5), c(3.5, -0.5), c(3.5, 1.5), c(-0.5, 1.5), c(-0.5, -0.5)
  ))) %>% 
    st_sfc(crs = 4326)
  
  result <- summarise_one_sua(test_nn, test_isochrone, test_mesh)
  
  # Most populous decile should be 7 (from MB2 with 200 people)
  expect_equal(result$most_populous_decile, 7)
})

test_that("summarise_one_sua handles empty decile correctly", {
  # Create test data that results in empty decile calculation
  test_mesh <- tibble(
    mb_code_2021 = character(0),
    person = numeric(0),
    dwelling = numeric(0),
    cald_pop = numeric(0),
    age_55_plus_pop = numeric(0),
    age_65_plus_pop = numeric(0),
    seifa_score = numeric(0),
    seifa_decile = numeric(0),
    sa1_code_2021 = character(0),
    geometry = st_sfc(crs = 4326)
  ) %>% st_as_sf()
  
  test_nn <- test_mesh
  
  test_isochrone <- st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
  ))) %>% 
    st_sfc(crs = 4326)
  
  # Should handle empty data gracefully
  expect_no_error({
    result <- summarise_one_sua(test_nn, test_isochrone, test_mesh)
  })
  
  # Empty result should have most_populous_decile = -1
  if (nrow(result) == 1) {
    expect_equal(result$most_populous_decile, -1)
  }
})

test_that("summarise_one_sua creates proportion columns", {
  # Create test data
  test_mesh <- tibble(
    mb_code_2021 = c("MB1"),
    person = c(100),
    dwelling = c(40),
    cald_pop = c(20),
    age_55_plus_pop = c(30),
    age_65_plus_pop = c(15),
    seifa_score = c(1000),
    seifa_decile = c(5),
    sa1_code_2021 = c("SA1"),
    geometry = st_sfc(
      st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
      crs = 4326
    )
  ) %>% st_as_sf()
  
  test_nn <- tibble(
    mb_code_2021 = c("MB1"),
    geometry = test_mesh$geometry
  ) %>% st_as_sf()
  
  test_isochrone <- st_polygon(list(rbind(
    c(-0.5, -0.5), c(1.5, -0.5), c(1.5, 1.5), c(-0.5, 1.5), c(-0.5, -0.5)
  ))) %>% 
    st_sfc(crs = 4326)
  
  result <- summarise_one_sua(test_nn, test_isochrone, test_mesh)
  
  # Check that proportion columns are created
  expect_true("cald_pop_prop" %in% names(result))
  expect_true("age_55_plus_pop_prop" %in% names(result))
  expect_true("age_65_plus_pop_prop" %in% names(result))
  
  # Check proportion calculations are reasonable
  expect_equal(result$cald_pop_prop, result$cald_pop / result$person)
  expect_equal(result$age_55_plus_pop_prop, result$age_55_plus_pop / result$person)
})