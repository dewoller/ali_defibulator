# ABOUTME: Tests for spatial anti-join function that removes points within specified distance
# ABOUTME: Validates distance calculations and anti-join logic for deduplication

library(testthat)
library(dplyr)
library(geosphere)

test_that("antijoin_within_distance removes points within distance threshold", {
  # Create test data with some points close together
  df1 <- tibble(
    id = 1:4,
    latitude = c(-37.8, -37.81, -37.9, -37.95),
    longitude = c(144.9, 144.91, 145.0, 145.1),
    name = c("Point1", "Point2", "Point3", "Point4")
  )
  
  df2 <- tibble(
    latitude = c(-37.8, -37.9),  # Close to Point1 and Point3
    longitude = c(144.9, 145.0)
  )
  
  # Test with 200m threshold - should remove points close to df2
  result <- antijoin_within_distance(df1, df2, limit_distance = 200)
  
  # Expectations
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) <= nrow(df1))  # Should have fewer or equal rows
  expect_true(all(c("id", "latitude", "longitude", "name") %in% names(result)))
  expect_false(".id" %in% names(result))  # Internal .id should be removed
})

test_that("antijoin_within_distance validates required columns", {
  # Test missing latitude column
  df1_bad <- tibble(
    id = 1:2,
    longitude = c(144.9, 145.0),
    name = c("Point1", "Point2")
  )
  
  df2 <- tibble(
    latitude = c(-37.8),
    longitude = c(144.9)
  )
  
  expect_error(
    antijoin_within_distance(df1_bad, df2),
    "latitude.*%in%.*names"
  )
  
  # Test missing longitude column
  df1_bad2 <- tibble(
    id = 1:2,
    latitude = c(-37.8, -37.9),
    name = c("Point1", "Point2")
  )
  
  expect_error(
    antijoin_within_distance(df1_bad2, df2),
    "longitude.*%in%.*names"
  )
})

test_that("antijoin_within_distance calculates distances correctly", {
  # Create test points with known distances
  df1 <- tibble(
    id = 1:3,
    latitude = c(-37.8, -37.8, -37.9),
    longitude = c(144.9, 145.0, 144.9),  # Second point ~11km east, third ~11km south
    name = c("Origin", "East", "South")
  )
  
  df2 <- tibble(
    latitude = c(-37.8),
    longitude = c(144.9)  # At origin
  )
  
  # Test with 1km threshold - should only remove origin point
  result_1km <- antijoin_within_distance(df1, df2, limit_distance = 1000)
  expect_equal(nrow(result_1km), 2)  # East and South should remain
  expect_true(all(c("East", "South") %in% result_1km$name))
  
  # Test with 20km threshold - should remove all points
  result_20km <- antijoin_within_distance(df1, df2, limit_distance = 20000)
  expect_equal(nrow(result_20km), 0)  # All points should be removed
})

test_that("antijoin_within_distance handles identical coordinates", {
  # Test with exact coordinate matches
  df1 <- tibble(
    id = 1:3,
    latitude = c(-37.8, -37.8, -37.9),
    longitude = c(144.9, 144.9, 145.0),  # First two are identical
    name = c("Duplicate1", "Duplicate2", "Different")
  )
  
  df2 <- tibble(
    latitude = c(-37.8),
    longitude = c(144.9)  # Matches first two points exactly
  )
  
  result <- antijoin_within_distance(df1, df2, limit_distance = 1)
  
  # Only "Different" should remain
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "Different")
})

test_that("antijoin_within_distance handles empty dataframes", {
  # Empty df1
  df1_empty <- tibble(
    id = integer(0),
    latitude = numeric(0),
    longitude = numeric(0)
  )
  
  df2 <- tibble(
    latitude = c(-37.8),
    longitude = c(144.9)
  )
  
  result_empty1 <- antijoin_within_distance(df1_empty, df2)
  expect_equal(nrow(result_empty1), 0)
  
  # Empty df2
  df1 <- tibble(
    id = 1:2,
    latitude = c(-37.8, -37.9),
    longitude = c(144.9, 145.0)
  )
  
  df2_empty <- tibble(
    latitude = numeric(0),
    longitude = numeric(0)
  )
  
  result_empty2 <- antijoin_within_distance(df1, df2_empty)
  expect_equal(nrow(result_empty2), 2)  # All points should remain
})

test_that("antijoin_within_distance preserves all original columns", {
  # Test with various column types
  df1 <- tibble(
    id = 1:3,
    latitude = c(-37.8, -37.81, -37.9),
    longitude = c(144.9, 144.91, 145.0),
    name = c("A", "B", "C"),
    value = c(10.5, 20.3, 30.7),
    is_active = c(TRUE, FALSE, TRUE),
    category = factor(c("Type1", "Type2", "Type1"))
  )
  
  df2 <- tibble(
    latitude = c(-37.8),
    longitude = c(144.9)
  )
  
  result <- antijoin_within_distance(df1, df2, limit_distance = 500)
  
  # Check all original columns are preserved
  expect_true(all(c("id", "latitude", "longitude", "name", "value", "is_active", "category") %in% names(result)))
  
  # Check data types are preserved
  expect_type(result$id, "integer")
  expect_type(result$value, "double")
  expect_type(result$is_active, "logical")
  expect_s3_class(result$category, "factor")
})

test_that("antijoin_within_distance with custom distance limits", {
  # Create points at different distances
  df1 <- tibble(
    id = 1:4,
    latitude = c(-37.8, -37.801, -37.81, -37.82),  # Progressively further
    longitude = c(144.9, 144.901, 144.91, 144.92),
    name = paste0("Point", 1:4)
  )
  
  df2 <- tibble(
    latitude = c(-37.8),
    longitude = c(144.9)  # Reference point
  )
  
  # Test different distance thresholds
  result_50 <- antijoin_within_distance(df1, df2, limit_distance = 50)
  result_500 <- antijoin_within_distance(df1, df2, limit_distance = 500)
  result_2000 <- antijoin_within_distance(df1, df2, limit_distance = 2000)
  
  # With increasing distance limits, more points should be filtered out
  expect_true(nrow(result_50) >= nrow(result_500))
  expect_true(nrow(result_500) >= nrow(result_2000))
})