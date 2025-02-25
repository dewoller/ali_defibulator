library(testthat)
library(sf)
library(dplyr)

# Test for the st_singles_to_multi function
test_that("st_singles_to_multi correctly converts single geometries to multi", {
  # Create test data with single polygons
  singles <- tibble(
    id = 1:3,
    isochrone = st_sfc(
      st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
      st_polygon(list(rbind(c(1,1), c(2,1), c(2,2), c(1,2), c(1,1)))),
      st_polygon(list(rbind(c(2,2), c(3,2), c(3,3), c(2,3), c(2,2)))),
      crs = 4326
    )
  )
  
  # Call the function
  result <- st_singles_to_multi(singles)
  
  # Expectations
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(singles))
  
  # Check that the geometry type is now MULTIPOLYGON
  expect_equal(st_geometry_type(result$isochrone)[1], "MULTIPOLYGON")
})

# Create a basic test fixture for meshblocks
create_mock_meshblocks <- function(n = 10) {
  tibble(
    mb_code_2021 = as.character(1:n),
    person = sample(50:500, n),
    dwelling = sample(20:200, n),
    sa1_code_2021 = sample(paste0("2", sample(1000:9999, n, replace = TRUE)), n),
    cald_pop = round(runif(n, 0, 0.3) * person),
    age_55_plus_pop = round(runif(n, 0.1, 0.4) * person),
    age_65_plus_pop = round(runif(n, 0.05, 0.2) * person),
    seifa_score = sample(800:1200, n),
    seifa_decile = sample(1:10, n, replace = TRUE),
    centroid = st_sfc(
      lapply(1:n, function(i) st_point(c(144 + i/100, -37 - i/100))),
      crs = 4326
    )
  ) %>%
    mutate(geometry = centroid) %>%
    st_as_sf()
}

# Create a basic test fixture for defibs
create_mock_defibs <- function(n = 10) {
  tibble(
    sua_id = 1:n,
    company = paste("Defib", 1:n),
    address = paste("Address", 1:n),
    postcode = sample(3000:3200, n),
    is_sja_defib = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.3, 0.7)),
    latitude = runif(n, -38, -37),
    longitude = runif(n, 144, 145)
  ) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
}

# Create a basic test fixture for VACAR data
create_mock_vacar <- function(n = 10) {
  tibble(
    va_internal_id = 1:n,
    va_date = as.Date("2022-01-01") + sample(0:365, n, replace = TRUE),
    va_xllwgs84 = runif(n, 144, 145),
    va_yllwgs84 = runif(n, -38, -37),
    va_age = sample(20:90, n, replace = TRUE),
    va_gender = sample(c("Male", "Female"), n, replace = TRUE)
  ) %>%
    st_as_sf(coords = c("va_xllwgs84", "va_yllwgs84"), crs = 4326)
}
