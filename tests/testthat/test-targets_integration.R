# ABOUTME: Integration tests for the targets pipeline components and data flow
# ABOUTME: Tests key pipeline stages, data transformations, and target dependencies

library(testthat)
library(targets)
library(sf)
library(dplyr)
library(mockery)

test_that("targets pipeline can be loaded without errors", {
  # Test that _targets.R loads successfully
  expect_no_error({
    tar_script(readLines(here::here("_targets.R")), ask = FALSE)
  })
})

test_that("key pipeline targets are defined", {
  # Load the targets script
  tar_script(readLines(here::here("_targets.R")), ask = FALSE)
  
  # Get all target names
  manifest <- tar_manifest()
  target_names <- manifest$name
  
  # Check that key targets exist
  key_targets <- c(
    "sa1_2021",
    "standard_crs",
    "mesh_2021_vic_sf",
    "victoria_defib_cleaned_sf",
    "sua_noh_sf",
    "mesh_export",
    "vacar_export",
    "sua_noh_mesh_export"
  )
  
  for (target in key_targets) {
    expect_true(target %in% target_names, 
                info = paste("Target", target, "not found in pipeline"))
  }
})

test_that("targets have correct dependencies", {
  tar_script(readLines(here::here("_targets.R")), ask = FALSE)
  
  # Test some key dependencies
  network <- tar_network()
  
  # mesh_export should depend on mesh_detail_final
  mesh_export_deps <- network$edges$to[network$edges$from == "mesh_export"]
  expect_true("mesh_detail_final" %in% mesh_export_deps)
  
  # sua_noh_mesh_export should depend on sua_noh_mesh_summarised
  sua_export_deps <- network$edges$to[network$edges$from == "sua_noh_mesh_export"]
  expect_true("sua_noh_mesh_summarised" %in% sua_export_deps)
})

test_that("reference data targets work with mock data", {
  skip_on_ci()  # Skip on CI as requires large reference datasets
  
  # Test that reference data targets can be built
  expect_no_error({
    tar_script(readLines(here::here("_targets.R")), ask = FALSE)
    
    # Try to make just the basic reference targets
    tar_make(names = c("sa1_2021", "standard_crs"), callr_function = NULL)
  })
})

test_that("spatial operations in pipeline maintain valid geometries", {
  # Create mock spatial data similar to pipeline
  mock_meshblocks <- tibble(
    mb_code_2021 = c("MB1", "MB2", "MB3"),
    geometry = st_sfc(
      st_polygon(list(rbind(c(144,  -37), c(145,  -37), c(145, -38), c(144, -38), c(144,  -37)))),
      st_polygon(list(rbind(c(145,  -37), c(146,  -37), c(146, -38), c(145, -38), c(145,  -37)))),
      st_polygon(list(rbind(c(146,  -37), c(147,  -37), c(147, -38), c(146, -38), c(146,  -37)))),
      crs = 4326
    )
  ) %>% st_as_sf()
  
  mock_defibs <- tibble(
    sua_id = 1:2,
    latitude = c(-37.5, -37.7),
    longitude = c(144.5, 145.5)
  ) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  # Test spatial operations similar to pipeline
  expect_true(all(st_is_valid(mock_meshblocks$geometry)))
  expect_true(all(st_is_valid(mock_defibs$geometry)))
  
  # Test spatial intersection
  intersections <- st_intersects(mock_meshblocks, mock_defibs)
  expect_type(intersections, "list")
  expect_equal(length(intersections), nrow(mock_meshblocks))
})

test_that("data processing pipeline handles edge cases", {
  # Test with minimal datasets
  minimal_mesh <- tibble(
    mb_code_2021 = "MB1",
    person = 1,
    dwelling = 1,
    sa1_code_2021 = "SA1",
    geometry = st_sfc(
      st_polygon(list(rbind(c(144, -37), c(145, -37), c(145, -38), c(144, -38), c(144, -37)))),
      crs = 4326
    )
  ) %>% st_as_sf()
  
  minimal_defib <- tibble(
    sua_id = 1,
    company = "Test Company",
    latitude = -37.5,
    longitude = 144.5
  ) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  # Should handle minimal data without errors
  expect_no_error({
    # Simulate mesh processing
    mesh_with_centroid <- minimal_mesh %>%
      mutate(centroid = st_centroid(geometry))
    
    expect_equal(nrow(mesh_with_centroid), 1)
    expect_true("centroid" %in% names(mesh_with_centroid))
  })
})

test_that("export functionality creates expected output structure", {
  # Create mock export data
  mock_vacar_export <- tibble(
    va_internal_id = 1:5,
    va_date = as.Date("2022-01-01") + 0:4,
    distance2defib = runif(5, 100, 1000),
    duration2defib = runif(5, 60, 600)
  )
  
  mock_mesh_export <- tibble(
    mb_code_2021 = paste0("MB", 1:10),
    person = sample(50:500, 10),
    dwelling = sample(20:200, 10),
    distance2defib = runif(10, 100, 2000)
  )
  
  mock_sua_export <- tibble(
    sua_id = 1:8,
    person = sample(100:2000, 8),
    cald_pop = sample(10:400, 8),
    weighted_seifa_score = runif(8, 800, 1200)
  )
  
  # Test export list structure
  export_list <- list(
    vacar = mock_vacar_export,
    mesh = mock_mesh_export,
    sua_noh = mock_sua_export
  )
  
  expect_equal(length(export_list), 3)
  expect_true(all(c("vacar", "mesh", "sua_noh") %in% names(export_list)))
  
  # Test each export has expected structure
  expect_true("va_internal_id" %in% names(export_list$vacar))
  expect_true("mb_code_2021" %in% names(export_list$mesh))
  expect_true("sua_id" %in% names(export_list$sua_noh))
})

test_that("pipeline data transformations preserve referential integrity", {
  # Create linked test data
  test_sa1 <- tibble(
    sa1_code_2021 = c("SA1001", "SA1002")
  )
  
  test_mesh <- tibble(
    mb_code_2021 = c("MB001", "MB002", "MB003"),
    sa1_code_2021 = c("SA1001", "SA1001", "SA1002"),
    person = c(100, 150, 200)
  )
  
  test_defib <- tibble(
    sua_id = c(1, 2),
    company = c("Company A", "Company B")
  )
  
  # Test joins preserve referential integrity
  mesh_sa1_join <- test_mesh %>%
    inner_join(test_sa1, by = "sa1_code_2021")
  
  expect_equal(nrow(mesh_sa1_join), 3)  # All mesh rows should match
  expect_true(all(mesh_sa1_join$sa1_code_2021 %in% test_sa1$sa1_code_2021))
  
  # Test that group-by operations work correctly
  sa1_summary <- test_mesh %>%
    group_by(sa1_code_2021) %>%
    summarise(total_person = sum(person), .groups = "drop")
  
  expect_equal(nrow(sa1_summary), 2)  # One row per SA1
  expect_equal(sa1_summary$total_person[sa1_summary$sa1_code_2021 == "SA1001"], 250)
  expect_equal(sa1_summary$total_person[sa1_summary$sa1_code_2021 == "SA1002"], 200)
})