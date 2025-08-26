# ABOUTME: Enhanced mock data generation for comprehensive testing of the defibrillator analysis pipeline
# ABOUTME: Creates realistic test datasets that mirror the structure and relationships of production data

library(sf)
library(dplyr)
library(tibble)
library(purrr)
library(lubridate)

#' Create comprehensive mock datasets for testing
#' @param n_meshblocks Number of meshblocks to create
#' @param n_defibs Number of defibrillators to create  
#' @param n_vacar Number of VACAR incidents to create
#' @param n_sa1 Number of SA1 areas to create
#' @param bbox Bounding box for data generation (c(xmin, ymin, xmax, ymax))
#' @return List containing all mock datasets
create_comprehensive_mock_data <- function(
  n_meshblocks = 200,
  n_defibs = 30, 
  n_vacar = 100,
  n_sa1 = 40,
  bbox = c(144.5, -38.0, 145.5, -37.0)
) {
  
  set.seed(42)  # Reproducible data
  
  # Generate SA1 areas first
  sa1_data <- tibble(
    sa1_code_2021 = sprintf("2%07d", sample(1000000:9999999, n_sa1)),
    sa2_code_2021 = sprintf("2%08d", rep(sample(10000000:99999999, n_sa1 %/% 4), each = 4)[1:n_sa1]),
    gcc_code_2021 = "2GMEL",  # Greater Melbourne
    ste_code_2021 = "2",      # Victoria
    ste_name_2021 = "Victoria"
  ) %>%
    mutate(
      # Create polygons for SA1 areas
      geometry = map(1:n_sa1, function(i) {
        x_center <- runif(1, bbox[1], bbox[3])
        y_center <- runif(1, bbox[2], bbox[4])
        size <- runif(1, 0.01, 0.05)  # SA1 size variation
        
        st_polygon(list(rbind(
          c(x_center - size, y_center - size),
          c(x_center + size, y_center - size),
          c(x_center + size, y_center + size),
          c(x_center - size, y_center + size),
          c(x_center - size, y_center - size)
        )))
      })
    ) %>%
    mutate(geometry = st_sfc(geometry, crs = 4326)) %>%
    st_as_sf()
  
  # Generate meshblocks within SA1 areas
  meshblocks <- tibble(
    mb_code_2021 = sprintf("2%010d", sample(1000000000:9999999999, n_meshblocks)),
    sa1_code_2021 = sample(sa1_data$sa1_code_2021, n_meshblocks, replace = TRUE),
    person = pmax(0, round(rnorm(n_meshblocks, 150, 80))),
    dwelling = pmax(0, round(person * runif(n_meshblocks, 0.3, 0.7))),
    area_sqkm = runif(n_meshblocks, 0.001, 0.1),
    mb_cat_2021 = sample(c("Residential", "Commercial", "Industrial", "Other"), 
                        n_meshblocks, replace = TRUE, prob = c(0.7, 0.15, 0.1, 0.05))
  ) %>%
    # Add demographics based on realistic distributions
    mutate(
      # CALD population (typically 20-40% in Melbourne)
      cald_pop = round(person * runif(n_meshblocks, 0.15, 0.45)),
      
      # Age 55+ population (typically 25-35%)
      age_55_plus_pop = round(person * runif(n_meshblocks, 0.2, 0.4)),
      
      # Age 65+ population (subset of 55+)
      age_65_plus_pop = round(age_55_plus_pop * runif(n_meshblocks, 0.4, 0.7)),
      
      # SEIFA scores (800-1200 range)
      seifa_score = round(rnorm(n_meshblocks, 1000, 150)),
      seifa_decile = pmax(1, pmin(10, round((seifa_score - 800) / 40 + 1)))
    ) %>%
    # Create geometries within corresponding SA1 areas
    mutate(
      geometry = map2(sa1_code_2021, 1:n_meshblocks, function(sa1_code, i) {
        sa1_geom <- sa1_data$geometry[sa1_data$sa1_code_2021 == sa1_code][[1]]
        bbox_sa1 <- st_bbox(sa1_geom)
        
        # Generate point within SA1 bounds
        x <- runif(1, bbox_sa1["xmin"], bbox_sa1["xmax"])
        y <- runif(1, bbox_sa1["ymin"], bbox_sa1["ymax"])
        
        # Create small polygon around point
        size <- 0.002
        st_polygon(list(rbind(
          c(x - size, y - size),
          c(x + size, y - size),
          c(x + size, y + size),
          c(x - size, y + size),
          c(x - size, y - size)
        )))
      })
    ) %>%
    mutate(geometry = st_sfc(geometry, crs = 4326)) %>%
    st_as_sf()
  
  # Generate defibrillator data
  defibrillators <- tibble(
    sua_id = 1:n_defibs,
    company = case_when(
      runif(n_defibs) < 0.3 ~ paste("ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET"),
      runif(n_defibs) < 0.5 ~ paste("Fire Station", sample(1:100, n_defibs, replace = TRUE)),
      runif(n_defibs) < 0.7 ~ paste("Shopping Centre", sample(letters, n_defibs, replace = TRUE)),
      TRUE ~ paste("Community Centre", sample(1:50, n_defibs, replace = TRUE))
    ),
    latitude = runif(n_defibs, bbox[2], bbox[4]),
    longitude = runif(n_defibs, bbox[1], bbox[3]),
    lat_base = latitude,  # Copy for data structure compatibility
    lon_base = longitude,
    address = paste(
      sample(1:999, n_defibs, replace = TRUE),
      sample(c("Main St", "High St", "Church St", "Collins St", "Bourke St"), 
             n_defibs, replace = TRUE),
      sample(c("MELBOURNE", "RICHMOND", "FITZROY", "CARLTON", "SOUTH YARRA"), 
             n_defibs, replace = TRUE),
      sample(3000:3200, n_defibs, replace = TRUE),
      "VIC"
    ),
    postcode = sample(3000:3200, n_defibs, replace = TRUE),
    is_sja_defib = str_detect(str_to_upper(company), "DEFIB IN")
  ) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  # Generate VACAR incident data  
  vacar_incidents <- tibble(
    va_internal_id = 1:n_vacar,
    va_date = as.Date("2019-01-01") + sample(0:(365*5-1), n_vacar, replace = TRUE),
    va_age = pmax(0, pmin(100, round(rnorm(n_vacar, 65, 15)))),
    va_gender = sample(c("Male", "Female"), n_vacar, replace = TRUE, prob = c(0.6, 0.4)),
    va_location_type = sample(c("Home", "Public", "Workplace", "Other"), 
                             n_vacar, replace = TRUE, prob = c(0.6, 0.2, 0.15, 0.05)),
    va_outcome = sample(c("ROSC", "Death", "Ongoing"), 
                       n_vacar, replace = TRUE, prob = c(0.3, 0.6, 0.1)),
    va_xllwgs84 = runif(n_vacar, bbox[1], bbox[3]),
    va_yllwgs84 = runif(n_vacar, bbox[2], bbox[4])
  ) %>%
    st_as_sf(coords = c("va_xllwgs84", "va_yllwgs84"), crs = 4326)
  
  # Generate census demographic data at SA1 level
  census_cald <- sa1_data %>%
    st_drop_geometry() %>%
    select(sa1_code_2021) %>%
    mutate(
      cald = round(runif(nrow(sa1_data), 100, 2000)),
      total_pop = round(cald / runif(nrow(sa1_data), 0.2, 0.4))
    )
  
  census_age_55_plus <- sa1_data %>%
    st_drop_geometry() %>%
    select(sa1_code_2021) %>%
    mutate(
      age_55_plus = round(runif(nrow(sa1_data), 200, 1500)),
      age_65_plus = round(age_55_plus * runif(nrow(sa1_data), 0.4, 0.7))
    )
  
  # Generate SEIFA data
  seifa_sa1 <- sa1_data %>%
    st_drop_geometry() %>%
    select(sa1_code_2021) %>%
    mutate(
      seifa_score = round(rnorm(nrow(sa1_data), 1000, 150)),
      seifa_decile = pmax(1, pmin(10, round((seifa_score - 800) / 40 + 1))),
      population = round(runif(nrow(sa1_data), 1000, 8000))
    )
  
  # Generate OSRM-style responses for mocking
  osrm_responses <- list(
    # Mock isochrone responses
    isochrones = map(1:n_defibs, function(i) {
      center_x <- st_coordinates(defibrillators[i,])[1]
      center_y <- st_coordinates(defibrillators[i,])[2]
      radius <- runif(1, 0.005, 0.015)  # Realistic walking isochrone size
      
      # Create circular approximation
      angles <- seq(0, 2*pi, length.out = 20)
      coords <- cbind(
        center_x + radius * cos(angles),
        center_y + radius * sin(angles)
      )
      
      st_polygon(list(rbind(coords, coords[1,])))
    }),
    
    # Mock distance/duration tables
    distance_table = matrix(
      runif(n_meshblocks * n_defibs, 100, 5000),  # 100m to 5km
      nrow = n_meshblocks,
      ncol = n_defibs
    ),
    
    duration_table = matrix(
      runif(n_meshblocks * n_defibs, 60, 1800),   # 1 minute to 30 minutes
      nrow = n_meshblocks,
      ncol = n_defibs
    )
  )
  
  # Return comprehensive mock data
  list(
    sa1_2021 = sa1_data,
    meshblocks = meshblocks,
    defibrillators = defibrillators,
    vacar_incidents = vacar_incidents,
    census_cald = census_cald,
    census_age_55_plus = census_age_55_plus,
    seifa_sa1 = seifa_sa1,
    osrm_responses = osrm_responses,
    
    # Additional derived data for testing
    mesh_centroids = meshblocks %>%
      mutate(centroid = st_centroid(geometry)),
    
    defib_no_sja = defibrillators %>%
      filter(!is_sja_defib),
    
    # Relationship mapping for testing joins
    mesh_to_sa1 = meshblocks %>%
      st_drop_geometry() %>%
      select(mb_code_2021, sa1_code_2021, person) %>%
      group_by(sa1_code_2021) %>%
      mutate(
        sa1_population = sum(person),
        mesh_pop_sa1_proportion = person / sa1_population
      ) %>%
      ungroup()
  )
}

#' Save mock data to test fixtures
#' @param mock_data List of mock datasets from create_comprehensive_mock_data
#' @param output_dir Directory to save fixtures
save_mock_data_fixtures <- function(mock_data, output_dir = "tests/fixtures/data") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save spatial data as GeoJSON
  spatial_datasets <- c("sa1_2021", "meshblocks", "defibrillators", "vacar_incidents", "mesh_centroids", "defib_no_sja")
  
  for (dataset_name in spatial_datasets) {
    if (dataset_name %in% names(mock_data)) {
      filepath <- file.path(output_dir, paste0("mock_", dataset_name, ".geojson"))
      st_write(mock_data[[dataset_name]], filepath, delete_dsn = TRUE, quiet = TRUE)
    }
  }
  
  # Save tabular data as CSV
  tabular_datasets <- c("census_cald", "census_age_55_plus", "seifa_sa1", "mesh_to_sa1")
  
  for (dataset_name in tabular_datasets) {
    if (dataset_name %in% names(mock_data)) {
      filepath <- file.path(output_dir, paste0("mock_", dataset_name, ".csv"))
      write.csv(mock_data[[dataset_name]], filepath, row.names = FALSE)
    }
  }
  
  # Save OSRM responses as RDS
  saveRDS(mock_data$osrm_responses, file.path(output_dir, "mock_osrm_responses.rds"))
  
  # Create summary info file
  summary_info <- tibble(
    dataset = names(mock_data),
    type = map_chr(mock_data, function(x) {
      if (inherits(x, "sf")) "spatial"
      else if (is.data.frame(x)) "tabular"
      else if (is.list(x)) "list"
      else class(x)[1]
    }),
    rows = map_int(mock_data, function(x) {
      if (is.data.frame(x)) nrow(x)
      else if (is.list(x)) length(x)
      else 1
    }),
    columns = map_int(mock_data, function(x) {
      if (is.data.frame(x)) ncol(x)
      else if (is.list(x)) length(names(x))
      else 0
    })
  )
  
  write.csv(summary_info, file.path(output_dir, "mock_data_summary.csv"), row.names = FALSE)
  
  message("Mock data saved to ", output_dir)
  return(invisible(TRUE))
}

# Main execution
if (interactive() || identical(Sys.getenv("CREATE_MOCK_DATA"), "TRUE")) {
  mock_data <- create_comprehensive_mock_data(
    n_meshblocks = 500,  # Reasonable size for testing
    n_defibs = 50,
    n_vacar = 200,
    n_sa1 = 100
  )
  
  save_mock_data_fixtures(mock_data)
}