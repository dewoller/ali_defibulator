library(sf)
library(dplyr)

# Create mock spatial data for testing
create_test_fixtures <- function() {
  # Create mock meshblocks
  meshblocks <- tibble(
    mb_code_2021 = as.character(1:20),
    person = sample(50:500, 20),
    dwelling = sample(20:200, 20),
    sa1_code_2021 = sample(paste0("2", sample(1000:9999, 20, replace = TRUE)), 20),
    cald_pop = round(runif(20, 0, 0.3) * person),
    age_55_plus_pop = round(runif(20, 0.1, 0.4) * person),
    age_65_plus_pop = round(runif(20, 0.05, 0.2) * person),
    seifa_score = sample(800:1200, 20),
    seifa_decile = sample(1:10, 20, replace = TRUE)
  ) %>%
    mutate(geometry = st_sfc(
      lapply(1:20, function(i) st_point(c(144 + i/100, -37 - i/100))),
      crs = 4326
    )) %>%
    st_as_sf()
  
  # Create mock defibrillator locations
  defibs <- tibble(
    sua_id = 1:15,
    company = paste("Defib", 1:15),
    address = paste("Address", 1:15),
    postcode = sample(3000:3200, 15),
    is_sja_defib = sample(c(TRUE, FALSE), 15, replace = TRUE, prob = c(0.3, 0.7)),
    latitude = runif(15, -38, -37),
    longitude = runif(15, 144, 145)
  ) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  # Create mock VACAR data
  vacar <- tibble(
    va_internal_id = 1:10,
    va_date = as.Date("2022-01-01") + sample(0:365, 10, replace = TRUE),
    va_xllwgs84 = runif(10, 144, 145),
    va_yllwgs84 = runif(10, -38, -37),
    va_age = sample(20:90, 10, replace = TRUE),
    va_gender = sample(c("Male", "Female"), 10, replace = TRUE)
  ) %>%
    st_as_sf(coords = c("va_xllwgs84", "va_yllwgs84"), crs = 4326)
  
  # Create mock census data
  census <- tibble(
    sa1_code_2021 = unique(meshblocks$sa1_code_2021),
    total_population = sample(1000:5000, length(unique(meshblocks$sa1_code_2021))),
    cald_population = round(runif(length(unique(meshblocks$sa1_code_2021)), 0.1, 0.4) * total_population)
  )
  
  # Save the fixtures
  dir.create("tests/fixtures/data", recursive = TRUE, showWarnings = FALSE)
  st_write(meshblocks, "tests/fixtures/data/mock_meshblocks.geojson", delete_dsn = TRUE)
  st_write(defibs, "tests/fixtures/data/mock_defibs.geojson", delete_dsn = TRUE)
  st_write(vacar, "tests/fixtures/data/mock_vacar.geojson", delete_dsn = TRUE)
  write.csv(census, "tests/fixtures/data/mock_census.csv", row.names = FALSE)
  
  # Create mock OSRM responses
  osrm_responses <- list(
    isochrone = st_sfc(
      st_polygon(list(rbind(
        c(145.0, -37.7),
        c(145.1, -37.7),
        c(145.1, -37.8),
        c(145.0, -37.8),
        c(145.0, -37.7)
      ))),
      crs = 4326
    ),
    table = list(
      durations = matrix(
        sample(60:600, 10*10, replace = TRUE),
        nrow = 10
      ),
      distances = matrix(
        sample(100:10000, 10*10, replace = TRUE),
        nrow = 10
      )
    )
  )
  
  # Save as RDS
  saveRDS(osrm_responses, "tests/fixtures/data/mock_osrm_responses.rds")
  
  return(list(
    meshblocks = meshblocks,
    defibs = defibs,
    vacar = vacar,
    census = census,
    osrm_responses = osrm_responses
  ))
}

# Run this function to create the fixtures
if (interactive()) {
  create_test_fixtures()
}
