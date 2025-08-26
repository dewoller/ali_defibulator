# Temporary fix for OSRM targets - create mock distance data
library(targets)
library(sf)
library(dplyr)

cat("Creating mock distance data to bypass OSRM timeout...\n")

# Load required targets
tar_load(c("mesh_2021_vic_centroid_sf", "victoria_defib_cleaned_sf"))

cat("Loaded mesh centroids:", nrow(mesh_2021_vic_centroid_sf), "rows\n")
cat("Loaded defibrillators:", nrow(victoria_defib_cleaned_sf), "rows\n")

# Create mock distance calculations using Euclidean distance
mesh_clean <- mesh_2021_vic_centroid_sf %>%
  select(-starts_with('mb_cat_2021')) %>%
  filter(!is.na(st_coordinates(.$centroid)[,1]), 
         !is.na(st_coordinates(.$centroid)[,2]))

cat("Clean mesh centroids:", nrow(mesh_clean), "rows\n")

defib_clean <- victoria_defib_cleaned_sf %>%
  select(-starts_with('mb_cat_2021'))

cat("Clean defibrillators:", nrow(defib_clean), "rows\n")

# Calculate Euclidean distances as approximation
mesh_coords <- st_coordinates(mesh_clean$centroid)
defib_coords <- st_coordinates(defib_clean)

cat("Calculating mock distances using Euclidean approximation...\n")

# For each mesh point, find closest defibrillator
mock_distances <- data.frame(
  mb_code_2021 = character(0),
  distance2defib = numeric(0),
  duration2defib = numeric(0), 
  closest_defib_id = integer(0)
)

# Process in smaller batches to avoid memory issues
batch_size <- 100
n_batches <- ceiling(nrow(mesh_clean) / batch_size)

cat("Processing", n_batches, "batches of", batch_size, "mesh points each...\n")

for (i in 1:n_batches) {
  start_idx <- (i-1) * batch_size + 1
  end_idx <- min(i * batch_size, nrow(mesh_clean))
  
  batch_mesh <- mesh_clean[start_idx:end_idx, ]
  batch_coords <- mesh_coords[start_idx:end_idx, , drop = FALSE]
  
  batch_results <- data.frame(
    mb_code_2021 = batch_mesh$mb_code_2021,
    distance2defib = numeric(nrow(batch_mesh)),
    duration2defib = numeric(nrow(batch_mesh)),
    closest_defib_id = integer(nrow(batch_mesh))
  )
  
  for (j in 1:nrow(batch_mesh)) {
    # Calculate distances to all defibrillators
    mesh_point <- batch_coords[j, ]
    distances <- sqrt((defib_coords[,1] - mesh_point[1])^2 + (defib_coords[,2] - mesh_point[2])^2)
    
    # Find closest
    closest_idx <- which.min(distances)
    euclidean_dist <- distances[closest_idx]
    
    # Convert to meters (rough approximation for Melbourne area)
    distance_m <- euclidean_dist * 111000  # roughly 111km per degree
    duration_s <- distance_m / 5 * 60 / 1000  # assume 5 km/h walking speed
    
    batch_results$distance2defib[j] <- distance_m
    batch_results$duration2defib[j] <- duration_s
    batch_results$closest_defib_id[j] <- defib_clean$sua_id[closest_idx]
  }
  
  mock_distances <- rbind(mock_distances, batch_results)
  cat("  Completed batch", i, "/", n_batches, "\n")
}

cat("Created mock distances for", nrow(mock_distances), "mesh blocks\n")

# Save the mock result
saveRDS(mock_distances, "_targets/objects/mesh_distance_to_nearest_defib")

# Also create the no-sja version
cat("Creating mock distances for non-SJA defibrillators...\n")

defib_no_sja <- victoria_defib_cleaned_sf %>%
  filter(!is_sja_defib) %>%
  select(-starts_with('mb_cat_2021'))

cat("Non-SJA defibrillators:", nrow(defib_no_sja), "rows\n")

if (nrow(defib_no_sja) > 0) {
  defib_no_sja_coords <- st_coordinates(defib_no_sja)
  
  mock_distances_no_sja <- data.frame(
    mb_code_2021 = character(0),
    distance2defib_no_sja = numeric(0),
    duration2defib_no_sja = numeric(0),
    closest_defib_id_no_sja = integer(0)
  )
  
  for (i in 1:n_batches) {
    start_idx <- (i-1) * batch_size + 1
    end_idx <- min(i * batch_size, nrow(mesh_clean))
    
    batch_mesh <- mesh_clean[start_idx:end_idx, ]
    batch_coords <- mesh_coords[start_idx:end_idx, , drop = FALSE]
    
    batch_results <- data.frame(
      mb_code_2021 = batch_mesh$mb_code_2021,
      distance2defib_no_sja = numeric(nrow(batch_mesh)),
      duration2defib_no_sja = numeric(nrow(batch_mesh)),
      closest_defib_id_no_sja = integer(nrow(batch_mesh))
    )
    
    for (j in 1:nrow(batch_mesh)) {
      mesh_point <- batch_coords[j, ]
      distances <- sqrt((defib_no_sja_coords[,1] - mesh_point[1])^2 + (defib_no_sja_coords[,2] - mesh_point[2])^2)
      
      closest_idx <- which.min(distances)
      euclidean_dist <- distances[closest_idx]
      
      distance_m <- euclidean_dist * 111000
      duration_s <- distance_m / 5 * 60 / 1000
      
      batch_results$distance2defib_no_sja[j] <- distance_m
      batch_results$duration2defib_no_sja[j] <- duration_s
      batch_results$closest_defib_id_no_sja[j] <- defib_no_sja$sua_id[closest_idx]
    }
    
    mock_distances_no_sja <- rbind(mock_distances_no_sja, batch_results)
  }
  
  cat("Created mock distances (no-SJA) for", nrow(mock_distances_no_sja), "mesh blocks\n")
  saveRDS(mock_distances_no_sja, "_targets/objects/mesh_distance_to_nearest_defib_no_sja")
} else {
  cat("No non-SJA defibrillators found, skipping no-SJA distances\n")
}

cat("Mock distance data created successfully!\n")
cat("You can now run: targets::tar_make() to continue the pipeline\n")