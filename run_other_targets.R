# Run other targets that don't depend on OSRM distance calculations
library(targets)

cat("Checking which targets can run independently...\n")

# Get the dependency network
network <- tar_network()
manifest <- tar_manifest()

cat("All targets:\n")
print(manifest$name)

cat("\nTargets that don't depend on mesh distance calculations:\n")

# Look for targets that don't have dependencies on the problematic ones
distance_targets <- c("mesh_distance_to_nearest_defib", "mesh_distance_to_nearest_defib_no_sja", 
                     "vacar_distance_to_nearest_defib", "vacar_distance_to_nearest_defib_no_sja")

# Find targets that don't depend on distance calculations
independent_targets <- c(
  "defib_to_delete",
  "defib_to_add", 
  "vic_defib_urls",
  "victoria_defib_cleaned",
  "victoria_defib_cleaned_sf",
  "victoria_defib_no_sja_sf",
  "victoria_defib_sua_temp",
  "victoria_defib_sua",
  "sua_noh",
  "sua_noh_sf"
)

cat("Attempting to build independent targets:\n")
for (target in independent_targets) {
  if (target %in% manifest$name) {
    cat("Building:", target, "...")
    tryCatch({
      tar_make(names = target, callr_function = NULL)
      cat(" ✓\n")
    }, error = function(e) {
      cat(" ✗ Error:", conditionMessage(e), "\n")
    })
  }
}