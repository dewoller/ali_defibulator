# Fix the victoria_defib_sua processing issue
library(targets)
library(sf)
library(dplyr)
library(purrr)
library(stringr)

cat("Loading victoria_defib_sua_temp target...\n")
tar_load('victoria_defib_sua_temp')

cat("Checking isochrone data types:\n")
# Check what types we actually have
isochrone_classes <- map_chr(victoria_defib_sua_temp$isochrone, function(x) {
  if (is.null(x)) return("NULL")
  if (inherits(x, "sfc")) return("sfc")
  if (inherits(x, "sf")) return("sf") 
  if (inherits(x, c("POLYGON", "sfg"))) return("polygon")
  return("other")
})

cat("Isochrone type distribution:\n")
print(table(isochrone_classes))

# Fixed processing
cat("Processing with corrected logic...\n")

victoria_defib_sua_fixed <- victoria_defib_sua_temp %>%
  mutate(type = map_chr(isochrone, function(x) {
    if (is.null(x)) return("null")
    if (inherits(x, "sfc")) return("sfc")  # This is what we actually have
    if (inherits(x, "sf")) return("sf")
    if (inherits(x, c("POLYGON", "sfg"))) return("polygon") 
    return("unknown")
  })) %>%
  # Filter for valid spatial objects (sfc is what point2isochrone returns)
  filter(type %in% c("sfc", "sf")) %>%
  select(-type) %>%
  # Make sure geometries are valid
  mutate(isochrone = map(isochrone, function(x) {
    tryCatch({
      st_make_valid(x)
    }, error = function(e) {
      cat("Warning: Could not validate geometry, keeping original\n")
      x
    })
  })) %>%
  # Calculate areas safely
  mutate(sua_area = map_dbl(isochrone, function(x) {
    tryCatch({
      st_area(x) %>% units::drop_units()
    }, error = function(e) {
      cat("Warning: Could not calculate area, using 0\n")
      0
    })
  })) %>%
  # Classify SJA defibrillators
  mutate(is_sja_defib = str_detect(str_to_upper(company), 'DEFIB IN'))

cat("Processed", nrow(victoria_defib_sua_fixed), "valid isochrones out of", nrow(victoria_defib_sua_temp), "total\n")

# Save the fixed result
cat("Saving corrected victoria_defib_sua...\n")
saveRDS(victoria_defib_sua_fixed, "_targets/objects/victoria_defib_sua")

# Also create sua_noh (remove holes)
cat("Creating sua_noh (removing holes)...\n")
sua_noh_fixed <- victoria_defib_sua_fixed %>%
  mutate(isochrone = map(isochrone, function(x) {
    tryCatch({
      nngeo::st_remove_holes(x)
    }, error = function(e) {
      cat("Warning: Could not remove holes, keeping original\n")
      x
    })
  }))

saveRDS(sua_noh_fixed, "_targets/objects/sua_noh")

cat("Fixed targets saved successfully!\n")
cat("You can now continue with: targets::tar_make()\n")