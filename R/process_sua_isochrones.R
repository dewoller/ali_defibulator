# ABOUTME: Process isochrone data for Service Unit Areas (SUAs) with error handling and validation
# ABOUTME: Validates geometries, calculates areas, and classifies defibrillator types

#' Process SUA isochrone data with robust error handling
#' 
#' @param sua_temp_data Data frame with isochrone column containing sfc geometries
#' @return Data frame with validated isochrones, areas, and SJA classification
#' @export
process_sua_isochrones <- function(sua_temp_data) {
  
  # Validate input
  if (!is.data.frame(sua_temp_data)) {
    stop("Input must be a data frame")
  }
  
  if (!"isochrone" %in% names(sua_temp_data)) {
    stop("Input data frame must have an 'isochrone' column")
  }
  
  if (!"company" %in% names(sua_temp_data)) {
    stop("Input data frame must have a 'company' column")
  }
  
  # Check isochrone data types and validate
  cat("Processing", nrow(sua_temp_data), "isochrones...\n")
  
  # Process with robust error handling
  processed_data <- sua_temp_data %>%
    mutate(
      # Validate and fix geometries
      isochrone = map(isochrone, function(x) {
        tryCatch({
          # Check if geometry is valid
          if (is.null(x)) {
            warning("NULL geometry found, skipping")
            return(NULL)
          }
          
          # Ensure it's a spatial geometry
          if (!inherits(x, c("sfc", "sf", "sfg"))) {
            warning("Non-spatial geometry found, skipping")
            return(NULL)
          }
          
          # Make geometry valid
          validated_geom <- st_make_valid(x)
          
          # Check if result is still valid
          if (!all(st_is_valid(validated_geom))) {
            warning("Geometry could not be made valid")
            return(NULL)
          }
          
          return(validated_geom)
          
        }, error = function(e) {
          warning("Error processing geometry: ", conditionMessage(e))
          return(NULL)
        })
      }),
      
      # Calculate areas safely
      sua_area = map_dbl(isochrone, function(x) {
        tryCatch({
          if (is.null(x)) return(0)
          
          area_result <- st_area(x)
          
          # Convert to numeric and drop units
          area_numeric <- as.numeric(area_result)
          if (is.na(area_numeric) || area_numeric < 0) {
            warning("Invalid area calculated, using 0")
            return(0)
          }
          
          return(area_numeric)
          
        }, error = function(e) {
          warning("Error calculating area: ", conditionMessage(e), ", using 0")
          return(0)
        })
      }),
      
      # Classify SJA defibrillators
      is_sja_defib = case_when(
        is.na(company) ~ FALSE,
        company == "" ~ FALSE,
        TRUE ~ str_detect(str_to_upper(company), "DEFIB IN")
      )
    ) %>%
    # Filter out rows where geometry processing failed
    filter(!map_lgl(isochrone, is.null))
  
  # Report results
  n_processed <- nrow(processed_data)
  n_original <- nrow(sua_temp_data)
  n_filtered <- n_original - n_processed
  
  cat("Successfully processed", n_processed, "out of", n_original, "isochrones\n")
  if (n_filtered > 0) {
    cat("Filtered out", n_filtered, "invalid isochrones\n")
  }
  
  # Summary statistics
  valid_areas <- processed_data$sua_area[processed_data$sua_area > 0]
  if (length(valid_areas) > 0) {
    cat("Area statistics (m²):\n")
    cat("  Min:", round(min(valid_areas)), "\n")
    cat("  Max:", round(max(valid_areas)), "\n") 
    cat("  Mean:", round(mean(valid_areas)), "\n")
    cat("  Median:", round(median(valid_areas)), "\n")
  }
  
  sja_count <- sum(processed_data$is_sja_defib)
  cat("SJA defibrillators:", sja_count, "out of", n_processed, 
      "(", round(100 * sja_count / n_processed, 1), "%)\n")
  
  return(processed_data)
}