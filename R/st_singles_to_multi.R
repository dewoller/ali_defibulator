st_singles_to_multi = function( sua )  {
	# Extract geometry from each sf object in the isochrone list
	geometries <- lapply(sua$isochrone, function(x) {
		if (inherits(x, "sf")) {
			st_geometry(x)  # Extract sfc from sf
		} else if (inherits(x, "sfc")) {
			x  # Already sfc
		} else {
			stop("Unexpected isochrone type: ", class(x))
		}
	})
	
	# Combine individual sfc objects into a single sfc column
	combined_sfc <- do.call(c, geometries)
	
	# Create sf object
	sua_sf <- sua %>%
		select(-isochrone) %>%
		mutate(geometry = combined_sfc) %>%
		st_as_sf()
	
	return(sua_sf)
}
	
