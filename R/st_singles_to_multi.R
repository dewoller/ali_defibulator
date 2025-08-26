st_singles_to_multi = function( sua )  {
	# Combine individual sfc objects into a single sfc column
	combined_sfc <- do.call(c, sua$isochrone)
	
	# Create sf object
	sua_sf <- sua %>%
		select(-isochrone) %>%
		mutate(geometry = combined_sfc) %>%
		st_as_sf()
	
	return(sua_sf)
}
	
