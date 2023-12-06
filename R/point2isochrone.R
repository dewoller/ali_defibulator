point2isochrone = function( latitude, longitude, time_limit=4*60, mode = "foot",
													 orsm='http://localhost:5000'
													 ) {

	options(osrm.server = "http://localhost:1234/")
	options(osrm.profile = "foot")

	isochrone <- osrmIsochrone(loc = c( longitude, latitude), breaks = seq(0, 5, 5))
	# Now you can plot the isochrone polygon
	# plot(st_geometry(isochrone))

	if (nrow(isochrone) == 0) {

		st_point(c( longitude, latitude)) %>%
			st_sfc( crs = 4326) %>%
			st_transform( 3857) %>%
			st_buffer( dist = 400) %>%
			st_transform( 4326)  %>%
		{.} -> isochrone
	}

	isochrone
}


