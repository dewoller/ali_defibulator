point2isochrone = function( latitude, longitude, time_limit=4*60, mode = "foot",
													 orsm='http://localhost:5000'
 ) {

	options(osrm.server = "http://localhost:1234/")
	options(osrm.profile = "foot")

	isochrone <- osrmIsochrone(loc = c( longitude, latitude), breaks = seq(0, 5, 5))
	# Now you can plot the isochrone polygon
	# plot(st_geometry(isochrone))

	isochrone
}


