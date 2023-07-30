find_closest_osrm_point = function( pointB, setA ) {
	# point: a data frame with a single row containing a latitude and longitude
	# points: a data frame with multiple rows containing latitudes and longitudes
	# returns: a data frame with a single row containing the closest point in points to point
	#


  # Calculate distances from the current point in setB to all points in setA
  table <- osrmTable(src = setA, dst = pointB, osrm.server = "http://localhost:1234/", osrm.profile = "foot", measure=c('duration', 'distance'))

  durations <- table[["durations"]]
  dists <- table[["distances"]]
  
  # Find the closest point in setA
  closest_point <- setA[which.min(dists), ]
  closest_point$duration <- min(durations)
  closest_point$distance <- min(dists)
  return(closest_point)
}


find_closest_osrm_points = function( setB, setA ) {
	# point: a data frame with a single row containing a latitude and longitude
	# points: a data frame with multiple rows containing latitudes and longitudes
	# returns: a data frame with a single row containing the closest point in points to point
	#
# Apply the function to each point in setB


	setB %>%
		mutate( closest_point = 
			map2( latitude, longitude,
				~find_closest_osrm_point(
					pointB= tibble( latitude=.x, longitude=.y), 
					setA = setA) )) %>%
		unnest( closest_point ) 


}
