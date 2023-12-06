	
if(FALSE) {


	sf1 = head( mesh_in_victoria, 10)
	sf2 = victoria_defib_cleaned_sf

}
find_closest_points = function( sf1, sf2 ) {

	# Step 1: Calculate the Distance Matrix
	distance_matrix <- st_distance(sf1, sf2)

	# Step 2: Find the Index of the Nearest Points
	nearest_indices <- apply(distance_matrix, 1, which.min)
	nearest_distance <- apply(distance_matrix, 1, min)  # Extract minimum distances

	# Step 3: Extract the Coordinates of the Nearest Points
	nearest_points <- sf2[nearest_indices,]

	# Combine the original points and their nearest points for easier analysis
	result <- cbind(st_drop_geometry(sf1), st_drop_geometry(nearest_points), nearest_distance)

	result

}
