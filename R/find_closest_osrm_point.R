find_closest_osrm_point = function(longitude, latitude, setA) {
    # pointB: a data frame with a single row containing a latitude and longitude
    # seetA: a data frame with multiple rows containing latitudes and longitudes
    # returns: a data frame with a single row containing the closest point in points to point
    #


    # Calculate distances from the current point in setB to all points in setA
    table <- osrmTable(
        src = setA,
        dst = matrix(c(longitude, latitude), ncol = 2),
        osrm.server = "http://localhost:1234/",
        osrm.profile = "foot",
        measure = c("duration", "distance")
    )

    durations <- table[["durations"]]
    dists <- table[["distances"]]

    # Find the closest point in setA
    closest_point <- setA[which.min(dists), ]
    closest_point$duration <- min(durations)
    closest_point$distance <- min(dists)
    return(closest_point)
}


find_closest_osrm_points = function(setB, setA) {
    # setB, setA: a data frame with a multiple rows containing latitudes and longitudes
    # returns: a data frame with a single row containing the closest point in points to point
    # Apply the function to each point in setB


    setB %>%
        mutate(
            closest_point =
                map2(
                    latitude, longitude,
                    ~ find_closest_osrm_point(
                        pointB = tibble(latitude = .x, longitude = .y),
                        setA = setA
                    )
                )
        ) %>%
        unnest(closest_point)
}


find_closest_osrm_points_closest_n = function(setB, setA, n = 10, setB_coords = st_coordinates(setB)) {
    # for each in setB, find the n closest points in setA, and then find the closest osrm from these
    
    # Validate input coordinates
    if (any(is.na(setB_coords))) {
        warning("Found NA coordinates in setB. Removing rows with NA coordinates.")
        valid_rows <- !is.na(setB_coords[,1]) & !is.na(setB_coords[,2])
        setB <- setB[valid_rows,]
        setB_coords <- setB_coords[valid_rows,]
    }
    
    if (nrow(setB) == 0) {
        stop("No valid coordinates found in setB after filtering NA values")
    }

    dist_matrix <- st_distance(setB, setA)

    # Initialize a list to store the indices of the n closest points
    closest_indices_list <- vector("list", length = nrow(setB))

    # Loop through each point in setB
    for (i in 1:nrow(setB)) {
        # Sort distances for the current point and get indices of n closest points
        sorted_indices <- order(dist_matrix[i, ])
        closest_indices <- sorted_indices[1:min(n, length(sorted_indices))]
        closest_indices_list[[i]] <- closest_indices
    }

    # Retrieve the actual points from setA
    closest_points_list <- lapply(closest_indices_list, function(indices) setA[indices, ])

    # Create a safe version of find_closest_osrm_point
    safe_find_closest <- purrr::safely(find_closest_osrm_point)
    
    # for each setB item from closest_points_list, find the closest osrm point
    result <- setB %>%
        mutate(closest_points = closest_points_list) %>%
        mutate(
            latitude = setB_coords[, 2],
            longitude = setB_coords[, 1]
        ) %>%
        st_drop_geometry()
    
    # Process each row safely
    results <- pmap(
        list(
            longitude = result$longitude,
            latitude = result$latitude,
            setA = result$closest_points
        ),
        safe_find_closest
    )
    
    # Split results and errors
    successful <- map_lgl(results, ~!is.null(.$result))
    
    if (!all(successful)) {
        n_errors <- sum(!successful)
        warning(sprintf("OSRM errors occurred for %d points", n_errors))
    }
    
    # Add results back to the dataframe
    result %>%
        mutate(closest_point = map(results, "result")) %>%
        filter(!map_lgl(closest_point, is.null)) %>%
        unnest(closest_point)
}
