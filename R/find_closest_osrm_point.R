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

    dist_matrix <- st_distance(setB, setA)

    # Initialize a list to store the indices of the 10 closest points
    closest_indices_list <- vector("list", length = nrow(setB))

    # Loop through each point in setB
    for (i in 1:nrow(setB)) {
        # Sort distances for the current point and get indices of 10 closest points
        sorted_indices <- order(dist_matrix[i, ])
        closest_indices <- sorted_indices[1:n]
        closest_indices_list[[i]] <- closest_indices
    }

    # Retrieve the actual points from Y
    closest_points_list <- lapply(closest_indices_list, function(indices) setA[indices, ])

    # for each setB item from closest_points_list, find the closest osrm point
    setB %>%
        mutate(closest_points = closest_points_list) %>%
        mutate(
            latitude = setB_coords[, 2],
            longitude = setB_coords[, 1]
        ) %>%
        st_drop_geometry() %>%
        mutate(
            closest_point =
                pmap(
                    list(longitude, latitude, closest_points),
                    ~ find_closest_osrm_point(..1, ..2, setA = ..3)
                )
        ) %>%
        unnest(closest_point)

}

