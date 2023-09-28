strip_duplicate_points = function( df, tolerance=50 ) {

df = df %>%
  mutate(id = row_number()) 

	working_df = df
	save_crs = NULL
if ('sf' %in%	class( working_df )) {
		save_crs = st_crs( working_df )

		working_df %>%
			mutate(
				longitude = st_coordinates(geometry)[, 1],
				latitude = st_coordinates(geometry)[, 2]
			) %>%
			st_drop_geometry()  %>%
			{.} -> working_df
	}

assertthat::assert_that( 'latitude' %in% names(working_df) )
assertthat::assert_that( 'longitude' %in% names(working_df) )


# Add an id to each point
working_df <- working_df %>%
		select( latitude, longitude, id ) 

	# Create all combinations of id
	tidyr::expand_grid(id1 = working_df$id, id2 = working_df$id) %>%
		filter(id1 < id2) %>%
		merge( working_df, by.x = "id1", by.y = "id") %>%
		merge(working_df, by.x = "id2", by.y = "id") %>%
		rename(
			longitude1 = longitude.x,
			latitude1 = latitude.x,
			longitude2 = longitude.y,
			latitude2 = latitude.y) %>%
		as_tibble()  %>%
		mutate( distance = geosphere::distVincentySphere( 
			cbind(longitude1, latitude1), 
			cbind(longitude2, latitude2) ) 
		)  %>%
	{.} -> final


	duplicates = final %>% filter( distance < tolerance) 
	df = filter( df, id %ni% duplicates$id1 )  %>%
		select( -id )

	if ( !is.null( save_crs ) ) {
		df = df %>%
			st_as_sf( crs = save_crs ) %>%
		{.} -> df
	}

	df
}

