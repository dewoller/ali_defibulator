# anti-join df1 against df2 within distance
antijoin_within_distance = function( df1, df2, limit_distance=400 ) {

	assertthat::assert_that( 'latitude' %in% names(df1) )
	assertthat::assert_that( 'longitude' %in% names(df1) )
	assertthat::assert_that( 'latitude' %in% names(df2) )
	assertthat::assert_that( 'longitude' %in% names(df2) )

	wdf1 = df1 %>%
		mutate(.id = row_number()) 

	wdf2 = df2 %>%
		select( latitude, longitude) %>%
		mutate(.id = row_number()) 



	# Create all combinations of id
	tidyr::expand_grid(id1 = wdf1$.id, id2 = wdf2$.id) %>%
		inner_join( wdf1, c( "id1" = ".id")) %>%
		inner_join( wdf2, c( "id2" = ".id")) %>%
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


	duplicates = final %>% filter( distance < limit_distance ) 

	wdf1 %>%
		filter( .id %ni% duplicates$id1 )  %>%
		select( - .id)

}

