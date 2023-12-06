library(nngeo)
	victoria_defib_cleaned %>%
		head(1755) %>% 
		tail(10)  %>%
  	mutate( isochrone =map2( latitude, longitude, point2isochrone ) )   %>%
		{.} -> a



select( isochrone ) %>%

		mutate( isochrone = map( isochrone, st_make_valid ))  %>%
		mutate( sua_area = map_dbl( isochrone, ~st_area(.x) %>% units::drop_units() )) 


mesh_in_victoria %>%
filter( !st_is_empty( centroid)) %>%
		st_drop_geometry()  %>%
		select( mb_code_2021, centroid) %>%
		{.} -> a

		victoria_defib_sua_no_holes_grouped %>%
		select( latitude, longitude )

p <- st_as_sf(victoria_defib_sua_no_holes_grouped, coords = c("longitude", "latitude"), crs = 4326)


a %>% filter( st_is_empty( centroid)) %>%
	inner_join( mesh_in_victoria, by = c('mb_code_2021' = 'mb_code_2021')) %>%
	select( geometry, mb_code_2021) %>%
	{.} -> b
	{.} -> a



nn <- st_nn(p, a$centroid, k = 20, progress = TRUE)

victoria_defib_sua_no_holes_grouped$nn <- nn


victoria_defib_sua_no_holes_grouped %>%
	mutate( nn = map( nn, ~a$mb_code_2021[.x] )) %>%
	{.} -> a



victoria_defib_sua_no_holes_grouped %>%
	head(1) %>%
summarise_one_sua_set()
