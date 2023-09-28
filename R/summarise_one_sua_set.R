summarise_one_sua_set = function( sua, mesh_in_reservoir ){

	sua %>%
		st_singles_to_multi() %>%
		st_intersection( mesh_in_reservoir )  %>%
	{.} -> mesh_in_reservoir_intersect_sua

mesh_in_reservoir_intersect_sua %>%
		mutate( sua_area = sua_area / 1e6 ) %>%
		mutate( area_covered = st_area( geometry )  %>%
			as.numeric() / 1e6 ) %>%
		st_drop_geometry() %>%
		mutate( prop_covered = area_covered / area_sqkm  ) %>%
		as_tibble()  %>%
		mutate( across( c( 'person', 'dwelling'), ~prop_covered * .x )) %>%
		select(-centroid) %>%
		{.} -> all_data

	all_data %>%
		group_by( street,  sua_area) %>%
		summarise( population_weighted_seifa = weighted.mean( seifa_score, person), .groups='drop')  %>%
		{.} -> point_seifa
	

	all_data %>% 
		count( street, sua_area, seifa_decile, wt=person) %>%
		group_by( street, sua_area) %>%
		filter( n == max(n)) %>%
		rename( most_populous_seifa_decile = seifa_decile)  %>%
		select( -n) %>%
		{.} -> point_decile

	all_data %>% 
		count( street, sua_area, mb_category_name_2021, wt=area_covered) %>%
		group_by( street, sua_area) %>%
		filter( n == max(n)) %>%
		rename( largest_mb_category = mb_category_name_2021)  %>%
		select( -n) %>%
		{.} -> point_mb_category

	all_data %>%
		group_by( street,  sua_area) %>%
		summarise( across( c( 'person', 'dwelling', 'area_covered'), sum ),
			.groups = 'drop') %>%
		rename( person_covered = person,
			dwelling_covered = dwelling) %>%
		inner_join(  point_mb_category, by = c('street', 'sua_area')) %>%
		inner_join(  point_seifa, by = c('street', 'sua_area')) %>%
		inner_join(  point_decile, by = c('street', 'sua_area')) %>%
		rename( sua_area_sqkm = sua_area) 

}
