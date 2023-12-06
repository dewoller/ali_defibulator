if( FALSE) {

sua = victoria_defib_sua_reservoir_no_holes

canonical_ll  = victoria_defib_cleaned 

}

summarise_one_sua_set = function( intersected_sua ) {

	intersected_sua %>%
		group_by( latitude, longitude) %>%
		summarise( population_weighted_seifa = weighted.mean( seifa_score, person), .groups='drop')  %>%
		{.} -> point_seifa
	

	intersected_sua %>% 
		count( latitude, longitude,  seifa_decile, wt=person) %>%
		group_by( latitude, longitude ) %>%
		slice_max( n, with_ties=FALSE) %>%
		rename( most_populous_seifa_decile = seifa_decile)  %>%
		select( -n) %>%
		{.} -> point_decile

	intersected_sua %>% 
		count( latitude, longitude,  mb_category_name_2021, wt=area_covered) %>%
		group_by( latitude, longitude) %>%
		slice_max( n, with_ties=FALSE) %>%
		rename( largest_mb_overlap_category = mb_category_name_2021)  %>%
		select( -n) %>%
		{.} -> point_mb_category

	intersected_sua %>%
		group_by( latitude, longitude, sua_area ) %>%
		summarise( across( c( 'person', 'dwelling', 'area_covered'), sum ),
			.groups = 'drop') %>%
		rename( person_covered = person,
			dwelling_covered = dwelling) %>%
		inner_join(  point_mb_category, by = join_by(latitude, longitude)) %>%
		inner_join(  point_seifa, by = join_by(latitude, longitude)) %>%
		inner_join(  point_decile, by = join_by(latitude, longitude)) %>%
		rename( sua_area_sqkm = sua_area) 

}
