summarise_one_sua_set = function( sua, mesh_in_reza ){
	sua %>% 
		select( isochrone) %>% 
		unlist(recursive = FALSE) %>% 
		do.call(rbind, .) %>%
		bind_cols( sua %>% select( -isochrone)) %>%
		st_intersection( mesh_in_reza ) %>%
		mutate( sua_area = sua_area / 1e6 ) %>%
		mutate( area_covered = st_area( geometry )  %>%
			as.numeric() / 1e6 ) %>%
		st_drop_geometry() %>%
		mutate( prop_covered = area_covered / area_sqkm  ) %>%
		as_tibble() %>%
		mutate( across( c( 'person', 'dwelling'), ~prop_covered * .x )) %>%
		select(-centroid)
	# %>%
	# 	group_by( street, mb_category_name_2021, sua_area, seifa_score, seifa_decile) %>%
	# 	summarise( across( c( 'person', 'dwelling', 'area_covered', 'area_sqkm'), sum ),
	# 		.groups = 'drop') %>%
	# 	mutate( category_coverage_proportion = area_covered / sua_area ) 
	#

}
