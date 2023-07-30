point2lat_long = function( df, column='point' ) {

	df %>%
		pluck(column) %>%
		st_coordinates() %>% 
		tibble::as_tibble()  %>%
		set_names( c('longitude', 'latitude')) 

}
