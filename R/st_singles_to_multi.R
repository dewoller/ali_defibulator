st_singles_to_multi = function( sua )  {
	sua %>% 
		select( isochrone) %>% 
		unlist(recursive = FALSE) %>% 
		do.call(rbind, .) %>%
		bind_cols( sua %>% select( -isochrone)) 

}
	
