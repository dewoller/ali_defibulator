
get_victoria_defib_ll = function( url ) {

	rvest::read_html(url)%>% 
		rvest::html_text() %>% 
		str_remove_all( "data:[^\\s]*")  %>%
		str_extract_all( "window.AVAEDAPP.public_results = \\[.*\\]") %>% .[[1]]  %>%
		str_extract_all( '"latitude":[^,]*,"longitude":[^,]*,') %>% .[[1]]  %>%
		str_remove_all( '"latitude":|"longitude":|,$|"') %>%
		str_split( "," ) %>%
		unlist() %>%
		matrix( ncol = 2, byrow = TRUE) %>%
		as_tibble() %>%
		set_names( "latitude", "longitude")

}

