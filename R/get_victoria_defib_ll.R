################################################################################
get_victoria_defib_file = function( url, postcode ) {


	tempfile = glue::glue('data/postcode/pc_{postcode}.html')

	size = fs::file_info(tempfile) %>% .['size'] %>% as.numeric()
	if (!is.na( size) && size < 19000) fs::file_delete( tempfile )
	
	if (fs::file_exists( tempfile ) )  return( tempfile )

	count=0
	size = fs::file_info(tempfile) %>% .['size'] %>% as.numeric()
	while (is.na( size) | size < 19000) {
		'curl "{url}" | perl -pne \'s/,"photo":"data[^"]*"//g\'> {tempfile}' %>%
			glue::glue() %>%
			system()
		size = fs::file_info(tempfile) %>% .['size'] %>% as.numeric()
		count = count + 1
		if (count > 10) {
			return(NULL)
		}
	}

	tempfile
}

################################################################################

get_victoria_defib_ll = function( infile ) {

	if (is.null( infile)) return(NULL)

	read_file( infile ) %>% 
		str_remove( regex(".*?window.AVAEDAPP.public_results = ", dotall=TRUE)) %>%
		str_split_1('\\n')  %>%
		.[1] %>%
		str_sub( 1,-11) %>%
		fromJSON( )  %>%
	{.} -> json_data
	
	if( length(json_data) == 0) {
		return(NULL)
	}

	lat = 
	json_data$aeds %>% 
		map( ~ .x %>% .$latitude)


	lon = 
	json_data$aeds %>% 
		map( ~ .x %>% .$longitude)

	address = paste(ifelse(!is.na(json_data$unit_no) , paste0(json_data$unit_no, "/"), ""),
		json_data$street_no, ' ',
		json_data$street_name, ' ',
		json_data$street_type, ', ',
		json_data$suburb_name, ' ',
		json_data$post_code, ' ',
		json_data$state, 
		sep = "") %>% str_trim()



	tibble( lat=lat, lon=lon 
		, lat_base = json_data$latitude 
		, lon_base = json_data$longitude 
		, company = json_data$company_name 
		, address = address
		, postcode = json_data$post_code
		) %>%
		unnest(cols = c(lat, lon), keep_empty = TRUE) %>% # unnest simultaneously
		mutate( across( starts_with('l'), as.numeric)) %>%
		mutate(
			lat = ifelse(is.na(lat), lat_base, lat), 
			lon = ifelse(is.na(lon), lon_base, lon)
		)

}
