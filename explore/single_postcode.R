
readRDS('data/postcode2021.rds') %>%
	mutate( url = glue::glue("
		https://registermyaed.ambulance.vic.gov.au/find-an-aed?lat={cent_lat}&lng={cent_long}&place={postcode_2021}&availability=all"
	)) %>%
	st_drop_geometry() %>%
	as_tibble() %>%
	select( postcode_2021, url , cent_long, cent_lat )  %>%
	filter( postcode_2021 == '3036') %>%
	pull(url ) %>%
	get_victoria_defib_ll()



readRDS('data/postcode2021.rds') %>%
	mutate( url = glue::glue("
		https://registermyaed.ambulance.vic.gov.au/find-an-aed?lat={cent_lat}&lng={cent_long}&place={postcode_2021}&availability=all"
	)) %>%
	st_drop_geometry() %>%
	as_tibble() %>%
	select( postcode_2021, url , cent_long, cent_lat )  %>%
	filter( postcode_2021 == '3036') %>%
	pull(url ) %>% clipr::write_clip()
