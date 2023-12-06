victoria_defib %>%
	count( lat, lon, company, sort=TRUE )  %>%
	mutate( across( c( 'lat','lon'), as.numeric)) %>%
	{.} -> df


library(leaflet)


victori_defib_sua %>%
	pull( isochrone ) %>%


	a %>% pull(sua_area)


a=
victoria_defib_cleaned %>%
	tail( n=-1745L) %>%
	head( 5  ) %>%
	mutate( isochrone =map2( latitude, longitude, point2isochrone ) ) %>%
		mutate( isochrone = map( isochrone, st_make_valid ))  %>%
		utate( sua_area = map_dbl( isochrone, ~st_area(.x) %>% units::drop_units() )) 


	head(2) %>%
	pull( isochrone ) %>% str()



tail(letters)
tail(letters, n = -6L)

victoria_defib %>%
	filter( lat_base == lat & lon_base == lon) %>%
	{.} -> victoria_defib_file_3224

victoria_defib %>%
	count( postcode, sort=TRUE ) %>%
	{.} -> vic_defib_postcode_counts

victoria_defib %>%
	filter( postcode == 3550 ) %>%
	select( address )


# Now, from_json contains the closest matching lat/lon points from sja_defib along with the distance


library(geosphere)

sja_defib = 


for (i in 1:nrow(from_json)) {
	closest_idx <- which.min(distHaversine(from_json[i, c("lon", "lat")], sja_defib[, c("longitude", "latitude")]))
	from_json[i, "closest_lat"] <- sja_defib[closest_idx, "latitude"]
	from_json[i, "closest_lon"] <- sja_defib[closest_idx, "longitude"]
}


read_csv( sja_defib_file) %>%
	mutate( address = str_remove_all(address, '^\\s*|,.*|\\(.*|Reservoir\\s*$')) %>%
	mutate( address = str_remove_all(address, '^\\s*|,.*|\\(.*|Reservoir\\s*$')) %>%
	rename( street = address ) %>%
	mutate( full_address = glue::glue("{street}, Reservoir, Victoria, 3073 AUSTRALIA")) %>%
	geocode(full_address, method = 'osm', lat = latitude , long = longitude) %>%
	mutate( type='sja') %>%
	select( -street)


# debugging code

p='3224'
vic_defib_urls %>%
	filter( postcode_2021 == p) %>%
	pull(url) %>%
	get_victoria_defib_file(p)


victoria_defib_file_3224 %>%
	get_victoria_defib_ll()


# mapping

# Step 4: Initialize Map
leaflet() %>%
	addTiles()  %>% 
	addCircleMarkers(
		data=victoria_defib_cleaned_sf %>%
			tail( n=-1746L) %>%
			head( 2  ),
		color = "red",
		radius = 5
)
