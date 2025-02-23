
# Sample points
points_df <- data.frame(
  x = c(1, 3, 2.5),
  y = c(4, 2, 3.5)
)
points_sf <- st_as_sf(points_df, coords = c("x", "y"), crs = 4326)  # Convert to sf object

# Sample polygon
coords <- matrix(c(0, 0, 3, 0, 3, 3, 0, 3, 0, 0), ncol = 2, byrow = TRUE)
poly <- st_polygon(list(coords)) 
poly_sf <- st_sfc(poly, crs = 4326)  # Convert to sf object



victoria_defib_cleaned


victoria_defib_cleaned_sf %>%
	filter( is_sja_defib )  %>%
	{.} -> a


victoria_defib_cleaned_sf %>%
	# st_drop_geometry() %>%
	filter( is_sja_defib )  %>%
  mutate(postcode = str_extract(address, "\\d{4}"))  %>%
  filter( postcode == "3073" )  %>%
	{.} -> a


victoria_defib_cleaned_sf %>%
	# st_drop_geometry() %>%
	filter( str_detect( address, 'Raml') )  %>%
	{.} -> b

ggplot() +
  geom_sf(data = reservoir_buffered_sf, fill = "lightblue") +  # Plot polygon
  geom_sf(data = reservoir_sf, fill = "pink") +  # Plot polygon
  geom_sf(data = a ) +                    # Plot points
  geom_sf(data = b, color='red' ) +                    # Plot points
  theme_minimal() 



reservoir_defib_sf %>%
	arrange( desc( sua_id))



reservoir_sua_sf %>%
	arrange( desc( sua_id))



sua_holes_mesh_summarised %>%
	arrange( desc( sua_id))


sua_noh_mesh %>%
	arrange( desc( sua_id))

sua_noh %>%
	arrange( desc( sua_id))


sua_noh_sf %>%
	arrange( desc( sua_id))



victoria_defib_sua %>%
	arrange( desc( sua_id))


victoria_defib_sua_temp %>%
	head(1) %>% dput()


victoria_defib_sua %>%
	arrange( desc( sua_id))


victoria_defib_sua = 
	victoria_defib_sua_temp %>%
		mutate( sua_id = row_number()) %>% 
		mutate(type = map_chr(isochrone, ~ {
			cl <- class(.x)
			if ("sf" %in% cl) {
				return("sf")
			} else if ("POLYGON" %in% cl || "sfg" %in% cl) {
				return("polygon")
			} else {
				return("unknown")
			}
		})) %>%
		filter(type == "sf") 

%>%
		select(-type) %>%
		mutate(isochrone = map(isochrone, st_make_valid)) %>%
		mutate( sua_area = map_dbl(isochrone, ~ st_area(.x) %>% units::drop_units())) %>%
	,




