if(FALSE) {

	meshes = mesh_in_victoria
	sua = victoria_defib_sua_no_holes_grouped

}
find_n_closest_mb = function( sua, meshes, n=20 ) {

	meshes %>%
		filter( !st_is_empty( centroid)) %>%
		st_drop_geometry()  %>%
		select( mb_code_2021, centroid) %>%
		{.} -> mc


	sua_sf <- st_as_sf(sua, coords = c("longitude", "latitude"), crs = 4326)


	sua_sf %>%
		nngeo::st_nn( mc$centroid, k = n, progress = TRUE)  %>%
	{.} -> nn



	sua %>%
		mutate( nn_id = nn ) %>%
		mutate( nn = map( nn, ~meshes %>% filter( mb_code_2021 %in% mc[.x,]$mb_code_2021 )))   %>%
	{.} -> rv


	rv


}
