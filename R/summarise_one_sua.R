summarise_one_sua <- function(nn, isochrone, mesh_detail_final) {

    #
    nn %>%
        inner_join(mesh_detail_final, by = join_by(mb_code_2021)) %>%
        mutate(area = st_area(.)) %>%
        st_intersection(isochrone) %>%
        mutate(overlap = (st_area(.))) %>%
        st_drop_geometry() %>%
        mutate(proportion = overlap / area) %>%
        mutate( across( 
            c( ends_with( '_pop'), dwelling, person ),
            ~ proportion * .x
        )) %>%
        # population weighted seifa, divide out down below
        mutate(seifa_score = seifa_score * person) %>%
        { .  } -> rv

    # which decile has the highst populaiton, amoung the overlaps
    rv %>%
        count(seifa_decile, wt = person, name = "population") %>%
        slice_max(population, n = 1, with_ties = FALSE) %>%
        pull(seifa_decile) %>%
        { .  } -> most_populous_decile

    if (length(most_populous_decile) == 0) {
        most_populous_decile <- -1
    }


	rv %>%
		# select(ends_with("_pop"), dwelling, person, overlap, everything()) %>%
		# following line is implicit 
		# because there should be only one sua under consideration here, 
		# returning a single row of data
		# group_by( mb_code_2021, sa1_code_2021 ) %>%
		summarise(
			n_mesh_intersections = n(),
			weighted_seifa_score = sum(seifa_score) / sum(person),
			mutate(across(
				c(ends_with("_pop"), dwelling, person, overlap),
				sum,
				na.rm = TRUE
			)),
			.groups = "drop",
		) %>%
		# select(ends_with("_pop"), dwelling, person, overlap, everything()) %>%
		mutate(most_populous_decile = most_populous_decile) %>%
		mutate(across(
			ends_with("_pop"),
			.fns = list(prop = ~ .x / person)
		)) %>%
		as_tibble()

}
