vacar_distance_to_nearest_defib_no_sja



    st_coordinates( vacar_sf )




            read_csv("data/VACAR_Data_2019_2023.csv") %>%
                clean_names() %>%
                filter(xllwgs84 == 0 ) %>%
                pull( date ) %>% clipr::write_clip()


                rename_with(~ paste0("va_", .), everything()) %>%
                mutate(va_internal_id = row_number()) %>%
                mutate(va_date = dmy(va_date)) %>%
                st_as_sf(coords = c("va_xllwgs84", "va_yllwgs84"), crs = standard_crs)


sua_holes_mesh_summarised %>%
select( ends_with( 'pop'))


sua_noh_mesh %>%
    head(2) %>%
    tail(1) %>%
    mutate(details = map2(nn, isochrone, summarise_one_sua, mesh_detail_final)) %>%
    unnest_wider(details) %>%
select( ends_with( 'pop'))


unnest( nn_id ) %>%


select( sua_id, nn_id) %>%




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
        select( ends_with('_pop'), everything()) %>%
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
        select(ends_with("_pop"), dwelling, person, overlap, everything()) %>%
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
        select(ends_with("_pop"), dwelling, person, overlap, everything()) %>%
        mutate(most_populous_decile = most_populous_decile) %>%
        mutate(across(
            ends_with("_pop"),
            .fns = list(prop = ~ .x / person)
        )) %>%
        as_tibble()

}
