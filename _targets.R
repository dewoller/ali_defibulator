
targets::tar_source()
library(targets)
library(tarchetypes)





# setup the defib urls
vic_defib_urls <-
    readRDS("data/postcode2021.rds") %>%
    mutate(url = glue::glue("
		https://registermyaed.ambulance.vic.gov.au/find-an-aed?lat={cent_lat}&lng={cent_long}&place={postcode_2021}&availability=all")) %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    select(postcode_2021, url, cent_long, cent_lat)

victoria_defib_targets <- tar_map(
    values = vic_defib_urls,
    names = "postcode_2021",
    tar_target(victoria_defib_file, get_victoria_defib_file(url, postcode_2021)),
    tar_target(victoria_defib, get_victoria_defib_ll(victoria_defib_file))
)


tar_plan(

################################################################################
# reference datasets setup

    sa1_2021 = strayr::read_absmap("sa12021"),

    #
    standard_crs = sa1_2021 %>% st_crs(),

    #
    suburb_2021 = strayr::read_absmap("suburb2021") %>% janitor::clean_names(),

    #
    reservoir_sf =
        suburb_2021 %>%
            filter(str_detect(suburb_name_2021, "Reservoir\\s*\\(Vic.\\)")) %>%
            select(starts_with("suburb")),

    #
    reservoir_buffered_sf = reservoir_sf %>% st_buffer(400),

    #

################################################################################
# all the URLS
    victoria_defib_targets, 
    #
    tar_combine(
        victoria_defib_cleaned,
        victoria_defib_targets[[2]],
        command =
            dplyr::bind_rows(!!!.x) %>%
                distinct(lat, lon, .keep_all = TRUE) %>%
                rename(latitude = lat, longitude = lon)
    )

    ,


################################################################################
# Meshblock setup

mb_all  = 
excel_sheets('~/code/reference_datasets/census_2021/Mesh Block Counts, 2021.xlsx') %>%
	str_subset('Table') %>%
	tibble('sheet' = .) %>%
	mutate( mb_data  = map( sheet, ~ read_excel('~/code/reference_datasets/census_2021/Mesh Block Counts, 2021.xlsx', skip=6, sheet = .x) %>% janitor::clean_names() )) %>%
	unnest( mb_data ) %>%
	select( -sheet) 

   ,

	#
	mesh_2021_vic_sf =

        sf::read_sf("~/code/reference_datasets/census_2021/MB_2021_AUST_GDA2020.shp") %>%
            janitor::clean_names() %>%
            filter(ste_code21 == "2") %>%
            select(
                mb_code_2021 = mb_code21,
                sa1_code_2021 = sa1_code21,
                sa2_code_2021 = sa2_code21,
                sa3_code_2021 = sa3_code21,
                sa4_code_2021 = sa4_code21,
                gcc_code_2021 = gcc_code21,
                ste_code_2021 = ste_code21,
                ste_name_2021 = ste_name21,
                gcc_name_2021 = gcc_name21,
                sa4_name_2021 = sa4_name21,
                sa3_name_2021 = sa3_name21,
                sa2_name_2021 = sa2_name21,
                area_sqkm = areasqkm21
            ) %>%
            st_transform(standard_crs),
 

    # mesh_in_reservoir =
    #     mesh_2021_vic_sf %>%
    #         st_join(reservoir_sf, join = st_intersects, left = FALSE) %>%
    #         mutate(centroid = st_centroid(geometry)) %>%
    #         inner_join(mb_all, by = join_by(mb_code_2021)) %>%
    #         inner_join(sa1_2021_seifa_final, by = join_by(sa1_code_2021)),

#
    mesh_2021_vic_centroid_sf =

        mesh_2021_vic_sf %>%
            filter(ste_code_2021 == "2") %>%
            mutate(centroid = st_centroid(geometry)) 
            ,

 mesh2sa1 =
     mesh_2021_vic_sf %>%
         st_drop_geometry() %>%
         select(mb_code_2021, sa1_code_2021) %>%
         tidylog::inner_join(mb_all, by = join_by(mb_code_2021)) %>%
         select(mb_code_2021, sa1_code_2021, person, dwelling, area_albers_sqkm) %>%
         group_by(sa1_code_2021) %>%
         mutate(sa1_population = sum(person)) %>%
         mutate(mesh_pop_sa1_proportion = person / sa1_population) %>%
         ungroup() 
	,

# at the mesh level, add in the cald / age 55+ data
# at the mesh level, add in the vacar summary data
mesh_detail_final = 
    mesh2sa1 %>%
    select( -sa1_population) %>%
    tidylog::inner_join( cald_mesh, by = join_by(mb_code_2021)) %>%
    tidylog::inner_join( age_55_plus_mesh, by = join_by(mb_code_2021))  %>%
    tidylog::left_join( 
        count( vacar_mesh_detail, mb_code_2021, name='n_vacar_arrest') , 
        by = join_by(mb_code_2021)) %>%
    tidylog::inner_join(sa1_2021_seifa_final, by = join_by(sa1_code_2021, mb_code_2021))

,

mesh_export = 
mesh_detail_final %>%
tidylog::inner_join(mesh_distance_to_nearest_defib, by = join_by(mb_code_2021)) %>%
tidylog::inner_join(mesh_distance_to_nearest_defib_no_sja, by = join_by(mb_code_2021)) %>%
tidylog::inner_join(count( vacar_mesh_detail, mb_code_2021, name='n_vacar_in_mesh'), by = join_by(mb_code_2021)) 

,

################################################################################
# VACAR setup

    vacar_sf =
            read_csv("data/VACAR_Data_2019_2023.csv") %>%
            clean_names() %>%
            rename_with(~ paste0("va_", .), everything()) %>%
            mutate( va_internal_id = row_number()) %>%
            mutate( va_date = dmy(va_date))  %>%
            st_as_sf(coords = c("va_xllwgs84", "va_yllwgs84"), crs = standard_crs)  

        ,

    vacar_mesh_detail = 
            mesh_2021_vic_sf %>%
                st_join(vacar_sf, join = st_intersects, left = FALSE) %>%
                st_drop_geometry() %>%
                select( mb_code_2021, starts_with('va_')) 
 
,

    vacar_sua = 
            sua_noh_sf %>%
                st_join(vacar_sf, join = st_intersects, left = FALSE) %>%
                st_drop_geometry() %>%
                select( sua_id, starts_with('va_')) %>%
                as_tibble()

,

vacar_export = vacar_sf %>%
    st_drop_geometry() %>%
    tidylog::inner_join(vacar_distance_to_nearest_defib, by = join_by(va_internal_id)) %>%
    tidylog::inner_join(vacar_distance_to_nearest_defib_no_sja, by = join_by(va_internal_id)) %>%
    tidylog::inner_join(select(vacar_mesh_detail, mb_code_2021, va_internal_id), by = join_by(va_internal_id)) %>%
    tidylog::inner_join(mesh_export, by = join_by(mb_code_2021)) %>%
    tidylog::left_join(count(vacar_sua, va_internal_id, name = "n_within_sua"), by = join_by(va_internal_id)) 

,
#     vacar_nearest_defib_index = st_nearest_feature( vacar_sf, victoria_defib_cleaned_sf)
#     ,
#     vacar_nearest_non_sja_defib_index = st_nearest_feature( vacar_sf, victoria_defib_no_sja_sf),
# ,

################################################################################
# find the closest defib to each VACAR and meshblock


vacar_distance_to_nearest_defib =
    find_closest_osrm_points_closest_n(vacar_sf, victoria_defib_cleaned_sf, n = 10) %>%
    select(va_internal_id, distance2defib = distance, duration2defib = duration)

,

vacar_distance_to_nearest_defib_no_sja =
    find_closest_osrm_points_closest_n(vacar_sf, victoria_defib_no_sja_sf, n = 10) %>%
    select(va_internal_id, distance2defib_no_sja = distance, duration2defib_no_sja = duration)

,

mesh_distance_to_nearest_defib =

    mesh_2021_vic_centroid_sf %>%
    find_closest_osrm_points_closest_n(., victoria_defib_cleaned_sf, n = 10, st_coordinates(.$centroid)) %>%
    select(mb_code_2021, distance2defib = distance, duration2defib = duration)

,

mesh_distance_to_nearest_defib_no_sja =
    mesh_2021_vic_centroid_sf %>%
    find_closest_osrm_points_closest_n(., victoria_defib_no_sja_sf, n = 10, st_coordinates(.$centroid)) %>%
    select(mb_code_2021, distance2defib_no_sja = distance, duration2defib_no_sja = duration)

,
################################################################################
# Defib setup


            #
    victoria_defib_cleaned_sf =
        victoria_defib_cleaned %>%
            mutate( sua_id = row_number()) %>% 
            mutate( is_sja_defib = str_detect( str_to_upper( company), 'DEFIB IN'))  %>%
            st_as_sf(coords = c("longitude", "latitude"), crs = standard_crs) ,

    victoria_defib_no_sja_sf = victoria_defib_cleaned_sf %>% filter( !is_sja_defib),

    victoria_defib_sua =
        victoria_defib_cleaned %>%
            mutate(isochrone = map2(latitude, longitude, point2isochrone)) %>%
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
            filter(type == "sf") %>%
            select(-type) %>%
            mutate(isochrone = map(isochrone, st_make_valid)) %>%
            mutate( sua_id = row_number()) %>% 
            mutate( sua_area = map_dbl(isochrone, ~ st_area(.x) %>% units::drop_units())) %>%
            mutate( is_sja_defib = str_detect( str_to_upper( company), 'DEFIB IN')) 
            ,

    sua_noh = 
        victoria_defib_sua %>%
            mutate(isochrone = map(isochrone, nngeo::st_remove_holes)),

    sua_noh_sf = 
        sua_noh %>%
        st_singles_to_multi()

,
    #  which defib points are in reservoir buffered area
    reservoir_defib_sf =
        victoria_defib_cleaned_sf %>%
            st_intersection(reservoir_buffered_sf) %>%
            select(-suburb_name_2021, -suburb_code_2021),


    # which are SUA in reservoir
    reservoir_sua_sf =
        sua_noh_sf %>%
        inner_join(
            reservoir_defib_sf %>%
                st_drop_geometry() %>%
                select(sua_id),
            by = join_by(sua_id)
        )

,

    ################################################################################
    # find intersection of Meshblock and SUA
    #
    sua_noh_mesh =
        sua_noh %>%
        find_n_closest_mb(mesh_2021_vic_centroid_sf, n = 100)

,

    # find the total CALD, total 55+, total 65+, seifa, dwellings and total population for each SUA
    sua_holes_mesh_summarised =
    sua_noh_mesh %>%
        mutate(details = map2(nn, isochrone, summarise_one_sua, mesh_detail_final)) %>%
        unnest_wider(details)

    ,

sua_noh_mesh_export =
    sua_holes_mesh_summarised %>%
    select( -starts_with('nn'), -isochrone, -starts_with('.group')) %>%
    tidylog::left_join(count(vacar_sua, sua_id, name = "n_vacar_arrest"), by = join_by(sua_id))

,


    ################################################################################
    # Census 2021
    census_cald_columns =
        read_xlsx("../../reference_datasets/Metadata_2021_GCP_DataPack_R1_R2.xlsx", sheet = "Cell Descriptors Information") %>%
            clean_names() %>%
            filter(str_detect(data_packfile, "G11")) %>%
            filter(long %in% c(
                "TOTAL_Uses_other_language_and_speaks_English_Total_Total"
                # , "TOTAL_Uses_other_language_and_speaks_English_Not_well_or_not_at_all_Total"
                # , "TOTAL_Uses_other_language_and_speaks_English_Proficiency_in_English_not_stated_Total"
            )) %>%
            pull(short)

    ,

    cald_sa1 = 

	read_csv("../../reference_datasets/2021_Census_GCP_Statistical_Area_1_for_VIC/2021Census_G11D_VIC_SA1.csv") %>%
		select(SA1_CODE_2021, all_of(census_cald_columns)) %>%
		mutate(SA1_CODE_2021 = as.character(SA1_CODE_2021)) %>%
		mutate(cald = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
		select(-all_of(census_cald_columns)) %>%
        clean_names()

,

census_age_55_plus_columns =

    read_xlsx("../../reference_datasets/Metadata_2021_GCP_DataPack_R1_R2.xlsx", sheet = "Cell Descriptors Information") %>%
		clean_names() %>%
		filter(str_detect(data_packfile, "G01")) %>%
		filter(str_detect(long, "^Age[_ ]groups[_ ][5678]5.*Persons$")) %>%
        pull(short)

	,

age_55_plus_sa1 =
    read_csv("../../reference_datasets/2021_Census_GCP_Statistical_Area_1_for_VIC/2021Census_G01_VIC_SA1.csv") %>%
		select(SA1_CODE_2021, all_of(census_age_55_plus_columns)) %>%
		mutate(SA1_CODE_2021 = as.character(SA1_CODE_2021)) %>%
		mutate(age_55_plus= rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
		mutate(age_65_plus= age_55_plus - Age_55_64_yr_P ) %>%
		select(-all_of(census_age_55_plus_columns)) %>%
        clean_names()

	,

age_55_plus_mesh = 
    age_55_plus_sa1 %>%
        inner_join(mesh2sa1, by = join_by(sa1_code_2021)) %>%
        mutate(
            across(
                starts_with("age_"), 
                .fns = list( 
                    pop = ~ .x * mesh_pop_sa1_proportion,
                    prop = ~ .x * mesh_pop_sa1_proportion / person
        ))) %>%
        select(-ends_with("_plus")) %>%
        select(mb_code_2021, starts_with("age_"))
,

cald_mesh= 
    cald_sa1 %>%
        inner_join(mesh2sa1, by = join_by(sa1_code_2021)) %>%
        mutate(
            across(
                starts_with("cald"), 
                .fns = list( 
                    pop= ~ .x * mesh_pop_sa1_proportion,
                    prop = ~ .x * mesh_pop_sa1_proportion / person
        ))) %>%
        select(-ends_with("cald")) %>%
        select(mb_code_2021, starts_with("cald"))

,



################################################################################
    # 	# sa1 level seifa estimation
    #
    sa1_seifa_decile =

        read_xlsx("~/code/reference_datasets/census_2021/Statistical Area Level 1, Indexes, SEIFA 2021.xlsx", sheet = "Table 2", skip = 5) %>%
            janitor::clean_names() %>%
            mutate(sa1_code_2021 = as.character(x2021_statistical_area_level_1_sa1)) %>%
            rename(decile = decile_6) %>%
            rename(population = usual_resident_population) %>%
            select(sa1_code_2021, decile, score, population),

    #
    sa2_seifa_decile =

        read_xlsx("~/code/reference_datasets/census_2021/Statistical Area Level 2, Indexes, SEIFA 2021.xlsx", sheet = "Table 2", skip = 5) %>%
            janitor::clean_names() %>%
            mutate(sa2_code_2021 = as.character(x2021_statistical_area_level_2_sa2_9_digit_code)) %>%
            rename(decile = decile_7) %>%
            rename(population = usual_resident_population) %>%
            select(sa2_code_2021, decile, score, population),
    #
    # fill in the missing seifa sa1 decile and score with sa2 values
    sa1_2021_seifa_final =

        sa1_2021 %>%
            st_drop_geometry() %>%
            left_join(sa1_seifa_decile, by = "sa1_code_2021") %>%
            left_join(sa2_seifa_decile, by = c("sa2_code_2021" = "sa2_code_2021")) %>%
            select(sa1_code_2021, decile.x, score.x, decile.y, score.y) %>%
            mutate(decile = ifelse(is.na(decile.x), decile.y, decile.x)) %>%
            mutate(score = ifelse(is.na(score.x), score.y, score.x)) %>%
            select(sa1_code_2021, seifa_decile = decile, seifa_score = score) %>%
            as_tibble() %>%
            tidylog::inner_join( select( mesh2sa1, ends_with('2021')), by = join_by(sa1_code_2021)) 

,


   #
   #
    #
    	# xls	output
    	export_sja =
    	list(
    		vacar= vacar_export,
    		mesh= mesh_export,
    		sua_noh = sua_noh_mesh_export
    		) %>%
    		openxlsx::write.xlsx('output/victoria_export.xlsx')

   ,


all_json_output =
    list(
        "reservoir_sf" = reservoir_sf,
        "reservoir_buffered_sf" = reservoir_buffered_sf,
        "mesh_2021_vic_sf" = mesh_2021_vic_sf %>%
            inner_join(mesh_export, by = join_by(mb_code_2021)),
        "mesh_2021_vic_centroid_sf" = mesh_2021_vic_centroid_sf %>%
            inner_join(mesh_export, by = join_by(mb_code_2021)),
        "vacar_sf" = vacar_sf %>%
            inner_join(vacar_export, by = join_by(va_internal_id)),
        "victoria_defib_cleaned_sf" = victoria_defib_cleaned_sf %>%
            inner_join(sua_noh_mesh_export, by = join_by(sua_id)),
        "victoria_defib_no_sja_sf" = victoria_defib_no_sja_sf %>%
            inner_join(sua_noh_mesh_export, by = join_by(sua_id)),
        "sua_noh_sf" = sua_noh_sf %>%
            inner_join(sua_noh_mesh_export, by = join_by(sua_id)),
        "reservoir_defib_sf" = reservoir_defib_sf %>%
            inner_join(sua_noh_mesh_export, by = join_by(sua_id)),
        "reservoir_sua_sf" = reservoir_sua_sf %>%
            inner_join(sua_noh_mesh_export, by = join_by(sua_id))
    ) %>%
    purrr::walk2(names(.), ., ~ st_write(.y, glue::glue("output/{.x}.geojson"), delete_dsn = TRUE))



    #
    # 	,
    #
    #
    #
    #
    #
)
