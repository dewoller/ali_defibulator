

# Load packages required to define the pipeline:
targets::tar_source()
library(targets)
library(tarchetypes)
# library(tarchetypes) # Load other packages as needed. # nolint


singletons = tribble( 
	~street, ~full_address, ~latitude, ~longitude,
	"Reservoir Station", "Reservoir Station, Victoria, 3073 AUSTRALIA", -37.7168,145.0071
)


tar_plan(


	postcode_2021 = strayr::read_absmap("postcode2021") 

	,

	sa1_2021 = strayr::read_absmap("sa12021"), 
	standard_crs = sa1_2021 %>% st_crs() ,

	suburb_2021 = strayr::read_absmap("suburb2021")  %>%
		janitor::clean_names(),

	reservoir = 
	suburb_2021 %>% 
		filter( str_detect( suburb_name_2021 ,'Reservoir\\s*\\(Vic.\\)')) %>%
		select( starts_with('suburb'))

	,

	reservoir_buffered = 
	reservoir %>% 
		st_buffer( 400 ) 


	,

	sja_defib = 
	postcode_2021 %>%
		filter( str_detect( postcode_num_2021, "^30")) %>%
		mutate( url = glue::glue("
			https://registermyaed.ambulance.vic.gov.au/find-an-aed?lat={cent_lat}&lng={cent_long}&place={postcode_2021}"
		)) %>%
		st_drop_geometry() %>%
		mutate( ll = map( url, get_sja_defib_ll )) 
	,



	defib = 
	read_csv( "data/defib_ressie.csv") %>%
		mutate( address = str_remove_all(address, '^\\s*|,.*|\\(.*|Reservoir\\s*$')) %>%
		mutate( address = str_remove_all(address, '^\\s*|,.*|\\(.*|Reservoir\\s*$')) %>%
		rename( street = address ) %>%
		mutate( full_address = glue::glue("{street}, Reservoir, Victoria, 3073 AUSTRALIA")) %>%
		geocode(full_address, method = 'osm', lat = latitude , long = longitude) 

	,
	defib_cleaned = defib %>% 
		drop_na( latitude, longitude ) %>%
		bind_rows( singletons ) 

	,

	defib_sf  = 
	defib_cleaned %>%
		st_as_sf( coords = c("longitude", "latitude"), crs = standard_crs) 

	,

	sja_defib_cleaned_sf  = 
	sja_defib_cleaned %>%
		st_as_sf( coords = c("longitude", "latitude"), crs = standard_crs) 

	,

	sua = 

	defib_cleaned %>%
		mutate( isochrone =map2( latitude, longitude, point2isochrone ) )  %>%
		mutate( isochrone = map( isochrone, st_make_valid ))  %>%
		mutate( sua_area = map_dbl( isochrone, ~st_area(.x) %>% units::drop_units() )) 

	,

	sua_nh = sua %>% mutate(isochrone = map(isochrone, nngeo::st_remove_holes ))
	,


	sua_vic = 
	sja_defib_cleaned %>%
		mutate( isochrone =map2( latitude, longitude, point2isochrone ) )  %>%
		mutate( isochrone = map( isochrone, st_make_valid ))  %>%
		mutate( sua_area = map_dbl( isochrone, ~st_area(.x) %>% units::drop_units() )) 

	,


	sua_vic_reservoir =
	sua_vic %>% mutate( is_in_reservoir = map( isochrone, ~st_intersects( .x, reservoir_buffered)  )) %>%
		unnest( is_in_reservoir ) %>%
		unnest( is_in_reservoir ) 


	,
	sua_nh_vic_reservoir = sua_vic_reservoir %>% mutate(isochrone = map(isochrone, nngeo::st_remove_holes ))
	,


	sja_defib_cleaned  = 

	sja_defib %>%
		unnest( ll ) %>%
		distinct( latitude, longitude ) %>%
		drop_na( latitude, longitude ) %>%
		filter( latitude !=  'null' ) %>%
		mutate( across( everything(), as.numeric )) %>%
		mutate( street = glue::glue( 'latitude: {latitude}, longitude: {longitude}')) %>%
		strip_duplicate_points( tolerance=400)  %>%
		antijoin_within_distance( defib_cleaned, limit_distance = 400 ) 

	,


	sja_defib_reservoir_sf =
	sja_defib_cleaned %>%
		st_as_sf( coords = c("longitude", "latitude"), crs = standard_crs) %>%
		st_intersection( reservoir_buffered ) 

	,

	mesh_in_reservoir = 

	meshblocks_2021_shape %>%
		st_join( reservoir, join = st_intersects, left=FALSE) %>%
		mutate( centroid = st_centroid(geometry))  %>%
		inner_join( mb_all, by=join_by(mb_code_2021)) %>%
		inner_join( sa1_2021_seifa_final, by = join_by(sa1_code_2021) ) 

	,

	mesh_in_victoria = 

	meshblocks_2021_shape %>%
		filter( ste_code_2021 == '2') %>%
		mutate( centroid = st_centroid(geometry))  %>%
		inner_join( mb_all, by=join_by(mb_code_2021)) %>%
		inner_join( sa1_2021_seifa_final, by = join_by(sa1_code_2021) ) 

	,
	sua_mb_summary = summarise_one_sua_set( sua, mesh_in_reservoir ),
	sua_mb_summary_nh = summarise_one_sua_set( sua_nh, mesh_in_reservoir ),

	sua_mb_summary_vic = summarise_one_sua_set( sua_vic_reservoir, mesh_in_reservoir),
	sua_mb_summary_nh_vic = summarise_one_sua_set( sua_nh_vic_reservoir, mesh_in_reservoir ),


	export_reservoir = 
	list( 
		sua_mb_summary_nh = sua_mb_summary_nh,
		mesh_closest_defib = mesh_closest_defib
		) %>%
		openxlsx::write.xlsx('output/defib_sja.xlsx')


	,

	export_vic = 
	list( 
		sua_mb_summary_nh_vic = sua_mb_summary_nh_vic,
		mesh_closest_defib_vic = mesh_closest_defib_vic
	) %>%
		openxlsx::write.xlsx('output/defib_victoria.xlsx')

	,

	maps_output = 

	list( 
		"mesh_in_reservoir" = mesh_in_reservoir,
		"mesh_in_victoria" = mesh_in_victoria,
		"sja_defib" = defib_sf,
		"vic_defib_reservoir" = sja_defib_reservoir_sf,
		"reservoir_buffered" = reservoir_buffered,
		"reservoir" = reservoir,
		"postcode_2021" = postcode_2021,
		"sa1_2021_seifa" = sa1_2021 %>% inner_join( sa1_2021_seifa_final, by = join_by(sa1_code_2021) ),
		 "suburb_2021" = suburb_2021,
		"victoria_meshblocks_2021" = meshblocks_2021_shape
		)  %>%
		purrr::walk2(names(.), ., ~st_write( .y, glue::glue('output/{.x}.geojson'), delete_dsn = TRUE))


	,

	mesh_closest_defib = 
	mesh_in_reservoir %>% 
		select( centroid, mb_code_2021, area_sqkm, sa1_code_2021, dwelling, person ) %>%
		st_drop_geometry() %>%
		mutate( closest_point = map( centroid, 
			~ find_closest_osrm_point(
				pointB= st_sfc(.x, crs=standard_crs),
				setA =  defib_sf) )) %>% 
		unnest( closest_point ) %>%
		inner_join( sa1_2021_seifa_final, by = join_by(sa1_code_2021) ) %>%
		select( -centroid, -full_address, -geometry)

	,


	mesh_closest_defib_vic = 

	mesh_in_reservoir %>% 
		select( centroid, mb_code_2021, area_sqkm, sa1_code_2021, dwelling, person ) %>%
		st_drop_geometry() %>%
		mutate( closest_point = map( centroid, 
			~ find_closest_osrm_point(
				pointB= st_sfc(.x, crs=standard_crs),
				setA =  sja_defib_reservoir_sf) )) %>% 
		unnest( closest_point ) %>%
		inner_join( sa1_2021_seifa_final, by = join_by(sa1_code_2021) ) %>%
		select( -centroid, -geometry )

	,





	sa1_seifa_decile =

	read_xlsx("~/code/reference_datasets/census_2021/Statistical Area Level 1, Indexes, SEIFA 2021.xlsx", sheet = "Table 2", skip = 5) %>%
		janitor::clean_names() %>%
		mutate(sa1_code_2021 = as.character(x2021_statistical_area_level_1_sa1)) %>%
		rename(decile = decile_6) %>%
		rename(population = usual_resident_population) %>%
		select(sa1_code_2021, decile, score, population)

	,
	#

	sa2_seifa_decile =

	read_xlsx("~/code/reference_datasets/census_2021/Statistical Area Level 2, Indexes, SEIFA 2021.xlsx", sheet = "Table 2", skip = 5) %>%
		janitor::clean_names() %>%
		mutate(sa2_code_2021 = as.character(x2021_statistical_area_level_2_sa2_9_digit_code)) %>%
		rename(decile = decile_7) %>%
		rename(population = usual_resident_population) %>%
		select(sa2_code_2021, decile, score, population)

	,
	#




	# fill in the missing seifa sa1 decile and score with sa2 values
	sa1_2021_seifa_final =

	sa1_2021 %>% 
		st_drop_geometry() %>%
		left_join( sa1_seifa_decile,  by = "sa1_code_2021") %>%
		left_join( sa2_seifa_decile, by = c("sa2_code_2021" = "sa2_code_2021")) %>%
		select( sa1_code_2021,  decile.x, score.x,  decile.y, score.y) %>%
		mutate( decile = ifelse( is.na(decile.x), decile.y, decile.x)) %>%
		mutate( score = ifelse( is.na(score.x), score.y, score.x)) %>%
		select( sa1_code_2021, seifa_decile = decile, seifa_score=score) %>%
		as_tibble()


	,


	mb_all  = 
	excel_sheets('~/code/reference_datasets/census_2021/Mesh Block Counts, 2021.xlsx') %>%
		str_subset('Table') %>%
		tibble('sheet' = .) %>%
		mutate( mb_data  = map( sheet, ~ read_excel('~/code/reference_datasets/census_2021/Mesh Block Counts, 2021.xlsx', skip=6, sheet = .x) %>% janitor::clean_names() )) %>%
		unnest( mb_data ) %>%
		select( -sheet) 

	,

	meshblocks_2021_shape = 


	sf::read_sf('~/code/reference_datasets/census_2021/MB_2021_AUST_GDA2020.shp') %>%
		janitor::clean_names()  %>%
		filter( ste_code21 == '2') %>%
		select( 
			mb_code_2021 = mb_code21
			,  sa1_code_2021 = sa1_code21
			,  sa2_code_2021 = sa2_code21
			,  sa3_code_2021 = sa3_code21
			,  sa4_code_2021 = sa4_code21
			,  gcc_code_2021 = gcc_code21
			,  ste_code_2021 = ste_code21
			,  ste_name_2021 = ste_name21
			,  gcc_name_2021 = gcc_name21
			,  sa4_name_2021 = sa4_name21
			,  sa3_name_2021 = sa3_name21
			,  sa2_name_2021 = sa2_name21
			,  area_sqkm = areasqkm21 ) %>%
		st_transform( standard_crs) 








)
