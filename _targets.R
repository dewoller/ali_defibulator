

# Load packages required to define the pipeline:
targets::tar_source()
library(targets)
library(tarchetypes)
# library(tarchetypes) # Load other packages as needed. # nolint


sja_defib_addresses_toadd = tribble( 
	~street, ~full_address, ~latitude, ~longitude,
	"Reservoir Station", "Reservoir Station, Victoria, 3073 AUSTRALIA", -37.7168,145.0071
)

vic_defib_addresses =	tribble(
	 ~type, ~full_address,
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","26 PALM AVENUE, RESERVOIR, VIC 3073",
"COLES SUPERMARKET - RESERVOIR","325 SPRING STREET, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","895 HIGH STREET, RESERVOIR, VIC 3073",
"DAREBIN CITY COUNCIL - RESERVOIR COMMUNITY & LEARNING CENTRE","23 EDWARDES STREET, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE DEFIB IN YOUR STREET","42 COMPTON STREET, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","2C CUTHBERT ROAD, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","55 MIRANDA ROAD, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","15A DELAWARE STREET, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","13 GRIMWADE STREET, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","6 CHAUVEL STREET, RESERVOIR, VIC 3073",
"MAHARISHI SCHOOL","2-8 DUNDEE STREET, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","119 CHEDDAR ROAD, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","64 GLASGOW AVENUE, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","11 TAYLOR AVENUE, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","35 STURDEE STREET, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","4 MCCARTEN STREET, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","49 WINTER CRESCENT, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","3 CRAWLEY STREET, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","43 KINSALE STREET, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","1 LUKE STREET, RESERVOIR, VIC 3073",
"WILLIAM RUTHVEN PRIMARY SCHOOL","60 MERRILANDS ROAD, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","157 CHEDDAR ROAD, RESERVOIR, VIC 3073",
"WYNCITY KEON PARK","16 KEON PARADE, THOMASTOWN, VIC 3074",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","847 PLENTY ROAD, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","2 RONA STREET, RESERVOIR, VIC 3073",
"COLES SUPERMARKET - SUMMERHILL","850 PLENTY ROAD, RESERVOIR, VIC 3083",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","14 GODLEY STREET, RESERVOIR, VIC 3073",
"HOLY NAME PRIMARY SCHOOL","6-20 ROBB STREET, RESERVOIR, VIC 3073",
"PRESTON TOYOTA","705 HIGH STREET, PRESTON, VIC 3072",
"SVC PRODUCTS PTY LTD","2 CHAFFEY STREET, THOMASTOWN, VIC 3074",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","2 LIPSCOMB COURT, RESERVOIR, VIC 3073",
"ICON EQUIPMENT P/L","13-17 HILLWIN STREET, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","9 BRYAN STREET, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","66-68 DREDGE STREET, RESERVOIR, VIC 3073",
"GABBIN PAPER TUBES","15 DUNSTANS COURT, THOMASTOWN, VIC 3074",
"FOILMAKERS AUSTRALIA","21 TEMPLE DRIVE, THOMASTOWN, VIC 3074",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","12 BARTROP STREET, RESERVOIR, VIC 3073",
"ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET","7 NEWTON STREET, RESERVOIR, VIC 3073",
"DAREBIN CITY COUNCIL - EAST PRESTON COMMUNITY CENTRE","7 NEWTON STREET, RESERVOIR, VIC 3073",
"HOMEWOOD CONSULTING","350 SETTLEMENT ROAD, THOMASTOWN, VIC 3074",
"CONTROL CERTIFICATION AND INSTRUMENTATION","326 SETTLEMENT ROAD, THOMASTOWN, VIC 3074",
"APCO SERVICE STATION","228 SETTLEMENT ROAD, THOMASTOWN, VIC 3074",
"DAREBIN CITY COUNCIL - OPERATIONS CENTRE RESERVOIR","15 CARAWA DRIVE, RESERVOIR, VIC 3073",
"WOOLWORTHS GROUP LIMITED - PRESTON","330-336 MURRAY ROAD, PRESTON, VIC 3072",
"BUDGET RENT A CAR","342 MURRAY ROAD, PRESTON, VIC 3072",
"DAREBIN CITY COUNCIL - DAREBIN INTERCULTURAL CENTRE","59A ROSEBERRY AVENUE, PRESTON, VIC 3073",
"7 CHEFS PTY LTD","106 MCBRYDE STREET, FAWKNER, VIC 3060",
"DAHLSENS THOMASTOWN","246 MAHONEYS ROAD, THOMASTOWN, VIC 3074",
"DAREBIN CITY COUNCIL - PRESTON MUNICIPAL COMPLEX","350 HIGH STREET, PRESTON, VIC 3072",
"DAREBIN CITY COUNCIL - PRETON MUNICIPAL COMPLEX","274 GOWER STREET, PRESTON, VIC 3072",
)


tar_plan(


	postcode_2021 = strayr::read_absmap("postcode2021") 

	,

	sa1_2021 = strayr::read_absmap("sa12021"), 
	standard_crs = sa1_2021 %>% st_crs() ,

	suburb_2021 = strayr::read_absmap("suburb2021")  %>%
		janitor::clean_names(),

	reservoir_sf = 
	suburb_2021 %>% 
		filter( str_detect( suburb_name_2021 ,'Reservoir\\s*\\(Vic.\\)')) %>%
		select( starts_with('suburb'))

	,

	reservoir_buffered_sf = 
	reservoir_sf %>% 
		st_buffer( 400 ) 


	,

	# # must accommodate new requirement of excluding sja defibs
	# victoria_defib = 

	# postcode_2021 %>%
	# 	filter( str_detect( postcode_num_2021, "^30")) %>%
	# 	mutate( url = glue::glue("
	# 		https://registermyaed.ambulance.vic.gov.au/find-an-aed?lat={cent_lat}&lng={cent_long}&place={postcode_2021}&availability=all"
	# 	)) %>%
	# 	st_drop_geometry() %>%
	# 	mutate( ll = map( url, get_victoria_defib_ll )) 
	# ,

	# victoria_defib_cleaned %>%
	# 	filter( is.na( latitude))

	victoria_defib_cleaned =

	vic_defib_addresses %>%
		filter( !str_detect( type, "ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET")) %>%
		geocode(full_address, method = 'osm', lat = latitude , long = longitude) %>%
		mutate( full_address = str_c( type, ' ', full_address)) %>%
		select( -type ) %>%
		mutate( type='non-sja')
	
	,

	tar_file(sja_defib_file,  'data/defib_ressie.csv'),


	sja_defib = 
	read_csv( sja_defib_file) %>%
		mutate( address = str_remove_all(address, '^\\s*|,.*|\\(.*|Reservoir\\s*$')) %>%
		mutate( address = str_remove_all(address, '^\\s*|,.*|\\(.*|Reservoir\\s*$')) %>%
		rename( street = address ) %>%
		mutate( full_address = glue::glue("{street}, Reservoir, Victoria, 3073 AUSTRALIA")) %>%
		geocode(full_address, method = 'osm', lat = latitude , long = longitude) %>%
		mutate( type='sja') %>%
		select( -street)


	,


	sja_defib_cleaned = sja_defib %>% 
		drop_na( latitude, longitude ) %>%
		bind_rows( sja_defib_addresses_toadd ) %>%
		mutate( type='sja') 

	,

	sja_defib_sf  = 
	sja_defib_cleaned %>%
		st_as_sf( coords = c("longitude", "latitude"), crs = standard_crs) 

,

	sja_and_vic_defib_sf =
	sja_defib_sf %>%
		bind_rows( victoria_defib_cleaned_sf ) 

	,

	victoria_defib_cleaned_sf  = 
	victoria_defib_cleaned %>%
		st_as_sf( coords = c("longitude", "latitude"), crs = standard_crs) 

	,

	sja_defib_sua = 

	sja_defib_cleaned %>%
		mutate( isochrone =map2( latitude, longitude, point2isochrone ) )  %>%
		mutate( isochrone = map( isochrone, st_make_valid ))  %>%
		mutate( sua_area = map_dbl( isochrone, ~st_area(.x) %>% units::drop_units() )) 

	,

	sja_defib_no_holes = sja_defib_sua %>% mutate(isochrone = map(isochrone, nngeo::st_remove_holes ))
	,



	victoria_defib_sua = 
	victoria_defib_cleaned %>%
		mutate( isochrone =map2( latitude, longitude, point2isochrone ) )  %>%
		mutate( isochrone = map( isochrone, st_make_valid ))  %>%
		mutate( sua_area = map_dbl( isochrone, ~st_area(.x) %>% units::drop_units() )) 

	,


	victoria_defib_sua_reservoir =
	victoria_defib_sua %>% mutate( is_in_reservoir = map( isochrone, ~st_intersects( .x, reservoir_buffered_sf)  )) %>%
		unnest( is_in_reservoir ) %>%
		unnest( is_in_reservoir ) 


	,
	victoria_defib_sua_reservoir_no_holes = victoria_defib_sua_reservoir %>% mutate(isochrone = map(isochrone, nngeo::st_remove_holes ))
	,



	victoria_defib_reservoir_sf =
	victoria_defib_cleaned %>%
		st_as_sf( coords = c("longitude", "latitude"), crs = standard_crs) %>%
		st_intersection( reservoir_buffered_sf )  %>%
		select( -suburb_name_2021, -suburb_code_2021)

	,


	# meshblock stuff
	mesh_in_reservoir = 

	meshblocks_2021_shape %>%
		st_join( reservoir_sf, join = st_intersects, left=FALSE) %>%
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

	# meshblock calculations
	sua_mb_summary = summarise_one_sua_set( sja_defib_sua, mesh_in_reservoir, sja_defib_cleaned ),
	sua_mb_summary_nh = summarise_one_sua_set( sja_defib_no_holes, mesh_in_reservoir, sja_defib_cleaned ),

	sua_mb_summary_vic = 
	summarise_one_sua_set( victoria_defib_sua_reservoir, mesh_in_reservoir, victoria_defib_cleaned),

	sua_mb_summary_nh_vic = 
	summarise_one_sua_set( victoria_defib_sua_reservoir_no_holes, mesh_in_reservoir, victoria_defib_cleaned ),



	mesh_closest_defib_all = 

	mesh_in_reservoir %>% 
		select( centroid, mb_code_2021, area_sqkm, sa1_code_2021, dwelling, person ) %>%
		st_drop_geometry() %>%
		mutate( closest_point = map( centroid, 
			~ find_closest_osrm_point(
				pointB= st_sfc(.x, crs=standard_crs),
				setA =  sja_and_vic_defib_sf) )) %>% 
		unnest( closest_point ) %>%
		inner_join( sa1_2021_seifa_final, by = join_by(sa1_code_2021) ) %>%
		select( -centroid, -geometry)

	,


	mesh_closest_defib_vic = 

	mesh_in_reservoir %>% 
		select( centroid, mb_code_2021, area_sqkm, sa1_code_2021, dwelling, person ) %>%
		st_drop_geometry() %>%
		mutate( closest_point = map( centroid, 
			~ find_closest_osrm_point(
				pointB= st_sfc(.x, crs=standard_crs),
				setA =  victoria_defib_reservoir_sf) )) %>% 
		unnest( closest_point ) %>%
		inner_join( sa1_2021_seifa_final, by = join_by(sa1_code_2021) ) %>%
		select( -centroid, -geometry )

	,

	mesh_closest_defib_transistion = 

mesh_closest_defib_vic %>%
		select(-area_sqkm,-sa1_code_2021,-dwelling,-person,-seifa_decile, -seifa_score) %>%
		rename( 
			pre_sja_closest_defib = full_address,
			pre_sja_closest_type = type,
			pre_sja_closest_duration = duration,
			pre_sja_closest_distance = distance,
	) %>%
		inner_join( mesh_closest_defib_all, by = join_by(mb_code_2021) ) %>%
		rename( 
			post_sja_closest_defib = full_address,
			post_sja_closest_type = type,
			post_sja_closest_duration = duration,
			post_sja_closest_distance = distance,
	) 
	
,

	# sa1 level seifa estimation

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

,



	# xls	output
	export_reservoir = 
	list( 
		sja_mb_coverage = sua_mb_summary_nh,
		mesh_closest_defib_all= mesh_closest_defib_all
		) %>%
		openxlsx::write.xlsx('output/only_sja.xlsx')


	,

	export_vic = 
	list( 
		pre_sja_mb_coverage = sua_mb_summary_nh_vic,
		mesh_closest_defib_vic = mesh_closest_defib_vic
	) %>%
		openxlsx::write.xlsx('output/pre_sja.xlsx')

	,

	export_both = 
	list( 
		sja_mb_coverage = sua_mb_summary_nh,
		pre_sja_mb_coverage = sua_mb_summary_nh_vic,
		mesh_closest_defib_transistion = mesh_closest_defib_transistion
	) %>%
		openxlsx::write.xlsx('output/pre_to_sja_transistion.xlsx')


	,

	maps_output = 

	list( 
		"sja_defib" = sja_defib_sf,
		"pre_sja_defib_reservoir" = victoria_defib_reservoir_sf,
		"reservoir_buffered_sf" = reservoir_buffered_sf,
		"reservoir" = reservoir_sf,
		"sa1_2021_seifa" = sa1_2021 %>% inner_join( sa1_2021_seifa_final, by = join_by(sa1_code_2021) ),
		"sja_defib_sua_noholes" =  sja_defib_no_holes %>% st_singles_to_multi(),
		"pre_sja_defib_sua_noholes" = victoria_defib_sua_reservoir_no_holes %>% st_singles_to_multi(),
		"sja_defib_sua" =  sja_defib_sua %>% st_singles_to_multi(),
		"pre_sja_defib_sua" = victoria_defib_sua_reservoir%>% st_singles_to_multi()
		)  %>%
		purrr::walk2(names(.), ., ~st_write( .y, glue::glue('output/{.x}.geojson'), delete_dsn = TRUE))


	,





)
