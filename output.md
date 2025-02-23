_targets.R
---

targets::tar_source()
library(targets)
library(tarchetypes)


defib_to_delete = c("DAREBIN CITY COUNCIL - EAST PRESTON COMMUNITY CENTRE")

defib_to_add = 
data.frame(
  stringsAsFactors = FALSE,
          latitude = c(-37.7086),
         longitude = c(145.0326),
          lat_base = c(-37.7086),
          lon_base = c(145.0326),
           company = c( "ST JOHN AMBULANCE VICTORIA DEFIB IN YOUR STREET"),
           address = c("33 Ramleh Road, RESERVOIR 3073 VIC"),
          postcode = c("3073")
)


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

	# deal with reservoir

	# the reservoir SF only
	reservoir_sf =
	suburb_2021 %>%
		filter(str_detect(suburb_name_2021, "Reservoir\\s*\\(Vic.\\)")) %>%
		select(geometry),

	#
	reservoir_buffered_sf = reservoir_sf %>% st_buffer(400),

	#
	#  which mesh blocks are in reservoir buffered area
	reservoir_mesh =
	mesh_2021_vic_sf %>%
		select( mb_code_2021 ) %>%
		st_intersection(reservoir_buffered_sf) %>%
		st_drop_geometry() %>%
		mutate( mesh_in_reservoir_buffer = TRUE) 

	,
	#  which defib points are in reservoir buffered area
	reservoir_defib_sf =
	victoria_defib_cleaned_sf %>%
		select( sua_id ) %>%
		st_intersection(reservoir_buffered_sf) %>%
		mutate( defib_in_reservoir_buffer = TRUE) 

	,

	# which SUA are in reservoir
	# may be problems, but we do not use it
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
	# this is the mass of targets that is all the defib URLs, one per postcode, 
	# and the call to download the list of defibs for the postcode
	victoria_defib_targets, 
	#
	tar_combine(
		# this is the output, all the cleaned and deduplicated URLs
		victoria_defib_cleaned_prelim,
		victoria_defib_targets[[2]],
		command =
		dplyr::bind_rows(!!!.x) %>%
			distinct(lat, lon, .keep_all = TRUE) %>%
			rename(latitude = lat, longitude = lon)
	)

	,

		victoria_defib_cleaned  =
		victoria_defib_cleaned_prelim %>%
		filter( !company %in% defib_to_delete) %>%
		bind_rows(defib_to_add) %>%
		mutate( sua_id = row_number()) 

,

	################################################################################
	# Meshblock setup

	# all the meshblock detail data
	mb_all  = 
	excel_sheets('~/code/reference_datasets/census_2021/Mesh Block Counts, 2021.xlsx') %>%
		str_subset('Table') %>%
		tibble('sheet' = .) %>%
		mutate( mb_data  = map( sheet, ~ read_excel('~/code/reference_datasets/census_2021/Mesh Block Counts, 2021.xlsx', skip=6, sheet = .x) %>% janitor::clean_names() )) %>%
		unnest( mb_data ) %>%
		select( -sheet) 

	,

	# meshblock shape for victoria
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


	mesh_2021_vic_centroid_sf =

	mesh_2021_vic_sf %>%
		filter(ste_code_2021 == "2") %>%
		mutate(centroid = st_centroid(geometry)) 
	,

	# mesh
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
			count( vacar_mesh_detail, mb_code_2021, name='n_vacar_in_mesh') , 
			by = join_by(mb_code_2021)) %>%
		tidylog::inner_join(sa1_2021_seifa_final, by = join_by(sa1_code_2021, mb_code_2021))

	,

	sua_detail_sja = 
	sua_noh_mesh_export %>%
		select( 
			sua_id,
			ends_with( 'pop'),
			dwelling_sua_sja = dwelling,
			person_sua_sja = person,
			most_populous_decile_sua_sja = most_populous_decile,
			weighted_seifa_score_sua_sja = weighted_seifa_score
		) %>%
		rename_with(~str_replace(.x, "_pop$", "_pop_sua_sja"), ends_with("_pop"))

	,

	sua_detail_no_sja = 
	sua_noh_mesh_export %>%
		filter( !is_sja_defib) %>%
		select( 
			sua_id,
			ends_with( 'pop'),
			dwelling_sua_no_sja = dwelling,
			person_sua_no_sja = person,
			most_populous_decile_sua_no_sja = most_populous_decile,
			weighted_seifa_score_sua_no_sja = weighted_seifa_score
		) %>%
		rename_with(~str_replace(.x, "_pop$", "_pop_sua_no_sja"), ends_with("_pop"))

	,


	mesh_export = 

	mesh_detail_final %>%
		tidylog::inner_join(mesh_distance_to_nearest_defib, by = join_by(mb_code_2021)) %>%
		tidylog::inner_join(mesh_distance_to_nearest_defib_no_sja, by = join_by(mb_code_2021)) %>%
		tidylog::left_join(sua_detail_no_sja, by=join_by(closest_defib_id_no_sja==sua_id)) %>%
		tidylog::left_join(sua_detail_sja, by=join_by(closest_defib_id==sua_id)) %>%
		tidylog::left_join(reservoir_mesh, by = join_by(mb_code_2021)) %>%
		replace_na( list( mesh_in_reservoir_buffer = FALSE))

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

	# vacar level	summary
	vacar_export = 

	vacar_sf %>%
		st_drop_geometry() %>%
		tidylog::inner_join(vacar_distance_to_nearest_defib, by = join_by(va_internal_id)) %>%
		tidylog::inner_join(vacar_distance_to_nearest_defib_no_sja, by = join_by(va_internal_id)) %>%
		tidylog::inner_join(select(vacar_mesh_detail, mb_code_2021, va_internal_id), by = join_by(va_internal_id)) %>%
		mutate( sja_improvment_duration = duration2defib_no_sja - duration2defib ) %>%
		mutate( sja_improvment_distance = distance2defib_no_sja - distance2defib ) %>%
		mutate( sja_did_improvment = closest_defib_id_no_sja != closest_defib_id ) %>%
		# phew
		tidylog::inner_join(select( mesh_export,
			-starts_with('closest'),
			-ends_with('_sja'),
			-starts_with('duration2'),
			-starts_with('distance2')),
			by = join_by(mb_code_2021)) %>%
		tidylog::left_join(count(vacar_sua, va_internal_id, name = "within_how_many_sua"), by = join_by(va_internal_id))  %>%
		tidylog::left_join(sua_detail_no_sja, by=join_by(closest_defib_id_no_sja==sua_id)) %>%
		tidylog::left_join(sua_detail_sja, by=join_by(closest_defib_id==sua_id)) 

	,


	################################################################################
	# find the closest defib to each VACAR and meshblock


	vacar_distance_to_nearest_defib =
	find_closest_osrm_points_closest_n(vacar_sf, victoria_defib_cleaned_sf, n = 10) %>%
		mutate( closest_defib_name = paste( company, address, postcode )) %>%
		select(va_internal_id,
			distance2defib = distance,
			duration2defib = duration,
			closest_defib_name,
			closest_defib_id = sua_id)

	,

	vacar_distance_to_nearest_defib_no_sja =
	find_closest_osrm_points_closest_n(vacar_sf, victoria_defib_no_sja_sf, n = 10) %>%
		mutate( closest_defib_name = paste( company, address, postcode )) %>%
		select(va_internal_id, 
			distance2defib_no_sja = distance, 
			duration2defib_no_sja = duration,
			closest_defib_name_no_sja = closest_defib_name,
			closest_defib_id_no_sja = sua_id
		)

	,

	mesh_distance_to_nearest_defib =

	mesh_2021_vic_centroid_sf %>%
		find_closest_osrm_points_closest_n(., victoria_defib_cleaned_sf, n = 10, st_coordinates(.$centroid)) %>%
		select(mb_code_2021,
			distance2defib = distance,
			duration2defib = duration,
			closest_defib_id = sua_id
	)

	,

	mesh_distance_to_nearest_defib_no_sja =
	mesh_2021_vic_centroid_sf %>%
		find_closest_osrm_points_closest_n(., victoria_defib_no_sja_sf, n = 10, st_coordinates(.$centroid)) %>%
		select(mb_code_2021,
			distance2defib_no_sja = distance,
			duration2defib_no_sja = duration,
			closest_defib_id_no_sja = sua_id
			)

	,
	################################################################################
	# Defib setup


	#
	victoria_defib_cleaned_sf =
	victoria_defib_cleaned %>%
		mutate( is_sja_defib = str_detect( str_to_upper( company), 'DEFIB IN'))  %>%
		st_as_sf(coords = c("longitude", "latitude"), crs = standard_crs) ,

	victoria_defib_no_sja_sf = victoria_defib_cleaned_sf %>% filter( !is_sja_defib),

	victoria_defib_sua_temp =
	victoria_defib_cleaned %>%
		mutate(isochrone = map2(latitude, longitude, point2isochrone)) 

	,

victoria_defib_sua = 
	victoria_defib_sua_temp %>%
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

	################################################################################
	# find intersection of Meshblock and SUA
	#
	sua_noh_with_closest_mesh =
	sua_noh %>%
		find_n_closest_mb(mesh_2021_vic_centroid_sf, n = 100)

	,

	# find the total CALD, total 55+, total 65+, seifa, dwellings and total population for each SUA
	sua_noh_mesh_summarised =
	sua_noh_with_closest_mesh %>%
		mutate(details = map2(nn, isochrone, summarise_one_sua, mesh_detail_final)) %>%
		unnest_wider(details)

	,

	sua_noh_mesh_export =

	sua_noh_mesh_summarised %>%
		select( -starts_with('nn'), -isochrone, -starts_with('.group')) %>%
		tidylog::left_join(count(vacar_sua, sua_id, name = "n_vacar_arrest"), by = join_by(sua_id)) %>%
		tidylog::left_join( st_drop_geometry( reservoir_defib_sf ), by = join_by(sua_id)) %>%
		replace_na( list( defib_in_reservoir_buffer = FALSE))


	,


	################################################################################
	# Census 2021
	census_cald_columns =

	read_xlsx("../../reference_datasets/Metadata_2021_GCP_DataPack_R1_R2.xlsx", sheet = "Cell Descriptors Information") %>%
		clean_names() %>%
		filter(str_detect(data_packfile, "G11")) %>%
		filter(long %in% c(
			"TOTAL_Uses_other_language_and_speaks_English_Total_Total"
			, "TOTAL_Uses_other_language_and_speaks_English_Not_well_or_not_at_all_Total"
			, "TOTAL_Uses_other_language_and_speaks_English_Proficiency_in_English_not_stated_Total"
		)) %>%
		pull(short)

	,

	# the number of CALD for each SA1
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
		# take the elder pp and allocate them across the meshblocks that make up the SA1
		mutate(
			across(
				starts_with("age_"), 
				.fns = list( 
					pop = ~ .x * mesh_pop_sa1_proportion
				))) %>%
		select(-ends_with("_plus")) %>%
		select(mb_code_2021, starts_with("age_"))

	,

	cald_mesh= 
	cald_sa1 %>%
		inner_join(mesh2sa1, by = join_by(sa1_code_2021)) %>%
		# take the cald pp and allocate them across the meshblocks that make up the SA1
		mutate(
			across(
				starts_with("cald"), 
				.fns = list( 
					pop = ~ .x * mesh_pop_sa1_proportion
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


---
deploy.R
---

config = config::get( config='one_loc', file='deploy.yml')

"R_files" = fs::dir_ls('R', regexp='\\.R$')
"targets_files" = fs::dir_ls('_targets', recurse=TRUE)


main_file = 'one_row_shiny.R'
root_files=c(  '_targets.R'  )

appFiles = c(R_files, targets_files, main_file, root_files)

rsconnect::deployApp( 
	appPrimaryDoc=main_file,
	appDir=getwd(), 
	appFiles=appFiles,
	appName=config$appName, 
	# appId=config$appId,
	account=config$account,
	server='shinyapps.io',
	lint=FALSE,
	forceUpdate=TRUE)



config = config::get( config='one_loc', file='deploy.yml')
rsconnect::showLogs( 	
	appName=config$appname, 
	account=config$account,
	server="shinyapps.io", entries=500 )






---
one_row_shiny.R
---

# Load the necessary packages
library(shiny)
library(leaflet)
library(dplyr)
library(targets)

targets::tar_load( sua )

df = list( "none" = tar_read( sua),
	"no_holes" = tar_read( sua_nh),
	"convex_hull" = tar_read( sua_ch) )

ui <- fluidPage(
  
  titlePanel("Select a Row and Plot on Leaflet Map"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("row", "Select a row:", choices = NULL),
      selectInput("df", "Select a smoothing_technique:", choices = c("none", "no_holes", "convex_hull"))
    ),
    
    mainPanel(
      leafletOutput("map", height = 700)
    )
  )
)

server <- function(input, output, session) {
  
  # Here we're assuming you have a data frame called 'df' with 'lat' and 'lon' columns.
  # Make sure to replace 'df', 'lat', and 'lon' with your actual data frame and column names.
  
  # Update the selectInput choices based on the rownames of the dataframe
  observe({
    updateSelectInput(session, "row", choices = (sua$street))
  })
  
  output$map <- renderLeaflet({
    # Get the selected row
    selected_row <- df[[input$df]] %>% filter( street == input$row ) 
    
		# Create a leaflet map
		leaflet() %>%
			addTiles() %>%
			# Add a marker for the selected row
			addMarkers(lng = selected_row$longitude, lat = selected_row$latitude, popup = paste("Row:", input$row)) %>% 
			addPolygons(
				data = selected_row$isochrone[[1]],
				fillColor = "blue",
				fillOpacity = 0.4,
				color = "blue"
			)
	})



  
}

shinyApp(ui = ui, server = server)


---
run.R
---
#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

targets::tar_make()
# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint


---
R/antijoin_within_distance.R
---
# anti-join df1 against df2 within distance
antijoin_within_distance = function( df1, df2, limit_distance=400 ) {

	assertthat::assert_that( 'latitude' %in% names(df1) )
	assertthat::assert_that( 'longitude' %in% names(df1) )
	assertthat::assert_that( 'latitude' %in% names(df2) )
	assertthat::assert_that( 'longitude' %in% names(df2) )

	wdf1 = df1 %>%
		mutate(.id = row_number()) 

	wdf2 = df2 %>%
		select( latitude, longitude) %>%
		mutate(.id = row_number()) 



	# Create all combinations of id
	tidyr::expand_grid(id1 = wdf1$.id, id2 = wdf2$.id) %>%
		inner_join( wdf1, c( "id1" = ".id")) %>%
		inner_join( wdf2, c( "id2" = ".id")) %>%
		rename(
			longitude1 = longitude.x,
			latitude1 = latitude.x,
			longitude2 = longitude.y,
			latitude2 = latitude.y) %>%
		as_tibble()  %>%
		mutate( distance = geosphere::distVincentySphere( 
			cbind(longitude1, latitude1), 
			cbind(longitude2, latitude2) ) 
		)  %>%
		{.} -> final


	duplicates = final %>% filter( distance < limit_distance ) 

	wdf1 %>%
		filter( .id %ni% duplicates$id1 )  %>%
		select( - .id)

}



---
R/find_closest_osrm_point.R
---
find_closest_osrm_point = function(longitude, latitude, setA) {
    # pointB: a data frame with a single row containing a latitude and longitude
    # seetA: a data frame with multiple rows containing latitudes and longitudes
    # returns: a data frame with a single row containing the closest point in points to point
    #


    # Calculate distances from the current point in setB to all points in setA
    table <- osrmTable(
        src = setA,
        dst = matrix(c(longitude, latitude), ncol = 2),
        osrm.server = "http://localhost:1234/",
        osrm.profile = "foot",
        measure = c("duration", "distance")
    )

    durations <- table[["durations"]]
    dists <- table[["distances"]]

    # Find the closest point in setA
    closest_point <- setA[which.min(dists), ]
    closest_point$duration <- min(durations)
    closest_point$distance <- min(dists)
    return(closest_point)
}


find_closest_osrm_points = function(setB, setA) {
    # setB, setA: a data frame with a multiple rows containing latitudes and longitudes
    # returns: a data frame with a single row containing the closest point in points to point
    # Apply the function to each point in setB


    setB %>%
        mutate(
            closest_point =
                map2(
                    latitude, longitude,
                    ~ find_closest_osrm_point(
                        pointB = tibble(latitude = .x, longitude = .y),
                        setA = setA
                    )
                )
        ) %>%
        unnest(closest_point)
}


find_closest_osrm_points_closest_n = function(setB, setA, n = 10, setB_coords = st_coordinates(setB)) {
    # for each in setB, find the n closest points in setA, and then find the closest osrm from these

    dist_matrix <- st_distance(setB, setA)

    # Initialize a list to store the indices of the 10 closest points
    closest_indices_list <- vector("list", length = nrow(setB))

    # Loop through each point in setB
    for (i in 1:nrow(setB)) {
        # Sort distances for the current point and get indices of 10 closest points
        sorted_indices <- order(dist_matrix[i, ])
        closest_indices <- sorted_indices[1:n]
        closest_indices_list[[i]] <- closest_indices
    }

    # Retrieve the actual points from Y
    closest_points_list <- lapply(closest_indices_list, function(indices) setA[indices, ])

    # for each setB item from closest_points_list, find the closest osrm point
    setB %>%
        mutate(closest_points = closest_points_list) %>%
        mutate(
            latitude = setB_coords[, 2],
            longitude = setB_coords[, 1]
        ) %>%
        st_drop_geometry() %>%
        mutate(
            closest_point =
                pmap(
                    list(longitude, latitude, closest_points),
                    ~ find_closest_osrm_point(..1, ..2, setA = ..3)
                )
        ) %>%
        unnest(closest_point)

}



---
R/find_closest_points.R
---
	
if(FALSE) {


	sf1 = head( mesh_in_victoria, 10)
	sf2 = victoria_defib_cleaned_sf

}
find_closest_points = function( sf1, sf2 ) {

	# Step 1: Calculate the Distance Matrix
	distance_matrix <- st_distance(sf1, sf2)

	# Step 2: Find the Index of the Nearest Points
	nearest_indices <- apply(distance_matrix, 1, which.min)
	nearest_distance <- apply(distance_matrix, 1, min)  # Extract minimum distances

	# Step 3: Extract the Coordinates of the Nearest Points
	nearest_points <- sf2[nearest_indices,]

	# Combine the original points and their nearest points for easier analysis
	result <- cbind(st_drop_geometry(sf1), st_drop_geometry(nearest_points), nearest_distance)

	result

}


---
R/find_n_closest_mb.R
---
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


---
R/get_reservoir_defib_addresses.R
---

get_reservoir_defib_addresses = function() {


	tribble(
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

}


---
R/get_victoria_defib_ll.R
---
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


---
R/ni.R
---
`%ni%` = Negate(`%in%`)


---
R/packages.R
---

library(dotenv)
library(conflicted)
library(purrr)
library(lubridate)
library(stringr)
library(janitor)
library(glue)
library(dplyr)
library(fs)
library(sf)
library(readr)
library(tidygeocoder)
library(jsonlite)
library(leaflet)
library(osmdata)
library(osrm)
library(tidyr)
library(rlang)
library(readxl)
library(strayr)
library(nngeo)
library(tidyverse)
library(sf)
library(units)


conflicted::conflicts_prefer(
    lubridate::month,
    lubridate::year,
    dplyr::lag,
    dplyr::filter,
    dplyr::summarise,
    dplyr::mutate,
    dplyr::rename,
    dplyr::arrange,
    dplyr::count,
    readxl::read_xlsx,
    purrr::flatten,
    .quiet = TRUE
)





---
R/point2isochrone.R
---
point2isochrone = function( latitude, longitude, time_limit=4*60, mode = "foot",
													 orsm='http://localhost:5000'
													 ) {

	options(osrm.server = "http://localhost:1234/")
	options(osrm.profile = "foot")

	isochrone <- osrmIsochrone(loc = c( longitude, latitude), breaks = seq(0, 5, 5))
	# Now you can plot the isochrone polygon
	# plot(st_geometry(isochrone))

	if (nrow(isochrone) == 0) {

		st_point(c( longitude, latitude)) %>%
			st_sfc( crs = 4326) %>%
			st_transform( 3857) %>%
			st_buffer( dist = 400) %>%
			st_transform( 4326)  %>%
		{.} -> isochrone
	}

	isochrone
}




---
R/point2lat_long.R
---
point2lat_long = function( df, column='point' ) {

	df %>%
		pluck(column) %>%
		st_coordinates() %>% 
		tibble::as_tibble()  %>%
		set_names( c('longitude', 'latitude')) 

}


---
R/python_Rify.R
---


python_Rify = function( file_path ) {

	# make sure the python code is loaded
	reticulate::source_python( file_path )

	signature = get_first_python_function_signature( file_path )
	function_name =  signature[[1]]
	function_args = paste( signature[[2]], collapse=',' )

	function_text =glue::glue(.open='<<', .close='>>', "

function( << function_args >> ) {

create_arg_list = function( args ) {

args %>% map_chr( function(x) {
	if( is.character(x) ) {
		str_c(\"'\",str_replace_all( x, \"'\", \"\\\\'\"),\"'\")
	} else {
		str_c(x)
	}
}) %>% str_c( collapse=',')

}

function_args_string = create_arg_list( list( <<function_args>> ))

		py_run_string(str_c('
# the following is python code
rv = << function_name >> ( ', function_args_string, '  )
'))
		return( reticulate::py$rv)

	}

	")

	assign(	 str_c('python_', function_name), eval( parse( text=function_text	)) , envir = .GlobalEnv)

}


if(FALSE) {
file_path = 'python/abc.py'
python_Rify( file_path )

python_asdf1

	# doesn't work yet
python_asdf1(list(1:2),2)

python_asdf1('a',2)


}

# Define a Python function to parse the Python file
get_first_python_function_signature <- function(file_path) {

  python_code <- sprintf('
import ast

def get_first_function_args(file_path):
    with open(file_path, "r") as source_code:
        tree = ast.parse(source_code.read())
        for node in tree.body:
            if isinstance(node, ast.FunctionDef):
                return (node.name, [arg.arg for arg in node.args.args])

    return []

result = get_first_function_args("%s")
  ', file_path)

  # Run the Python code
  reticulate::py_run_string(python_code)

  # Return the result
  return(reticulate::py$`result`)
}



---
R/st_singles_to_multi.R
---
st_singles_to_multi = function( sua )  {
	sua %>% 
		select( isochrone) %>% 
		unlist(recursive = FALSE) %>% 
		do.call(rbind, .) %>%
		bind_cols( sua %>% select( -isochrone)) 

}
	


---
R/strip_duplicate_points.R
---
strip_duplicate_points = function( df, tolerance=50 ) {

df = df %>%
  mutate(id = row_number()) 

	working_df = df
	save_crs = NULL
if ('sf' %in%	class( working_df )) {
		save_crs = st_crs( working_df )

		working_df %>%
			mutate(
				longitude = st_coordinates(geometry)[, 1],
				latitude = st_coordinates(geometry)[, 2]
			) %>%
			st_drop_geometry()  %>%
			{.} -> working_df
	}

assertthat::assert_that( 'latitude' %in% names(working_df) )
assertthat::assert_that( 'longitude' %in% names(working_df) )


# Add an id to each point
working_df <- working_df %>%
		select( latitude, longitude, id ) 

	# Create all combinations of id
	tidyr::expand_grid(id1 = working_df$id, id2 = working_df$id) %>%
		filter(id1 < id2) %>%
		merge( working_df, by.x = "id1", by.y = "id") %>%
		merge(working_df, by.x = "id2", by.y = "id") %>%
		rename(
			longitude1 = longitude.x,
			latitude1 = latitude.x,
			longitude2 = longitude.y,
			latitude2 = latitude.y) %>%
		as_tibble()  %>%
		mutate( distance = geosphere::distVincentySphere( 
			cbind(longitude1, latitude1), 
			cbind(longitude2, latitude2) ) 
		)  %>%
	{.} -> final


	duplicates = final %>% filter( distance < tolerance) 
	df = filter( df, id %ni% duplicates$id1 )  %>%
		select( -id )

	if ( !is.null( save_crs ) ) {
		df = df %>%
			st_as_sf( crs = save_crs ) %>%
		{.} -> df
	}

	df
}



---
R/summarise_one_sua.R
---
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


---
R/summarise_one_sua_set.R
---
if( FALSE) {

sua = victoria_defib_sua_reservoir_no_holes

canonical_ll  = victoria_defib_cleaned 

}

summarise_one_sua_set = function( intersected_sua ) {

	intersected_sua %>%
		group_by( latitude, longitude) %>%
		summarise( population_weighted_seifa = weighted.mean( seifa_score, person), .groups='drop')  %>%
		{.} -> point_seifa
	

	intersected_sua %>% 
		count( latitude, longitude,  seifa_decile, wt=person) %>%
		group_by( latitude, longitude ) %>%
		slice_max( n, with_ties=FALSE) %>%
		rename( most_populous_seifa_decile = seifa_decile)  %>%
		select( -n) %>%
		{.} -> point_decile

	intersected_sua %>% 
		count( latitude, longitude,  mb_category_name_2021, wt=area_covered) %>%
		group_by( latitude, longitude) %>%
		slice_max( n, with_ties=FALSE) %>%
		rename( largest_mb_overlap_category = mb_category_name_2021)  %>%
		select( -n) %>%
		{.} -> point_mb_category

	intersected_sua %>%
		group_by( latitude, longitude, sua_area ) %>%
		summarise( across( c( 'person', 'dwelling', 'area_covered'), sum ),
			.groups = 'drop') %>%
		rename( person_covered = person,
			dwelling_covered = dwelling) %>%
		inner_join(  point_mb_category, by = join_by(latitude, longitude)) %>%
		inner_join(  point_seifa, by = join_by(latitude, longitude)) %>%
		inner_join(  point_decile, by = join_by(latitude, longitude)) %>%
		rename( sua_area_sqkm = sua_area) 

}


---
R/tar_combine.R
---


---
demonstrate.Rmd
---



```{r}

victoria_defib %>%
	# filter( str_detect( address, '3073') ) %>%
	# filter( str_detect( company, 'JOHN') )  %>%
	{.} -> from_json


sja_defib %>% 
	drop_na() %>%
	bind_rows(sja_defib_addresses_toadd )  %>%
	distinct() %>%
	mutate( id = row_number() ) %>%
	{.} -> sja_defib

# Assuming from_json and sja_defib are your data frames

# Load the geosphere library for the distHaversine function
library(geosphere)

# Create empty columns in from_json for the closest_lat, closest_lon, and distance
from_json$distance <- NA
from_json$closest_id <- NA
# Loop through each row in from_json
for (i in 1:nrow(from_json)) {
  # Calculate the distances between the current row in from_json and all rows in sja_defib
  distances <- distHaversine(c(from_json[i, "lon"][[1]], from_json[i, "lat"][[1]]),
                             matrix(c(sja_defib$longitude, sja_defib$latitude), ncol = 2, byrow = FALSE))
  # Find the index of the row in sja_defib with the minimum distance
  closest_idx <- which.min(distances)
  # Update from_json with the closest latitude, longitude, and distance
  from_json[i, "distance"] <- distances[closest_idx]
  from_json[i, "closest_id"] <- closest_idx
}

# closest web from every provided SJA ppoing.  Ramleh missing from web
from_json %>%
	inner_join( sja_defib, by=c('closest_id'='id')) %>%
	select( address, full_address, distance, closest_id ) %>%
	group_by(closest_id) %>%
	slice_min( distance ) %>%
	ungroup() %>%
	arrange(  desc( distance)) 


```


---
