victoria_defib_sua_no_holes %>%
st_drop_geometry() %>%
count( latitude, longitude, sort=TRUE )



victoria_defib_sua_no_holes_sf


library(sf)
library(leaflet)
library(dplyr)

# Assuming 'sf_object' is your sf object
# sf_object <- read_sf("your_sf_file.geojson") # Example to read sf object

# Calculate center and extent
center <- st_centroid(victoria_defib_sua_no_holes_sf) %>% st_coordinates()
extent <- st_bbox(victoria_defib_sua_no_holes_sf)

leaflet(victoria_defib_sua_no_holes_sf) %>%
  addTiles() %>%
  addPolygons() %>%
  setView(lng = center[1], lat = center[2], zoom = 6)




res_sf =
victoria_defib_sua_no_holes_sf  %>%
filter( postcode==3073)

# Calculate center and extent
center <- st_centroid(res_sf) %>% st_coordinates()
bbox <- st_bbox(res_sf)

victoria_defib_sua_no_holes_sf %>%
filter( sua_id %in% vacar2sua$sua_id) %>%
leaflet() %>%
  addTiles() %>%
  addPolygons() %>%
  addCircleMarkers(data=
 inner_join( vacar_sf, vacar2sua)
  ,  radius = 1, color = "red") 


reservoir_sua_sf %>%
leaflet() %>%
  addTiles() %>%
  addPolygons() %>%
  addCircleMarkers(data=
 inner_join( vacar_sf, vacar2sua)
  ,  radius = 1, color = "red") 


victoria_defib_reservoir_sf %>%
st_drop_geometry() %>%
select( sua_id ) %>%
inner_join( victoria_defib_sua_no_holes_sf) %>%


ictoria_defib_sua_no_holes_sf

vacar_sf %>%
st_drop_geometry() %>%
count( va_casid, sort=TRUE)



            read_csv("data/VACAR_Data_2019_2023.csv") %>% 
            distinct(xllwgs84, yllwgs84, .keep_all = TRUE) %>%


victoria_defib_cleaned %>%
# filter( postcode == '3084') %>%
ount( postcode, sort=TRUE) %>%


victoria_defib_cleaned %>%

