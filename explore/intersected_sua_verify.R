
intersected_sua %>%
filter( postcode==3084) %>%
ggplot( aes( sua_area, overlap, color=n_mesh_intersections )) +
geom_point()

intersected_sua %>%
mutate( diff = abs( sua_area - as.numeric( overlap ))) %>%
filter(  diff > 0 )  %>%
arrange( desc( diff )) %>%
select( diff, sua_area, overlap, everything())

intersected_sua %>%
ggplot( aes( n_mesh_intersections )) +
geom_histogram()

intersected_sua %>%
summarise( min(n_mesh_intersections ))


a= intersected_sua %>%
filter( n_mesh_intersections == 0)  

%
################################################################################

sua_sf = st_singles_to_multi( a )

head( 1 ) %>%
pull( isochrone ) %



a = function(abc) {

{{{ .  }}}

  asdf

  asdf


}

a %>%
filter( str_detect( postcode, '^2'))


targets::tar_source()
library(osmdata)
library(targets)


min_longitude <- 141.0  # Westernmost point
min_latitude <- -39.2   # Southernmost point
max_longitude <- 150.0  # Easternmost point
max_latitude <- -34.0   # Northernmost point
bbox_victoria <- st_bbox(c(xmin = min_longitude, ymin = min_latitude, 
                           xmax = max_longitude, ymax = max_latitude),
                         crs = st_crs(4326))
victoria_sf <- st_as_sfc(bbox_victoria)
# Create the map centered at Reservoir, Victoria
map <- leaflet(options = leafletOptions(minZoom = 12)) %>%
  setView(lng = mean(min_longitude, max_longitude),
          lat = mean(min_latitude, max_latitude),
          zoom = 11)
map <- addTiles(map)
map

leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  setView(lng = mean(c(min_longitude, max_longitude)), 
          lat = mean(c(min_latitude, max_latitude)), 
          zoom = 6) %>%
  addPolygons(data=sua_sf,  fillOpacity = 1, color = "red") %>%
  addPolygons(data=mesh_in_victoria,  fillOpacity = 0.5, color = "#25de110a")



leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  setView(lng = mean(c(min_longitude, max_longitude)), 
          lat = mean(c(min_latitude, max_latitude)), 
          zoom = 6) %>%
  addPolygons(data=filter( sua_sf, postcode==3084),  fillOpacity = 1, color = "red") 
















