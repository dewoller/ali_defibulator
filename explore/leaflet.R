library(leaflet)
library(osmdata)
library(targets)


# Reservoir, Victoria bounding box
reservoir_bbox <- list(
  west = 144.9867,
  south = -37.7275,
  east = 145.0277,
  north = -37.6933
)


# Create the map centered at Reservoir, Victoria
map <- leaflet(options = leafletOptions(minZoom = 12)) %>%
  setView(lng = mean(reservoir_bbox$west, reservoir_bbox$east),
          lat = mean(reservoir_bbox$south, reservoir_bbox$north),
          zoom = 13)

# Add the base map layer (OpenStreetMap tiles)
map <- addTiles(map)


# tar_load( isochrone ) 





# Note that purrr::map returns the map object, so the assignment is not necessary, but it's done here for clarity.


tar_load( defib1 ) 
tar_load( sua_sf ) 

map %>%
  addMarkers(data = defib1,
             lat = ~latitude,
             lng = ~longitude
  ) %>%
  addPolygons(data=sua_sf, fillColor = 'blue' , fillOpacity = 0.1, color = "lightblue")


map

