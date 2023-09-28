targets::tar_source()

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




tar_load( defib1 ) 
tar_load( sua ) 
sua_sf = st_singles_to_multi( sua )

map %>%
  addMarkers(data = defib1,
             lat = ~latitude,
             lng = ~longitude
  ) %>%
  addPolygons(data=sua_sf, fillColor =  , fillOpacity = 0.5, color = "lightblue")





tar_load( sja_defib_reservoir_sf ) 
tar_load( sua_nh_vic_reservoir ) 

st_singles_to_multi(sua_nh_vic_reservoir ) %>%
	mutate( within_reservoir = (street %in% sja_defib_reservoir_sf$street )) %>%
	{.} -> sua_sf

pal <- colorNumeric("viridis", sua_sf$within_reservoir)

map %>%
  addMarkers(data = sja_defib_reservoir_sf) %>%
  addPolygons(data=sua_sf, fillColor = ~pal( within_reservoir ) , fillOpacity = 0.5) 


