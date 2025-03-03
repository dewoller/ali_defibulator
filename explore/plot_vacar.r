# explore/plot_vacar.R
# Standalone script to plot VACAR points and their corresponding SUA overlaps using leaflet

# Load required libraries
library(targets)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)

# Source targets functions
tar_source()

# Load required data
tar_load(vacar_sf)
tar_load(vacar_sua)
tar_load(sua_noh_sf)

# Create a color palette for SUA
pal <- colorFactor(
  palette = "viridis",
  domain = sua_noh_sf$sua_id
)

# Get unique VACAR points that overlap with SUA
vacar_with_sua <- vacar_sf %>%
  filter(va_internal_id %in% vacar_sua$va_internal_id)

# Join VACAR with SUA information
vacar_sua_joined <- vacar_sua %>%
  left_join(vacar_with_sua, by = "va_internal_id")

# Get SUA polygons that overlap with VACAR points
sua_with_vacar <- sua_noh_sf %>%
  filter(sua_id %in% vacar_sua$sua_id)

# Create popup content for VACAR points
vacar_popup <- vacar_with_sua %>%
  st_drop_geometry() %>%
  mutate(popup_content = paste0(
    "<strong>VACAR ID:</strong> ", va_internal_id, "<br>",
    "<strong>Date:</strong> ", va_date, "<br>",
    "<strong>Location:</strong> ", va_location, "<br>"
  ))

# Create the map
map <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Add SUA polygons
  addPolygons(
    data = sua_with_vacar,
    fillColor = ~pal(sua_id),
    fillOpacity = 0.5,
    color = "#444444",
    weight = 1,
    smoothFactor = 0.5,
    group = "SUA",
    label = ~paste("SUA ID:", sua_id),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#000000",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>%
  # Add VACAR points
  addCircleMarkers(
    data = vacar_with_sua,
    radius = 5,
    color = "red",
    fillColor = "red",
    fillOpacity = 0.8,
    weight = 1,
    group = "VACAR",
    label = ~paste("VACAR ID:", va_internal_id),
    popup = ~vacar_popup$popup_content[match(va_internal_id, vacar_popup$va_internal_id)]
  ) %>%
  # Add layer controls
  addLayersControl(
    overlayGroups = c("SUA", "VACAR"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Add legend
  addLegend(
    position = "bottomright",
    colors = c("red", "blue"),
    labels = c("VACAR Locations", "SUA Boundaries"),
    opacity = 0.7,
    title = "Legend"
  ) %>%
  # Add scale bar
  addScaleBar(position = "bottomleft") %>%
  # Add fullscreen control
  addFullscreenControl()

# Display the map
map

# Save the map as an HTML file (optional)
# saveWidget(map, "vacar_sua_map.html", selfcontained = TRUE)