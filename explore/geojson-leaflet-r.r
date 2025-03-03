#!/usr/bin/env Rscript

# Load required libraries
suppressPackageStartupMessages({
  library(leaflet)
  library(sf)
  library(argparse)
  library(jsonlite)
})

# Parse command line arguments
parser <- ArgumentParser(description='Plot two GeoJSON files side by side on Leaflet maps')
parser$add_argument('file1', help='Path to first GeoJSON file')
parser$add_argument('file2', help='Path to second GeoJSON file', nargs='?', default=NULL)
parser$add_argument('--title1', default='Map 1', help='Title for first map')
parser$add_argument('--title2', default='Map 2', help='Title for second map')
parser$add_argument('--color1', default='blue', help='Color for first map features')
parser$add_argument('--color2', default='red', help='Color for second map features')
parser$add_argument('--output', default='geojson_maps.html', help='Output HTML file')
parser$add_argument('--width', default=1200, type='integer', help='Total width of the maps in pixels')
parser$add_argument('--height', default=600, type='integer', help='Height of the maps in pixels')

args <- parser$parse_args()

# Function to validate GeoJSON file
validate_geojson <- function(file_path) {
  tryCatch({
    if (!file.exists(file_path)) {
      stop(paste("File does not exist:", file_path))
    }
    
    # Try to read file content
    content <- readLines(file_path, warn = FALSE)
    content <- paste(content, collapse = "")
    
    # Validate JSON format
    validate(content)
    
    return(TRUE)
  }, error = function(e) {
    stop(paste("Error with GeoJSON file", file_path, ":", e$message))
  })
}

# Validate input files
validate_geojson(args$file1)
if (!is.null(args$file2)) {
  validate_geojson(args$file2)
}

# Read GeoJSON files
cat("Reading GeoJSON files...\n")
geojson1 <- st_read(args$file1, quiet = TRUE)

# If second file is provided, read it too
if (!is.null(args$file2)) {
  geojson2 <- st_read(args$file2, quiet = TRUE)
} else {
  # If only one file provided, use it for both maps (for testing)
  cat("Second file not provided. Using first file for both maps.\n")
  geojson2 <- geojson1
}

# Calculate the center coordinates for maps
bbox1 <- st_bbox(geojson1)
center_lon1 <- (bbox1[1] + bbox1[3]) / 2
center_lat1 <- (bbox1[2] + bbox1[4]) / 2

bbox2 <- st_bbox(geojson2)
center_lon2 <- (bbox2[1] + bbox2[3]) / 2
center_lat2 <- (bbox2[2] + bbox2[4]) / 2

# Use average coordinates for both maps
center_lon <- (center_lon1 + center_lon2) / 2
center_lat <- (center_lat1 + center_lat2) / 2

# Convert geojson to JSON for embedding
geojson1_json <- st_write(geojson1, "", driver = "GeoJSON", quiet = TRUE)
geojson1_txt <- paste(readLines(""), collapse = "\n")

geojson2_json <- st_write(geojson2, "", driver = "GeoJSON", quiet = TRUE)
geojson2_txt <- paste(readLines(""), collapse = "\n")

# Create maps 
cat("Creating maps and saving to HTML...\n")

# Determine output file path
output_file <- args$output
if (!grepl("\\.html$", output_file)) {
  output_file <- paste0(output_file, ".html")
}

# Create a simpler solution that embeds the GeoJSON directly in the HTML
html_content <- paste0(
  "<!DOCTYPE html>\n",
  "<html>\n",
  "<head>\n",
  "  <meta charset='utf-8'>\n",
  "  <title>GeoJSON Comparison Maps</title>\n",
  "  <script src='https://unpkg.com/leaflet@1.9.4/dist/leaflet.js'></script>\n",
  "  <link rel='stylesheet' href='https://unpkg.com/leaflet@1.9.4/dist/leaflet.css'>\n",
  "  <style>\n",
  "    body { margin: 0; padding: 0; font-family: Arial, sans-serif; }\n",
  "    h1 { text-align: center; margin: 10px 0; }\n",
  "    #map-container { display: flex; width: 100%; height: 90vh; }\n",
  "    .map { flex: 1; height: 100%; border: 1px solid #ccc; }\n",
  "    .map-title { 
  "      position: absolute; 
  "      top: 10px; 
  "      right: 10px; 
  "      z-index: 1000; 
  "      background: white; 
  "      padding: 5px 10px; 
  "      border-radius: 4px; 
  "      box-shadow: 0 0 10px rgba(0,0,0,0.2);
  "    }\n",
  "  </style>\n",
  "</head>\n",
  "<body>\n",
  "  <h1>GeoJSON Comparison</h1>\n",
  "  <div id='map-container'>\n",
  "    <div id='map1' class='map'></div>\n",
  "    <div id='map2' class='map'></div>\n",
  "  </div>\n",
  "  <script>\n",
  "    // Initialize maps\n",
  "    var map1 = L.map('map1').setView([", center_lat, ", ", center_lon, "], 10);\n",
  "    var map2 = L.map('map2').setView([", center_lat, ", ", center_lon, "], 10);\n",
  "    \n",
  "    // Add title overlays\n",
  "    var title1 = L.control({position: 'topright'});\n",
  "    title1.onAdd = function(map) {\n",
  "      var div = L.DomUtil.create('div', 'map-title');\n",
  "      div.innerHTML = '", args$title1, "';\n",
  "      return div;\n",
  "    };\n",
  "    title1.addTo(map1);\n",
  "    \n",
  "    var title2 = L.control({position: 'topright'});\n",
  "    title2.onAdd = function(map) {\n",
  "      var div = L.DomUtil.create('div', 'map-title');\n",
  "      div.innerHTML = '", args$title2, "';\n",
  "      return div;\n",
  "    };\n",
  "    title2.addTo(map2);\n",
  "    \n",
  "    // Add basemaps\n",
  "    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {\n",
  "      attribution: '© OpenStreetMap contributors'\n",
  "    }).addTo(map1);\n",
  "    \n",
  "    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {\n",
  "      attribution: '© OpenStreetMap contributors'\n",
  "    }).addTo(map2);\n",
  "    \n",
  "    // Synchronize maps\n",
  "    map1.sync(map2);\n",
  "    map2.sync(map1);\n",
  "    \n",
  "    // Add GeoJSON data\n",
  "    var geojson1 = ", geojson1_txt, ";\n",
  "    var geojson2 = ", geojson2_txt, ";\n",
  "    \n",
  "    L.geoJSON(geojson1, {\n",
  "      style: {\n",
  "        color: '", args$color1, "',\n",
  "        weight: 2,\n",
  "        fillOpacity: 0.5\n",
  "      },\n",
  "      onEachFeature: function(feature, layer) {\n",
  "        if (feature.properties) {\n",
  "          var popupContent = '<table>';\n",
  "          for (var p in feature.properties) {\n",
  "            popupContent += '<tr><th>' + p + '</th><td>' + feature.properties[p] + '</td></tr>';\n",
  "          }\n",
  "          popupContent += '</table>';\n",
  "          layer.bindPopup(popupContent);\n",
  "        }\n",
  "      }\n",
  "    }).addTo(map1);\n",
  "    \n",
  "    L.geoJSON(geojson2, {\n",
  "      style: {\n",
  "        color: '", args$color2, "',\n",
  "        weight: 2,\n",
  "        fillOpacity: 0.5\n",
  "      },\n",
  "      onEachFeature: function(feature, layer) {\n",
  "        if (feature.properties) {\n",
  "          var popupContent = '<table>';\n",
  "          for (var p in feature.properties) {\n",
  "            popupContent += '<tr><th>' + p + '</th><td>' + feature.properties[p] + '</td></tr>';\n",
  "          }\n",
  "          popupContent += '</table>';\n",
  "          layer.bindPopup(popupContent);\n",
  "        }\n",
  "      }\n",
  "    }).addTo(map2);\n",
  "  </script>\n",
  "  <script src='https://unpkg.com/leaflet.sync@0.2.4/L.Map.Sync.js'></script>\n",
  "</body>\n",
  "</html>"
)

# Write the HTML file
writeLines(html_content, output_file)

cat(paste("Maps saved to", normalizePath(output_file), "\n"))
cat("Open this file in a web browser to view the maps.\n")

# Print usage example
cat("\nUsage example:\n")
cat("Rscript geojson_map.R path/to/file1.geojson path/to/file2.geojson --title1 'First Dataset' --title2 'Second Dataset' --color1 blue --color2 red\n")
