
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
