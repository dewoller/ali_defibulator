
# Load required libraries
library(rvest)
library(jsonlite)
library(stringr)

# Fetch the webpage content
webpage <- read_html('tmp/3074.html')

# Extract the content of the script tag (assuming it's uniquely identifiable)
script_content <- 

webpage %>%
  html_nodes(xpath = '//script[contains(., "AVAEDAPP")]') %>%
  html_text() %>%
  pluck(2) %>%
	str_remove_all( ',"photo":"data:[^"]*"')  %>%
	str_detect(',"photo":"data:[^"]*.') %>%

script_content <- 
webpage %>%
  html_nodes(xpath = '//script[contains(., "AVAEDAPP")]') %>%
  html_text() %>%
  pluck(2) %>%
	str_remove_all( ',"photo":"data:[^"]*.')

	str_remove_all( ',"photo":"data:[^"]*"')  

str_length( script_content )
script_content 

script_content %>%
	str_detect(':"data:') %>%


# Isolate the JSON string from the rest of the JavaScript code
json_str <- str_match(script_content, "window.AVAEDAPP.public_results = (\\{.*?\\});")[, 2]


json_str <- str_match(script_content, "window\\.AVAEDAPP\\.public_results = (\\[.*?\\])")[, 2]



cat (json_str)

# Parse the JSON string into a list
json_data <- fromJSON(json_str, flatten = TRUE)

# Extract the values you need
value_needed <- json_data$some_key

[{"company_name":"YARRA PLENTY REGIONAL LIBRARY","org_type":"COMMUNITY","site_location":null,"unit_
no":null,"street_no":"52","street_name":"MAIN","street_type":"STREET","suburb_name":"THOMASTOWN","p
ost_code":"3074","state":"VIC","latitude":-37.679409149999998,"longitude":145.00698609,"landmarks":
"PUBLIC LIBRARY. NEXT TO TRAC (THOMASTOWN RECREATION & AQUATIC CENTRE)","public_access":true,"excep
t_public_hols":true,"except_school_hols":false,"seasonal_avail":false,"seasonal_start_date":null,"s
easonal_end_date":null,"avail":[{"day_name":"Monday","available_ind":"YES","opening_time":"09:00","
closing_time":"20:30"},{"day_name":"Tuesday","available_ind":"YES","opening_time":"09:00","closing_
time":"20:30"},{"day_name":"Wednesday","available_ind":"YES","opening_time":"09:00","closing_time":
"20:30"},{"day_name":"Thursday","available_ind":"YES","opening_time":"09:00","closing_time":"22:00"
},{"day_name":"Friday","available_ind":"YES","opening_time":"09:00","closing_time":"17:00"},{"day_n
ame":"Saturday","available_ind":"YES","opening_time":"10:00","closing_time":"17:00"},{"day_name":"S
unday","available_ind":"NO","opening_time":null,"closing_time":null},{"day_name":"Public Holiday","
available_ind":"NO","opening_time":null,"closing_time":null}]

