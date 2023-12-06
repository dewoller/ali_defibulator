library(tidyverse)

# Assume we have a tibble `my_tibble`
my_tibble <- tibble(col1 = 1:3, col2 = 4:6, col3 = 7:9)

# Define a function that takes ... to represent any number of arguments
# This function will create a new tibble from those arguments
my_function <- function(..., n) {
  # The arguments are accessed using list(...)
  args <- list(...)
  # Create a new tibble with each element of the list replicated 'n' times
  map_dfr(args, ~ rep(.x, n))

}

pmap_dfr(my_tibble, function(...) {
	args <- list(...)
	map_dfr(args, ~ rep(.x, 3))
})

print(result)

%>% clipr::write_clip()
