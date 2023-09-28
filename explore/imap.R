# Load the purrr package
library(purrr)

# Create a list of data frames
df1 <- data.frame(A = 1:3, B = 4:6)
df2 <- data.frame(A = 7:9, B = 10:12)
my_list <- list("FirstDF" = df1, "SecondDF" = df2)

# Define a function to process both the name and the data frame
my_function <- function(name, df) {
  # Calculate the mean of column 'A' in the data frame
  mean(df$A)
  
}

# Use imap to apply the function to each data frame
map2_dbl(names(my_list), my_list, my_function) %>%

# Print the result
print(result)
