library(testthat)
library(devtools)

# Load the source code from the project
load_all()

# Run all tests
test_dir("tests/testthat/")
