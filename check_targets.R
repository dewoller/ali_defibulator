library(targets)
library(dplyr)

cat("Checking targets status...\n")
progress <- tar_progress()
print(progress)

cat("\nNon-skipped targets:\n")
non_skipped <- progress %>% filter(progress != "skipped")
print(non_skipped)

cat("\nFailed targets:\n")
failed <- progress %>% filter(progress == "errored")
print(failed)

cat("\nRunning targets:\n") 
running <- progress %>% filter(progress == "running")
print(running)