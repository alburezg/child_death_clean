# README
# Estimate child death and child survival dataframes

if(!require("stringr")) {
  install.packages("stringr")
  library(stringr)
} 

files <- list.files(pattern = ".R$")[-1]
( files <- str_sort(files, numeric = TRUE) )

# Create

# Load functions and data
source(files[1])
source(files[2])
source(files[3])

# Child death estimates
source(files[4])
source(files[5])
source(files[6])
source(files[7])
source(files[8])
source(files[9])

# Child survival estimates
source(files[10])
source(files[11])
source(files[12])
