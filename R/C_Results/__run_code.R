
if(!require("stringr")) {
  install.packages("stringr")
  library(stringr)
} 

files <- list.files(pattern = ".R$")[-1]
( files <- str_sort(files, numeric = TRUE) )

# Load functions
source(files[1])

# Load data
source(files[2])

# Country and region labels
source(files[3])

# Fig1
source(files[4])

# Fig2
source(files[5])

# Fig3
source(files[6])

# Fig4
source(files[7])
