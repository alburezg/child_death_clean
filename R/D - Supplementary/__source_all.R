
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

# Tables with full country reuslts (complete)
source(files[4])

# Tables with full country reuslts (complete) (selection of cohorts and ages)
source(files[5])

# FigS1 - Total burden of chid death
source(files[6])

# Table: global estimates for all regions
source(files[7])



source(files[9])
