
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

# Fig S1 (CD) - global estimates
source(files[4])

# FigS2 - Global burden of child death by region
source(files[5])

# Fig S3 - Children outliving their mothers - global estimates
source(files[6])

# Fig S4 - Expected child survival and demographic transition
source(files[7])

# Table S5 - Cumulative number of child death for a woman living to age a (CD)
source(files[8])

# Table S6 - Number of surviving children for a woman living to age a (CS)
source(files[9])

# Table S7 - First difference of child death for a woman living to age a (ΔCD)
source(files[10])

# Table S8 - Burden of child death at each age a
source(files[11])

# Table S9 - Number of children expected to outlive their mothers
source(files[12])

# Table S10 - Children outliving their mothers as a share of the mother’s cohort TFR
source(files[13])

# NOT RUN:
# Export full country-cohort-age results for Tables S6-10 as csv files
# The resulting files are quite large in size and are not included in the SI pdf file
# source(files[13])
