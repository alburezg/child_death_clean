# UPDATED 20200925 after Demography's R&R

print("Running script: 4 - convert_period_asfr_to_cohort_asfr.R")

# Using the interpolated values from the previous script, we approximate cohort fertility 
# by taking values on the diagonal. 
# For example, for the 1950 "cohort", 1_f_15 would be based on fertility rates for 1965, 
# the 1_f_16 for the period data from 1966, etc.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The files created with this script can be loaded with:
# ASFRC <- read.csv(file = paste0("../../Data/derived/","ASFRC.csv"), stringsAsFactors = F)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Parameters ----

export <- T
visual_examination <- F




