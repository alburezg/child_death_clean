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

# 2. Get cohort approximations---------

# We need one df per scenario, including 
# a. estimates 1950-2020 (same for all dfs), and
# b. 2020-2100 data by scenario (low, med, high)

# 2.1. Low scenario ===============

ASFRC_low <- 
  convert_period_asfr_to_cohort_asfr(
  fert_per_1_1 = fert_per_1_1_low
  , variant_name = "low"
  , export = export
  , returnme = T
)

# 2.1. Medium scenario ===============

ASFRC_medium <- 
  convert_period_asfr_to_cohort_asfr(
    fert_per_1_1 = fert_per_1_1_medium
    , variant_name = "medium"
    , export = export
    , returnme = T
  )

# 2.1. High scenario ===============

ASFRC_high <- 
  convert_period_asfr_to_cohort_asfr(
    fert_per_1_1 = fert_per_1_1_high
    , variant_name = "high"
    , export = export
    , returnme = T
  )

print("4 - convert_period_asfr_to_cohort_asfr: success!")
