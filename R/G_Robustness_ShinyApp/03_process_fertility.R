print("Running script: 3 - ungroup_fertility_ASFR.R")

# UPDATED 20200925 after Demography's R&R

# This script takes 5-year gropued age specific rates reported and projected by 
# the UN and returns 1x1 projected period ASFR for all countries in the world
# https://population.un.org/wpp/Download/Standard/Fertility/

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The files created with this script can be loaded with:

ASFRC_low <- read.csv(paste0("../../Data/derived/", "ASFRC_","low",".csv"), stringsAsFactors = F)
ASFRC_medium <- read.csv(paste0("../../Data/derived/", "ASFRC_","medium",".csv"), stringsAsFactors = F)
ASFRC_high <- read.csv(paste0("../../Data/derived/", "ASFRC_","high",".csv"), stringsAsFactors = F)
ASFRC_constant <- read.csv(paste0("../../Data/derived/", "ASFRC_","constant",".csv"), stringsAsFactors = F)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# visual_examination <- F
# export <- T
# fert_df <- asfr_wpp

allowed_types <- c("Country", "SDG region")
export <- T
# 1. Expand ASFR by type of scenario ------------

# 1.1. Low variant (2020-2100) ==================

fert_per_1_1_low <- 
  ungroup_fertility_ASFR(
    fert_df = bind_rows(asfr_obs, asfr_pred_low)
    , variant_name = "low"
    , allowed_types = allowed_types
    , export = export
    , returnme = T
  )

# 1.2. Medium variant (2020-2100) ==================

fert_per_1_1_medium <- 
  ungroup_fertility_ASFR(
    fert_df = bind_rows(asfr_obs, asfr_pred_medium)
    , variant_name = "medium"
    , allowed_types = allowed_types
    , export = export
    , returnme = T
  )

# 1.3. High variant (2020-2100) ==================

fert_per_1_1_high <- 
  ungroup_fertility_ASFR(
    fert_df = bind_rows(asfr_obs, asfr_pred_high)
    , variant_name = "high"
    , allowed_types = allowed_types
    , export = export
    , returnme = T
  )

# 1.4. Constant variant ===============

fert_per_1_1_constant <- 
  ungroup_fertility_ASFR(
    fert_df = bind_rows(asfr_obs, asfr_constant)
    , variant_name = "constant"
    , allowed_types = allowed_types
    , export = export
    , returnme = T
  )

# 2. Merge 

# fert_per_1_1_all <- 
#   bind_rows(
#     fert_per_1_1_low %>% mutate(source = "low")
#     , fert_per_1_1_medium %>% mutate(source = "medium")
#     , fert_per_1_1_high %>% mutate(source = "high")
#   )

# rm("fert_per_1_1_estimates", "fert_per_1_1_low", "fert_per_1_1_medium", "fert_per_1_1_high")

# 3. Export

# write.csv(
#   x = fert_per_1_1_all
#   , file = paste0("../../Data/derived/", "fert_per_1_1_all.csv")
#   , row.names = F
# )

print("3 - ungroup_fertility_ASFR: success!")

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

# 2.1. COnstant scenario ===============

ASFRC_constant <- 
  convert_period_asfr_to_cohort_asfr(
    fert_per_1_1 = fert_per_1_1_constant
    , variant_name = "constant"
    , export = export
    , returnme = T
  )

print("4 - convert_period_asfr_to_cohort_asfr: success!")
