print("Running script: 3 - ungroup_fertility_ASFR.R")

# UPDATED 20200925 after Demography's R&R

# This script takes 5-year gropued age specific rates reported and projected by 
# the UN and returns 1x1 projected period ASFR for all countries in the world
# https://population.un.org/wpp/Download/Standard/Fertility/

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The files created with this script can be loaded with:

# 1. Read all into a uique file (recommended)
# fert_per_1_1 <- read.csv(file = paste0("../../Data/derived/","fert_per_1_1.csv"), stringsAsFactors = F)

# 2. Load separately by type of region
# fil <- list.files("output/")
# fil <- fil[grepl("^fert_per_1_1_", fil)]
#  
# for(f in fil) {
#   obj <- read.csv(file = paste0("output/",f), stringsAsFactors = F)
#   new_name <- gsub(".csv", "", f)
#   assign(new_name, obj)
#   print(paste(new_name, "loaded."))
# }
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
