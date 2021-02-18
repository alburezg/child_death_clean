print(paste0("Running script: ", "8 - sum_CD_asbolute_by_ex"))


# Global burden of child death. Estimated as the sum of the
# (non-cumulative) burden of child death over all ages. This
# measure, considers the size and structure of different birth 
# cohorts of women to determine the number of child deaths 
# accumulated by all women in a given birth cohort and region 
# throughout their lives. 

# Note that this should not sum the values of ALL ages of child burden.
# Doing so would be slightly misleading as we cannot expect women in all
# country/cohorts to survive to age 100 (the current upper age limit) in
# the data. Instead, the values in each country/cohorts combination
# should only be summed up to the female life expectancy for that country/cohort.
# This would then represent the `actual` experience number of child deaths
# assuming that all women survive to the life expectancty.
# This is equivalent to the way in which E[CS] is estimated.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data requirements: 
# The abs_df data frame created in the previous script can be loaded with
# abs_df <- readRDS('../../Data/estimates/abs_df.RDS')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1. Get cumulative child burden by region ----

sum_burden <-
  abs_df %>% 
  group_by(region, cohort) %>% 
  dplyr::summarise(
    value = sum(absolute, na.rm = T)
    , sd = sd(absolute, na.rm = T)
    , low_sd = value - sd
    , high_sd = value + sd
  ) %>% 
  ungroup %>% 
  mutate(
    value = value 
    , low_sd = low_sd 
    , high_sd = high_sd 
    , cohort2 = paste0(cohort, " birth cohort")
  ) %>% 
  select(region, cohort, cohort2, value, low = low_sd, high = high_sd)


# 1.4 Export ====

saveRDS(sum_burden, file = paste0("../../Data/estimates/sum_burden_",fertility_variant,".RDS"))
# saveRDS(burden_df, file = "../../Data/estimates/burden_df.RDS")
