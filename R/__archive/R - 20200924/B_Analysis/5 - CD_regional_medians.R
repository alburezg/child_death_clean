
print(paste0("Running script: ", "5 - CD_regional_medians"))

# Use the country-level estimates produced by the previous script to estimate
# the levels of child death at a regional level. Regions are UN SDG regions.
# Median and different percentiles are estimated.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Data requirements: 
# The ECL data frame created in the previous script can be loaded with
# df_cl_m_full <- readRDS('../../Data/estimates/df_cl_m_1950to1999_15to100.RData')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Regional means ----

# 1.1. Filter cohorts and regions ====

cl_pop <- merge(
  df_cl_m_full %>% 
    filter(type == "country") 
  , female_births %>% 
    select(cohort = year, cohort_size = value, everything())
  , by = c('country', 'cohort')
  , all.x = T
) 

# 1.2. Stat summary by group ====

# Chose percentiles to estimate for each region
quant_low <- 0.40
quant_high <- 0.60

# Get weighted and IQR by region
# usig estimates for individual countries

sum_cl <-
  cl_pop %>%
  group_by(region, age, cohort) %>%
  summarise(
    median = median(value)
    , low_iqr = quantile(value, quant_low)
    , high_iqr = quantile(value, quant_high)
  ) %>%
  ungroup() %>% 
  mutate(cohort2 = paste0("Women born in ", cohort))

# 1.3. Export ====

saveRDS(sum_cl, file = "../../Data/estimates/sum_cl.RDS")
saveRDS(cl_pop, file = "../../Data/estimates/cl_pop.RDS")
