
print(paste0("Running script: ", "5 - CD_regional_medians"))

# Regional estimates use countries-level data to aggregate estimates at the region-level
# An alternative (not included in this script) uses the ECL estimates derived from the 
# region as a whole

# When estimating region means from individual country data, it is 
# necessary to weight the relative contribution of each country: 
# This is a weighted average with weights proportional to the size 
# of the cohorts in different countries. 
# Comparing this estimates to those averaged by UNN SDG region directly
# tells us something about the relationship between the weighted average 
# trajectory across countries and the value obtained using aggregate rates for  
# all regions. 
# My sense is that the weighted average trajectory across countries is 
# a better measure since no country actually 
# experiences the average for the region. We should aim to use rates at 
# the lowest levels of aggregation (in this case, countries), as this would 
# allow us to account for some heterogeneity across countries.  Different 
# give us a sense of the spread across countries in the region. It will eventually 
# be shown as a shaded area with the  respective color of the line.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Data requirements: 
# The ECL data frame created in the previous script can be loaded with
# df_cl_m_full <- readRDS('../../Data/estimates/df_cl_m_1950to1999_15to100.RData')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Weighted regional means ----

# 1.1. Filter cohorts and regions ====

# DEPRECATED 20201019
# Cosider for sensitivity analysis, although it is essentially 
# describing somethig different
# The mean should be weighted accordin to the size of 
# each prospective birth cohort. 
# This can be obtained my merging with the population file:

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
    # , mean_weighted = weighted.mean(value, w = cohort_size)
    , low_iqr = quantile(value, quant_low)
    , high_iqr = quantile(value, quant_high)
  ) %>%
  ungroup() %>%
  # mutate(
    # low_iqr = median - q25
    # , high_iqr = median + q75
    # low_iqr = median - q40
    # , high_iqr = median + q60
     # low_sd = mean_weighted - sd * 0.5
    # , high_sd = mean_weighted + sd * 0.5
  # ) %>% 
  mutate(cohort2 = paste0("Women born in ", cohort))

# 1.3. Export ====

saveRDS(sum_cl, file = "../../Data/estimates/sum_cl.RDS")
saveRDS(cl_pop, file = "../../Data/estimates/cl_pop.RDS")
