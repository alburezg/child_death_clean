print(paste0("Running script: ", "7 - CD_asbolute_by_ex"))

# Estimate the total number of child deaths experienced by all women in a region 
# and birth cohort at each age $a$.
# We obtain this by multiplying $\Delta CD$ by the absolute number of women expected 
# to survive to each age, considering the original size of each female birth cohort 
# and the mortality rates prevalent in their countries of origin.
# This measure removes the assumption of female survival by accounting for the size 
# and age structure of the population.

# In practice, for each country/cohort combination, we need:
#  - First difference of cumulative child loss (ECLC)
#   This was estimated in previous script
#  - Number of woman surviving to age a 
#   This is estimated in this script. 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data requirements: 
# The ECL data frame created in the previous script can be loaded with
# lx_df <- read.csv("../../Data/estimates/lx_df.csv", stringsAsFactors = F)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Absolute child loss by age ----


# For every country, we need:
# Enumerator: first difference of cumulative child loss (ECLC)
#   This was estimated in previous script
# Denominator: number of woman surviving to age a 
#   This will be estimated below

# 1.1. Radix by birth cohort (denominator) ====

# This cannot be obtained from the WPP, which only has period 
# poulations estimates
# Therefore, I need to apply the specific female cohort life table for the
# respective population of women by birth cohort and country/region

# Since these are real numbers, I need to get the size of the female 
# birth cohorts by country and year
# This can be obtained from the WPP estimates of the yearly number of births
# Note that this value is grouped and was ungrouped in another script
# cf WPP_ungroup_age_year

# The function below applies cohort life tables to real-life populatinos
# with the intention of gettin the lx column
# where radices are the initial size of birth cohorts 
# of women using wpp data

# lx_df is a df with the number of women surviving up to age 100 for specific
# birth cohort-country cobminations
# Put differently, it is the number of woman at risk of losing a child
# ie the denominator for the absolute measure of child loss

# This takes around 10min to run parallelised

# Otherwise it can also be estimated again
if(!exists("lx_df")) {
  
  numCores <- ifelse(detectCores() > 8, 25, detectCores() - 1)
  
  lx_df <- apply_lt(
    female_births = female_births
    , LTCF = LTCF
    , numCores = numCores 
  ) 
  
  write.csv(lx_df, "../../Data/estimates/lx_df.csv", row.names = F)
  
}

# 1.2. Total yearly child deaths ====

# Merge the two dfs to get the wanted measure
# I merge before multipltyig to make sure that the values that
# I will multiply are the actual corret country-cohort-age combinatinos
# as the dfs might be ordered strangely after moving them around so much

# Note that diff_df is created in the previous script 4.3 (20190815) 

abs_df <- merge(
  df_cl_diff
  , lx_df
  , by = c('country', 'cohort', 'age')
  , all.x = T
) %>% 
  filter(dplyr::between(age, 15, 100)) %>% 
  filter(dplyr::between(cohort, 1950, 2000)) %>%
  filter(type == 'country') %>% 
  mutate(
    absolute = lx * diff
  ) %>% 
  select(region, type, country, cohort, cohort2, age, diff, lx, absolute) %>% 
  arrange(country, cohort, age)

# 1.3. Stat summary by group ====

# Now it is possible to group the data and get summary
# statistics if this is wanted
# Note that for this measure, we do not estimate percentiles, as 
# it is ultimately the sum of the estimates from a deterministic model
# We do, however, keep the lx column as this will be used later on, in the results section
# to visualise the heterogeneity within each region.

sum_abs <-
  abs_df %>%
  group_by(region, age, cohort) %>%
  summarise(
    value = sum(absolute)
    , lx = sum(lx)
  ) %>%
  ungroup %>% 
  arrange(region, cohort, age) 

# 1.4 Export ====

saveRDS(sum_abs, file = "../../Data/estimates/sum_abs.RDS")
saveRDS(abs_df, file = "../../Data/estimates/abs_df.RDS")
