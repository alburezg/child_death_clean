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

# 1. Get life expectancy for each country/cohort combination ----

# 1.1 Keep only ex at birth for each country and cohort ====

ex_df <- 
  LTCF %>% 
  filter(Age == 0) %>% 
  # Round age
  mutate(ex_round = round(ex)) %>% 
  select(country = Country, cohort = Cohort, ex_round)


# 1.2. Merge with other dfs ====

# If the analysis is done at the country level
# take the life expectancy for every  cohort/country 
# combination into account.

# First, determine which cohort/country combinations are present in the data

country_cohort <- 
  abs_df %>%
  filter(type == 'country') %>% 
  select(country, cohort) %>% 
  distinct()

# 1.3. Get e_x for each country ====

# Merge country-level data with LT data to get life expectancy for 
# cohorts/countries of interst

life_expectancy_country <- merge(
  country_cohort
  , ex_df
  , by.x = c('country', 'cohort')
  , by.y = c('country', 'cohort')
  , all.x = T
)

# Here, life_expectancy is a df with the cohort ex for the birth cohorts of women
# The next step is to keep only those ages in the df with the BCD values

# 2 Merge e_x with df of child burden values ----

# 2.1. Get cumulative child burden ====

# Sum of all values of CB up to age a

burden_df <- 
  abs_df %>% 
  filter(type == "country") %>% 
  filter(age < 100) %>% 
  group_by(country, cohort) %>% 
  mutate(cum = cumsum(absolute)) %>% 
  ungroup %>% 
  select(region, country, cohort, age, cum)

# In order to keep only the child burden values where the age of the mother 
# correspond to the life expectancy at birth for her cohort/country 
# combination
# This keeps a single record for every cohort/country

burden_ex_country <- merge(
  burden_df 
  , life_expectancy_country
  , by.x = c("country", 'cohort', 'age')
  , by.y = c("country", 'cohort', 'ex_round')
  , all.x = F
  , all.y = T
) %>% 
  mutate(region = as.character(region))

# 2.2. Aggregate by region ====

sum_burden <-
  burden_ex_country %>%
  group_by(region, cohort) %>%
  dplyr::summarise(
    value = sum(cum, na.rm = T)
    , sd = sd(cum, na.rm = T)
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
  select(region, cohort, cohort2, value, low = low_sd, high = high_sd) %>% 
  arrange(region, cohort) 


# 1.4 Export ====

saveRDS(sum_burden, file = "../../Data/estimates/sum_burden.RDS")
saveRDS(burden_df, file = "../../Data/estimates/burden_df.RDS")
