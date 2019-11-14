print(paste0("Running script: ", "9 - CD_by_mother_ex"))

# Expected child loss by mother's life expectancy

# the x axis is the birth cohort of mothers and the 
# y axis is the expected number of child deaths at age 
# 'a', where 'a' is the cohort life expectancy of mothers 
# (each line represents a given country). That way we could 
# show the 'actual' number of child deaths that a mother in 
# a given country/cohort would experience if she survived to 
# the mean age at death in her birth cohort. I think this can 
# be an intuitive way of looking at across-country differences over time. 

# To do this, take abs_df and keep a single value for each region/mother's
# birth cohort combination. This value will be the cohort life expetancy 
# at birth of that region/cohort combination

# Cohort life expectancy can be obtained from the female cohort life tables
# loaded in a previous script: LTCF

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Data requirements: 
# The ECL data frame created in the previous script can be loaded with
# - df_cl_m_full
# df_cl_m_full <- readRDS('../../Data/estimates/df_cl_m_1950to1999_15to100.RData')
# - female_births
# - LTCF
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A. All countries by region ~~~~ ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Get regional means ~~~~ ----

# 1.1 From individual country data
# Get weighted mean by regio using ECL
# estimate for every individual country

# Same as in script 4.2, but keeping all birth
# cohorts:

# The mean should be weighted accordin to the size of 
# each prospective birth cohort. 
# This can be obtained my merging with the population file:

# 1.2. Get birth cohort size ====

df_cl_m_to_merge <- 
  df_cl_m_full %>% 
  filter(type == "country")

cl_pop <- merge(
  df_cl_m_to_merge
  , female_births %>% 
    select(cohort = year, cohort_size = value, everything())
  , by = c('country', 'cohort')
  , all.x = T
)

# 1.3 Keep only ex at birth for each country and cohort

ex_df <- 
  LTCF %>% 
  filter(Age == 0) %>% 
  # Round age
  mutate(ex_round = round(ex)) %>% 
  select(country = Country, cohort = Cohort, ex_round)

# 2. Merge with other dfs ~~~~ ----

# If the analysis is done at the country level
# There are two ways to merge with with LT data 
# to get life expectancy for cohorts of interst
# The first is to take the life expectancy for every  cohort/country 
# combination into account. This is the preferred alternative.

# The second (less correct and not implemented but available in earlier commits for reference) 
# is to take the regional e_x into account
# Check 20190812 github

# Determine which cohort/country combinations are present in the data

country_cohort <- 
  cl_pop %>%
  filter(type == 'country') %>% 
  select(country, cohort) %>% 
  distinct()

# Note that this df includes all countries and regions
# Depending on whether the analysis is done at the regional (groupped)
# or country level, the files can be merged and filtered later on

# 2.1. Get e_x for each country ====

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
# The next step is to keep only those ages in the df with the ECL values

# 2.2. Merge e_x with df of ECL values ====

# In order to keep only the ECL values where the age of the mother 
# correspond to the life expectancy at birth for her cohort/country 
# combination
# This keeps a single record for every cohort/country

cl_ex_country <- merge(
  cl_pop 
  , life_expectancy_country
  , by.x = c("country", 'cohort', 'age')
  , by.y = c("country", 'cohort', 'ex_round')
  , all.x = F
  , all.y = T
) 

# 2.3. Get female population for each cohort-region ====

# This is the number of women in a given birth cohort that survived to 
# the mean age at death for that cohort of women. I.e. how many women
# survived up to the life expectancy of that cohort:

# This can be used for weighting the measures in the visualisation
# e.g. for seeing: What share of the world population was each cohort of women?

pop <- 
  female_births %>% 
  select(country, cohort = year, pop = value) %>% 
  group_by(cohort) %>% 
  mutate(
    share = pop/sum(pop)*100
  ) %>% 
  ungroup

cl_ex_pop_country <- 
  merge(
    cl_ex_country
    , pop
    , by = c("country", 'cohort')
    , all.x = T
  ) %>% 
  filter(between(cohort, 1950, 1999) )

# Every row in this df is the ECL values 
# for every country/cohort combination
# at the mothers age that matches the life expectancy at birth
# for the given country/cohort combination

# Note that this is still country-level data that needs to be aggregated
# in order to be plotted by region

# However, cl_ex_pop_country can be used directly in ggplots that
# need individual level data, such as boxplots

# 3. Stat summary by group ~~~~ ----

# Now it is possible to group the data and get summary
# statistics if this is wanted

# Chose percentiles to estimate for each region
quant_low <- 0.40
quant_high <- 0.60

sum_cl_ex <-
  cl_ex_pop_country %>%
  # No need to filter by age, since the data only contains
  # one observation for every coutry/cohort, which is
  # the e_0 for that combination
  group_by(region, cohort) %>%
  summarise(
    # IQR-based
    median = median(value)
    , low_iqr = quantile(value, quant_low)
    , high_iqr = quantile(value, quant_high)
  ) %>%
  ungroup() %>%
  mutate(cohort = as.numeric(cohort)) 

# 3.3. Export ====

saveRDS(sum_cl_ex, file = "../../Data/estimates/sum_cl_ex.RDS")
saveRDS(cl_ex_pop_country, file = "../../Data/estimates/cl_ex_pop_country.RDS")
