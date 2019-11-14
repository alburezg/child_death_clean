print(paste0("Running script: ", "9 - CD_fraction_outlive_mother"))

# Estimate the fraction of a woman's cohort TFR that will outlive her. 
# Higher values of SOM indicate that a larger fraction of a woman's offspring is expected 
# to live longer than her, independently of the prevalent levels of fertility. 
# It is defined as: FOM = E[CS]/CTFR
# where E[CS] is the Expected Value of Child Death (see previous script). 
# The denominator, CTFR, is the cohort total fertility rate for a given country/cohort 
# combination.
# This script computes both FOM and FDM (fraction of children died before mothers).
# FOM and FDM are, in fact, complementary measures to the degree that FOM+FDM = 1
# for any given country/cohort combination.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data requirements: 
# LTCF
# ASFRC
# df_cl_m_full:
# df_cl_m_full <- readRDS('../../Data/estimates/df_cl_m_1950to1999_15to100.RDS')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1.1. Enumerator ====

# 1.1.1 Keep only ex at birth for each country and cohort

ex_df <- 
  LTCF %>% 
  filter(Age == 0) %>% 
  # Round age
  mutate(ex_round = round(ex)) %>% 
  select(country = Country, cohort = Cohort, ex_round)

# Keep unique region/cohor combinations

# use ECL dataframe

country_cohort <- 
  df_cl_m_full %>% 
  filter(type == 'country') %>% 
  select(country, cohort) %>% 
  distinct()

life_expectancy <- merge(
  country_cohort
  , ex_df
  , by = c('country', 'cohort')
  , all.x = T
)

# life_expectancy is a df with the cohort ex for the birth cohorts of women
# The next step is to keep only those ages in the df with the child death values

ecl_ex <- merge(
  df_cl_m_full
  , life_expectancy
  , by.x = c("country", 'cohort', 'age')
  , by.y = c("country", 'cohort', 'ex_round')
  , all.x = F
  , all.y = T
) %>% 
  select(-age)

# 1.2. Denominator ====

# The denominator is the CTFR by birth cohort

ctfr <- 
  ASFRC %>% 
  filter(dplyr::between(Cohort, 1950, 1999)) %>% 
  group_by(country, Cohort) %>% 
  summarise(tfr = sum(ASFR)/1000) %>% 
  ungroup %>% 
  rename(cohort = Cohort) 

# 1.3. DF with enumerator and denominator

ecl_ctfr <- merge(
  ecl_ex
  , ctfr
  , by = c("country", "cohort")
  , all.x = T
  , all.y = F
) %>% 
  filter(type == 'country')

# 1.4. Stat summary by group ====

# Now it is possible to group the data and get summary stat

quant_low <- 0.40
quant_high <- 0.60

sum_ecl_ctfr <-
  ecl_ctfr %>%
  mutate(share = 1 - value / tfr) %>% 
  group_by(region, cohort) %>%
  summarise(
    value = median(share)
    , low_iqr = quantile(share, quant_low)
    , high_iqr = quantile(share, quant_high)
  ) %>%
  ungroup %>% 
  mutate(cohort = as.numeric(cohort))

# Do the same, but for child survival

sum_ecs_ctfr <-
  ecl_ctfr %>%
  mutate(share = value / tfr) %>% 
  group_by(region, cohort) %>%
  summarise(
    value = median(share)
    , low_iqr = quantile(share, quant_low)
    , high_iqr = quantile(share, quant_high)
  ) %>%
  ungroup %>% 
  mutate(cohort = as.numeric(cohort))

# 3.3. Export ====

saveRDS(ecl_ctfr, file = "../../Data/estimates/ecl_ctfr.RDS")
saveRDS(sum_ecl_ctfr, file = "../../Data/estimates/sum_ecl_ctfr.RDS")
saveRDS(sum_ecs_ctfr, file = "../../Data/estimates/sum_ecs_ctfr.RDS")
