print(paste0("Running script: ", "8 - CD_share_outlive_mother"))

# We defined the number of children expected to outlive their mothers $E[CS_{(-,c,r)}]$ in the main text as:
#   
#   \begin{equation}
# E[CS_{(-,c,r)}] = \frac{\sum_{a=15}^{e} d_{(a,c,r)} CS_{(a,c,r)}}{\sum_{a=15}^{e} d_{(a,c,r)}}
# \label{eq:ECS}
# \end{equation}
# 
# where $d_{(a,c,r)}$ is the cohort life table death distribution of women and $e$ is the life expectancy for women in cohort $c$ and region $r$.
# 
# The share of children that will outlive their mothers $SOM_{(-, c, r)}$ can then be defined by weighting equation \ref{eq:ECS} by the Cohort Total Fertility of women in cohort $c$ and region $r$, $CTFR_{(-, c, r)}$:
#   
#   
#   \begin{equation}
# SOM_{(-, c, r)} = \frac{E[CS_{(-,c,r)}]}{CTFR_{(-, c, r)}}.
# \end{equation}

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
