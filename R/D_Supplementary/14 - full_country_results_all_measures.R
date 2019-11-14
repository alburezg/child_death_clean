
# Export full country-cohort-age results for Tables S6-10 as csv files
# The resulting files are quite large in size and are not included in 
# the SI pdf file.

# This script generates complete tables of child death and child survival estimates
# for supplementary materials.
# This script basically generates the underlying data used to produce 
# Figures 2-4 in the main text, plus data on all other cohorts (1950-1999) 
# which are not included in the figures, but whose values were estimated.

# The tables are exported as csv files in long format, which is easier to manipulate.
# The tables include estimates for all countries and regions, including 
# Australia and NZ, which are not included in the figures in the main text.
# The regional estimates include  median and IQR values, computed from the 
# individual-country estimates. Details of the estimation can be found in the main 
# text and in the Supporting Information.

# One table is generated for each different region and for country and regional-level
# estimates. Tables follow the following naming convention:
# [Figure to which the data corresponds in the main text]
# [whether country or regional estimates]
# [type of measure]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data required for this script: 
# Child death: 
# df_cl_m_full
# Child survival
# df_cs_m_full
# cs_pop
# df_cl_diff
# abs_df
# cs_ex_pop_country
# cl_ex_pop_country
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Data from Fig 1 ~~~~ ----

# 1.1 Cohort ex 

ex_df <- 
  LTCB %>% 
  filter(dplyr::between(Cohort, lower_year, upper_year)) %>%
  filter(Age == 0) %>% 
  select(country = Country, cohort = Cohort, ex)

# 1.2. Cohort TFR

ctfr <- 
  ASFRC %>% 
  filter(dplyr::between(Cohort, lower_year, upper_year)) %>%
  group_by(country, Cohort) %>% 
  summarise(tfr = sum(ASFR)/1000) %>% 
  ungroup %>% 
  rename(cohort = Cohort) 

# 1.3. Merge and get regions

ex_ctfr <- merge(
  ex_df
  , ctfr
  , by = c('country', 'cohort')
)

# Country level

ex_ctfr_con <- 
  merge(
    ex_ctfr
    , un_reg
    , by.x = 'country'
    , by.y = 'level1'
    , all.x = T
    , all.y = F
  ) %>% 
  mutate(
    region = factor(default_region, levels = regions_long)
  ) %>% 
  filter(type == 'country') %>% 
  select(country, region, cohort, ex, tfr)

# Regional estimates

# 4. Summarise by region ====

ex_ctfr_sum <- 
  ex_ctfr_con %>% 
  group_by(region, cohort) %>% 
  summarise(
    ex = median(ex)
    , tfr = median(tfr)
  ) %>% 
  ungroup

print("8 - tab.2.1_child_death_full saved to ../../Output")

# 2. Data from Fig 2 ~~~~ ----

# 2.1	Child death (CD) ====
# ~~~~~~~~~~~~~

# Full country results for the (cumulative) number of child deaths 
# for a woman surviving to selected ages (median and IQR).

cl_full <- merge(
  df_cl_m_full %>% 
    filter(type == "country")
  , female_births %>% 
    select(cohort = year, cohort_size = value, everything())
  , by = c('country', 'cohort')
  , all.x = T
)

# Get estimated values for countries

cl_countries <- 
  cl_full %>% 
  filter(type == 'country') %>% 
  mutate(
    region = as.character(region)
    , cohort = as.numeric(cohort)
    # , measure = "child_death"
    ) %>% 
  select(country, region, cohort, age, value) %>% 
  arrange(country, cohort, age)
  
# Get summary values (median and IQR) for regions

cl_regions <- 
  cl_countries %>% 
  group_by(region, age, cohort) %>%
  summarise(
    median = median(value)
    , iqr = IQR(value)
    , mean = mean(value)
    , sd = sd(value)
  ) %>%
  ungroup() %>% 
  mutate(
    region = as.character(region)
    # , measure = "child_death"
         ) %>% 
  arrange(region, cohort, age)

print("8 - tab.2.1_child_death_full saved to ../../Output")

# 2.2.	Child survival (CS) ====
# ~~~~~~~~~~~~~

# Full country results for the expected number of children surviving for a woman aged a (selected ages; median and IQR).

cs_full <- merge(
  df_cs_m_full %>% 
    filter(type == "country")
  , female_births %>% 
    select(cohort = year, cohort_size = value, everything())
  , by = c('country', 'cohort')
  , all.x = T
)

cs_countries <- 
  cs_full %>% 
  filter(type == 'country') %>% 
  mutate(
    region = as.character(region)
    , cohort = as.numeric(cohort)
    # , measure = "child_survival"
  ) %>% 
  select(country, region, cohort, age, value) %>% 
  arrange(country, cohort, age)

# Get summary values (median and IQR) for regions

cs_regions <- 
  cs_countries %>% 
  group_by(region, age, cohort) %>%
  summarise(
    median = median(value)
    , iqr = IQR(value)
    , mean = mean(value)
    , sd = sd(value)
  ) %>%
  ungroup() %>% 
  mutate(
    region = as.character(region)
    # , measure = "child_survival"
  ) %>% 
  arrange(region, cohort, age)


print("8 - tab.2.2_child_survival_full.csv saved to ../../Output")

# 3. Data from Fig 3 ~~~~ ----

# 3.1.	First difference of child death (DELTA CD) ====
# ~~~~~~~~~~~~~

# First difference of child death for a woman surviving to age $a$ 
# (full country results; selected ages; median and IQR).

diff_countries <- 
  df_cl_diff %>% 
  filter(type == 'country') %>% 
  mutate(
    region = as.character(region)
    , cohort = as.numeric(cohort)
    # , measure = "child_death_first_diff"
  ) %>% 
  select(country, region, cohort, age, value = diff) %>% 
  arrange(country, cohort, age)

diff_regions <- 
  diff_countries %>% 
  group_by(region, age, cohort) %>%
  summarise(
    median = median(value, na.rm = T)
    , iqr = IQR(value, na.rm = T)
    , mean = mean(value, na.rm = T)
    , sd = sd(value, na.rm = T)
  ) %>%
  ungroup() %>% 
  mutate(
    region = as.character(region)
    # , measure = "child_death_first_diff"
  ) %>% 
  arrange(region, cohort, age)

print("8 - tab.3.1_child_death_first_diff.csv saved to ../../Output")

# 3.2.	Burden of child death ====
# ~~~~~~~~~~~~~

abs_countries <- 
  abs_df %>% 
  filter(type == 'country') %>% 
  mutate(
    region = as.character(region)
    , cohort = as.numeric(cohort)
    # , measure = "child_death_burden"
  ) %>% 
  select(country, region, cohort, age, value = absolute) %>% 
  arrange(country, cohort, age)

abs_regions <- 
  abs_countries %>% 
  group_by(region, age, cohort) %>%
  summarise(
    median = sum(value, na.rm = T)
    , iqr = IQR(value, na.rm = T)
    , mean = mean(value, na.rm = T)
    , sd = sd(value, na.rm = T)
  ) %>%
  ungroup() %>% 
  mutate(
    region = as.character(region)
    # , measure = "child_death_burden"
  ) %>% 
  arrange(region, cohort, age)

print("8 - tab.3.2_child_death_burden.csv saved to ../../Output")

# 4. Data from Fig 4 ~~~~ ----

# 4.1.	Number of children expected to outlive mothers ====
# ~~~~~~~~~~~~~

csex_countries <- 
  cs_ex_pop_country %>% 
  filter(type == 'country') %>% 
  mutate(
    region = as.character(region)
    , cohort = as.numeric(cohort)
    # , measure = "num_children_outlive_mother"
  ) %>% 
  select(country, region, cohort, value) %>% 
  arrange(country, cohort)
  

csex_regions <- 
  csex_countries %>% 
  group_by(region, cohort) %>%
  summarise(
    median = median(value)
    , iqr = IQR(value)
    , mean = mean(value)
    , sd = sd(value)
  ) %>%
  ungroup() %>% 
  mutate(
    region = as.character(region)
    # , measure = "num_children_outlive_mother"
  ) %>% 
  arrange(region, cohort)

print("8 - tab.4.1_child_outlive_expected.csv saved to ../../Output")

# 4.2.	Share of children outlive mothers ====
# ~~~~~~~~~~~~~

out_countries <-
  ecl_ctfr %>%
  filter(type == 'country') %>% 
  mutate(
    region = as.character(region)
    # , measure = "share_children_outlive_mother"
  ) %>% 
  mutate(share = 1 - value / tfr) %>% 
  select(country, region, cohort, value = share) %>% 
  arrange(country, cohort)


out_regions <- 
  out_countries %>% 
  group_by(region, cohort) %>%
  summarise(
    median = median(value)
    , iqr = IQR(value)
    , mean = mean(value)
    , sd = sd(value)
  ) %>%
  ungroup() %>% 
  mutate(
    region = as.character(region)
    # , measure = "share_children_outlive_mother"
  ) %>% 
  arrange(region, cohort)


# 7 . Export ----

# 7.1. Country-level estimates ====

write.csv(ex_ctfr_con, "../../Output/Fig1_countries_TFR_e0.csv", row.names = F)

write.csv(cl_countries, "../../Output/Fig2_countries_child_death_cumulative.csv", row.names = F)
write.csv(cs_countries, "../../Output/Fig2_countries_child_survival_cumulative.csv", row.names = F)

write.csv(diff_countries, "../../Output/Fig3_countries_child_death_first_difference.csv", row.names = F)
write.csv(abs_countries, "../../Output/Fig3_countries_child_death_burden.csv", row.names = F)

write.csv(csex_countries, "../../Output/Fig4_countries_outlive_mother_number.csv", row.names = F)
write.csv(out_countries, "../../Output/Fig4_countries_outlive_mother_share.csv", row.names = F)

print("8 - complete country-level estimates saved to ../../Output")

# 7.2. Regional estimates ====

write.csv(ex_ctfr_sum, "../../Output/Fig1_regions_TFR_e0.csv", row.names = F)

write.csv(cl_regions, "../../Output/Fig2_regions_child_death_cumulative.csv", row.names = F)
write.csv(cs_regions, "../../Output/Fig2_regions_child_survival_cumulative.csv", row.names = F)

write.csv(diff_regions, "../../Output/Fig3_regions_child_death_first_difference.csv", row.names = F)
write.csv(abs_regions, "../../Output/Fig3_regions_child_death_burden.csv", row.names = F)

write.csv(csex_regions, "../../Output/Fig4_regions_outlive_mother_number.csv", row.names = F)
write.csv(out_regions, "../../Output/Fig4_regions_outlive_mother_share.csv", row.names = F)

print("8 - complete regional estimates saved to ../../Output")