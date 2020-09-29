#!/usr/bin/env Rscript

#*********************************

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Files needed (these should be in ../../Data/):

# - Files in /wpp_data/ are the original WPP data without 
# any previous formatting they were all downloaded from 
# https://population.un.org/wpp/Download/Standard/CSV/

# wpp_data/","WPP2019_Period_Indicators_Medium.csv
# wpp_data/WPP2019_TotalPopulationBySex.csv
# wpp_data/WPP2019_PopulationByAgeSex_Medium.csv

# - Files in /derived/ were created in the "A - Data formatting" cycle
# derived/ASFRC.csv
# derived/LTCF.csv
# derived/LTCB.csv
# derived/wpp_female_births_1_1.csv
# derived/wpp_all_births_1_1.csv

# The full list of all lx.kids.arr_{country} combinations should also be 
# in the /derived folder. They are sourced by the get_lx_array() function
# later on

# This one was downloaded from the WPP website and edited by hand for ease of use:
# wpp_data/un_regions.csv 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Read wpp data 

# csv version. including with different indicators for the medium scenario

wpp_period_med <- read.csv(
  file = paste0("../../Data/wpp_data/","WPP2019_Period_Indicators_Medium.csv")
  , stringsAsFactors = F
  ) %>% 
  mutate(
    region = tolower(Location)
    , region = fix_un_countries(region)
  ) %>% 
  select(-Location)

# 1. Fertility data ----

# Note that these are the ungrouped version of the UN data, which is
# groups in 5-age groups for every 5 calendar-year interva
# I did the ungrouping with the scripts in foled WPP_ungroup_data

# Period fertility data ====
# UN fertility projections

fert_wpp_per <- wpp_period_med %>% 
  select(
    region
    , period = Time
    , TFR
    , CBR
    , births = Births
    )

# Cohort ASFR ====

# , derived from WPP data in previous script

ASFRC <- read.csv(file = paste0("../../Data/derived/","ASFRC.csv"), stringsAsFactors = F)

# Mean age at childbirth (MAC)

mac_wpp_per <- wpp_period_med %>% 
  select(
    region
    , period = Time
    , value = MAC
  )

# 2. Mortality data ----

# Cohort life tables ====

# Female
LTCF <- data.table::fread(file = paste0("../../Data/derived/","LTCF.csv"), stringsAsFactors = F) %>% 
  data.frame
# Both sex
LTCB <- data.table::fread(file = paste0("../../Data/derived/","LTCB.csv"), stringsAsFactors = F) %>% 
  data.frame

# 3. Birth cohort size ----

# 3.1. Female birth cohorts ====

female_births <- read.csv(
  file = paste0("../../Data/derived/","wpp_female_births_1_1.csv")
  , stringsAsFactors = F
  )

all_births <- read.csv(
  file = paste0("../../Data/derived/","wpp_all_births_1_1.csv")
  , stringsAsFactors = F
)

# 4. Total population ====

# UN WPP medium scenario
# https://population.un.org/wpp/Download/Standard/CSV/

# Total popualtion by sex

world_pop <- 
  read.csv("../../Data/wpp_data/WPP2019_TotalPopulationBySex.csv", stringsAsFactors = F) %>% 
  filter(Variant == "Medium") %>% 
  select(
    region = Location
    , year = Time
    , pop_m = PopMale
    , pop_f = PopFemale
    , pop_all = PopTotal
  ) %>% 
  mutate_at(c('pop_m', 'pop_f', "pop_all"), function(col) col*1000) %>% 
  mutate(
    region = tolower(region)
    , region = fix_un_countries(region)
  )

# World populatio by sex and age group

world_pop_age <- 
  read.csv("../../Data/wpp_data/WPP2019_PopulationByAgeSex_Medium.csv", stringsAsFactors = F) %>% 
  select(
    country = Location
    , year = Time
    , agegr = AgeGrp
    , agegr_start = AgeGrpStart
    , pop_m = PopMale
    , pop_f = PopFemale
    , pop_all = PopTotal
    ) %>% 
  mutate_at(c('pop_m', 'pop_f', "pop_all"), function(col) col*1000) %>% 
  mutate(
    country = tolower(country)
    , country = fix_un_countries(country)
    )

# 5. UN regions ====

un_regions <- read.csv(file = paste0("../../Data/wpp_data/","un_regions.csv"), stringsAsFactors = F)

print("UN data loaded (ungrouped)")
print(Sys.time())
#*********************************