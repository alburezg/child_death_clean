source("../functions.R")

# Data wrangling
wrangling <- c("tidyverse", "scales", "knitr", "kableExtra", "maps", "countrycode", "viridis", "rworldmap", "patchwork")

library2(wrangling)

# Load data ----------------



# 1. UN WPP data ----
# ~~~~~~~~~~~~~~~~~~~~~~

# LTCB (both sexes)
LTCB <- data.table::fread(file = paste0("../../Data/derived/","LTCB.csv"), stringsAsFactors = F) %>% 
  data.frame

# LTCB (Women only)
LTCF <- data.table::fread(file = paste0("../../Data/derived/","LTCF.csv"), stringsAsFactors = F) %>% 
  data.frame

# ASFRC
ASFRC <- read.csv(file = paste0("../../Data/derived/","ASFRC.csv"), stringsAsFactors = F)

# un_regions
un_regions <- read.csv(file = paste0("../../Data/wpp_data/un_regions.csv"), stringsAsFactors = F)

# For recoding countries
cont <- read.csv(file = paste0("../../Data/wpp_data/countries_code.csv"), stringsAsFactors = F)


female_births <- read.csv(
  file = paste0("../../Data/derived/","wpp_female_births_1_1.csv")
  , stringsAsFactors = F
)

# 2. Child death ----
# ~~~~~~~~~~~~~~~~~~~~~~

# df_cl_m_full
df_cl_m_full <- readRDS('../../Data/estimates/df_cl_m_1950to1999_15to100.RDS')

# Regional median of child death:
# sum_cl
sum_cl <- readRDS('../../Data/estimates/sum_cl.RDS')

# ecl_ctfr
ecl_ctfr <- readRDS("../../Data/estimates/ecl_ctfr.RDS")

# sum_cl_ex
sum_cl_ex <- readRDS("../../Data/estimates/sum_cl_ex.RDS")

# First difference child death:
# df_cl_diff
df_cl_diff <- readRDS("../../Data/estimates/df_cl_diff.RDS")

# Regional median of first difference of child death:
# sum_diff
sum_diff <- readRDS('../../Data/estimates/sum_diff.RDS')

# Burden of child death:
# At each age: 
abs_df <- readRDS("../../Data/estimates/abs_df.RDS")

# Summed over all ages (up to life expectancy):
sum_burden <- readRDS("../../Data/estimates/sum_burden.RDS")


# sum_abs
sum_abs <- readRDS('../../Data/estimates/sum_abs.RDS')

# Expected children die before mothers
# cl_ex_pop_country
cl_ex_pop_country <- readRDS('../../Data/estimates/cl_ex_pop_country.RDS')

sum_ecl_ctfr <- readRDS('../../Data/estimates/sum_ecl_ctfr.RDS')

# 3. Child survival ----
# ~~~~~~~~~~~~~~~~~~~~~~

# df_cs_m_full
df_cs_m_full <- readRDS('../../Data/estimates/df_cs_m_1950to1999_15to100.RDS')

# Regional median of child death:
# sum_cs
sum_cs <- readRDS('../../Data/estimates/sum_cs.RDS') %>%
  arrange(region, cohort, age) 

# cs_pop
cs_pop <- readRDS('../../Data/estimates/cs_pop.RDS')

# Expected children outlived mothers
# cs_ex_pop_country
cs_ex_pop_country <- readRDS("../../Data/estimates/cs_ex_pop_country.RDS")

# sum_cs_ex
sum_cs_ex <- readRDS("../../Data/estimates/sum_cs_ex.RDS")

# sum_ecs_ctfr
sum_ecs_ctfr <- readRDS('../../Data/estimates/sum_ecs_ctfr.RDS')

print("2 - All data loaded!")

# Region grouping ---------------

# Decide how countries will be groupped in the analysis
# On 20191010, we had decided to group all countries by UN SDG region 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data requirements: 
# un_regions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

un_reg <- 
  un_regions %>% 
  # fix text formatting
  mutate_all(.funs = fix_un_countries) %>% 
  # Chose which regions should be used as default
  # Very important as this will shape the final analysis
  # [PREFERRED 20190814]: Use UN SDG regions but remove UAS/NZ and Ocenia (other)
  mutate(default_region = un_sdg_groups)

# Define labels for using in plots later on

# regions_long <- c(
#   "sub-saharan africa"
#   , "northern africa and western asia"
# 
#   , "latin america and the caribbean"
#   , "europe and northern america"
#   
#   , "eastern and south-eastern asia"
#   , "central and southern asia"
#   
#   , "australia_new zealand"
#   , "oceania (excluding australia and new zealand)"
# )
# 
# regions_short <- c(
#   "Sub-Sah Africa"
#   , "N Africa & W Asia"
#   
#   , "LATAM & Caribbean"
#   , "Europe & N America"
#   
#   , "E & SE Asia"
#   , "C & S Asia"
#   
#   , "AUS & NZ"
#   , "Oceania (other)"
# )

regions_long <- c(
  "sub-saharan africa"
  , "eastern and south-eastern asia"
  
  , "northern africa and western asia"
  , "latin america and the caribbean"
  
  , "central and southern asia"
  , "europe and northern america"
  
  , "australia_new zealand"
  , "oceania (excluding australia and new zealand)"
)

regions_short <- c(
  "Sub-Saharan Africa"
  , "East & SE Asia"
  
  , "North Africa & West Asia"
  , "LATAM & Caribbean"
  
  , "Central & South Asia"
  , "Europe & N America"
  
  , "AUS & NZ"
  , "Oceania (other)"
)

# These regions are not included in the plots, but estimates are presented 
# in the Supplementary Materials of the paper
regions_to_remove <- c("oceania (excluding australia and new zealand)", "australia_new zealand")
