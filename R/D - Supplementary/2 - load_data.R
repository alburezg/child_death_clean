

# 1. UN WPP data ----
# ~~~~~~~~~~~~~~~~~~~~~~

# LTCB (both sexes)
LTCB <- data.table::fread(file = paste0("../../Data/derived/","LTCB.csv"), stringsAsFactors = F) %>% 
  data.frame

# ASFRC
ASFRC <- read.csv(file = paste0("../../Data/derived/","ASFRC.csv"), stringsAsFactors = F)

# un_regions
un_regions <- read.csv(file = paste0("../../Data/wpp_data/","un_regions.csv"), stringsAsFactors = F)

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
# abs_df
abs_df <- readRDS("../../Data/estimates/abs_df.RDS")

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
