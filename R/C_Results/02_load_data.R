
# 1. UN WPP data ----
# ~~~~~~~~~~~~~~~~~~~~~~

# LTCB (Women only)
LTCF <- data.table::fread(file = paste0("../../Data/derived/","LTCF.csv"), stringsAsFactors = F) %>% 
  data.frame

# ASFRC
ASFRC <- read.csv(file = paste0("../../Data/derived/","ASFRC.csv"), stringsAsFactors = F)
# ASFRC <- read.csv(file = paste0("../../Data/derived/","ASFRC_",fertility_variant,".csv"), stringsAsFactors = F)

# un_regions
un_regions <- read.csv(file = paste0("../../Data/wpp_data/","un_regions.csv"), stringsAsFactors = F)

female_births <- read.csv(
  # file = paste0("../../Data/derived/","wpp_female_births_1_1_",fertility_variant,".csv")
  file = paste0("../../Data/derived/","wpp_female_births_1_1.csv")
  , stringsAsFactors = F
)

# 2. Child death ----
# ~~~~~~~~~~~~~~~~~~~~~~

# df_cl_m_full
# df_cl_m_full <- readRDS(paste0("../../Data/estimates/df_cl_m_1950to1999_15to100_",fertility_variant,".RDS"))
df_cl_m_full <- readRDS(paste0("../../Data/estimates/df_cl_m_1950to1999_15to100.RDS"))

# Regional median of child death:
# sum_cl
sum_cl <- readRDS(paste0("../../Data/estimates/sum_cl.RDS"))
# sum_cl <- readRDS(paste0("../../Data/estimates/sum_cl_",fertility_variant,".RDS"))

# ecl_ctfr
ecl_ctfr <- readRDS(paste0("../../Data/estimates/ecl_ctfr.RDS"))
# ecl_ctfr <- readRDS(paste0("../../Data/estimates/ecl_ctfr_",fertility_variant,".RDS"))

# sum_cl_ex
sum_cl_ex <- readRDS(paste0("../../Data/estimates/sum_cl_ex.RDS"))
# sum_cl_ex <- readRDS(paste0("../../Data/estimates/sum_cl_ex_",fertility_variant,".RDS"))

# First difference child death:
# df_cl_diff
df_cl_diff <- readRDS(paste0("../../Data/estimates/df_cl_diff.RDS"))
# df_cl_diff <- readRDS(paste0("../../Data/estimates/df_cl_diff_",fertility_variant,".RDS"))

# Regional median of first difference of child death:
# sum_diff
sum_diff <- readRDS(paste0("../../Data/estimates/sum_diff.RDS"))
# sum_diff <- readRDS(paste0("../../Data/estimates/sum_diff_",fertility_variant,".RDS"))

# Burden of child death:
# abs_df
abs_df <- readRDS(paste0("../../Data/estimates/abs_df.RDS"))
# abs_df <- readRDS(paste0("../../Data/estimates/abs_df_",fertility_variant,".RDS"))

# sum_abs
sum_abs <- readRDS(paste0("../../Data/estimates/sum_abs.RDS"))
# sum_abs <- readRDS(paste0("../../Data/estimates/sum_abs_",fertility_variant,".RDS"))

# Summed over all ages (up to life expectancy):
sum_burden <- readRDS(paste0("../../Data/estimates/sum_burden.RDS"))
# sum_burden <- readRDS(paste0("../../Data/estimates/sum_burden_",fertility_variant,".RDS"))

# Expected children die before mothers
# cl_ex_pop_country
cl_ex_pop_country <- readRDS(paste0("../../Data/estimates/cl_ex_pop_country.RDS"))
# cl_ex_pop_country <- readRDS(paste0("../../Data/estimates/cl_ex_pop_country_",fertility_variant,".RDS"))

sum_ecl_ctfr <- readRDS(paste0("../../Data/estimates/sum_ecl_ctfr.RDS"))
# sum_ecl_ctfr <- readRDS(paste0("../../Data/estimates/sum_ecl_ctfr_",fertility_variant,".RDS"))

# 3. Child survival ----
# ~~~~~~~~~~~~~~~~~~~~~~

# df_cs_m_full
df_cs_m_full <- readRDS(paste0("../../Data/estimates/df_cs_m_1950to1999_15to100.RDS"))
# df_cs_m_full <- readRDS(paste0("../../Data/estimates/df_cs_m_1950to1999_15to100_",fertility_variant,".RDS"))

# Regional median of child death:
# sum_cs
sum_cs <- readRDS(paste0("../../Data/estimates/sum_cs.RDS")) %>%
# sum_cs <- readRDS(paste0("../../Data/estimates/sum_cs_",fertility_variant,".RDS")) %>%
  arrange(region, cohort, age) 

# cs_pop
cs_pop <- readRDS(paste0("../../Data/estimates/cs_pop.RDS"))
# cs_pop <- readRDS(paste0("../../Data/estimates/cs_pop_",fertility_variant,".RDS"))

# Expected children outlived mothers
# cs_ex_pop_country
# cs_ex_pop_country <- readRDS(paste0("../../Data/estimates/cs_ex_pop_country_",fertility_variant,".RDS"))
cs_ex_pop_country <- readRDS(paste0("../../Data/estimates/cs_ex_pop_country.RDS"))

# sum_cs_ex
sum_cs_ex <- readRDS(paste0("../../Data/estimates/sum_cs_ex.RDS"))
# sum_cs_ex <- readRDS(paste0("../../Data/estimates/sum_cs_ex_",fertility_variant,".RDS"))

# sum_ecs_ctfr
sum_ecs_ctfr <- readRDS(paste0("../../Data/estimates/sum_ecs_ctfr.RDS"))
# sum_ecs_ctfr <- readRDS(paste0("../../Data/estimates/sum_ecs_ctfr_",fertility_variant,".RDS"))

print("2 - All data loaded!")
