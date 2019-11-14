print(paste0("Running script: ", "11 - CS_regional_medians"))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Data requirements: 
# The ECL data frame created in this script can be loaded (if created) with
# df_cs_m_full <- readRDS('../../Data/estimates/df_cs_m_1950to1999_15to100.RData')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A. Group by country->region ~~~~ ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Weighted regional means ====

# Get weighted and IQR by region
# usig estimates for individual countries

cs_pop <- merge(
  df_cs_m_full 
  , female_births %>% 
    select(cohort = year, cohort_size = value, everything())
  , by = c('country', 'cohort')
  , all.x = T
)

# 3. Stat summary by group ====

quant_low <- 0.40
quant_high <- 0.60

# DEPRECATED: Tidyverse approach too slow
# sum_cs <-
#   cs_pop %>%
#   filter(type == "country") %>% 
#   group_by(region, age, cohort) %>%
#   summarise(
#     median = median(value)
#     , low_iqr = quantile(value, quant_low)
#     , high_iqr = quantile(value, quant_high)
#   ) %>%
#   ungroup() %>% 
#   mutate(cohort2 = paste0("Women born in ", cohort))

# Data.table alternative is preferred

cs_pop2 <- cs_pop %>% filter(type == "country")

sum_cs <- data.table(cs_pop2)[ , list(median = median(value, na.rm = T), low_iqr = quantile(value, quant_low, na.rm = T), high_iqr = quantile(value, quant_high, na.rm = T)), by = list(region, age, cohort)]

sum_cs <- 
  sum_cs %>% 
  mutate(
   cohort2 = paste0("Women born in ", cohort)
  ) %>% 
  arrange(region, cohort, age) 

# 1.3. Export ====

saveRDS(sum_cs, file = "../../Data/estimates/sum_cs.RDS")
saveRDS(cs_pop, file = "../../Data/estimates/cs_pop.RDS")
