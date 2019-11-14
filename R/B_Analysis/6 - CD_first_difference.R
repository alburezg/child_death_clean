print(paste0("Running script: 6 - CD_first_difference"))

# The first difference of the expected child death is equivalent
# to the number of child deaths experienced by a woman
# aged 'a' exactly (i.e. at each age 'a')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data requirements: 
# df_cl_m_full <- readRDS('../../Data/estimates/df_cl_m_1950to1999_15to100.RDS')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Diference df ~~~~ ----

# Get differences by group for all countries and regions


val_list <- split(
  df_cl_m_full 
  , list(df_cl_m_full$country, df_cl_m_full$cohort)
  )

names(val_list) <- paste0(names(val_list), ".")

# Add NA to the start of each series since I am not considering the prvious age (eg 24)
# when getting the differences
diff_l <- lapply(val_list, function(df) {
  df$diff <- c(diff(df$value, differences = 1), NA)
  df$value <- NULL
  df
}) 

df_cl_diff <- 
  rbindlist(diff_l, use.names = T) %>% 
  filter(type == "country") %>% 
  mutate(cohort = as.numeric(cohort)) %>% 
  arrange(country, cohort, age) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A. Using individual countries ~~~~ ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Stat summary by group ====

quant_low <- 0.40
quant_high <- 0.60

# DEPRECATED: Tidyverse approach too slow
# sum_diff <-
#   df_cl_diff %>%
#   group_by(region, age, cohort) %>%
#   summarise(
#     median = median(diff, na.rm = T)
#     , low_iqr = quantile(diff, quant_low, na.rm = T)
#     , high_iqr = quantile(diff, quant_high, na.rm = T)
#   ) %>%
#   ungroup()
  

# Data.table alternative is preffered

# Get summary measures
sum_diff <- data.table(df_cl_diff)[ , list(median = median(diff, na.rm = T), low_iqr = quantile(diff, quant_low, na.rm = T), high_iqr = quantile(diff, quant_high, na.rm = T)), by = list(region, age, cohort)]

sum_diff <- 
  sum_diff %>% 
  mutate(cohort2 = paste0("Women born in", cohort)) %>% 
  arrange(region, cohort, age) 

# 2. Export ====

saveRDS(sum_diff, file = "../../Data/estimates/sum_diff.RDS")
saveRDS(df_cl_diff, file = "../../Data/estimates/df_cl_diff.RDS")
