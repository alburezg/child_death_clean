
print("Running script: 4 - convert_period_asfr_to_cohort_asfr.R")

# Using the interpolated values from the previous script, we approximate cohort fertility 
# by taking values on the diagonal. 
# For example, for the 1950 "cohort", 1_f_15 would be based on fertility rates for 1965, 
# the 1_f_16 for the period data from 1966, etc.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The files created with this script can be loaded with:
# ASFRC <- read.csv(file = paste0("../../Data/derived/","ASFRC.csv"), stringsAsFactors = F)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Parameters ----

export <- T
visual_examination <- F

# 2. Get cohort asfr df  ----

# fert_per_1_1 was created in the previous script and can also be read from the corresponding csv file
# it is the ungrouped age-specific fertility rates for 1-calendar-year intervals for all countries and regions
# in the world, derived from the WPP 2019 data

# In order to create pseudo-cohort fertility data, 
# the first step is to create a 'cohort' column

ASFRC_temp <- 
  fert_per_1_1 %>% 
  mutate(cohort = year - age) %>% 
  select(country, Cohort = cohort, Age = age, ASFR = value) %>% 
  arrange(country, Cohort, Age)

# 2.2. Add all cohort/age combinations ====

coh <- sort( unique(ASFRC_temp$Cohort) )
a <- sort(unique(ASFRC_temp$Age))
con <- as.character( unique(ASFRC_temp$country) )

len_con <- length(a) * length(coh)
len_a <- length(con) * length(coh)

coh2 <- sort(rep(coh, length(a)))

comb <- data.frame(
  country = sort(rep(con, len_con))
  , Cohort = rep(coh2, length(con))
  , Age = rep(a, len_a)
) %>% 
  arrange(country, Cohort, Age) 
  
# Merge with created data structure
ASFRC <- merge(
  ASFRC_temp
  , comb
  , all = T
) %>% 
  arrange(country, Cohort, Age)

# View(ASFRC_complete)

# 3. Checks ----

# Pending

# 4. Export ----

if(export) {
  write.csv(x = ASFRC, file = paste0("../../Data/derived/", "ASFRC.csv"), row.names = F)
}

print("4 - convert_period_asfr_to_cohort_asfr: success!")
