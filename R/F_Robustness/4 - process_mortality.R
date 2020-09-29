
print("Running script: 6 - ungroup_mortality_from_mx.R")

# This script takes abridged period life  tables from the UN and creates complete life tables 
# (ie with single age groups) for all countries in the world. The UN life tables are grouped
# by 5-year age groups and 5-year calendar years.
# It does so by interpolatnig values on the mx column and creating life tables based on that
# column later on. 
# Afterwards, we interpolate again the values to create single-year complete (period) life tables.

# Note that not all resulting life tables have values for the upper age groups
# In some cases, mortality is high enough that there would be no surviving individuals
# to age 100, for example. In this case, the reuslting life table will only have rows 
# going up to age 95

# Data: https://population.un.org/wpp/Download/Standard/Mortality/

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The files created with this script can be loaded with:
# Women only
# lt_1_1_F <- data.table::fread(file = paste0("../../Data/derived/","lt_per_1_1_F.csv"), stringsAsFactors = F) %>% data.frame
# Both sexes
# lt_1_1_B <- data.table::fread(file = paste0("../../Data/derived/","lt_per_1_1_B.csv"), stringsAsFactors = F) %>% data.frame
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print('Expanding life tables for women.')
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

# Do all the mortality formating to return cohort single-age
# single-year life tables for robustness analysis
# and creates matrix of sruvival probs all in one sweep
# Saves output

if(re_estimate_matrix_of_survival_probs){
  # 1. Median LT ----
  
  process_mortality_robust_top(
    lt_per = lt_median
    , numCores = numCores
  )
  
  # 2. Lower95 LT ----
  
  process_mortality_robust_top(
    lt_per = lt_lower95
    , numCores = numCores
  )
  
  # 3. Upper95 LT ----
  
  process_mortality_robust_top(
    lt_per = lt_upper95
    , numCores = numCores
  )
  
  # 4. Constant LT ----
  
  process_mortality_robust_top(
    lt_per = lt_constant
    , numCores = numCores
  )
}


