
print("Running script: 7 - convert_period_LT_to_cohort_LT.R")

# This scripts takes a list of period life tables and converts them to psuedo-cohort life tables.
# It does so by extracting “pseudo-cohort” data by looking at diagonals of the period data. 
# the period LT cover the 1950-2100 period. We could have cohorts from 1950-1999. 
# The mortality rate for the 1950 cohort would be the 1m0 for 1950, 1m1 based on 1951 data, 
# 1m2 based on 1952 data, etc.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The files created with this script can be loaded with:
# get name_out below, in the parameters
# For women
# LTCF <- data.table::fread(file = paste0("../../Data/derived/", "LTCF.csv"), stringsAsFactors = F) %>% data.frame
# For both sexes
# LTCB <- read.csv(file = paste0("../../Data/derived/", "LTCB.csv"), stringsAsFactors = F)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Define function  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

# Use with WPP data formatted in previous script



# 2. Run for women LT ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

numCores <- ifelse(detectCores() > 8, 25, detectCores())

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print('7.1 - Converting LT from period to (pseudo) cohort for women.')
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")



# 3. Run for both-sex LT ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

numCores <- ifelse(detectCores() > 8, 25, detectCores())

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print('7.2 - Converting LT from period to (pseudo) cohort for both sexes.')
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

LTCB <- 
  convert_period_LT_to_cohort_LT(
  lt_1_1_B
  , sex = "B"
  , export = T
  , run_graphic_tests = F
  , years = 1950:2100
  , ages = 1:100
  , parallel = T
  , numCores = numCores
)

print("Script 7 - convert_period_LT_to_cohort_LT.R: success!")
