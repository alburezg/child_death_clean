
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code to reshape UN WPP population estimates and period demographic rates.
# The scripts in this directory will
#    1. Load the functions and packages needed in the scripts
#    2. Load the data needed for the analysis 
#  A. Format fertility rates
#    3. Interpolate single-age and single-year rates from the UN grouped rates
#    4. Approximate cohort rates using values on the diagonal of the period ones
#  B. Format population data
#    5. Estimate size of annual female birth cohorts
#  C. Format mortality rates
#    6. Expand the UN abridged period life tables by interpolating on the nMx 
#       column and by single-year
#    7. Approximate cohort life tables using values on the diagonal of the period ones
#    8. Create matrices of survival probabilities for all UN regions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(files <- list.files(pattern = ".R$")[-1])


# 1. Load the functions and packages needed in the scripts ====

source(files[1])

# 2. Load the data needed for the analysis ====

source(files[2])

# A. Format fertility rates ----

# 3. Interpolate single-age and single-year rates from the UN grouped rates ====

# This script takes 5-year gropued age specific rates reported and projected by 
# the UN and returns 1x1 projected period ASFR for all countries in the world

source(files[3])

# 4. Approximate cohort rates using values on the diagonal of the period ones ====

# Using the interpolated values from the previous script, we approximate cohort fertility 
# by taking values on the diagonal. 
# For example, for the 1950 "cohort", 1_f_15 would be based on fertility rates for 1965, 
# the 1_f_16 for the period data from 1966, etc.

source(files[4])

# B. Format population data ----

# 5. Estimate size of annual female birth cohorts ====

# We require the size of annual female birth cohorts by UN region.
# However, the WPP only reports the number of births (both sexes) in 5-year bands.
# To estimate the yearly number of female births (ie "female birth cohort size"):
#  a. multiply the 5-year number of births by the sex ratio at birth, and
#  b. ungroup to year-specific estimates using linear interpolation.

source(files[5])

# C. Format mortality rates ----

# 6. Expand the UN abridged period life tables by interpolating on the nMx column ====

# This script takes abridged period life  tables from the UN and creates complete life tables 
# (ie with single age groups) for all countries in the world. The UN life tables are grouped
# by 5-year age groups and 5-year calendar years.
# It does so by interpolatnig values on the mx column and creating life tables based on that
# column later on. 
# Afterwards, we interpolate again the values to create single-year complete (period) life tables.

source(files[6])

# 7. Approximate cohort life tables using values on the diagonal of the period ones ====

# This scripts takes a list of period life tables and converts them to pseudo-cohort life tables.
# It does so by extracting “pseudo-cohort” data by looking at diagonals of the period data. 
# the period LT cover the 1950-2100 period. We could have cohorts from 1950-1999. 
# The mortality rate for the 1950 cohort would be the 1m0 for 1950, 1m1 based on 1951 data, 
# 1m2 based on 1952 data, etc.

source(files[7])

# 8. Create matrices of survival probabilities for all UN regions ====

# The function creates is a matrix of the survival probabilities of children. 
# There is one matrix for each birth cohort of mothers showing the probability
# that a child will reach a certain age.

source(files[8])
