

# *~^**~^**~^**~^**~^**~^**~^*#
# Code by                     #
# Diego Alburez-Gutierrez     #
# gatemonte@gmail.com         #
# @d_alburez                  #
# unless stated otherwise.    #
# Last edited 20200110        #
# GNU GENERAL PUBLIC LICENSE  #
# Version 3, 29 June 2007     #
# *~^**~^**~^**~^**~^**~^**~^*#


# This should be a self-contained code that takes as input UNWPP rates downloaded
# directly from the website and performs robustness checks for the main outcome of
# the study: cumulative child death for a woman surviving to age a
# I want to do this for a single country under three fertility assumptions
# and three mortality assumptions. The plot should look like this:
# Rows (mortality assumptions): Normal, 80 PI (prediction interval), and constant
# cols (fert): low, medium, high, constant
# constant/constant is stable population

country_keep <- c("Guatemala", "Zimbabwe")

(files <- list.files(pattern = ".R$")[-1])

# A. Expand fertility and mortality rates ----------------

# 1. Load the functions and packages needed in the scripts ====

source(files[1])

# 2. Load the data needed for the analysis ====

source(files[2])

# 3. Format fertility rates ----


source(files[3])

# 4. Format Mortality ====

# Using the interpolated values from the previous script, we approximate cohort fertility 
# by taking values on the diagonal. 
# For example, for the 1950 "cohort", 1_f_15 would be based on fertility rates for 1965, 
# the 1_f_16 for the period data from 1966, etc.

source(files[4])

# B. Analysis -------------

get_cumlative_child_death_robust <- function(){
  
  
  
}