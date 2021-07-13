

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

library(parallel)
library(countrycode)

# country_keep <- c("Guatemala", "Zimbabwe")
country_keep <- c("Guatemala")
baseline_year_constant_rates <- "2000-2005"
numCores <- ifelse(detectCores() > 8, 25, 3)
re_estimate_matrix_of_survival_probs <- T

(files <- list.files(pattern = ".R$")[-1])

# A. Expand fertility and mortality rates ----------------

# 1. Load the functions and packages needed in the scripts ====

source(files[1])

# 2. Load the data needed for the analysis ====

source(files[2])

# 3. Format fertility rates ----

source(files[3])

# 4. Format Mortality ====

source(files[4])

# Estimate child death
source(files[5])

# Plot comparisson

source(files[6])
