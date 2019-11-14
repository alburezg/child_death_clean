
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Scripts to produce figures and tables in the Supplemetnary Information 
# of the paper (S1-S10). 
# Note that all of the analysis has already been done before and these
# scripts only prduce plots and tables using data generated in previous scripts. 
# The scripts in this directory should be run sequentially to:
#    1. Load the functions and packages needed in the scripts 
#    2. Load the data needed for the analysis 
#    3. Load data about country grouping and format them for the analysis  
#    4. Produce Fig S1 (CD) - global estimates 
#    5. Produce FigS2 - Global burden of child death by region 
#    6. Produce Fig S3 - Children outliving their mothers - global estimates 
#    7. Produce Fig S4 - Expected child survival and demographic transition 
#    8. Produce Table S5 - Cumulative number of child death for a woman living to age a (CD) 
#    9. Produce Table S6 - Number of surviving children for a woman living to age a (CS) 
#   10. Produce Table S7 - First difference of child death for a woman living to age a (Delta CD) 
#   11. Produce Table S8 - Burden of child death at each age a 
#   12. Produce Table S9 - Number of children expected to outlive their mothers 
#   13. Produce Table S10 - Children outliving their mothers as a share of the mother’s cohort TFR 
#   14. Tables with full country results 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require("stringr")) {
  install.packages("stringr")
  library(stringr)
} 

files <- list.files(pattern = ".R$")[-1]
( files <- str_sort(files, numeric = TRUE) )

# 1. Load the functions and packages needed in the scripts ====

source(files[1])

# 2. Load the data needed for the analysis ====

source(files[2])

# 3. Load data about country grouping and format them for the analysis ==== 

source(files[3])

# 4. Produce Fig S1 (CD) - global estimates ====



source(files[4])

# 5.  Produce FigS2 - Global burden of child death by region ====



source(files[5])

# 6.  Produce Fig S3 - Children outliving their mothers - global estimates ====



source(files[6])

# 7.  Produce Fig S4 - Expected child survival and demographic transition ====



source(files[7])

# 8.  Produce Table S5 - Cumulative number of child death for a woman living to age a (CD) ====

# Full country results for the cumulative number of child deaths 
# for a woman surviving to ages 20, 45, and 100. Regional estimates 
# show the median value and IQR in parenthesis.

source(files[8])

# 9.  Produce Table S6 - Number of surviving children for a woman living to age a (CS) ====

# Full country results for the expected number of children surviving for a 
# woman surviving to ages 20, 45, and 100. Regional estimates show the median 
# value and IQR in parenthesis.

source(files[9])

# 10.  Produce Table S7 - First difference of child death for a woman living to age a (Delta CD) ====

# First difference of child death for a woman surviving to ages 20, 45, and 100. 
# Regional estimates show the median value and IQR in parenthesis.

source(files[10])

# 11.  Produce Table S8 - Burden of child death at each age a ====

# Child deaths experienced by women in birth cohort c at exact age a. 
# Obtained by weighting the first difference of child death by the life table 
# distribution of women. Regional values include point estimate and standard deviation, 
# in hundreds of thousands.

source(files[11])

# 12.  Produce Table S9 - Number of children expected to outlive their mothers ====

# Number of children expected to live longer than their mothers, asuming 
# that the mothers survive to mean age at death (life expectancy) in 
# their cohort and country of birth. Regional estimates show the median 
# value and IQR in parenthesis.

source(files[12])

# 13.  Produce Table S10 - Children outliving their mothers as a share of the mother’s cohort TFR ====

# Number of children expected to live longer than their mothers 
# as a share of the woman's cohort TFR. Regional estimates show 
# the median value and IQR in parenthesis.

source(files[13])

# 14. Tables with full country results ====

# Export full country-cohort-age results for Tables S6-10 as csv files
# The resulting files are quite large in size and are not included in 
# the SI pdf file.

# This script generates complete tables of child death and child survival estimates
# for supplementary materials.
# This script basically generates the underlying data used to produce 
# Figures 2-4 in the main text, plus data on all other cohorts (1950-1999) 
# which are not included in the figures, but whose values were estimated.

# The tables are exported as csv files in long format, which is easier to manipulate.
# The tables include estimates for all countries and regions, including 
# Australia and NZ, which are not included in the figures in the main text.
# The regional estimates include  median and IQR values, computed from the 
# individual-country estimates. Details of the estimation can be found in the main 
# text and in the Supporting Information.

# One table is generated for each different region and for country and regional-level
# estimates. Tables follow the following naming convention:
# [Figure to which the data corresponds in the main text]_[whether country or 
# regional estimates]_[type of measure]

# NOT RUN:
# source(files[13])
