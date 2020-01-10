

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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Scripts to produce figures and tables in the Supplementary Information 
# of the paper (S1-S10). 
# Note that all of the analysis has already been done before and these
# scripts only prduce plots and tables using data generated in previous scripts. 
# The scripts in this directory should be run sequentially to:
#    1. Load the functions and packages needed in the scripts 
#    2. Load the data needed for the analysis 
#    3. Load data about country grouping and format them for the analysis  
#    4. Fig S1 (CD) - global estimates 
#    5. FigS2 - Global burden of child death by region 
#    6. Fig S3 - Children outliving their mothers - global estimates 
#    7. Fig S4 - Expected child survival and demographic transition 
#    8. Table S1 - Cumulative number of child death for a woman living to age a (CD) 
#    9. Table S2 - Number of surviving children for a woman living to age a (CS) 
#   10. Table S3 - First difference of child death for a woman living to age a (Delta CD) 
#   11. Table S4 - Burden of child death at each age a 
#   12. Table S5 - Number of children expected to outlive their mothers 
#   13. Table S6 - Children outliving their mothers as a share of the mother’s cohort TFR 
#   14. Tables with full country results (Datasets_S1_S2)
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

# 4. Fig S1 (CD) - global estimates ====

# Global patterns of child death and child survival 
# for women in three different birth cohorts.
# (A) Cumulative number of child deaths (CD) 
# experienced by a woman reaching age $a$.
# (B) Total number of children surviving (CS) 
# for a woman reaching age $a$. Values in the vertical 
# axis represent the total number of children `currently alive'.
# (C) Number of child deaths experienced at each age $a$ by a woman 
# reaching that age (i.e. conditional on female survival).
# This is the First Difference of Child Death ($\Delta CD$).
# (D) Burden of child death: total number of child deaths experienced by 
# all women in a given birth cohort at each age $a$ (in millions).
# The solid lines represent median values and the bands the variability 
# among countries for each cohort.

source(files[4])

# 5.  FigS2 - Global burden of child death by region ====

# Global trends in the absolute and relative number of children 
# expected to live longer than their mothers. 
# (A) Number of children expected to outlive an average woman.
# Values in the vertical axis show the number of children alive 
# at the time of a woman's death if she survives to the life 
# expectancy in her cohort and country of birth. 
# (B) Children expected to outlive a woman as a fraction of her 
# cohort's TFR. Higher values indicate that a larger fraction of 
# a woman's offspring is expected to live longer than her, 
# independently of the prevalent levels of fertility. 
# The solid lines represent regional median values and the 
# bands the variability among all countries in the world for 
# each birth cohort.

source(files[5])

# 6.  Fig S3 - Children outliving their mothers - global estimates ====

# Global burden of child death. Estimated as the sum of the
# (non-cumulative) burden of child death over all ages. This
# measure, considers the size and structure of different birth 
# cohorts of women to determine the number of child deaths 
# accumulated by all women in a given birth cohort and region 
# throughout their lives. Lower values for subsequent cohorts 
# of women in a region show than women born in younger cohorts 
# can expect to experience fewer child deaths compared to preceding generations. 
# Estimates for Oceania, Australia, and New Zealand shown in inset plot.

source(files[6])

# 7.  Fig S4 - Expected child survival and demographic transition ====

# Changes in the experience of child death throughout the Demographic 
# Transition (horizontal axis, proxied by life expectancy) in different
# regions of the world. (A-F) Association between the fraction of a
# woman's offspring expected to live longer than her and the cohort 
# life expectancy for the woman's cohort. The panels show country-level 
# trajectories by UN SDG region.

# The figure adresses the question of where there is a relationship
# between progress on the demographic transition and expossure to
# offspring mortality.
# In other words, do female cohorts in countries more advanced towards the demographic transition
# lose a smaller share of their children?
# If so, then the experience of child death does really decline with the demographic transition,
# as Livi Bacci suggested.

source(files[7])

# 8.  Table S1 - Cumulative number of child death for a woman living to age a (CD) ====

# Full country results for the cumulative number of child deaths 
# for a woman surviving to ages 20, 45, and 100. Regional estimates 
# show the median value and IQR in parenthesis.

source(files[8])

# 9.  Table S2 - Number of surviving children for a woman living to age a (CS) ====

# Full country results for the expected number of children surviving for a 
# woman surviving to ages 20, 45, and 100. Regional estimates show the median 
# value and IQR in parenthesis.

source(files[9])

# 10.  Table S3 - First difference of child death for a woman living to age a (Delta CD) ====

# First difference of child death for a woman surviving to ages 20, 45, and 100. 
# Regional estimates show the median value and IQR in parenthesis.

source(files[10])

# 11.  Table S4 - Burden of child death at each age a ====

# Child deaths experienced by women in birth cohort c at exact age a. 
# Obtained by weighting the first difference of child death by the life table 
# distribution of women. Regional values include point estimate and standard deviation, 
# in hundreds of thousands.

source(files[11])

# 12.  Table S5 - Number of children expected to outlive their mothers ====

# Number of children expected to live longer than their mothers, asuming 
# that the mothers survive to mean age at death (life expectancy) in 
# their cohort and country of birth. Regional estimates show the median 
# value and IQR in parenthesis.

source(files[12])

# 13.  Table S6 - Children outliving their mothers as a share of the mother’s cohort TFR ====

# Number of children expected to live longer than their mothers 
# as a share of the woman's cohort TFR. Regional estimates show 
# the median value and IQR in parenthesis.

source(files[13])

# 14. 14 - Datasets_S1_S2 ====

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

source(files[14])
