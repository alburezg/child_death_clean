

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


(files <- list.files(pattern = ".R$")[-1])

# 1. Load the functions, data, and packages needed in the scripts ====

source(files[1])

# Fig S1 (CD) - global estimates ====

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

source(files[2])

# FigS2 - Number of children expected to outlive their mothers ====

# Number of children expected to live longer than their mothers, asuming 
# that the mothers survive to mean age at death (life expectancy) in 
# their cohort and country of birth.

source(files[3])

# FigS3. Share of child deaths experienced after woman's retirenemt ----------

source(files[4])

# FigS4. Share of children outlive an average woman  ----------

source(files[5])

# Fig S5. Heatmap of child death by woman's cohort --------------

source(files[6])

# Datasets_S1_S2 ====

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

source(files[7])
