

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
# Scripts to produce Figs. 1-4 from the main text of the paper.
# Note that all of the analysis has already been done and these
# scripts only procude plots using data generated in previous scripts. 
# the scripts in this directory should be run sequentially to:
#    1. Load the functions and packages needed in the scripts 
#    2. Load the data needed for the analysis 
#    3. Load data about country grouping and format them for the analysis  
#    4. Produce Fig. 1
#    5. Produce Fig. 2
#    6. Produce Fig. 3
#    7. Produce Fig. 4
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(files <- list.files(pattern = ".R$", full.names = T)[-1])

# 1. Load the functions and packages needed in the scripts ====

source(files[1])

# 2. Load the data needed for the analysis ====

source(files[2])

# 3. Load data about country grouping and format them for the analysis ==== 

source(files[3])

# 4. Produce Fig. 1 Cohort fertility and female life expectancy ====

# Cohort fertility and life expectancy for UN SDG regions (solid lines, median values) 
# for the 1950-1999 annual birth cohorts (cohorts approximated using UN WPP period data). 
# The DT theory predicts a progression from the top-left of the figure (high fertility and 
# mortality) to the bottom-right (low fertility and mortality) for younger birth cohorts.
# Regions with longer trajectories are expected to experience the largest fertility and mortality decline.
# Horizontal trajectories (e.g. Europe and N America) result from increases in mortality but little change 
# in fertility for younger generations. 
# Estimates for Oceania, Australia, and New Zealand omitted.

source(files[4])

# 5. Fig. 2 Mean frequency of child death (top) and child survival (bottom) ====

# Frequency of child death and child survival over the life course 
# of two selected birth cohorts of women (by region and birth cohort).
# These measures pertain to a hypothetical woman aged $a$, standing 
# before us (i.e. surviving to that age). (A and B) Cumulative number 
# of child deaths (CD) experienced by a woman reaching age $a$. 
# The slope of the curves is positive at all ages, even after the end 
# of a woman's reproductive age, because offspring mortality continues 
# to accumulate over the life course. (C and D) Total number of children 
# surviving (CS) for a woman reaching age $a$. Values in the 
# vertical axis represent the total number of children `currently alive' - 
# those surviving exposure to the life table mortality function corresponding 
# to their birth cohorts. The solid lines represent median values and the 
# bands the variability among countries in each region.
# See Materials and Methods for details of the estimation procedures.

source(files[5])

# 6. Fig. 3 - Timing of child death over the life course ====

# Timing of child death over the life course for two female birth cohorts.
# (A and B) Number of child deaths experienced at each age $a$ by a woman reaching 
# that age (i.e. conditional on female survival). This is the First Difference of 
# Child Death ($\Delta CD$) - its cumulative sum over age yields the measure of 
# Child Death introduced above. (C and D) Burden of child death: total number of 
# child deaths experienced by all women in a region and birth cohort at each age $a$.
# We obtained it by multiplying $\Delta CD$ by the absolute number of women expected 
# to survive to each age, considering the original size of each female birth cohort 
# and the mortality rates prevalent in their countries of origin.
# This measure removes the assumption of female survival by accounting for the size 
# and age structure of the population. The solid lines show the regional median and 
# the bands percentiles within each region.
# See Materials and Methods for details of the estimation procedures.

source(files[6])

# 7. Fig. 4 Generational burden of child death by region ====

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

source(files[7])

# 8. Fig. 5 Children outliving their mothers ====

# Absolute and relative number of children expected to live 
# longer than their mothers. (A) Number of children expected 
# to outlive an average woman. Values in the vertical axis 
# show the number of children alive at the time of a woman's 
# death if she survives to the life expectancy in her cohort 
# and country of birth. (B) Children expected to outlive a 
# woman as a fraction of her cohort's TFR. Higher values indicate 
# that a larger fraction of a woman's offspring is expected to 
# live longer than her, independently of the prevalent levels 
# of fertility. The solid lines represent regional median values 
# and the bands the variability among countries in each region.

source(files[8])

# 9. Fig. 6 Map: Children outliving their mothers ====

# World map showing country-level variation in the share
# of a cohort's TFR expected to outlive their mothers, for
# women entering retirement age (70) in 2020 - eg born in 1950.

source(files[9])

# 10. Fig. 7 Heatmap ====

# Heatmap showing the proportion of a female cohort's TFR surviving
# to each age in the 15-100 age range for women born between 1950 
# and 2000. Colours show the share of surviving children by each 
# maternal age and solid lines show the ages at which different 
# percentiles of children are expecte to survive for the different
# world regions.

source(files[10])
