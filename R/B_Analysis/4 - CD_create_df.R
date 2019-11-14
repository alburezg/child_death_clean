print(paste0("Running script: ", "4 - CD_create_df"))

# This script takes cohort age-specific fertility rates (ASFRC) 
# and matrices of survival probabilities (lx.kids.arr) to implement 
# equation 1 in the main text for all countries:

# \underbrace{CD_{(a,c, r)}}_{\text{child death}}= \underbrace{\sum_{x=15}^{x=a} {_1F_{(x,c,r)}}}_{\text{children born}}-\underbrace{\sum_{x=15}^{x=a} {_1F_{(x,c,r)}} l_{(a-x,c+x,r)}}_{\text{children surviving}}

# where $CD_{(a,c,r)}$, the expected number of child deaths experienced
# by a woman born in cohort $c$ and UN SDG region $r$ (using the M49 standard), 
# conditional on her surviving to age $a$

# The output, df_cl_m_full, is a data frame containing the expected CD value by woman's age.

# 0. Parameters ----

reference_years <- 1950:2000
countries <- unique(ASFRC$country)

# Parameters for the function
cos <- c(1950:2100) # cohorts
# cos <- c(1950:2099) # cohorts
xs <- c(15:49) # reproductive ages
mas <- c(15:100) # woman ages

# 1. Create ECLC df ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!exists('df_cl_full')) {
 
  df_cl_full <- child_loss(
    countries = countries
    , reference_years = reference_years
    , path = "../../Data/derived"
    , ASFRC = ASFRC
    , ages_keep = 15:100
  )
  
  # Determine which rows are regions and which countries
  
  allowed_types <- c("country", "un_sdg-groups")
  
  df_cl_m_full <- merge(
    df_cl_full
    , un_reg
    , by.x = 'country'
    , by.y = 'level1'
    , all.x = T
  ) %>% 
    mutate(
      # Here you chose which regions will be used
      # (default column defined in script 2_UN_country_grouping.R)
      region = factor(default_region, levels = regions_long)
      # level2 = factor(un_sdg_groups, levels = old_sdg)
      , cohort2 = paste0("Women born in ", variable)
      , value = value/1000
    ) %>% 
    filter(type %in% allowed_types) %>% 
    # na values in col region are regions like 'world', central america', etc
    # and can safely be ignored
    filter(!is.na(region)) %>% 
    select(region, type, country, cohort = variable, cohort2, age, value) %>% 
    arrange(country, cohort, age) %>% 
    # This last line, essentially removes ages 100 for the 2000 cohort 
    # which are not available
    filter(!is.na(value))
  
  # Make sure all regions were properly coded
  !length(df_cl_m_full %>% filter(is.na(region)) %>% pull)
  
  # saveRDS(df_cl_full, '../../Data/estimates/df_cl_1950to1999_15to100.RDS')
  saveRDS(df_cl_m_full, '../../Data/estimates/df_cl_m_1950to1999_15to100.RDS')
  
}