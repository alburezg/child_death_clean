print(paste0("Running script: ", "10 - CS_create_df"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Data requirements: 
# The ECL data frame created in this script can be loaded (if created) with
# df_cs_m_full <- readRDS('../../Data/df_cs_m_1950to1999_15to100.RData')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# All years, ages 0-100

reference_years <- 1950:2000
countries <- unique(ASFRC$country)

labs <- reference_years

sex_keep <- F

# Parameters for the function
# cos <- c(1950:2099) # cohorts
cos <- c(1950:2100) # cohorts
xs <- c(15:49)
mas <- c(15:100) # woman ages

df_cs_full <- child_survival(
  countries = countries
  , reference_years = reference_years
  , path = "../../Data/derived"
  , ages_keep = 15:100
  , ASFRC = ASFRC
)

allowed_types <- c("country", "un_sdg-groups")

df_cs_m_full <- merge(
  df_cs_full
  , un_reg
  , by.x = 'country'
  , by.y = 'level1'
  , all.x = T
) %>% 
  mutate(
    value = value/1000
    , region = factor(default_region, levels = regions_long)
    , cohort2 = paste0("Women born in ", variable)
  ) %>% 
  select(region, type, country, cohort = variable, cohort2, age, value) %>% 
  arrange(country, cohort, age) %>% 
  filter(type %in% allowed_types) %>% 
  # This last line, essentially removes ages 100 for the 2000 cohort 
  # which are not available
  filter(!is.na(value))

# Make sure all regions were properly coded
!length(df_cs_m_full %>% filter(is.na(region)) %>% pull)

saveRDS(df_cs_m_full, '../../Data/estimates/df_cs_m_1950to1999_15to100.RDS')
