
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

convert_period_LT_to_cohort_LT <- 
  function(lt_1_1, sex = "B", export = T, run_graphic_tests = F, years = 1951:2100, ages = 1:100, parallel = T, numCores = 4) {
    
    # 1. Create cohort life table ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # LTC ... Cohort life table
    LTC_df <- 
      lt_1_1 %>% 
      select(Country = country, Year = year, Age = age, mx, qx, ax) %>% 
      mutate(
        Cohort = Year - Age
        , Country = as.character(Country)
      ) %>% 
      arrange(Country, Cohort, Age) %>% 
      filter(Cohort > min(years) - 1) %>% 
      # filter(!Country %in% ignore) %>% 
      # Assign to new cohort life tables:
      # lx (number of people left alive)
      # dx (annual number of deaths)
      # nLx (person-live yeares lived in interval)
      mutate(
        lx = ifelse(Age == 0, 1, NA)
        , dx = NA
        , nLx = NA
        , Tx = NA
        , ex = NA
      ) %>% 
      select(Country, Cohort, Year, Age, mx, qx, ax, lx, dx, nLx, Tx, ex)
    
    # Sequentially, the function takes about 1 minute per country (for all years)
    # to exeute currently, so estimate around 2-3 hours for all countries
    # Parallelised on 4 cores is much faster, taking about 8-10 minutes
    
    # Return a df with new columns added
    
    closeAllConnections()
    
    LTC <- LT_period_to_cohort(
      df = LTC_df
      , years = years
      , ages = ages
      , parallel = parallel
      , numCores = numCores
    )
    
    rm("LTC_df")
    
    gc()
    
    # 2. Graphical tests ----
    
    if(run_graphic_tests) {
      
      ages_keep <- c(0, 10, 50, 90)
      country_keep <- countries[sample(1:length(countries), 20)][1:4]
      
      df_plot <- LTC %>% 
        filter(Country %in% country_keep)
      
      # 2.1. Plot mx log ====
      
      (
        test_coh_mx <- 
          df_plot %>%
          # filter(Cohort %in% c(1950, 1960, 1970, 1980, 1990, 1999)) %>% 
          filter(Cohort %in% seq(1950, 2100, 5)) %>% 
          ggplot(aes(x = Age, y = mx)) +
          geom_point(size = 0.5) +
          scale_y_continuous("log(nmx)",trans='log2') +
          facet_grid( Country ~ Cohort, scales = "free") +
          theme_bw()
      )
      
      # 2.2. Plot ex ====
      
      (
        test_coh_ex <- 
          df_plot %>%
          filter(Age %in% ages_keep) %>% 
          ggplot(aes(x = Cohort, y = ex)) +
          geom_line() +
          facet_grid(Country ~ Age, scales = "free") +
          theme_bw()
      )
      
      ggsave("../../Output/A.7.test_coh_mx.pdf", test_coh_mx)
      ggsave("../../Output/A.7.test_coh_ex.pdf", test_coh_ex)
      
    }
    
    # 3. Export ====
    
    if(export) {
      
      # 3.1. Save all as one file
      file <- paste0("../../Data/derived/", "LTC", sex, ".csv")
      
      write.csv(x = LTC, file = file, row.names = F)
      
      print(paste("All cohort life tables saved to", file))
      
    }
    
    return(LTC)
    
  }

# 2. Run for women LT ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

numCores <- ifelse(detectCores() > 8, 25, detectCores())

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print('7.1 - Converting LT from period to (pseudo) cohort for women.')
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

LTCF <- 
  convert_period_LT_to_cohort_LT(
  lt_1_1 = lt_1_1_F
  , sex = "F"
  , export = T
  , run_graphic_tests = F
  , years = 1950:2100
  , ages = 1:100
  , parallel = T
  , numCores = numCores
  )

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
