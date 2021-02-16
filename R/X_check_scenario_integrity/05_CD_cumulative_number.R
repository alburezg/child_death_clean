print(paste0("Running script: ", "4 - CD_create_df"))

# reference_years <- 2000
reference_years <- 1950:2000
variant_fert_all <- c("low", "medium", "high", "constant")
# variant_mort_all <- c("Median PI", "Upper 80 PI", "Lower 80 PI", "Upper 95 PI", "Lower 95 PI", "constant")
variant_mort_all <- c("Lower 95 PI", "Median PI", "Upper 95 PI", "constant")

# Parameters for the function
cos <- c(1950:2100) # cohorts
xs <- c(15:49) # reproductive ages
mas <- c(15:100) # woman ages


  # Get for all four combinations of mortality and fert variants

# m <- variant_mort_all[1]
# f <- variant_fert_all[1]

  for(m in variant_mort_all){
    for(f in variant_fert_all){

      ASFRC <- get(paste0("ASFRC_", f))
      
      child_loss_robust(
        countries = tolower(country_keep)
        , reference_years = reference_years
        , path = "../../Data/derived"
        , ASFRC = ASFRC %>% filter(Age < 50)
        , ages_keep = 15:100
        , variant_fert = f
        , variant_mort = m
      )
      
    }
  }
    