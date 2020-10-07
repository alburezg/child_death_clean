
# Read entire outputs

readme <- list.files(
  path = "../../Data/estimates/"
  , pattern = "df_cl_m_1950to1999_15to100_fert"
  , full.names = T
  )

cl_robust_all <- 
  lapply(readme, readRDS) %>% 
  bind_rows() 

mort_old <- c("Lower 95 PI", "Upper 95 PI", "constant", "Median PI")
fert_old <- c("low", "high", "constant", "medium")

# mort_levs <- c("low mortality", "high mortality", "constant mortality", "medium mortality")
# fert_levs <- c("low fertility", "high fertility", "constant fertility", "medium fertility")
mort_levs <- c("lm", "hm", "cm", "mm")
fert_levs <- c("lf", "hf", "cf", "mf")


cl_small <- 
  cl_robust_all %>% 
  filter(cohort %in% seq(1950, 2000, 5)) %>%
  filter(age %in% seq(15, 100, 5)) %>% 
  mutate(
    variant_mort = plyr::mapvalues(variant_mort, mort_old, mort_levs)
    , variant_mort = factor(variant_mort, mort_levs)
    , variant_fert = plyr::mapvalues(variant_fert, fert_old, fert_levs)
    , variant_fert = factor(variant_fert, fert_levs)
    , variant = paste(variant_mort, " - ", variant_fert)
  ) %>% 
  # select(variant, country, cohort, age, child_death = value)
  select(variant_mort,variant_fert, country, cohort, age, child_death = value)

write.csv(cl_small, "../../Output/datasetS1_variants.csv", row.names = F)
