
# functions --------

library(countrycode)

# Get regions and countries from emiliy's data
get_regions_iso <- function(df, regions){
  merge(df, regions, by = "country") %>% 
    mutate(
      region = trimws(region) 
      , iso = countrycode(country, origin = "country.name", "iso3c", warn = F)
    ) %>% 
    select(iso, country, region, everything())
}



# 1. load data ---------

# 1.1 Emily estimates =============

regions_emily <- read.csv("../../Data/emily/regions.csv", stringsAsFactors = F) 

surv <- 
  read.csv("../../Data/emily/20200214_mothers.csv", stringsAsFactors = F) %>% 
      # mutate(measure = "mothers") %>% 
  get_regions_iso(., regions_emily) %>% 
  filter(region %in% "Africa") %>% 
  select(iso, starts_with("m")) %>% 
  pivot_longer(-c(iso), names_to = "variable", values_to = "surv") 
  # mutate(variable = recode(variable, "mrom45" = "mom45"))

# 1.2. Equivalent KC estimates ========
# lpwj

kc <- 
  read.csv("../../Data/emily/_kin_cohort_estimates_mothers.csv", stringsAsFactors = F) %>% 
  mutate(iso = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
  select(iso, starts_with("m")) %>% 
  pivot_longer(-c(iso), names_to = "variable", values_to = "kc") %>% 
  mutate(variable = gsub("kc", "", variable))

# 2. Get error for SSA ---------------

both <- 
  left_join(surv, kc, by = c("iso", "variable")) %>% 
  filter(variable == "mom45") %>% 
  mutate(country = countrycode(iso, destination = "country.name", origin = "iso3c")) %>% 
  select(country, everything()) %>% 
  select(-iso, - variable) 
  

# How many countries are being compared?
paste(unique(both$country), collapse = ", ")  

unique(both$country) %>% 
  length

# 2.1. median absolute error ===========

both %>% 
  mutate(err = abs(surv - kc)) %>% 
  pull(err) %>% 
  median()

# 2.2 median percentage error ============

both %>% 
  mutate(err = abs(surv - kc)/surv) %>% 
  pull(err) %>% 
  median() * 100

# 2.3.Maximum diff between KC and survey

both %>% 
  mutate(err = abs(surv - kc)/surv) %>% 
  arrange(desc(err)) %>% 
  slice(1) %>% 
  pull(err)
