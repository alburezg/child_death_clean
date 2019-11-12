
# S9 - Number of children expected to outlive their mothers

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data required: 
# cs_ex_pop_country
# cl_ex_pop_country
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Include full country results
# this includes estimates for all countries AND regions, including Austrialia and NZ

# 0. Params ----

options("encoding" = "UTF-8")

caption <- "Number of children expected to live longer than their mothers, asuming that the mothers survive to mean age at death (life expectancy) in their cohort and country of birth. Regional estimates show the median value and IQR in parenthesis."
lab <- "S9"

# Save tables as pdf?
export <- F
export_latex <- T

# For small tables (to be included in the Supplementary Materials)

cohorts <- c(seq(1950, 1999, 10), 1999)

# 4.1.	Expected children outlive mothers ====
# ~~~~~~~~~~~~~

csex_countries <- 
  cs_ex_pop_country %>% 
  filter(type == 'country') %>% 
  filter(cohort %in% cohorts) %>% 
  mutate(region = as.character(region)) %>% 
  select(country, region, cohort, value)
  

csex_regions <- 
  csex_countries %>% 
  group_by(region, cohort) %>%
  summarise(
    median = round(median(value), 2)
    , iqr = round(IQR(value), 1)
  ) %>%
  ungroup() %>% 
  mutate(
    type = "region"
    , value = paste0(median, " (", iqr,")")
    , region = as.character(region)
    , area = as.character("")
  ) %>% 
  select(area, region, cohort, value)

csex_long <- bind_rows(
  csex_countries %>% 
    mutate(
      value = as.character(round(value, 2))
    ) %>% 
    select(region, area = country, cohort, value)
  , csex_regions
) %>% 
  arrange(region, area, cohort)
  
# To wide

csex_w <- spread(
  csex_long 
  , cohort
  , value
) %>% 
  arrange(region, area)

# Add region names
csex_w$region[csex_w$area != ""] <- ""

# Format 0 values
csex_w[csex_w == "0"] <- '<0.01'

# Export ====

if(export) write.csv(csex_w, paste0("../../Output/tab",lab,".csv"), row.names = F)

if(export_latex) {
  
  # 29 rows fit in one page
  # 59 in 2 but there are issues showing it in multiple pages
  short <- format_table(csex_w, row_keep = 29, ages, cohorts, extra_header = F)
  
  k <- kable(
    short
    , format = "latex"
    , booktabs = TRUE
    , caption = caption
    , label = lab
    , align = "l"
    , row.names = F
    , escape = T
  ) 
  
  write(k, file = paste0("../../Output/tab", lab, ".tex"))
  
}

print("11 - S9 saved to ../../Output")

