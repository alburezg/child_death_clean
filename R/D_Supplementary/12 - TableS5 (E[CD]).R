
# S9 - Number of children expected to outlive their mothers

# Number of children expected to live longer than their mothers, asuming 
# that the mothers survive to mean age at death (life expectancy) in 
# their cohort and country of birth. Regional estimates show the median 
# value and IQR in parenthesis.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data required: 
# cs_ex_pop_country
# cl_ex_pop_country
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Include full country results
# this includes estimates for all countries AND regions, including Austrialia and NZ

# 0. Params ----

options("encoding" = "UTF-8")

caption <- 
  "Number of children expected to live longer than their mothers (E[CS]), asuming the latter 
survive to the mean age at death in their cohort and country of birth. 
Regional estimates (capitalized) for six cohorts show the median value and IQR in parenthesis."

lab <- "S5"

# Save tables as pdf?
export <- F
export_latex <- T

# For small tables (to be included in the Supplementary Materials)

cohorts <- c(seq(1950, 2000, 10), 2000)

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
    mutate(
      country = plyr::mapvalues(country, from = cont$new, to = cont$old, warn_missing = F)
      , region = plyr::mapvalues(region, from = cont$new, to = cont$old, warn_missing = F)
    ) %>% 
    select(region, area = country, cohort, value)
  , csex_regions %>% 
    mutate(region = plyr::mapvalues(region, from = cont$new, to = cont$old, warn_missing = F))
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

# Make regions bold in latex
rows <- csex_w$region != ""

# Format 0 values
# csex_w[csex_w == "0"] <- '<0.01'

# Export ====

if(export) write.csv(csex_w, paste0("../../Output/tab",lab,".csv"), row.names = F)

if(export_latex) {
  
  short <- format_table(csex_w, row_keep = NA, ages, cohorts, extra_header = F)
  short[ rows, 1] <- toupper(short[ rows, 1])
  
  k <- kable(
    short
    , format = "latex"
    , booktabs = TRUE
    , linesep = ""
    , longtable = T
    , caption = caption
    , label = lab
    , align = "l"
    , row.names = F
    , escape = T
  ) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"), font_size = 7)
  
  write(k, file = paste0("../../Output/tab", lab, ".tex"))
  
}

print("11 - S9 saved to ../../Output")

