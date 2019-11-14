
# S6 - Number of surviving children for a woman living to age a (CS)

# Full country results for the expected number of children surviving for a 
# woman surviving to ages 20, 45, and 100. Regional estimates show the median 
# value and IQR in parenthesis.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data required: 
# Child survival
# df_cs_m_full
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Params ----

options("encoding" = "UTF-8")

caption <- "Full country results for the expected number of children surviving for a woman surviving to ages 20, 45, and 100. Regional estimates show the median value and IQR in parenthesis."
lab <- "S6"

# Save tables as pdf?
export <- F
export_latex <- T

# For small tables (to be included in the Supplementary Materials)

ages <- c(20, 45, 100)
cohorts <- c(1950, 1975, 1999)


# 2.2.	Child survival (CS) ====
# ~~~~~~~~~~~~~

# Full country results for the expected number of children surviving for a woman aged a (selected ages; median and IQR).

cs_full <- merge(
  df_cs_m_full %>% 
    filter(type == "country") %>% 
    filter(cohort %in% cohorts)
  , female_births %>% 
    select(cohort = year, cohort_size = value, everything())
  , by = c('country', 'cohort')
  , all.x = T
)

cs_countries <- 
  cs_full %>% 
  filter(type == 'country') %>% 
  filter(age %in% ages) %>% 
  filter(cohort %in% cohorts) %>% 
  mutate(region = as.character(region)) %>% 
  select(country, region, cohort, age, value)

cs_regions <- 
  cs_countries %>% 
  group_by(region, age, cohort) %>%
  summarise(
    median = round(median(value), 2)
    , iqr = round(IQR(value), 1)
  ) %>%
  ungroup() %>% 
  mutate(
    type = "region"
    , CS = paste0(median, " (", iqr,")")
    , region = as.character(region)
    , area = as.character("")
  ) %>% 
  # mutate_each(as.character) %>% 
  select(area, region, age, cohort, CS)

cs_long <- bind_rows(
  cs_countries %>% 
    mutate(
      CS = as.character(round(value, 2))
    ) %>% 
    # mutate_each(as.character) %>% 
    select(region, area = country, age, cohort, CS)
  , cs_regions
) %>% 
  arrange(region, area, cohort, age)

# To wide

cs_w <- spread(
  cs_long %>% 
    mutate(cohort_age = paste0(cohort, "_", age)) %>% 
    select(-cohort, -age)
  , cohort_age
  , CS
) %>% 
  arrange(region, area)

# Order
cs_w <- cs_w[ , c("region", "area", paste0(sort(rep(cohorts, length(cohorts))), "_", ages))]

# Add region names

cs_w$region[cs_w$area != ""] <- ""

# Format 0 values

cs_w[cs_w == "0"] <- '<0.01'

# Export

# Export ====

if(export) write.csv(cs_w, paste0("../../Output/tab",lab,".csv"), row.names = F)

if(export_latex) {
  
  # 29 rows fit in one page
  # 59 in 2 but there are issues showing it in multiple pages
  short <- format_table(cs_w, row_keep = 29, ages, cohorts)
  
  k <- kable(
    short
    , format = "latex"
    , booktabs = TRUE
    , caption = caption
    , label = lab
    , align = "l"
    , row.names = F
    , escape = T
  ) %>% 
    add_header_above(c("Birth cohort"=1, "1950"=3, "1975"=3, "1999"=3)) %>% 
    kable_styling(latex_options = c("hold_position", "repeat_header"))
  
  write(k, file = paste0("../../Output/tab", lab, ".tex"))
  
}

print("8 - S6 saved to ../../Output")
