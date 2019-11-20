
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

caption <- 
  "Full country results for the expected number of children currently alive (CS) 
for a woman surviving to ages 20, 45, and 100 in three selected birth cohorts. 
Regional estimates (capitalized) show the median value and IQR in parenthesis. 
For reasons of space, 0 stands for <0.01 in the table."

lab <- "S2"

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
    mutate(
      area = plyr::mapvalues(area, from = cont$new, to = cont$old, warn_missing = F)
      , region = plyr::mapvalues(region, from = cont$new, to = cont$old, warn_missing = F)
    ) %>% 
    select(-cohort, -age)
  , cohort_age
  , CS
) %>% 
  arrange(region, area)

# Order
cs_w <- cs_w[ , c("region", "area", paste0(sort(rep(cohorts, length(cohorts))), "_", ages))]

# Add region names

cs_w$region[cs_w$area != ""] <- ""

# Make regions bold in latex
rows <- cs_w$region != ""

# Format 0 values
# cs_w[cs_w == "0"] <- '<0.01'

# Export ====

if(export) write.csv(cs_w, paste0("../../Output/tab",lab,".csv"), row.names = F)

if(export_latex) {
  
  short <- format_table(cs_w, row_keep = NA, ages, cohorts)
  
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
    add_header_above(c("Birth cohort"=1, "1950"=3, "1975"=3, "1999"=3)) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"), font_size = 6)
  
  write(k, file = paste0("../../Output/tab", lab, ".tex"))
  
}

print("8 - S6 saved to ../../Output")
