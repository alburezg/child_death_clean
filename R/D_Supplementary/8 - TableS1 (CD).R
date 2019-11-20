
# S5 - Cumulative number of child death for a woman living to age a (CD)

# Full country results for the cumulative number of child deaths 
# for a woman surviving to ages 20, 45, and 100. Regional estimates 
# show the median value and IQR in parenthesis.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data required: 
# Child death: 
# df_cl_m_full
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Include full country results
# this includes estimates for all countries AND regions, including Austrialia and NZ

# 0. Params ----

options("encoding" = "UTF-8")

caption <- 
"Full country results for the cumulative number of child deaths (CD) for a woman surviving 
to ages 20, 45, and 100 in three birth cohorts. 
Regional estimates (capitalized) show the median value and IQR in parenthesis.
For reasons of space, 0 stands for <0.01."

lab <- "S1"

# Save tables as pdf?
export <- F
export_latex <- T

# For small tables (to be included in the Supplementary Materials)

ages <- c(20, 45, 100)
cohorts <- c(1950, 1975, 1999)

# Data from Fig 2 ~~~~ ----

# 2.1	Child death (CD) ====
# ~~~~~~~~~~~~~

# Full country results for the (cumulative) number of child deaths for a woman surviving to selected ages (median and IQR).

cl_full <- merge(
  df_cl_m_full %>% 
    filter(type == "country") %>% 
    filter(cohort %in% cohorts)
  , female_births %>% 
    select(cohort = year, cohort_size = value, everything())
  , by = c('country', 'cohort')
  , all.x = T
)

cl_countries <- 
  cl_full %>% 
  filter(type == 'country') %>% 
  filter(age %in% ages) %>% 
  filter(cohort %in% cohorts) %>% 
  mutate(region = as.character(region)) %>% 
  select(country, region, cohort, age, value)
  
cl_regions <- 
  cl_countries %>% 
  group_by(region, age, cohort) %>%
  summarise(
    median = round(median(value), 2)
    , iqr = round(IQR(value), 1)
  ) %>%
  ungroup() %>% 
  mutate(
    type = "region"
    , CD = paste0(median, " (", iqr,")")
    , region = as.character(region)
    , area = as.character("")
         ) %>% 
  # mutate_each(as.character) %>% 
  select(area, region, age, cohort, CD)

cl_long <- bind_rows(
  cl_countries %>% 
    mutate(
      CD = as.character(round(value, 2))
           ) %>% 
    # mutate_each(as.character) %>% 
    select(region, area = country, age, cohort, CD)
  , cl_regions
) %>% 
  arrange(region, area, cohort, age)

# To wide

cl_w <- spread(
  cl_long %>% 
    mutate(cohort_age = paste0(cohort, "_", age)) %>% 
    mutate(
      area = plyr::mapvalues(area, from = cont$new, to = cont$old, warn_missing = F)
      , region = plyr::mapvalues(region, from = cont$new, to = cont$old, warn_missing = F)
      ) %>% 
    select(-cohort, -age)
  , cohort_age
  , CD
  ) %>% 
  arrange(region, area)

# Order
cl_w <- cl_w[ , c("region", "area", paste0(sort(rep(cohorts, length(cohorts))), "_", ages))]

# Add region names

cl_w$region[cl_w$area != ""] <- ""

# Make regions bold in latex
rows <- cl_w$region != ""

# Format 0 values

# cl_w[cl_w == "0"] <- '<0.01'

# Export ====

if(export) write.csv(cl_w, paste0("../../Output/tab",lab,".csv"), row.names = F)

if(export_latex) {
  
  short <- format_table(cl_w, row_keep = NA, ages, cohorts)
  
  short[ rows, 1] <- toupper(short[ rows, 1])

  k <-
    kable(
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

print("7 - S5 saved to ../../Output")
