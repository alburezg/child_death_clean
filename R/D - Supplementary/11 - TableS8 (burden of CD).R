# S8 - Burden of child death at each age a

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data required: 
# abs_df
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Include full country results
# this includes estimates for all countries AND regions, including Austrialia and NZ

# 0. Params ----

options("encoding" = "UTF-8")

caption <- "Child deaths experienced by women in birth cohort c at exact age a. Obtained by weighting the first difference of child death by the life table distribution of women. Regional values include point estimate and standard deviation, in hundreds of thousands."
lab <- "S8"

# Save tables as pdf?
export <- F
export_latex <- T

# For small tables (to be included in the Supplementary Materials)

cohorts <- c(1950, 1975, 1999)
ages <- c(25, 70, 99)
# IN  TENS OF THOUSENDS!
round_by <- 2

# 3.2.	Burden of child death ====
# ~~~~~~~~~~~~~

abs_countries <- 
  abs_df %>% 
  filter(type == 'country') %>% 
  filter(age %in% ages) %>% 
  filter(cohort %in% cohorts) %>% 
  # Save as thousands
  mutate(absolute = absolute/1e5) %>% 
  mutate(region = as.character(region)) %>% 
  select(country, region, cohort, age, value = absolute)

abs_regions <- 
  abs_countries %>% 
  group_by(region, age, cohort) %>%
  summarise(
    median = round(sum(value), round_by)
    , iqr = round(IQR(value), round_by)
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

abs_long <- bind_rows(
  abs_countries %>% 
    mutate(
      CS = as.character(round(value, round_by))
    ) %>% 
    # mutate_each(as.character) %>% 
    select(region, area = country, age, cohort, CS)
  , abs_regions
) %>% 
  arrange(region, area, cohort, age)

# To wide

abs_w <- spread(
  abs_long %>% 
    mutate(cohort_age = paste0(cohort, "_", age)) %>% 
    select(-cohort, -age)
  , cohort_age
  , CS
) %>% 
  arrange(region, area)

# Order
abs_w <- abs_w[ , c("region", "area", paste0(sort(rep(cohorts, length(cohorts))), "_", ages))]

# Add region names

abs_w$region[abs_w$area != ""] <- ""

abs_w[abs_w == "0"] <- '<0.01'

# Export ====

if(export) write.csv(abs_w, paste0("../../Output/tab",lab,".csv"), row.names = F)

if(export_latex) {
  
  # 29 rows fit in one page
  # 59 in 2 but there are issues showing it in multiple pages
  short <- format_table(abs_w, row_keep = 29, ages, cohorts)
  
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

print("10 - S8 saved to ../../Output")
