# S8 - Burden of child death at each age a

# Child deaths experienced by women in birth cohort c at exact age a. 
# Obtained by weighting the first difference of child death by the life table 
# distribution of women. Regional values include point estimate and standard deviation, 
# in hundreds of thousands.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data required: 
# abs_df
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Include full country results
# this includes estimates for all countries AND regions, including Austrialia and NZ

# 0. Params ----

options("encoding" = "UTF-8")

caption <- 
  "Burden of Child Death (BCD): child deaths experienced by women in birth cohort c at exact age a. 
Obtained by weighting the first difference of child death by the life table distribution of women. 
Regional estimates (capitalized) show the median value and IQR in parenthesis. 
Estimates in hundreds of thsousands.
For reasons of space, 0 stands for <0.01 in the table."

lab <- "S4"

# Save tables as pdf?
export <- F
export_latex <- T

# For small tables (to be included in the Supplementary Materials)

cohorts <- c(1950, 1975, 2000)
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
abs_w <- abs_w[ , c("region", "area", paste0(sort(rep(cohorts, length(cohorts))), "_", ages))]

# Add region names

abs_w$region[abs_w$area != ""] <- ""

# Make regions bold in latex
rows <- abs_w$region != ""

# abs_w[abs_w == "0"] <- '<0.01'

# Export ====

if(export) write.csv(abs_w, paste0("../../Output/tab",lab,".csv"), row.names = F)

if(export_latex) {
  
  short <- format_table(abs_w, row_keep = NA, ages, cohorts)
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
    add_header_above(c("Birth cohort"=1, "1950"=3, "1975"=3, "2000"=3)) %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"), font_size = 5)
  
  write(k, file = paste0("../../Output/tab", lab, ".tex"))

}

print("10 - S8 saved to ../../Output")
