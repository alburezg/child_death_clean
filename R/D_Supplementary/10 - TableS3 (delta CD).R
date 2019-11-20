# S7 - First difference of child death for a woman living to age a (deltaCD)

# First difference of child death for a woman surviving to ages 20, 45, and 100. 
# Regional estimates show the median value and IQR in parenthesis.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data required: 
# df_cl_diff
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Include full country results
# this includes estimates for all countries AND regions, including Austrialia and NZ

# 0. Params ----

options("encoding" = "UTF-8")

caption <- 
  "First difference of child death for a woman surviving to ages 20, 45, and 100 in three selected birth cohorts. 
Regional estimates (capitalized) show the median value and IQR in parenthesis. 
For reasons of space, 0 stands for <0.01 in the table."

lab <- "S3"

# Save tables as pdf?
export <- F
export_latex <- T

# For small tables (to be included in the Supplementary Materials)

cohorts <- c(1950, 1975, 1999)
ages <- c(25, 70, 99)

# 3.1.	First difference of child death (CD) ====
# ~~~~~~~~~~~~~

# First difference of child death for a woman surviving to age $a$ 
# (full country results; selected ages; median and IQR).

diff_countries <- 
  df_cl_diff %>% 
  filter(type == 'country') %>% 
  filter(age %in% ages) %>% 
  filter(cohort %in% cohorts) %>% 
  mutate(region = as.character(region)) %>% 
  select(country, region, cohort, age, value = diff)

diff_regions <- 
  diff_countries %>% 
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

diff_long <- bind_rows(
  diff_countries %>% 
    mutate(
      CS = as.character(round(value, 2))
    ) %>% 
    # mutate_each(as.character) %>% 
    select(region, area = country, age, cohort, CS)
  , diff_regions
) %>% 
  arrange(region, area, cohort, age)

# To wide

diff_w <- spread(
  diff_long %>% 
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
diff_w <- diff_w[ , c("region", "area", paste0(sort(rep(cohorts, length(cohorts))), "_", ages))]

# Add region names

diff_w$region[diff_w$area != ""] <- ""

# Make regions bold in latex
rows <- diff_w$region != ""

# Format 0 values
# diff_w[diff_w == "0"] <- '<0.01'

# Export ====

if(export) write.csv(diff_w, paste0("../../Output/tab",lab,".csv"), row.names = F)

if(export_latex) {
  
  short <- format_table(diff_w, row_keep = NA, ages, cohorts)
  
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
    kable_styling(latex_options = c("hold_position", "repeat_header"), font_size = 7)
  
  write(k, file = paste0("../../Output/tab", lab, ".tex"))
  
}

print("9 - S7 saved to ../../Output")
