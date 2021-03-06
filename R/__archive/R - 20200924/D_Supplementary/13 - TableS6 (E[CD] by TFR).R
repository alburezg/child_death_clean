# S10 - Children outliving their mothers as a share of the mother’s cohort TFR

# Number of children expected to live longer than their mothers 
# as a share of the woman's cohort TFR. Regional estimates show 
# the median value and IQR in parenthesis.

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
  "Number of children expected to live longer than their mothers as a fraction of their 
mothers' cohort TFR (FOW). 
Regional estimates (capitalized) for six selected birth cohorts show the median value and IQR in parenthesis."

lab <- "S6"

# Save tables as pdf?
export <- F
export_latex <- T

# For small tables (to be included in the Supplementary Materials)

cohorts <- c(seq(1950, 2000, 10), 2000)



# 4.2.	Share children outlive mothers ====
# ~~~~~~~~~~~~~

out_countries <-
  ecl_ctfr %>%
  filter(type == 'country') %>% 
  filter(cohort %in% cohorts) %>% 
  mutate(region = as.character(region)) %>% 
  mutate(share = 1 - value / tfr) %>% 
  select(country, region, cohort, value = share)


out_regions <- 
  out_countries %>% 
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
  # mutate_each(as.character) %>% 
  select(area, region, cohort, value)

out_long <- bind_rows(
  out_countries %>% 
    mutate(
      value = as.character(round(value, 2))
    ) %>% 
    mutate(
      country = plyr::mapvalues(country, from = cont$new, to = cont$old, warn_missing = F)
      , region = plyr::mapvalues(region, from = cont$new, to = cont$old, warn_missing = F)
    ) %>% 
    select(region, area = country, cohort, value)
  , out_regions %>% 
    mutate(region = plyr::mapvalues(region, from = cont$new, to = cont$old, warn_missing = F))
) %>% 
  arrange(region, area, cohort)

# To wide

out_w <- spread(
  out_long 
  , cohort
  , value
) %>% 
  arrange(region, area)

# Add region names

out_w$region[out_w$area != ""] <- ""

# Make regions bold in latex
rows <- out_w$region != ""

# Format 0 values
# out_w[out_w == "0"] <- '<0.01'

# Export ====

if(export) write.csv(out_w, paste0("../../Output/tab",lab,".csv"), row.names = F)

if(export_latex) {
  
  short <- format_table(out_w, row_keep = NA, ages, cohorts, extra_header = F)
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

print("12 - S10 saved to ../../Output")
