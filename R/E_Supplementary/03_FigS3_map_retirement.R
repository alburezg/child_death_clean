# UPDATE NEEDED!
# 20200429: this is the same as the map in the main text, but you can 
# change to include other maps in the supplementary materials

# World map: children outliving their mothesr
# for a woman aged 65 in 2020

# rcode mlfm68

# Data required: tally_share (created for fig 4)

# 0. Parameters

retirement_age <- 65
title_size <- 9
# Country border size (white lines)
# Use this for stacked maps
country_line_size <- 0.0005

# 0. Create world map ----
# Can be reused for all maps

world <- sf::st_as_sf(rworldmap::getMap(resolution = "low")) %>% 
  select(ID = ADMIN, country = ADM0_A3, geometry) %>% 
  mutate(ID = as.character(ID), country = as.character(country)) %>% 
  assign_contested_iso3_countries(.)

# 1. Child deaths after retirement ----
# Get df of distributino of child deaths by woman's age

rep_age <- 15:49
ret_age <- retirement_age:99

share_of_deaths_in_retirement <- 
  abs_df %>% 
  mutate(
    agegr = "other"
    , agegr = ifelse(age %in% rep_age, "reproductive", agegr)
    , agegr = ifelse(age %in% ret_age, "retirement", agegr)
  ) %>% 
  select(country, agegr, cohort, value = absolute) %>%
  group_by(country, cohort, agegr) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  ungroup()   %>% 
  pivot_wider(names_from = agegr, values_from = value) %>% 
  mutate(
    share_rep = reproductive / (retirement + reproductive + other)
    , share_ret = retirement / (retirement + reproductive + other)
    , share_more_in_rep = share_ret/share_rep
  ) %>% 
  select(country, cohort, starts_with("share"))

# Plot

# Make sure that various cohorts share the same range of colours
# in the legend colorbar
bar_br <- seq(0, 0.8, 0.2)
bar_lim <- c(0, 0.82)

col <- "share_ret"
viridis_direction <- -1

# WORKING
cohort_show <- 1955
bar_name <- paste0("Share of life-time\noffspring deaths\nexperienced after\nwoman's age 65")

p_title <- paste0("Child deaths experienced after age 65 by women born in ", cohort_show, " (i.e. aged 65 in 2020)")

p_name <- paste0("../../Output/figS3_map-share-cd-in-retirement.pdf")

p1 <- map_share_child_deaths_in_age_range(
  cohort_show = cohort_show
  , country_line_size = country_line_size
  , col = col, bar_name = bar_name
  , bar_br, bar_lim, p_title, viridis_direction
)

ggsave(p_name, p1, height = 7, width = 16, units = "cm")

# For map label - countries with highest and lowest values 

share_of_deaths_in_retirement %>%
  filter(cohort == cohort_show) %>%
  filter(country != 'channel islands') %>%
  mutate(
    country = ifelse(country == "eswatini", "swaziland", country)
    , country = countrycode(country, "country.name", "iso3c")
  ) %>%
  select(country, value = dplyr::starts_with(col)) %>% 
  # pull(value) %>% max
  arrange(value) %>% 
  slice(c(1, nrow(.)))
