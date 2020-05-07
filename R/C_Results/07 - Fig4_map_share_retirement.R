# UPDATE NEEDED!
# 20200429: this is the same as the map in the main text, but you can 
# change to include other maps in the supplementary materials

# World map: children outliving their mothesr
# for a woman aged 65 in 2020

# Data required: tally_share (created for fig 4)

# 0. Parameters

retirement_age <- 65
title_size <- 9
# Country border size (white lines)
# Use this if politting maps separatey
# country_line_size <- 0.005
# Use this for stacked maps
country_line_size <- 0.0005

# 0. Create world map ----
# Can be reused for all maps

world <- sf::st_as_sf(rworldmap::getMap(resolution = "low")) %>% 
  select(ID = ADMIN, country = ADM0_A3, geometry) %>% 
  mutate(ID = as.character(ID), country = as.character(country))

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
bar_name <- paste0("Share of lifetime\noffspring deaths\nexperienced after\nwoman's age 65")
p_title <- paste0("Women born in ", cohort_show, " and reaching retirement age approximately in ", cohort_show + retirement_age)

p1 <- map_share_child_deaths_in_age_range(
  cohort_show = cohort_show
  , country_line_size = country_line_size
  , col = col, bar_name = bar_name
  , bar_br, bar_lim, p_title, viridis_direction
)

cohort_show <- 2000
bar_name <- paste0("Share of lifetime\noffspring deaths\nexperienced after\nwoman's age 65")
p_title <- paste0("Women born in ", cohort_show, " and reaching retirement age approximately in ", cohort_show + retirement_age)


p2 <- map_share_child_deaths_in_age_range(
  cohort_show = cohort_show
  , country_line_size = country_line_size
  , col = col, bar_name = bar_name
  , bar_br, bar_lim, p_title, viridis_direction
)

patch <- p1 / p2  +   plot_layout(guides = "collect") 

p_name <- paste0("../../Output/fig4_map-share-cd-in-retirement",".pdf")
# ggsave(p_name, patch, height = 14, width = 16, units = "cm")

# SAve only 1 map!!
ggsave(p_name, p1, height = 7, width = 16, units = "cm")



# 7!.Child deaths in reproductive age --------

# Make sure that various cohorts share the same range of colours
# in the legend colorbar
bar_br <- seq(0, 0.8, 0.2)
bar_lim <- c(0, 0.9)

col <- "share_rep"
bar_name <- paste0("Share of all child\ndeaths experienced in\nreproductive age")

p1 <- map_share_child_deaths_in_age_range(
  cohort_show = 1950
  , country_line_size = country_line_size
  , col = col, bar_name = bar_name
  , bar_br, bar_lim
  )

# p2 <- map_share_child_deaths_in_age_range(
#   cohort_show = 1975
#   , country_line_size = country_line_size
#   , col = col, bar_name = bar_name
#   , bar_br, bar_lim
# )

p3 <- map_share_child_deaths_in_age_range(
  cohort_show = 2000
  , country_line_size = country_line_size
  , col = col, bar_name = bar_name
  , bar_br, bar_lim
)


p_name <- paste0("../../Output/map-share-cd-in-reproductive",".pdf")

# patch <- p1 / p2 / p3  +   plot_layout(guides = "collect")
# ggsave(p_name, patch, height = 21, width = 16, units = "cm")

patch <- p1 / p3  +   plot_layout(guides = "collect") 
ggsave(p_name, patch, height = 14, width = 16, units = "cm")

