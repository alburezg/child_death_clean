# UPDATE NEEDED!
# 20200429: this is the same as the map in the main text, but you can 
# change to include other maps in the supplementary materials

# World map: children outliving their mothesr
# for a woman aged 70 in 2020

# Data required: tally_share (created for fig 4)

# 0. Parameters

retirement_age <- 70
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

# 1. Share children outlive mothers ----

p1 <- map_share_outlived_mother(cohort_show = 1950, country_line_size)
# p2 <- map_share_outlived_mother(cohort_show = 1975, country_line_size)
p3 <- map_share_outlived_mother(cohort_show = 2000, country_line_size)

p_name <- paste0("../../Output/map-share-outlive",".pdf")

# patch <- p1 / p2 / p3  +   plot_layout(guides = "collect") 
# ggsave(p_name, patch, height = 21, width = 16, units = "cm")

patch <- p1 / p3  +   plot_layout(guides = "collect") 
ggsave(p_name, patch, height = 14, width = 16, units = "cm")


# 2. Total children survive to mother's retirement ----


p1 <- map_child_survival(cohort_show = 1950)
# p2 <- map_child_survival(cohort_show = 1975)
p3 <- map_child_survival(cohort_show = 2000)

p_name <- paste0("../../Output/map-cs",".pdf")

# patch <- p1 / p2 / p3  +   plot_layout(guides = "collect") 
# ggsave(p_name, patch, height = 21, width = 16, units = "cm")

patch <- p1 / p3  +   plot_layout(guides = "collect") 
ggsave(p_name, patch, height = 14, width = 16, units = "cm")

# 4. Total children died  ----

# before maternal age 70

p1 <- map_child_death(cohort_show = 1950)
# p2 <- map_child_death(cohort_show = 1975)
p3 <- map_child_death(cohort_show = 2000)

p_name <- paste0("../../Output/map-cd",".pdf")

# patch <- p1 / p2 / p3  +   plot_layout(guides = "collect") 
# ggsave(p_name, patch, height = 21, width = 16, units = "cm")

patch <- p1 / p3  +   plot_layout(guides = "collect") 
ggsave(p_name, patch, height = 14, width = 16, units = "cm")

# 5. Generational burden by region ----

# Plotting this by country doesn't make sense given population differentials
# One altenative is to do it by region, giving the same colour to all
# countries sharing a region

p1 <- map_burden_cd(cohort_show = 1950)
# p2 <- map_burden_cd(cohort_show = 1975)
p3 <- map_burden_cd(cohort_show = 2000)

p_name <- paste0("../../Output/map-burden",".pdf")

# patch <- p1 / p2 / p3  +   plot_layout(guides = "collect") 
# ggsave(p_name, patch, height = 21, width = 16, units = "cm")

patch <- p1 / p3  +   plot_layout(guides = "collect") 
ggsave(p_name, patch, height = 14, width = 16, units = "cm")

# END
print("All maps saved!")

# 6. Child deaths after retirement ----

# 6.1. Get df of distributino of child deaths by woman's age

rep_age <- 15:49
ret_age <- 70:99

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
  # filter(cohort %in% c(cohort_show)) %>% 
  pivot_wider(names_from = agegr, values_from = value) %>% 
  mutate(
    share_rep = reproductive / (retirement + reproductive + other)
    , share_ret = retirement / (retirement + reproductive + other)
    # , share_sum = share_rep + share_ret
    , share_more_in_rep = share_ret/share_rep
  ) %>% 
  select(country, cohort, starts_with("share"))

# Plot

# Make sure that various cohorts share the same range of colours
# in the legend colorbar
bar_br <- seq(0, 0.8, 0.2)
bar_lim <- c(0, 0.82)

col <- "share_ret"
bar_name <- paste0("Share of all child\ndeaths experienced after\nwoman's retirement")

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

p_name <- paste0("../../Output/map-share-cd-in-retirement",".pdf")

# patch <- p1 / p2 / p3  +   plot_layout(guides = "collect")
# ggsave(p_name, patch, height = 21, width = 16, units = "cm")

patch <- p1 / p3  +   plot_layout(guides = "collect") 
ggsave(p_name, patch, height = 14, width = 16, units = "cm")

# 7!. Share of child deaths in reproductive age --------

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