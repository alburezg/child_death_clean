# Cumulative child death up to age 65

# 0. Parameters

retirement_age <- 65
title_size <- 9
# Country border size (white lines)
# Use this if politting maps separatey
# country_line_size <- 0.005
# Use this for stacked maps
country_line_size <- 0.0005

p_title <- paste0("Cumulative numer of child deaths at age 65 for a woman born in ", cohort_show, " (i.e. aged 65 in 2020)")

p_name <- paste0("../../Output/figx_map_child_death",".pdf")


# 0. Create world map ----
# Can be reused for all maps

world <- sf::st_as_sf(rworldmap::getMap(resolution = "low")) %>% 
  select(ID = ADMIN, country = ADM0_A3, geometry) %>% 
  mutate(ID = as.character(ID), country = as.character(country))


p1 <- map_child_death(cohort_show = 1955)

ggsave(p_name, p1, height = 7, width = 16, units = "cm")

# Text for labels
# Min and max values

# values <-
  df_cl_m_full %>%
  filter(cohort == cohort_show) %>% 
  filter(age == retirement_age) %>% 
    arrange(value) %>% 
    slice(1, nrow(.)) %>% 
    select(country, value)

