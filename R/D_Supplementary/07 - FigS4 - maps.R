# UPDATE NEEDED!
# 20200429: this is the same as the map in the main text, but you can 
# change to include other maps in the supplementary materials

# World map: children outliving their mothesr
# for a woman aged 70 in 2020

# Data required: tally_share (created for fig 4)

# 0. Parameters

retirement_age <- 70
title_size <- 9

# 0. Create world map ----
# Can be reused for all maps

world <- sf::st_as_sf(rworldmap::getMap(resolution = "low")) %>% 
  select(ID = ADMIN, country = ADM0_A3, geometry) %>% 
  mutate(ID = as.character(ID), country = as.character(country))

# 1. Share children outlive mother ----

p1 <- map_share_outlived_mother(cohort_show = 1950)
p2 <- map_share_outlived_mother(cohort_show = 1975)
p3 <- map_share_outlived_mother(cohort_show = 2000)

p_name <- paste0("../../Output/figS4-share-survive",".pdf")

patch <- p1 / p2 / p3  +   plot_layout(guides = "collect") 

ggsave(p_name, patch, height = 21, width = 16, units = "cm")

# 2. Total children survive to mother's retirement ----


p1 <- map_child_survival(cohort_show = 1950)
p2 <- map_child_survival(cohort_show = 1975)
p3 <- map_child_survival(cohort_show = 2000)

p_name <- paste0("../../Output/figS4-cs",".pdf")

patch <- p1 / p2 / p3  +   plot_layout(guides = "collect") 

ggsave(p_name, patch, height = 21, width = 16, units = "cm")

# 3. Total children died at retirement ----

p1 <- map_child_death(cohort_show = 1950)
p2 <- map_child_death(cohort_show = 1975)
p3 <- map_child_death(cohort_show = 2000)

p_name <- paste0("../../Output/figS4-cd",".pdf")

patch <- p1 / p2 / p3  +   plot_layout(guides = "collect") 

ggsave(p_name, patch, height = 21, width = 16, units = "cm")

# 4. Generational burden by region ----

# Plotting this by country doesn't make sense given population differentials
# One altenative is to do it by region, giving the same colour to all
# countries sharing a region

p1 <- map_burden_cd(cohort_show = 1950)
p2 <- map_burden_cd(cohort_show = 1975)
p3 <- map_burden_cd(cohort_show = 2000)

p_name <- paste0("../../Output/figS4-burden",".pdf")

patch <- p1 / p2 / p3  +   plot_layout(guides = "collect") 

ggsave(p_name, patch, height = 21, width = 16, units = "cm")

print("All maps saved!")
