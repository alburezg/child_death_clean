# UPDATE NEEDED!
# 20200429: this is the same as the map in the main text, but you can 
# change to include other maps in the supplementary materials

# World map: children outliving their mothesr
# for a woman aged 70 in 2020

# Data required: tally_share (created for fig 4)

retirement_age <- 70
title_size <- 9

# 0. Create world map ----
# Can be reused for all maps

world <- sf::st_as_sf(rworldmap::getMap(resolution = "low")) %>% 
  select(ID = ADMIN, country = ADM0_A3, geometry) %>% 
  mutate(ID = as.character(ID), country = as.character(country))

# 1. Share children outlive mother ----

# Make sure that various cohorts share the same range of colours
# in the legend colorbar
bar_br <- seq(0.70, 1, 0.05)
bar_lim <- c(0.65, 1)

# 1.1. 1950 cohort====

cohort_show <- 1950

# bar_name <- paste0("Offspring expected\nto outlive a woman\nretiring in ", cohort_show + 70)
bar_name <- paste0("Offspring expected\nto outlive a woman")
p_title <- paste0("Women born in ", cohort_show, " and retiring in ", cohort_show + 70)
p_name <- paste0("../../Output/figS4-share-survive-",cohort_show,".pdf")


# Keep only people entering retirement age, defined as the 1955 cohort
# currently, approaching age 65

# Get share outlived mother:
cl_share <-
  ecl_ctfr %>%
  filter(cohort == cohort_show) %>% 
  filter(country != 'channel islands') %>% 
  mutate(
    share = 1 - value / tfr
    , country = ifelse(country == "eswatini", "swaziland", country)
    , country = countrycode(country, "country.name", "iso3c")
  ) %>% 
  select(country, value = share) 

# Join with map
w <- left_join(
  world
  , cl_share
  , by = "country"
) %>% 
  filter(! ID %in% "Antarctica")

# Plot

p1 <- 
ggplot(data = w) +
  geom_sf(aes(geometry = geometry, fill = value), colour = alpha("white", 1 / 2), size = 0.005) +
  scale_fill_viridis(
    name = bar_name
    , option="magma"
    , breaks = bar_br
    , limits = bar_lim
    , labels = function(br) paste0(round(br*100), "%")
  ) +
  labs(
    title = ""
  ) +
  coord_sf(crs = "+proj=robin") +
  ggtitle(p_title) +
  theme_minimal(base_size = 6) +
  theme(
    axis.line = element_blank(), axis.text = element_blank()
    , axis.ticks = element_blank(), axis.title = element_blank()
    , plot.title = element_text(size = title_size, face="bold")
    ) +
  guides(fill = guide_colourbar(barwidth = 1))

ggsave(p_name, height = 7, width = 16, units = "cm")

# 1.2. 2020 cohort====

cohort_show <- 2000
# cohort_show <- 1975

bar_name <- paste0("Offspring expected\nto outlive a woman")
p_title <- paste0("Women born in ", cohort_show, " and retiring in ", cohort_show + 70)
p_name <- paste0("../../Output/figS4-share-survive-",cohort_show,".pdf")

# Keep only people entering retirement age, defined as the 1955 cohort
# currently, approaching age 65

# Get share outlived mother:
cl_share <-
  ecl_ctfr %>%
  filter(cohort == cohort_show) %>% 
  filter(country != 'channel islands') %>% 
  mutate(
    share = 1 - value / tfr
    , country = ifelse(country == "eswatini", "swaziland", country)
    , country = countrycode(country, "country.name", "iso3c")
  ) %>% 
  select(country, value = share) 

# Join with map
w <- left_join(
  world
  , cl_share
  , by = "country"
) %>% 
  filter(! ID %in% "Antarctica")

# Plot

p2 <- 
ggplot(data = w) +
  geom_sf(aes(geometry = geometry, fill = value), colour = alpha("white", 1 / 2), size = 0.005) +
  scale_fill_viridis(
    name = bar_name
    , option="magma"
    , breaks = bar_br
    , limits = bar_lim
    , labels = function(br) paste0(round(br*100), "%")
  ) +
  labs(
    title = ""
  ) +
  coord_sf(crs = "+proj=robin") +
  ggtitle(p_title) +
  theme_minimal(base_size = 6) +
  theme(
    axis.line = element_blank(), axis.text = element_blank()
    , axis.ticks = element_blank(), axis.title = element_blank()
    , plot.title = element_text(size = title_size, face="bold")
  ) +
  guides(fill = guide_colourbar(barwidth = 1))

ggsave(p_name, height = 7, width = 16, units = "cm")
 
# 1.3. Patchwork ====

p_name <- paste0("../../Output/figS4-share-survive-patch",cohort_show,".pdf")

p1 / p2  + 
  plot_layout(guides = "collect")

ggsave(p_name, height = 14, width = 16, units = "cm")

# 2. Total children survive to mother's retirement ----

# Make sure that various cohorts share the same range of colours
# in the legend colorbar
bar_br <- seq(1, 3, 1)
bar_lim <- c(0, 3.3)

# 2.1. 1950 cohort====

cohort_show <- 1950

bar_name <- paste0("Offspring surviving to woman\nretiring in ", cohort_show + 70)
p_name <- paste0("../../Output/figS4-cs-",cohort_show,".pdf")

# Keep only people entering retirement age, defined as the 1955 cohort
# currently, approaching age 65

# Get share outlived mother:
values <-
  df_cl_m_full %>%
  filter(cohort == cohort_show) %>% 
  filter(age == retirement_age) %>% 
  filter(country != 'channel islands') %>% 
  mutate(
    country = ifelse(country == "eswatini", "swaziland", country)
    , country = countrycode(country, "country.name", "iso3c")
  ) %>% 
  select(country, value) 

# Join with map
w <- left_join(
  world
  , values
  , by = "country"
) %>% 
  filter(! ID %in% "Antarctica")

# Plot

ggplot(data = w) +
  geom_sf(aes(geometry = geometry, fill = value), colour = alpha("white", 1 / 2), size = 0.005) +
  scale_fill_viridis(
    name = bar_name
    , option="viridis"
    , breaks = bar_br
    , limits = bar_lim
  ) +
  labs(
    title = ""
  ) +
  coord_sf(crs = "+proj=robin") +
  theme_minimal(base_size = 6) +
  theme(
    axis.line = element_blank(), axis.text = element_blank()
    , axis.ticks = element_blank(), axis.title = element_blank()
  ) +
  guides(fill = guide_colourbar(barwidth = 1))

ggsave(p_name, height = 7, width = 16, units = "cm")

# 2.2. 2000 cohort====

cohort_show <- 2000

bar_name <- paste0("Offspring surviving to woman\nretiring in ", cohort_show + 70)
p_name <- paste0("../../Output/figS4-cs-",cohort_show,".pdf")

# Keep only people entering retirement age, defined as the 1955 cohort
# currently, approaching age 65

# Get share outlived mother:
values <-
  df_cl_m_full %>%
  filter(cohort == cohort_show) %>% 
  filter(age == retirement_age) %>% 
  filter(country != 'channel islands') %>% 
  mutate(
    country = ifelse(country == "eswatini", "swaziland", country)
    , country = countrycode(country, "country.name", "iso3c")
  ) %>% 
  select(country, value) 

# Join with map
w <- left_join(
  world
  , values
  , by = "country"
) %>% 
  filter(! ID %in% "Antarctica")

# Plot

ggplot(data = w) +
  geom_sf(aes(geometry = geometry, fill = value), colour = alpha("white", 1 / 2), size = 0.005) +
  scale_fill_viridis(
    name = bar_name
    , option="viridis"
    , breaks = bar_br
    , limits = bar_lim
  ) +
  labs(
    title = ""
  ) +
  coord_sf(crs = "+proj=robin") +
  theme_minimal(base_size = 6) +
  theme(
    axis.line = element_blank(), axis.text = element_blank()
    , axis.ticks = element_blank(), axis.title = element_blank()
  ) +
  guides(fill = guide_colourbar(barwidth = 1))

ggsave(p_name, height = 7, width = 16, units = "cm")

# 3. Total children died at retirement ----

# Make sure that various cohorts share the same range of colours
# in the legend colorbar
bar_br <- seq(0, 3, 1)
bar_lim <- c(0, 3.5)

# 3.1. 1950 cohort====

cohort_show <- 1950

bar_name <- paste0("Offspring died to woman\nretiring in ", cohort_show + 70)
p_name <- paste0("../../Output/figS4-cd-",cohort_show,".pdf")

# Keep only people entering retirement age, defined as the 1955 cohort
# currently, approaching age 65

# Get share outlived mother:
values <-
  df_cl_m_full %>%
  filter(cohort == cohort_show) %>% 
  filter(age == retirement_age) %>% 
  filter(country != 'channel islands') %>% 
  mutate(
    country = ifelse(country == "eswatini", "swaziland", country)
    , country = countrycode(country, "country.name", "iso3c")
  ) %>% 
  select(country, value) 

# Join with map
w <- left_join(
  world
  , values
  , by = "country"
) %>% 
  filter(! ID %in% "Antarctica")

# Plot

ggplot(data = w) +
  geom_sf(aes(geometry = geometry, fill = value), colour = alpha("white", 1 / 2), size = 0.005) +
  scale_fill_viridis(
    name = bar_name
    , option="magma"
    , direction = -1
    , breaks = bar_br
    , limits = bar_lim
  ) +
  labs(
    title = ""
  ) +
  coord_sf(crs = "+proj=robin") +
  theme_minimal(base_size = 6) +
  theme(
    axis.line = element_blank(), axis.text = element_blank()
    , axis.ticks = element_blank(), axis.title = element_blank()
  ) +
  guides(fill = guide_colourbar(barwidth = 1))

ggsave(p_name, height = 7, width = 16, units = "cm")

# 3.2. 2000 cohort====

cohort_show <- 2000

bar_name <- paste0("Offspring died to woman\nretiring in ", cohort_show + 70)
p_name <- paste0("../../Output/figS4-cd-",cohort_show,".pdf")

# Keep only people entering retirement age, defined as the 1955 cohort
# currently, approaching age 65

# Get share outlived mother:
values <-
  df_cl_m_full %>%
  filter(cohort == cohort_show) %>% 
  filter(age == retirement_age) %>% 
  filter(country != 'channel islands') %>% 
  mutate(
    country = ifelse(country == "eswatini", "swaziland", country)
    , country = countrycode(country, "country.name", "iso3c")
  ) %>% 
  select(country, value) 

# Join with map
w <- left_join(
  world
  , values
  , by = "country"
) %>% 
  filter(! ID %in% "Antarctica")

# Plot

ggplot(data = w) +
  geom_sf(aes(geometry = geometry, fill = value), colour = alpha("white", 1 / 2), size = 0.005) +
  scale_fill_viridis(
    name = bar_name
    , option="magma"
    , direction = -1
    , breaks = bar_br
    , limits = bar_lim
  ) +
  labs(
    title = ""
  ) +
  coord_sf(crs = "+proj=robin") +
  theme_minimal(base_size = 6) +
  theme(
    axis.line = element_blank(), axis.text = element_blank()
    , axis.ticks = element_blank(), axis.title = element_blank()
  ) +
  guides(fill = guide_colourbar(barwidth = 1))

ggsave(p_name, height = 7, width = 16, units = "cm")

# 4. Generational burden by region ----

# Plotting this by country doesn't make sense given population differentials
# One altenative is to do it by region, giving the same colour to all
# countries sharing a region

# Make sure that various cohorts share the same range of colours
# in the legend colorbar
bar_br <- seq(0, 30, 5)
bar_lim <- c(0, 30)

# 4.1. 1950 cohort====

cohort_show <- 1950

bar_name <- paste0("Offspring died to woman\nretiring in ", cohort_show + 70)
p_name <- paste0("../../Output/figS4-burden-",cohort_show,".pdf")

# Keep only people entering retirement age, defined as the 1955 cohort
# currently, approaching age 65

# Get share outlived mother:
values <-
  abs_df %>%
  # Get generations burden from age-specific burden
  group_by(cohort, country) %>% 
  mutate(value = cumsum(absolute)) %>% 
  ungroup() %>% 
  filter(cohort == cohort_show) %>% 
  filter(age == retirement_age) %>% 
  filter(country != 'channel islands') %>% 
  mutate(
    value = value / 1e6
     , country = ifelse(country == "eswatini", "swaziland", country)
    , country = countrycode(country, "country.name", "iso3c")
  ) %>% 
  select(country, value) 

# Join with map
w <- left_join(
  world
  , values
  , by = "country"
) %>% 
  filter(! ID %in% "Antarctica")

# Plot

p1 <- 
  ggplot(data = w) +
  geom_sf(aes(geometry = geometry, fill = value), colour = alpha("white", 1 / 2), size = 0.005) +
  scale_fill_viridis(
    name = bar_name
    , option="viridis"
    , breaks = bar_br
    , limits = bar_lim
  ) +
  labs(
    title = ""
  ) +
  coord_sf(crs = "+proj=robin") +
  theme_minimal(base_size = 6) +
  theme(
    axis.line = element_blank(), axis.text = element_blank()
    , axis.ticks = element_blank(), axis.title = element_blank()
  ) +
  guides(fill = guide_colourbar(barwidth = 1))

ggsave(p_name, height = 7, width = 16, units = "cm")

# 4.2. 2000 cohort====

cohort_show <- 2000

bar_name <- paste0("Offspring died to woman\nretiring in ", cohort_show + 70)
p_name <- paste0("../../Output/figS4-burden-",cohort_show,".pdf")

# Keep only people entering retirement age, defined as the 1955 cohort
# currently, approaching age 65

# Get share outlived mother:
values <-
  abs_df %>%
  # Get generations burden from age-specific burden
  group_by(cohort, country) %>% 
  mutate(value = cumsum(absolute)) %>% 
  ungroup() %>% 
  filter(cohort == cohort_show) %>% 
  filter(age == retirement_age) %>% 
  filter(country != 'channel islands') %>% 
  mutate(
    value = value / 1e6
    , country = ifelse(country == "eswatini", "swaziland", country)
    , country = countrycode(country, "country.name", "iso3c")
  ) %>% 
  select(country, value) 

# Join with map
w <- left_join(
  world
  , values
  , by = "country"
) %>% 
  filter(! ID %in% "Antarctica")

# Plot

p2 <- 
ggplot(data = w) +
  geom_sf(aes(geometry = geometry, fill = value), colour = alpha("white", 1 / 2), size = 0.005) +
  scale_fill_viridis(
    name = bar_name
    , option="viridis"
    , breaks = bar_br
    , limits = bar_lim
  ) +
  labs(
    title = ""
  ) +
  coord_sf(crs = "+proj=robin") +
  theme_minimal(base_size = 6) +
  theme(
    axis.line = element_blank(), axis.text = element_blank()
    , axis.ticks = element_blank(), axis.title = element_blank()
  ) +
  guides(fill = guide_colourbar(barwidth = 1))

ggsave(p_name, height = 7, width = 16, units = "cm")

# 4.3. Patch ====

p_name <- paste0("../../Output/figS4-burden-patch-",cohort_show,".pdf")

p1 / p2 + plot_layout(guides = "collect")

ggsave(p_name, height = 15, width = 16, units = "cm")
