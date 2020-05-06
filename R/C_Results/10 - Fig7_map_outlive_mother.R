# World map showing country-level variation in the share
# of a cohort's TFR expected to outlive their mothers, for
# women entering retirement age (70) in 2020 - eg born in 1950.

 # Data required: tally_share (created for fig 4)

cohort_show <- 1955
# cohort_show <- 2000

# 1. Data management ----

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


# 1.4. Plot ====

# Create world map
  # world <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
  # world$country <- countrycode(world$ID, "country.name", "iso3c")
  
  world <- sf::st_as_sf(rworldmap::getMap(resolution = "low")) %>% 
    select(ID = ADMIN, country = ADM0_A3, geometry) %>% 
    mutate(ID = as.character(ID), country = as.character(country))
  # world$country <- countrycode(world$ID, "country.name", "iso3c")


w <- left_join(
  world
  , cl_share
  , by = "country"
) %>% 
  filter(! ID %in% "Antarctica")

# Plot

ggplot(data = w) +
  geom_sf(aes(geometry = geometry, fill = value), colour = alpha("white", 1 / 2), size = 0.005) +
  scale_fill_viridis(
    # name = "Proportion of\nchildren expected\nto outlive their\nretiring mother"
    # name = "Children expected\nto outlive their\nretiring mother (as\nproportion of TFR)"
    # name = "Children expected\nto outlive their\nmother (proportion\nof TFR for women\nentering retirement)"
    # name = "Children expected\nto outlive an average\nmother (proportion\nof TFR for women\nentering retirement)"
    name = "Offspring expected\nto outlive a woman\nretiring in 2020"
    # , option="magma"
    , option="viridis"
    , direction = -1
    # , breaks = seq(0.65, 0.95, 0.05)
    , labels = function(br) paste0(round(br*100), "%")
    ) +
  labs(
    title = ""
  ) +
  # coord_sf(datum = NA) +
  coord_sf(crs = "+proj=robin") +
  # coord_sf() +
  theme_minimal(base_size = 6) +
  theme(
    axis.line = element_blank(), axis.text = element_blank()
    , axis.ticks = element_blank(), axis.title = element_blank()
    ) +
  guides(fill = guide_colourbar(barwidth = 1))

ggsave("../../Output/fig7_map_outlive.pdf", height = 7, width = 16, units = "cm")

