# World map showing country-level variation in the share
# of a cohort's TFR expected to outlive their mothers, for
# women entering retirement age (70) in 2020 - eg born in 1950.

 # Data required: tally_share (created for fig 4)

cohort_show <- 1955
retirement_age <- 65
menopause_age <- 50

# p_title <- paste0("Women born in ", cohort_show, " and reaching retirement age approximately in ", cohort_show + retirement_age)
# p_title <- paste0("Women born in ", cohort_show, " and reaching retirement age approximately in ", cohort_show + retirement_age)
# p_title <- paste0("Percentage of children outliving an average woman born in ", cohort_show, " (i.e. retiring approximately in ", cohort_show + retirement_age, ")")

p_title <- paste0("Percentage of children outliving an average woman born in ", cohort_show, " (i.e. aged 65 in 2020)")

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

  world <- sf::st_as_sf(rworldmap::getMap(resolution = "low")) %>% 
    select(ID = ADMIN, country = ADM0_A3, geometry) %>% 
    mutate(ID = as.character(ID), country = as.character(country))


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
    name = "Offspring expected\nto outlive an average\nwoman"
    # , option="magma"
    , option="viridis"
    , direction = -1
    # , breaks = seq(0.65, 0.95, 0.05)
    , labels = function(br) paste0(round(br*100), "%")
    ) +
  ggtitle(p_title) +
  # coord_sf(datum = NA) +
  coord_sf(crs = "+proj=robin") +
  # coord_sf() +
  theme_minimal(base_size = 6) +
  theme(
    axis.line = element_blank(), axis.text = element_blank()
    , axis.ticks = element_blank(), axis.title = element_blank()
    , plot.title = element_text(size = 9, face="bold")
    ) +
  guides(fill = guide_colourbar(barwidth = 1))

ggsave("../../Output/fig6_map_outlive.pdf", height = 7, width = 16, units = "cm")

# Text to add as label

cl_share %>% 
  arrange(value) %>% 
  slice(1, nrow(.))
