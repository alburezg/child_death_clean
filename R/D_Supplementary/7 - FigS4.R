
# Changes in the experience of child death throughout the Demographic 
# Transition (horizontal axis, proxied by life expectancy) in different
# regions of the world. (A-F) Association between the fraction of a
# woman's offspring expected to live longer than her and the cohort 
# life expectancy for the woman's cohort. The panels show country-level 
# trajectories by UN SDG region.

# The figure adresses the question of where there is a relationship
# between progress on the demographic transition and expossure to
# offspring mortality.
# In other words, do female cohorts in countries more advanced towards the demographic transition
# lose a smaller share of their children?
# If so, then the experience of child death does really decline with the demographic transition,
# as Livi Bacci suggested.

# 0. Parameters ----

lower_year <- 1950
upper_year <- 1999

# 0.2. Draft paper and presentation format (large)

width <- 13
height <- 11
base_size <- 14
region_line_size <- 1
point_size <- 0.25

# 1. X = TFR or ex ----

# Progression towards the DT can be approximated b ylooking at either TFR or ex


# 1.1. Cohort ex ====

ex_df <- 
  LTCB %>% 
  filter(dplyr::between(Cohort, lower_year, upper_year)) %>%
  # filter() %>% 
  filter(Age == 0) %>% 
  select(country = Country, cohort = Cohort, ex)

# 1.2. Cohort TFR ====

ctfr <- 
  ASFRC %>% 
  filter(dplyr::between(Cohort, lower_year, upper_year)) %>%
  group_by(country, Cohort) %>% 
  summarise(tfr = sum(ASFR)/1000) %>% 
  ungroup %>% 
  rename(cohort = Cohort) 

# 2. Y = E[CD]/TFR ----

expected <- 
  ecl_ctfr %>%
  dplyr::mutate(cohort = as.numeric(cohort)) %>% 
  filter(dplyr::between(cohort, lower_year, upper_year)) %>%
  mutate(share = 1 - (value / tfr))

# 3. Merge all ----

ecl_ctfr_ex <- 
  merge(
    expected
    , ex_df
    , by = c("cohort", "country")
    , all.x = T
    , all.y = F
    ) %>% 
  mutate(cohort = as.numeric(cohort)) %>% 
  select(country, region, cohort, cl = value, tfr, ex, share)

# 4. Plot ----

# 4.1. By ex ====

p_ecl_ex <- 
  ecl_ctfr_ex %>% 
  filter(!region %in% regions_to_remove) %>% 
  mutate(region = plyr::mapvalues(region, from = regions_long, to = regions_short)) %>% 
  ggplot(aes(x = ex, y = share)) +
  geom_point(
    aes(colour = cohort)
    , size = point_size
             ) +
  scale_y_continuous("Fraction of TFR expected to outlive mother") +
  scale_x_continuous("Life expectancy in cohort of women") +
  scale_color_gradient("Woman's birth cohort", low = "red", high = "blue", breaks = c(1950, 1975, 1999)) +
  # facet_grid(. ~ cohort) +
  facet_wrap(. ~ region) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

p_ecl_ex

ggsave(paste0("../../Output/figS4.pdf"), p_ecl_ex, width = width, height = height, units = "cm")

# NOT RUN:
# 4.2. By TFR ====
# 
# p_ecl_tfr <- 
#   ecl_ctfr_ex %>% 
#   filter(!region %in% regions_to_remove) %>% 
#   mutate(region = plyr::mapvalues(region, from = regions_long, to = regions_short)) %>% 
#   ggplot(aes(x = tfr, y = share)) +
#   geom_point(
#     aes(colour = cohort)
#     , size = point_size
#   ) +
#   scale_y_continuous("Fraction of TFR expected to outlive mother") +
#   scale_x_continuous("Cohort Total Fertility Rate") +
#   scale_color_gradient("Woman's birth cohort", low = "red", high = "blue", breaks = c(1950, 1975, 1999)) +
#   # facet_grid(. ~ cohort) +
#   facet_wrap(. ~ region) +
#   theme_bw() +
#   theme(
#     legend.position = "bottom"
#   )
# 
# p_ecl_tfr
# 
# ggsave(paste0("../../Output/figS4a.pdf"), p_ecl_tfr, width = width, height = height, units = "cm")
