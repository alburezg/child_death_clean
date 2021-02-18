
# Correlation betwen Population growth and Generational burden of child death

years_keep <- c(1950, 2000)

# Df of change in cohort size and generational burden

change <-
  abs_df %>%
  # Get generations burden from age-specific burden
  group_by(cohort, region, country) %>% 
  summarise(value = sum(absolute, na.rm = T)) %>% 
  ungroup() %>% 
  select(region, country, cohort, burden = value) %>% 
  # link to cohort size data 
  left_join(
    female_births %>% 
      rename(cohort = year, births = value)
    , by = c("country", "cohort")
  ) 


# 2. Plots ----


#  2.2. Isoplot ====

coh_br <- floor(seq(1950, 2000, length.out = 4))

change %>% 
  # mutate(burden = burden / 2) %>% 
  filter(! region %in% regions_to_remove) %>% 
  filter(cohort %in% coh_br) %>% 
  group_by(region, cohort) %>% 
  summarise(
    births = sum(births, na.rm = T)
    , burden = sum(burden, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    cohort = factor(cohort, levels = coh_br)
    , region = plyr::mapvalues(region, from = regions_long, to = regions_short)
    ) %>% 
  ggplot(aes(x = births, y = burden, shape = cohort, colour = region, label = region, group = region)) +
  geom_abline(slope = c(1), linetype = "dashed") +
  # label for diagonal line
  geom_text(
    aes(x = x, y = y, label = label)
    # , data = data.frame(births = 20e6, burden = 20e6, label = "45°", region = regions_short[1], cohort = "1950")
    , data = data.frame(x = 16e6, y = 20e6, label = "45°", region = "", cohort = "1950")
    , colour = "black"
  ) +
  geom_label_repel(
     data = . %>% 
      group_by(region) %>% 
      arrange(births) %>% slice(1)
     , nudge_x = 1.8e6
  ) +
  geom_path(size = 1) +
  geom_point(size = 4) +
  scale_x_continuous(
    "Size of female birth cohort"
    , breaks = seq(0, 100e6, 25e6)
      , labels = function(br) paste0(br/1e6, "M")
                     ) +
  scale_y_continuous(
    "Generational Burden of Child Death"
    , breaks = seq(0, 100e6, 25e6)
    , labels = function(br) paste0(br/1e6, "M")
    ) +
  scale_shape_manual("Cohort", values = c(16, 15, 1, 17)) +
  # coord_equal() +
  theme_bw() +
  theme(
    legend.position = "bottom"
    # Remove space over legend
    , legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")
  ) +
  guides(color = FALSE)

ggsave(paste0("../../Output/fig4_births_burden.pdf"))

print("Figure 4 saved to ../../Output")
