
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
  geom_text_repel(
     data = . %>% 
      group_by(region) %>% 
      arrange(births) %>% slice(1)
     , nudge_x = 1.8e6
  ) +
  geom_path(size = 1) +
  geom_point(size = 4) +
  scale_x_continuous(
    "Female birth cohort size"
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

ggsave("../../Output/figS3_births_burden.pdf")


# 2.1. As rates ====

rates <- 
  change %>% 
  # mutate(burden = burden / 2) %>% 
  filter(cohort %in% years_keep) %>% 
  # Reshape to get difference
  group_by(region, country) %>%
  mutate(
    share_burden = lead(burden) / burden 
    , share_births = lead(births) / births 
  ) %>% 
  slice(1) %>% 
  ungroup %>% 
  select(-cohort) %>% 
  select(- burden, - births)  

rates %>% 
  ggplot(aes(x = share_births, y = share_burden, colour = region)) + 
  geom_point() +
  # geom_abline(slope = c(1/8, 1/4, 1/2, 1)) +
  geom_abline(slope = c(1)) +
  # scale_x_continuous("Change in cohort size (log)") +
  # scale_y_continuous("Change in Generational Burden of Child Death (log)") +
  # geom_line(
  #   aes(x = share_births, y = share_burden, group = 1)
  #   , data = data.frame(share_births = exp(1:10), share_burden = exp(1:10), region = "")
  #           ) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x)
    , labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x)
    , labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  annotation_logticks() +
  labs(x = "Population growth (log)", y = "Change in Generational Burden of Child Death (log)") +
  coord_equal(ratio = 1) +
  theme_bw()
