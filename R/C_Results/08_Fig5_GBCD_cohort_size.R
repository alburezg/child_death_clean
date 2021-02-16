
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

ggsave(paste0("../../Output/fig5_births_burden_",fertility_variant,".pdf"))

# DEPRECTATED ----

# # Include chna and india
# 
# 
# # Correlation betwen Population growth and Generational burden of child death
# 
# years_keep <- c(1950, 2000)
# 
# # Df of change in cohort size and generational burden
# 
# change <-
#   abs_df %>%
#   # Get generations burden from age-specific burden
#   group_by(cohort, region, country) %>% 
#   summarise(value = sum(absolute, na.rm = T)) %>% 
#   ungroup() %>% 
#   select(region, country, cohort, burden = value) %>% 
#   # link to cohort size data 
#   left_join(
#     female_births %>% 
#       rename(cohort = year, births = value)
#     , by = c("country", "cohort")
#   ) %>% 
#   mutate(
#     cohort = factor(cohort, levels = coh_br)
#     , region = plyr::mapvalues(region, from = regions_long, to = regions_short)
#   )
# 
# 
# # 2. Plots
# 
# 
# #  2.2. Isoplot
# 
# coh_br <- floor(seq(1950, 2000, length.out = 4))
# 
# change_reg <- 
#   change %>% 
#   # mutate(burden = burden / 2) %>% 
#   filter(! region %in% regions_to_remove) %>% 
#   filter(cohort %in% coh_br) %>% 
#   group_by(region, cohort) %>% 
#   summarise(
#     births = sum(births, na.rm = T)
#     , burden = sum(burden, na.rm = T)
#   ) %>% 
#   ungroup()
# 
# change_small <- change %>% 
#   filter(country %in% c("china", "india")) %>% 
#   filter(cohort %in% coh_br) %>% 
#   mutate(country = stringr::str_to_title(country)  ) 
# # select(region = country, cohort, burden, births)
# 
# # change_reg <- bind_rows(
# #   change_reg, change_small
# # )
# 
# # Plot
# ggplot() +
#   # Background country lines
#   geom_path(
#     aes(x = births, y = burden, group = country, colour = region)
#     , data = change_small
#     , size = 1
#   ) +
#   geom_label_repel(
#     aes(x = births, y = burden, label = country, colour = region)
#     , data = change_small %>%
#       group_by(country) %>%
#       arrange(births) %>% slice(1)
#     , nudge_x = 1.8e6
#   ) +
#   geom_abline(slope = c(1), linetype = "dashed") +
#   # label for diagonal line
#   geom_text(
#     aes(x = x, y = y, label = label)
#     # , data = data.frame(births = 20e6, burden = 20e6, label = "45°", region = regions_short[1], cohort = "1950")
#     , data = data.frame(x = 16e6, y = 20e6, label = "45°", region = "", cohort = "1950")
#     , colour = "black"
#   ) +
#   geom_label_repel(
#     aes(x = births, y = burden, label = region, colour = region)
#     , data = change_reg %>% 
#       group_by(region) %>% 
#       arrange(births) %>% slice(1)
#     , nudge_x = 1.8e6
#   ) +
#   geom_path(
#     aes(x = births, y = burden, colour = region)
#     , size = 1
#     , data = change_reg
#   ) +
#   geom_point(aes(x = births, y = burden, shape = cohort, colour = region, group = region)
#              , size = 4
#              , data = change_reg
#   ) +
#   scale_x_continuous(
#     "Size of female birth cohort"
#     , breaks = seq(0, 100e6, 25e6)
#     , labels = function(br) paste0(br/1e6, "M")
#   ) +
#   scale_y_continuous(
#     "Generational Burden of Child Death"
#     , breaks = seq(0, 100e6, 25e6)
#     , labels = function(br) paste0(br/1e6, "M")
#   ) +
#   scale_shape_manual("Cohort", values = c(16, 15, 1, 17)) +
#   # coord_equal() +
#   theme_bw() +
#   theme(
#     legend.position = "bottom"
#     # Remove space over legend
#     , legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")
#   ) +
#   guides(color = FALSE)
# 
# ggsave("../../Output/fig5_births_burden.pdf")

# DEPCREACTED -----
# # 2.1. As rates 
# 
# rates <- 
#   change %>% 
#   # mutate(burden = burden / 2) %>% 
#   filter(cohort %in% years_keep) %>% 
#   # Reshape to get difference
#   group_by(region, country) %>%
#   mutate(
#     share_burden = lead(burden) / burden 
#     , share_births = lead(births) / births 
#   ) %>% 
#   slice(1) %>% 
#   ungroup %>% 
#   select(-cohort) %>% 
#   select(- burden, - births)  
# 
# rates %>% 
#   ggplot(aes(x = share_births, y = share_burden, colour = region)) + 
#   geom_point() +
#   # geom_abline(slope = c(1/8, 1/4, 1/2, 1)) +
#   geom_abline(slope = c(1)) +
#   # scale_x_continuous("Change in cohort size (log)") +
#   # scale_y_continuous("Change in Generational Burden of Child Death (log)") +
#   # geom_line(
#   #   aes(x = share_births, y = share_burden, group = 1)
#   #   , data = data.frame(share_births = exp(1:10), share_burden = exp(1:10), region = "")
#   #           ) +
#   scale_x_log10(
#     breaks = scales::trans_breaks("log10", function(x) 10^x)
#     , labels = scales::trans_format("log10", scales::math_format(10^.x))
#   ) +
#   scale_y_log10(
#     breaks = scales::trans_breaks("log10", function(x) 10^x)
#     , labels = scales::trans_format("log10", scales::math_format(10^.x))
#   ) +
#   annotation_logticks() +
#   labs(x = "Population growth (log)", y = "Change in Generational Burden of Child Death (log)") +
#   coord_equal(ratio = 1) +
#   theme_bw()