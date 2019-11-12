# 
# Is there a relationship between progress on the demographic transition and 
# expossure to offspring mortality?
# In other words, do female cohorts in countries more advanced towards the demographic transition 
# lose a smaller share of their children?
# If so, then the experience of child death does really decline with the demographic transition,
# as Livi Bacci suggested.

# 0. Parametesr ----

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
  scale_x_continuous("Cohort life expectancy") +
  scale_color_gradient("Woman's birth cohort", low = "red", high = "blue", breaks = c(1950, 1975, 1999)) +
  # facet_grid(. ~ cohort) +
  facet_wrap(. ~ region) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

p_ecl_ex

ggsave(paste0("../../Output/figS4.pdf"), p_ecl_ex, width = width, height = height, units = "cm")

# 4.2. By TFR ====

p_ecl_tfr <- 
  ecl_ctfr_ex %>% 
  filter(!region %in% regions_to_remove) %>% 
  mutate(region = plyr::mapvalues(region, from = regions_long, to = regions_short)) %>% 
  ggplot(aes(x = tfr, y = share)) +
  geom_point(
    aes(colour = cohort)
    , size = point_size
  ) +
  scale_y_continuous("Fraction of TFR expected to outlive mother") +
  scale_x_continuous("Cohort Total Fertility Rate") +
  scale_color_gradient("Woman's birth cohort", low = "red", high = "blue", breaks = c(1950, 1975, 1999)) +
  # facet_grid(. ~ cohort) +
  facet_wrap(. ~ region) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

p_ecl_tfr

ggsave(paste0("../../Output/figS4a.pdf"), p_ecl_tfr, width = width, height = height, units = "cm")

# DEPCREATED: GROUPED BY REGION

# # 4.3. TFR and ex
# 
# sum_cl_tfr_ex <- 
#   ecl_ctfr_ex %>% 
#   group_by(region, cohort) %>% 
#   summarise(
#     ex = median(ex)
#     , tfr = median(tfr)
#     , cl = median(cl)
#     , median = median(cl)
#     , low_iqr = quantile(cl, quant_low)
#     , high_iqr = quantile(cl, quant_high)
#   ) %>% 
#   ungroup
# 
# long <- reshape2::melt(sum_cl_tfr_ex, id = c("region", "cohort", "median", "low_iqr", "high_iqr"))
# 
# # p_cs_number_share <-
#   sum_cl_tfr_ex %>% 
#   filter(!region %in% regions_to_remove) %>% 
#   mutate(region = factor(as.character(region), levels = regions_long)) %>% 
#   ggplot() +
#   # Region summary lines
#   geom_line(
#     aes(x = cohort, y = value, group = region, colour = region)
#     , size = region_line_size
#     , show.legend = F
#   ) +
#   # Plot ECL quantiles as bands
#   geom_ribbon(
#     aes(x = cohort, ymin = low, ymax = high, group = region, fill = region)
#     , alpha = 0.4, show.legend = F
#   ) +
#   # Plot ECL shapes to help distinguish regions
#   geom_point(
#     aes(x = cohort, y = value, group = region, colour = region
#         , shape = region
#     )
#     , size = point_size
#     , data = . %>% filter(cohort %in% c(lower_year, 1975, upper_year))
#   ) +
#   # Add facet numbers
#   geom_text(aes(x = x, y = y, label = label), data = f_lab, size = 6) +
#   # SWE and ZWE lines
#   # geom_line(
#   #   aes(x = cohort, y = value, group = region)
#   #   , linetype = "longdash"
#   #   , colour = "black"
#   #   , show.legend = F
#   #   , data = tally_share_con
#   # ) +
#   
#   scale_x_continuous(
#     "Woman's birth cohort"
#     , breaks = seq(lower_year, 2000, 10)
#     , labels = c(lower_year, seq(60, 90, 10), 2000)
#   ) +
#   scale_y_continuous(
#     "Children outlive mother"
#     , br = trans_breaks(identity, identity, 4)
#   ) +
#   scale_color_discrete(col_lab, br = regions_long, labels = regions_short) +
#   scale_fill_discrete(col_lab, br = regions_long, labels = regions_short) +
#   scale_shape_discrete(col_lab, br = regions_long, labels = regions_short) +
#   scale_size_continuous("Population share") +
#   # coord_cartesian(ylim = c(0, NA)) +
#   # facet_grid(level ~ measure, scales = 'free_y') +
#   facet_wrap(level ~ ., scales = 'free_y') +
#   theme_bw(base_size = base_size) +
#   theme(
#     legend.position = "bottom"
#     # Remove space over legend
#     , legend.margin=margin(t=-0.25, r=0.5, b=0, l=0, unit="cm")
#     # Remove space between legends
#     , legend.key.size = unit(0.1, "cm")
#     # Move y axis closer to the plot
#     , axis.title.y = element_text(margin = margin(t = 0, r = - 0.5, b = 0, l = 0))
#     , plot.margin = unit(c(t=0.2, r=0.25, b=0.1, l=0.1), unit="cm")
#     # get rid of facet boxes
#     , strip.background = element_blank()
#     # Remove spacing between facets
#     # , panel.spacing.x=unit(0.07, "cm")
#   )
# 
# 
