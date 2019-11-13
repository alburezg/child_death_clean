
# 0. Parameters ----

# Chose percentiles to estimate for each region
quant_low <- 0.4
quant_high <- 0.6

age_br <- c(seq(5, 100, 20), 100)

# 0.1. PNAS plotting params (small)
# width <- 8
# height <- 6
# base_size <- 9
# region_line_size <- 0.4
# point_size <- 1.5

# 0.2. Draft paper and presentation format (large)

width <- 16
height <- 10
base_size <- 14
region_line_size <- 1
point_size <- 4

sources <- c(
 "Expected number of children"
  , "Expected number as fraction of TFR"
)

# 4.1.	Number of children expected to outlive mothers ====
# ~~~~~~~~~~~~~

csex_countries <- 
  cs_ex_pop_country %>% 
  filter(type == 'country') %>% 
  mutate(
    region = as.character(region)
    , cohort = as.numeric(cohort)
  ) %>% 
  select(country, region, cohort, value) %>% 
  arrange(country, cohort)


csex_world <- 
  csex_countries %>% 
  group_by(cohort) %>%
  summarise(
    median = median(value)
    , low_iqr = quantile(value, quant_low, na.rm = T)
    , high_iqr = quantile(value, quant_high, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(source = sources[1]) %>% 
  arrange(cohort)

# 4.2.	Share of children outlive mothers ====
# ~~~~~~~~~~~~~

out_countries <-
  ecl_ctfr %>%
  filter(type == 'country') %>% 
  mutate(
    region = as.character(region)
  ) %>% 
  mutate(share = 1 - value / tfr) %>% 
  select(country, region, cohort, value = share) %>% 
  arrange(country, cohort)


out_world <- 
  out_countries %>% 
  group_by(cohort) %>%
  summarise(
    median = median(value)
    , low_iqr = quantile(value, quant_low, na.rm = T)
    , high_iqr = quantile(value, quant_high, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(source = sources[2]) %>% 
  mutate(cohort = as.numeric(cohort)) %>% 
  arrange(cohort)

# 5.2. Fig S2 ====

# Measures without age component

world2 <- 
  bind_rows(
    csex_world
    , out_world
  ) %>% 
  mutate(
    source = factor(source, levels = sources)
  )

f_lab <- data.frame(
  x = 1955
  , y = c(4, 0.935)
  , label = LETTERS[1:2]
  , source = sources[1:2]
)

p_world2 <-
  world2 %>% 
  ggplot() +
  geom_line(
    aes(x = cohort, y = median, group = 1)
    , size = region_line_size
    , show.legend = FALSE
  ) +
  # Plot ECL quantiles as bands
  geom_ribbon(
    aes(x = cohort, ymin = low_iqr, ymax = high_iqr, group = 1)
    , alpha = 0.4, show.legend = F
  ) +
  # Add facet numbers
  geom_text(aes(x = x, y = y, label = label), data = f_lab, size = 6) +
  # geom_point(
  #   aes(x = cohort, y = median, group = cohort, colour = cohort
  #       , shape = cohort
  #   )
  #   , size = point_size
  #   , data = . %>% filter(age %in% age_br)
  # ) +
  # Add facet numbers
  # geom_text(aes(x = x, y = y, label = label), data = f_lab, size = 6) +
  scale_x_continuous(
    "Woman's birth cohort"
    , breaks = seq(1950, 2000, 10)
    , labels = c(1950, seq(60, 90, 10), 2000)
    ) +
    scale_y_continuous(
      "Children outlive mother"
      , br = trans_breaks(identity, identity, 4)
    ) +
  facet_wrap(. ~ source, scales = 'free', strip.position =  "top") +
  # Use with four measures
  theme_bw(base_size = base_size)+
    theme(
      legend.position = "bottom"
      # Remove space over legend
      , legend.margin=margin(t=-0.25, r=0.5, b=0, l=0, unit="cm")
      # Remove space between legends
      , legend.key.size = unit(0.1, "cm")
      # Move y axis closer to the plot
      , axis.title.y = element_text(margin = margin(t = 0, r = - 0.5, b = 0, l = 0))
      , plot.margin = unit(c(t=0.2, r=0.25, b=0.1, l=0.1), unit="cm")
      # get rid of facet boxes
      , strip.background = element_blank()
      # Remove spacing between facets
      # , panel.spacing.x=unit(0.07, "cm")
    )

p_world2

ggsave(paste0("../../Output/figS2.pdf"), p_world2, width = width, height = height, units = "cm")
