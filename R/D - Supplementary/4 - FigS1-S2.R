
# Chose percentiles to estimate for each region
# quant_low <- 0.45
# quant_high <- 0.55

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
height <- 12
base_size <- 15
region_line_size <- 0.6
point_size <- 2.5

sources <- c(
  "Child Death (CD)"
  , "Child Survival (CS)"
  , "First difference of CD"
  , "Burden of child death"
  , "Expected number of children"
  , "Expected number as fraction of TFR"
  # , "E[CS]"
  # , "E[CS]/TFR"
)

# 2. Data from Fig 2 ~~~~ ----

# 2.1	Child death (CD) ====
# ~~~~~~~~~~~~~

# Full country results for the (cumulative) number of child deaths 
# for a woman surviving to selected ages (median and IQR).

cl_full <- merge(
  df_cl_m_full %>% 
    filter(type == "country")
  , female_births %>% 
    select(cohort = year, cohort_size = value, everything())
  , by = c('country', 'cohort')
  , all.x = T
)

# Get estimated values for countries

cl_countries <- 
  cl_full %>% 
  filter(type == 'country') %>% 
  mutate(
    region = as.character(region)
    , cohort = as.numeric(cohort)
    # , measure = "child_death"
  ) %>% 
  select(country, region, cohort, age, value) %>% 
  arrange(country, cohort, age)

# Get summary values (median and IQR) for world

cl_world <- 
  cl_countries %>% 
  group_by(age, cohort) %>%
  summarise(
    median = median(value)
    , low_iqr = quantile(value, quant_low, na.rm = T)
    , high_iqr = quantile(value, quant_high, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(source = sources[1]) %>% 
  arrange(cohort, age)



# 2.2.	Child survival (CS) ====
# ~~~~~~~~~~~~~

# Full country results for the expected number of children surviving for a woman aged a (selected ages; median and IQR).

cs_full <- merge(
  df_cs_m_full %>% 
    filter(type == "country")
  , female_births %>% 
    select(cohort = year, cohort_size = value, everything())
  , by = c('country', 'cohort')
  , all.x = T
)

cs_countries <- 
  cs_full %>% 
  filter(type == 'country') %>% 
  mutate(
    region = as.character(region)
    , cohort = as.numeric(cohort)
    # , measure = "child_survival"
  ) %>% 
  select(country, region, cohort, age, value) %>% 
  arrange(country, cohort, age)

# Get summary values (median and IQR) for regions

cs_world <- 
  cs_countries %>% 
  group_by(age, cohort) %>%
  summarise(
    median = median(value)
    , low_iqr = quantile(value, quant_low, na.rm = T)
    , high_iqr = quantile(value, quant_high, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(source = sources[2]) %>% 
  arrange(cohort, age)


# 3. Data from Fig 3 ~~~~ ----

# 3.1.	First difference of child death (ΔCD) ====
# ~~~~~~~~~~~~~

# First difference of child death for a woman surviving to age $a$ 
# (full country results; selected ages; median and IQR).

diff_countries <- 
  df_cl_diff %>% 
  filter(type == 'country') %>% 
  mutate(
    region = as.character(region)
    , cohort = as.numeric(cohort)
    # , measure = "child_death_first_diff"
  ) %>% 
  select(country, region, cohort, age, value = diff) %>% 
  arrange(country, cohort, age)

diff_world <- 
  diff_countries %>% 
  group_by(age, cohort) %>%
  summarise(
    median = median(value)
    , low_iqr = quantile(value, quant_low, na.rm = T)
    , high_iqr = quantile(value, quant_high, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(source = sources[3]) %>% 
  arrange(cohort, age)


# 3.2.	Burden of child death ====
# ~~~~~~~~~~~~~

abs_countries <- 
  abs_df %>% 
  filter(type == 'country') %>% 
  mutate(
    region = as.character(region)
    , cohort = as.numeric(cohort)
  ) %>% 
  select(country, region, cohort, age, value = absolute) %>% 
  arrange(country, cohort, age)

abs_world <- 
  abs_countries %>% 
  # mutate(
  #   # Covert to billions
  #   value = value/1e6
  # ) %>%
  filter(age < 100) %>% 
  group_by(age, cohort) %>%
  summarise(
    median = sum(value, na.rm = T)
    # , sd = sd(value, na.rm = T)
    # , low_iqr = median - 2*sd
    # , high_iqr = median + 2*sd
    , low_iqr = NA
    , high_iqr = NA
  ) %>% 
  ungroup() %>% 
  mutate(source = sources[4]) %>% 
  arrange(cohort, age)


# 4. Data from Fig 4 ~~~~ ----

# 4.1.	Number of children expected to outlive mothers ====
# ~~~~~~~~~~~~~

csex_countries <- 
  cs_ex_pop_country %>% 
  filter(type == 'country') %>% 
  mutate(
    region = as.character(region)
    , cohort = as.numeric(cohort)
    # , measure = "num_children_outlive_mother"
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
  mutate(source = sources[5]) %>% 
  arrange(cohort)

# 4.2.	Share of children outlive mothers ====
# ~~~~~~~~~~~~~

out_countries <-
  ecl_ctfr %>%
  filter(type == 'country') %>% 
  mutate(
    region = as.character(region)
    # , measure = "share_children_outlive_mother"
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
  mutate(source = sources[6]) %>% 
  mutate(cohort = as.numeric(cohort)) %>% 
  arrange(cohort)


# 5. Plot ----

# 5.1. FigS1 ====

# Measures with age component

world1 <- 
  bind_rows(
    cl_world
    , cs_world
    , diff_world
    , abs_world
    # , csex_world
    # , out_world
  ) %>% 
  mutate(
    source = factor(source, levels = sources)
  )

f_lab <- data.frame(
  x = rep(20, 4)
  , y = c(2.5, 4, 0.0875, 5.6e6)
  , label = c("A", "B", "C", "D")
  , source = sources[1:4]
)

p_world1 <- 
  world1 %>% 
  filter(cohort %in% c(1950, 1975, 1999)) %>% 
  mutate(cohort = as.character(cohort)) %>% 
  ggplot() +
  geom_line(
    aes(x = age, y = median, group = cohort, colour = cohort)
    , size = region_line_size
    , show.legend = FALSE
  ) +
  # Plot ECL quantiles as bands
  geom_ribbon(
    aes(x = age, ymin = low_iqr, ymax = high_iqr, group = cohort, fill = cohort)
    , alpha = 0.4, show.legend = F
  ) +
  geom_point(
    aes(x = age, y = median, group = cohort, colour = cohort
        , shape = cohort
    )
    , size = point_size
    , data = . %>% filter(age %in% age_br)
  ) +
  # Add facet numbers
  geom_text(aes(x = x, y = y, label = label), data = f_lab, size = 6) +
  scale_x_continuous("Woman's life course (age in years)") +
  scale_y_continuous(
    ""
    , labels = function(x) ifelse(x > 1e6, paste0(round(x/1e6, 0), "M"), x)
    , position = "left"
    , sec.axis = dup_axis()
  ) +
  scale_color_discrete("Cohort") +
  # scale_fill_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_shape_discrete("Cohort") +
  # scale_size_continuous("Population share") +
  facet_wrap(. ~ source, scales = 'free', strip.position =  "left") +
  # Use with four measures
theme_bw(base_size = base_size) +
  theme(
    legend.position = "bottom"
    # Remove space over legend
    , legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")
    # Remove space between legends
    , legend.key.size = unit(0.1, "cm")
    # Remove title on left
    , axis.text.y.left = element_blank()
    , axis.ticks.y.left = element_blank()
    , axis.title.y.right = element_blank()
    # get rid of facet boxes
    , strip.background = element_blank()
    # , strip.text.y = element_blank()
    # Move y axis closer to the plot
    , axis.title.y = element_text(margin = margin(t = 0, r = -2, b = 0, l = 0))
    # Remove spacing between facets
    # , panel.spacing.x=unit(0.07, "cm")
    # , panel.spacing.y=unit(0.07, "cm")
  )

p_world1

ggsave(paste0("../../Output/figS1.pdf"), p_world1, width = width, height = height, units = "cm")

# 5.2. Fig S2 ====

# 0.2. Draft paper and presentation format (large)

width <- 16
height <- 10
base_size <- 14
region_line_size <- 1
point_size <- 4

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
  , source = sources[5:6]
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
