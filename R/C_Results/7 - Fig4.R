# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A. Plot CL and CS in four facets ~~~~ ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data requirements (20190827):
# sum_ecl_ctfr  (3.6.1_ECL_share_outlive_mother.R)
# cl_ex_pop_country (3.7_ECL_by_mother_ex.R)
# cs_ex_pop_country (4.6_ECS_by_mother_ex.R)
# ecl_ctfr (3.6.1_ECL_share_outlive_mother.R)
# sum_cl_ex
# sum_cs_ex
# sum_ecs_ctfr
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# All are cut when mother's age is equal to regional life expectancy.
# The panels
# A   B
# C   D

# represnt:

# A) CL (tally at woman's death)
# B) CS (tally at woman's death)
# C) CL (share of CTFR)
# D) CS (share of CTFR)

# 0. Plotting params ----

lower_year <- 1950
upper_year <- 1999
# upper_year <- 2000

point_br <- c(seq(lower_year, upper_year, 10) , upper_year)
col_lab <- ""


# 0.2. Draft paper and presentation format (large)

width <- 16
height <- 10
base_size <- 14
region_line_size <- 1
point_size <- 4

# 1. Merge dfs for tally part ----

# levels <- c("Expected number", "As share of TFR")
levels <- c("Expected number of children", "Expected number as fraction of TFR")
levels <- factor(levels, levels = levels)
measures <- c("Child death", "Child survival")

tally_df <-
  rbind(
    # Child loss
    sum_cl_ex %>% 
      mutate(
        level = levels[1]
        , measure = measures[1]
             ) 
    # Child survival
    , sum_cs_ex %>% 
      mutate(
        level = levels[1]
        , measure = measures[2]
      ) 
  ) %>% 
  select(region, cohort, value = median, low = low_iqr, high = high_iqr, level, measure)

# 2. Merge with dfs of 'share' ====

# Share of cohort's TFR, that is.

tally_share <- rbind(
  tally_df
  , sum_ecl_ctfr %>% 
    mutate(
      level = levels[2]
      , measure = measures[2]
    ) %>% 
    select(region, cohort, value, low = low_iqr, high = high_iqr, level, measure )
  , sum_ecs_ctfr %>% 
    mutate(
      level = levels[2]
      , measure = measures[1]
    ) %>% 
    select(region, cohort, value, low = low_iqr, high = high_iqr, level, measure)
)


# # 3.1. Tally part ====
# # ~~~~~~~~~~~~~~~~~~~~~~
# 
# # 3.1.1. Child death 
# 
# cl_ex_selected <- 
#   cl_ex_pop_country %>% 
#   select(region = country, cohort, value) %>% 
#   mutate(
#     level = levels[1]
#       , measure = measures[1]
#   )
# 
# # 3.1.2. Child survival 
# 
# cs_ex_selected <- 
#   cs_ex_pop_country %>% 
#   select(region = country, cohort, value) %>% 
#   mutate(
#     level = levels[1]
#     , measure = measures[2]
#   )
# 
# # 3.2. Share part ====
# # ~~~~~~~~~~~~~~~~~~~~~~
# 
# # 3.2.1. Child death 
# 
# ecl_ctfr_selected <- 
#   ecl_ctfr %>% 
#   mutate(share = value / tfr) %>% 
#   select(region = country, cohort, value = share) %>% 
#   mutate(
#     level = levels[2]
#     , measure = measures[1]
#   )
#   
# 
# # 3.2.2. Child survival 
# 
# ecs_ctfr_selected <- 
#   ecl_ctfr %>% 
#   mutate(share = 1 - value / tfr) %>% 
#   select(region = country, cohort, value = share) %>% 
#   mutate(
#     level = levels[2]
#     , measure = measures[2]
#   )
# 
# # 3.3. COnsolidate ====
# 
# tally_share_con <- 
#   rbind(
#     cl_ex_selected
#     , cs_ex_selected
#     , ecl_ctfr_selected
#     , ecs_ctfr_selected
#   ) %>% 
#   filter(region %in% con) %>% 
#   filter(measure %in% measures[2]) %>% 
#   mutate(cohort = as.numeric(cohort))

# ! 4. Plot with facets ----

# Add facet Label
coh <- paste0(c(lower_year, upper_year), " birth cohort")

f_lab <- data.frame(
  x = 1955
  , y = c(5.15, 0.959)
  , label = LETTERS[1:2]
  , level = levels
)

p_cs_number_share <-
  tally_share %>% 
  filter(!region %in% regions_to_remove) %>% 
  filter(measure %in% measures[2]) %>% 
  mutate(region = factor(as.character(region), levels = regions_long)) %>% 
  ggplot() +
  # Region summary lines
  geom_line(
    aes(x = cohort, y = value, group = region, colour = region)
    , size = region_line_size
    , show.legend = F
  ) +
  # Plot ECL quantiles as bands
  geom_ribbon(
    aes(x = cohort, ymin = low, ymax = high, group = region, fill = region)
    , alpha = 0.4, show.legend = F
  ) +
  # Plot ECL shapes to help distinguish regions
  geom_point(
    aes(x = cohort, y = value, group = region, colour = region
        , shape = region
    )
    , size = point_size
    , data = . %>% filter(cohort %in% c(lower_year, 1975, upper_year))
  ) +
  # Add facet numbers
  geom_text(aes(x = x, y = y, label = label), data = f_lab, size = 6) +
  # SWE and ZWE lines
  # geom_line(
  #   aes(x = cohort, y = value, group = region)
  #   , linetype = "longdash"
  #   , colour = "black"
  #   , show.legend = F
  #   , data = tally_share_con
  # ) +
  
  scale_x_continuous(
    "Woman's birth cohort"
    , breaks = seq(lower_year, 2000, 10)
    , labels = c(lower_year, seq(60, 90, 10), 2000)
    ) +
  scale_y_continuous(
    "Children outlive mother"
    , br = trans_breaks(identity, identity, 4)
                     ) +
  scale_color_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_fill_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_shape_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_size_continuous("Population share") +
  # coord_cartesian(ylim = c(0, NA)) +
  # facet_grid(level ~ measure, scales = 'free_y') +
  facet_wrap(level ~ ., scales = 'free_y') +
  theme_bw(base_size = base_size) +
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

p_cs_number_share

# ECS_expected_share_TFR
ggsave(paste0("../../Output/fig4.pdf"), p_cs_number_share, width = width, height = height, units = "cm")

print("7 - Figure 4 saved to ../../Output")