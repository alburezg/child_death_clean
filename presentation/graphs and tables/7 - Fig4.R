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

# Add facet Label
coh <- paste0(c(lower_year, upper_year), " birth cohort")

# Choose size options depending on whether image is intended for small format (e.g. PNAS).
# medium (regular draft) or large (presentation)

# 0.1. PNAS plotting params (small)
# width <- 8
# height <- 5
# base_size <- 9
# region_line_size <- 0.5
# point_size <- 2

# 0.2. Draft paper and presentation format (large)

width <- 16
height <- 16
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


# ! 3. Plot facet A ----

fig4a <-
  tally_share %>% 
  filter(!region %in% regions_to_remove) %>% 
  filter(measure %in% measures[2]) %>% 
  filter(level %in% levels[1]) %>% 
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
    , size = 5
    , data = . %>% filter(cohort %in% c(lower_year, 1975, upper_year))
  ) +
  scale_x_continuous(
    "Woman's birth cohort"
    , breaks = seq(lower_year, 2000, 10)
    # , labels = c(lower_year, seq(60, 90, 10), 2000)
    ) +
  scale_y_continuous(
    "Expected number of children outlive mother"
    , br = trans_breaks(identity, identity, 4)
                     ) +
  scale_color_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_fill_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_shape_discrete(col_lab, br = regions_long, labels = regions_short) +
  theme_bw(base_size = 17) +
  theme(
    legend.position = "bottom"
    # Remove space between legends
    , legend.key.size = unit(0.1, "cm")
    # get rid of facet boxes
    , strip.background = element_blank()
    # Remove spacing between facets
  )

fig4a

# ECS_expected_share_TFR
ggsave(paste0("../pres/fig4a.pdf"), fig4a, width = 20, height = 16, units = "cm")

# ! 3. Plot facet B ----

fig4b <-
  tally_share %>% 
  filter(!region %in% regions_to_remove) %>% 
  filter(measure %in% measures[2]) %>% 
  filter(level %in% levels[2]) %>% 
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
    , size = 5
    , data = . %>% filter(cohort %in% c(lower_year, 1975, upper_year))
  ) +
  scale_x_continuous(
    "Woman's birth cohort"
    , breaks = seq(lower_year, 2000, 10)
    # , labels = c(lower_year, seq(60, 90, 10), 2000)
  ) +
  scale_y_continuous(
    "Share of TFR will outlive mother"
    , br = trans_breaks(identity, identity, 4)
  ) +
  scale_color_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_fill_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_shape_discrete(col_lab, br = regions_long, labels = regions_short) +
  theme_bw(base_size = 17) +
  theme(
    legend.position = "bottom"
    # Remove space between legends
    , legend.key.size = unit(0.1, "cm")
    # get rid of facet boxes
    , strip.background = element_blank()
    # Remove spacing between facets
  )

fig4b

# ECS_expected_share_TFR
ggsave(paste0("../pres/fig4b.pdf"), fig4b, width = 20, height = 16, units = "cm")

print("7 - Figure 4 saved to ../../Output")