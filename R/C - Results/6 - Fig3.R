
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A. Plot first diff and weighted first diff ~~~~ 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data requirements
# sum_diff
# sum_abs
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Plotting params ----

point_br <- c(seq(1950, 1999, 10) , 1999)
age_br <- c(seq(5, 100, 20), 100)
col_lab <- ""


# Facet labels eventually in the plot
sources <- c("individual-level", "population-level")
sources <- factor(sources, levels = sources)

# Choose size options depending on whether image is intended for small format (e.g. PNAS).
# medium (regular draft) or large (presentation)

# 0.1. PNAS plotting params (small)
# width <- 8
# height <- 6
# base_size <- 9
# region_line_size <- 0.4
# point_size <- 1.5

# 0.2. Draft paper and presentation format (large)
# 8x6 is a good size for PNAS

width <- 16
height <- 12
base_size <- 15
region_line_size <- 1
point_size <- 2.5

# 1. Merge dfs ----

diff_abs <-
  rbind(
    # Child loss
    sum_diff %>% 
      mutate(source = levels(sources)[1]) %>% 
      select(region, age, cohort, value = median, low = low_iqr, high = high_iqr, source)
    # Child survival
    , sum_abs %>% 
      mutate(
        source = levels(sources)[2]
        , value = value / 1e6
        , low_sd = low_sd / 1e6
        , high_sd = high_sd / 1e6
        ) %>% 
      select(region, age, cohort, value, low = low_sd, high = high_sd, source)
    ) %>% 
  mutate(cohort2 = paste0(cohort, " birth cohort"))

# ! 2. Plot with facets ----

# Note that this is the most developed plot in terms of format
# having removed the space between axes, making it the smallest possible, etc

# Add facet Label
coh <- paste0(c(1950, 1999), " birth cohort")

f_lab <- data.frame(
  x = rep(90, 4)
  , y = c(0.105, 2.75, 0.105, 2.75)
  , label = c("A", "C", "B", "D")
  , source = rep(sources, 2)
  , cohort2 = sort(rep(coh, 2))
)

p_diff_abs <-
  diff_abs %>% 
  # na.omit() %>% 
  filter(cohort %in% c(1950, 1999)) %>% 
  filter(!region %in% regions_to_remove) %>% 
  filter(source %in% sources) %>% 
  mutate(region = factor(as.character(region), levels = regions_long)) %>% 
  ggplot() +
  geom_line(
    aes(x = age, y = value, group = region, colour = region)
    , size = region_line_size
    , show.legend = F
  ) +
  # Plot ECL quantiles as bands
  geom_ribbon(
    aes(x = age, ymin = low, ymax = high, group = region, fill = region)
    , alpha = 0.4
    , show.legend = F
  ) +
  # Plot ECL shapes to help distinguish regions
  geom_point(
    aes(x = age, y = value, group = region, colour = region
        # , size = share
        , shape = region
    )
    , size = point_size
    , data = . %>% filter(age %in% age_br)
  ) +
  # Add facet numbers
  geom_text(aes(x = x, y = y, label = label), data = f_lab, size = 6) +
  scale_x_continuous("Woman's age") +
  scale_y_continuous(
    # expression(Delta*"(child death)")
    # "First difference of child death"
    "Number of child deaths at each age"
    , position = "left"
    , sec.axis = dup_axis()
    , breaks = scales::pretty_breaks(n = 4)
    # Make sure the lower facet shows labels in millions
    # but the upper does does not
    , labels = function(x) ifelse(x < 1, x, paste0(x, "M"))
  ) +
  scale_color_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_fill_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_shape_discrete(col_lab, br = regions_long, labels = regions_short) +
  facet_grid(source ~ cohort2, scales = 'free_y', switch = "y") +
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
    # Remove spacing between facets
    # , panel.spacing.x=unit(0.07, "cm")
    # , panel.spacing.y=unit(0.07, "cm")
    # Move y axis closer to the plot
    , axis.title.y = element_text(margin = margin(t = 0, r = -2, b = 0, l = 0))
    )

p_diff_abs

ggsave(paste0("../../Output/fig3.pdf"), p_diff_abs, width = width, height = height, units = "cm")

print("6 - Figure 2 saved to ../../Output")