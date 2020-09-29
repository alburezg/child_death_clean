
# Generational burden of child death. Estimated as the sum of the
# (non-cumulative) burden of child death over all ages. This
# measure, considers the size and structure of different birth 
# cohorts of women to determine the number of child deaths 
# accumulated by all women in a given birth cohort and region 
# throughout their lives. Lower values for subsequent cohorts 
# of women in a region show than women born in younger cohorts 
# can expect to experience fewer child deaths compared to preceding generations. 
# Estimates for Oceania, Australia, and New Zealand shown in inset plot.

# Note that this should not sum the values of ALL ages.
# Doing so would be slightly misleading as we cannot expect women in all
# country/cohorts to survive to age 100 (the current upper age limit) in
# the data. Instead, the values in each country/cohorts combination
# should only be summed up to the female life expectancy for that country/cohort.
# This would then represent the `actual` experience number of child deaths
# assuming that all women survive to the life expectancty.
# This is equivalent to the way in which E[CS] is estimated.

# 0. Parameters ----

lower_year <- 1950
upper_year <- 2000

point_br <- c(seq(lower_year, upper_year, 10) , upper_year)
age_br <- c(seq(5, 100, 20), 100)
col_lab <- ""

new_order <- 
  rev(
  c(
    "europe and northern america"
    , "northern africa and western asia"
    , "latin america and the caribbean"
    , "eastern and south-eastern asia"
    , "sub-saharan africa"
    , "central and southern asia"
    , "australia_new zealand"
    , "oceania (excluding australia and new zealand)"
    )
  )

orders <- match(new_order, regions_long)

# Choose size options depending on whether image is intended for small format..
# medium (regular draft) or large (presentation)

# 0.1. plotting params (small)
# width <- 8
# height <- 6
# base_size <- 9
# region_line_size <- 0.4
# point_size <- 1.5

# 0.2. Draft paper and presentation format (large)
# 8x6 is a good size for small size

width <- 16
height <- 12
base_size <- 15
region_line_size <- 1
point_size <- 2.5

# 1. Format dfs ----

# Save in millions

sum_burden2 <-
  sum_burden %>% 
  mutate(
    value = value / 1e6
    , low = low / 1e6
    , high = high / 1e6
  ) 

# 

# 2. Plot ----

# Data visualisation inspired by @ikashnitsky: 
# https://ikashnitsky.github.io/images/190719/one-figure.png

base_size <- 15
base_inset <- 12
point_size <- 4
point_inset <- 3

p_sum_burden <-
  sum_burden2 %>% 
  filter(cohort %in% c(1950, 1975, 2000)) %>% 
  filter(!region %in% regions_to_remove) %>% 
  select(region, cohort, value) %>% 
  spread(., cohort, value) %>% 
  mutate(
    region = factor(region, levels = new_order)
  ) %>% 
  ggplot(aes(y = region)) +
  geom_segment(aes(x = `1950`, xend = `2000`, yend = region)) +
  geom_point(aes(x = `1950`), shape = 16, size = point_size) +
  geom_point(aes(x = `1975`), shape = 21, size = point_size, fill = "white") +
  geom_point(aes(x = `2000`), shape = 17, size = point_size) +
  scale_y_discrete("", br = new_order, labels = regions_short[orders]) +
  scale_x_continuous(
    "Total number of child deaths"
    , breaks = scales::pretty_breaks(n = 5)
    , labels = function(x) ifelse(x == 0, x, paste0(x, "M"))
  ) + 
  coord_cartesian(xlim = c(0, 55)) +
  theme_bw(base_size = base_size)

# p_sum_burden

# pdf("../../Output/figS1.pdf", width = 6.5, height = 5)
# p_sum_burden
# dev.off()    

# 2.1. Inset plot  ====

p_inset <-
  sum_burden2 %>% 
  filter(cohort %in% c(1950, 1975, 2000)) %>% 
  filter(region %in% regions_to_remove) %>% 
  select(region, cohort, value) %>% 
  spread(., cohort, value) %>% 
  mutate(
    # region = factor(region, levels = regions_long)
    region = 1:2
  ) %>% 
  ggplot(aes(y = region)) +
  geom_segment(aes(x = `1950`, xend = `2000`, yend = region)) +
  geom_point(aes(x = `1950`), shape = 16, size = point_inset) +
  geom_point(aes(x = `1975`), shape = 21, size = point_inset, fill = "white") +
  geom_point(aes(x = `2000`), shape = 17, size = point_inset) +
  # annotate("text", x = c(0.12, 0.25), y = c(2, 1), label = c("Oceania (other)", "AUS & NZ"),
  # size = c(3,3), hjust = .5, color = "grey20") +
  coord_cartesian(xlim = c(0.03, 0.32), ylim = c(0.75, 2.25)) +
  scale_y_continuous("", br = 1:2, labels = regions_short[7:8], minor_breaks = NULL) +
  scale_x_continuous(
    "Total number of child deaths"
    , breaks = scales::pretty_breaks(n = 3)
    , labels = function(x) ifelse(x == 0, x, paste0(x, "M"))
  ) +
  theme_bw(
    base_size = base_inset
  ) +
  theme(
    axis.title=element_text(size=7)
    , axis.text = element_text(size = 7)
  )

# 2.2. S3 - Complete plot ====

# x_adj <- -13
# y_adj <- -3.7

x_adj <- -30
y_adj <- -5.7

p_complete <- 
  p_sum_burden +
  annotation_custom(
    ggplotGrob(p_inset) 
    , xmin = 24, xmax = 57
    , ymin = 4.69, ymax = 6.69
  ) +
  # add legend manually
  annotate("rect", xmin = 29.5 + x_adj, xmax = 53 + x_adj, ymin = 7.2 + y_adj, ymax = 8.1 + y_adj,
           color = "grey50", fill = "white") +
  annotate("text", x = 40 + x_adj, y = 7.9 + y_adj, label = "Birth cohort of women",
           size = 3, hjust = .5, color = "grey20") +
  annotate("point", x = c(32 + x_adj, 39 + x_adj, 46 + x_adj), y = 7.5 + y_adj,
           pch = c(16, 21, 17), size = 2, color = 1) +
  annotate("text", x = c(33 + x_adj, 40 + x_adj, 47 + x_adj), y = 7.5 + y_adj,
           label = c("1950", "1975", "2000"),
           size = 3, hjust = 0, color = "grey20") 

p_complete

pdf("../../Output/fig4.pdf", width = 6.5, height = 5)
p_complete
dev.off()    
