
# Global burden of child death. Estimated as the sum of the
# (non-cumulative) burden of child death over all ages. This
# measure, considers the size and structure of different birth 
# cohorts of women to determine the number of child deaths 
# accumulated by all women in a given birth cohort and region 
# throughout their lives. Lower values for subsequent cohorts 
# of women in a region show than women born in younger cohorts 
# can expect to experience fewer child deaths compared to preceding generations. 
# Estimates for Oceania, Australia, and New Zealand shown in inset plot.

# 0. Parameters ----

lower_year <- 1950
upper_year <- 1999

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

sum_burden <-
  abs_df %>% 
  group_by(region, cohort) %>% 
  dplyr::summarise(
    value = sum(absolute, na.rm = T)
    , sd = sd(absolute, na.rm = T)
    , low_sd = value - sd
    , high_sd = value + sd
    ) %>% 
  ungroup %>% 
  mutate(
    value = value / 1e6
    , low_sd = low_sd / 1e6
    , high_sd = high_sd / 1e6
    , cohort2 = paste0(cohort, " birth cohort")
  ) %>% 
  select(region, cohort, cohort2, value, low = low_sd, high = high_sd)

# 2. Plot ----

# Data visualisation inspired by @ikashnitsky: 
# https://ikashnitsky.github.io/images/190719/one-figure.png

base_size <- 15
base_inset <- 11
point_size <- 4
point_inset <- 3

x_adj <- -28
y_adj <- -6.5

p_sum_burden <-
  sum_burden %>% 
  filter(cohort %in% c(1950, 1975, 1999)) %>% 
  filter(!region %in% regions_to_remove) %>% 
  select(region, cohort, value) %>% 
  spread(., cohort, value) %>% 
  mutate(
    region = factor(region, levels = new_order)
  ) %>% 
  ggplot(aes(y = region)) +
  geom_segment(aes(x = `1950`, xend = `1999`, yend = region)) +
  geom_point(aes(x = `1950`), shape = 16, size = point_size) +
  geom_point(aes(x = `1975`), shape = 21, size = point_size, fill = "white") +
  geom_point(aes(x = `1999`), shape = 17, size = point_size) +
  scale_y_discrete("", br = new_order, labels = regions_short[orders]) +
  scale_x_continuous(
    "Total number of child deaths"
    , breaks = scales::pretty_breaks(n = 6)
    , labels = function(x) ifelse(x == 0, x, paste0(x, "M"))
  ) + 
  coord_cartesian(xlim = c(0, 55)) +
  theme_bw(base_size = base_size)

p_sum_burden

# pdf("../../Output/figS1.pdf", width = 6.5, height = 5)
# p_sum_burden
# dev.off()    

# 2.1. Inset plot  ====

p_inset <-
  sum_burden %>% 
  filter(cohort %in% c(1950, 1975, 1999)) %>% 
  filter(region %in% regions_to_remove) %>% 
  select(region, cohort, value) %>% 
  spread(., cohort, value) %>% 
  mutate(
    # region = factor(region, levels = regions_long)
    region = 1:2
  ) %>% 
  ggplot(aes(y = region)) +
  geom_segment(aes(x = `1950`, xend = `1999`, yend = region)) +
  geom_point(aes(x = `1950`), shape = 16, size = point_inset) +
  geom_point(aes(x = `1975`), shape = 21, size = point_inset, fill = "white") +
  geom_point(aes(x = `1999`), shape = 17, size = point_inset) +
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
    # axis.text.y=element_blank()
    # , axis.ticks.y=element_blank()
    # , axis.title.y.left = element_blank()
    # , axis.title.y = element_blank()
    # , plot.margin = unit(c(t=0.2, r=0.25, b=0.1, l=0.1), unit="cm")
  )

# 2.2. S3 - Complete plot ====

p_complete <- 
  p_sum_burden +
  annotation_custom(
    ggplotGrob(p_inset) 
    , xmin = 28, xmax = 58.55
    , ymin = 4.69, ymax = 6.69
  ) +
  # add legend manually
  annotate("rect", xmin = 30 + x_adj, xmax = 50 + x_adj, ymin = 7.2 + y_adj, ymax = 8.1 + y_adj,
           color = "grey50", fill = "white") +
  annotate("text", x = 40 + x_adj, y = 7.9 + y_adj, label = "Birth cohort of women",
           size = 3, hjust = .5, color = "grey20") +
  annotate("point", x = c(32 + x_adj, 38 + x_adj, 44 + x_adj), y = 7.5 + y_adj,
           pch = c(16, 21, 17), size = 2, color = 1) +
  annotate("text", x = c(33 + x_adj, 39 + x_adj, 45 + x_adj), y = 7.5 + y_adj,
           label = c("1950", "1975", "1999"),
           size = 3, hjust = 0, color = "grey20") 

p_complete

pdf("../../Output/figS3.pdf", width = 6.5, height = 5)
p_complete
dev.off()    


# DEPRECATED:
# With inset but showing Oceania u y axis

# p_sum_burden <-
# sum_burden %>% 
#   filter(cohort %in% c(1950, 1975, 1999)) %>% 
#   select(region, cohort, value) %>% 
#   spread(., cohort, value) %>% 
#   mutate(
#     region = factor(region, levels = regions_long)
#   ) %>% 
#   ggplot(aes(y = region)) +
#   geom_segment(aes(x = `1950`, xend = `1999`, yend = region)) +
#   geom_point(aes(x = `1950`), shape = 16, size = 2) +
#   geom_point(aes(x = `1975`), shape = 21, size = 2, fill = "white") +
#   geom_point(aes(x = `1999`), shape = 17, size = 2) +
#   scale_y_discrete("", br = regions_long, labels = regions_short) +
#   scale_x_continuous(
#     "Total number of child deaths"
#     , breaks = scales::pretty_breaks(n = 6)
#     , labels = function(x) ifelse(x < 1, x, paste0(x, "M"))
#     ) +
#   # add legend manually
#   annotate("rect", xmin = 31, xmax = 55, ymin = 7.2, ymax = 8.1,
#            color = "grey50", fill = "white") +
#   annotate("text", x = 42, y = 7.9, label = "Birth cohort of women", 
#            size = 3, hjust = .5, color = "grey20") +
#   annotate("point", x = c(32.5, 40, 47.5), y = 7.5, 
#            pch = c(16, 21, 17), size = 2, color = 1) +
#   annotate("text", x = c(33.5, 41, 48.5), y = 7.5, 
#            label = c("1950", "1975", "1999"), 
#            size = 3, hjust = 0, color = "grey20") +
#   # annotate("rect", xmin = 24, xmax = 47, ymin = 7.2, ymax = 8.1,
#   #          color = "grey50", fill = "white") +
#   # annotate("text", x = 35, y = 7.9, label = "Birth cohort of women", 
#   #          size = 3, hjust = .5, color = "grey20") +
#   # annotate("point", x = c(25.5, 33, 40.5), y = 7.5, 
#   #          pch = c(16, 21, 17), size = 2, color = 1) +
#   # annotate("text", x = c(26.5, 34, 41.5), y = 7.5, 
#   #          label = c("1950", "1975", "1999"), 
#   #          size = 3, hjust = 0, color = "grey20") +
#   theme_bw()
# 
# p_sum_burden
# 
# # pdf("../../Output/figS1.pdf", width = 6.5, height = 5)
# # p_sum_burden
# # dev.off()    
# 
# # 2.1. Inset plot 
# 
# p_inset <-
#   sum_burden %>% 
#   filter(cohort %in% c(1950, 1975, 1999)) %>% 
#   filter(region %in% regions_to_remove) %>% 
#   select(region, cohort, value) %>% 
#   spread(., cohort, value) %>% 
#   mutate(
#     # region = factor(region, levels = regions_long)
#     region = 1:2
#   ) %>% 
#   ggplot(aes(y = region)) +
#   geom_segment(aes(x = `1950`, xend = `1999`, yend = region)) +
#   geom_point(aes(x = `1950`), shape = 16, size = 2) +
#   geom_point(aes(x = `1975`), shape = 21, size = 2, fill = "white") +
#   geom_point(aes(x = `1999`), shape = 17, size = 2) +
#   annotate("text", x = c(0.14, 0.23), y = c(2, 1), label = c("Oceania (other)", "AUS & NZ"), 
#            size = c(3,3), hjust = .5, color = "grey20") +
#   coord_cartesian(ylim = c(0.75, 2.25)) +
#   scale_y_continuous("", br = 1:2, labels = regions_short[7:8], minor_breaks = NULL) +
#   scale_x_continuous(
#     ""
#     , breaks = scales::pretty_breaks(n = 3)
#     , labels = function(x) paste0(x, "M")
#   ) +
#   theme_bw(
#     base_size = 10
#     ) +
#   theme(
#     axis.text.y=element_blank()
#     , axis.ticks.y=element_blank()
#     , legend.title = element_blank()
#     , axis.title.y = element_blank()
#     # , plot.margin = unit(c(t=0.2, r=0.25, b=0.1, l=0.1), unit="cm")
#   )
# 
# # 2.2. Complete plot 
# 
# p_complete <- 
#   p_sum_burden +
#   annotation_custom(
#   ggplotGrob(p_inset) 
#   , xmin = 5, xmax = 25
#   , ymin = 6.1, ymax = 8.6
# )
# 
# p_complete
# 
# pdf("../../Output/figS1.pdf", width = 6.5, height = 5)
# p_complete
# dev.off()    
