
# Frequency of child death and child survival over the life course 
# of two selected birth cohorts of women (by region and birth cohort).
# These measures pertain to a hypothetical woman aged $a$, standing 
# before us (i.e. surviving to that age). (A and B) Cumulative number 
# of child deaths (CD) experienced by a woman reaching age $a$. 
# The slope of the curves is positive at all ages, even after the end 
# of a woman's reproductive age, because offspring mortality continues 
# to accumulate over the life course. (C and D) Total number of children 
# surviving (CS) for a woman reaching age $a$. Values in the 
# vertical axis represent the total number of children `currently alive' - 
# those surviving exposure to the life table mortality function corresponding 
# to their birth cohorts. The solid lines represent median values and the 
# bands the variability among countries in each region.
# See Materials and Methods for details of the estimation procedures.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data requirements: 
# sum_cl 
# sum_cs 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Plotting params ----

lower_year <- 1950
upper_year <- 1999

point_br <- c(seq(lower_year, upper_year, 10) , upper_year)
col_lab <- ""
age_br <- c(seq(5, 100, 20), 100)

# Choose size options depending on whether image is intended for small format (e.g. PNAS).
# medium (regular draft) or large (presentation)

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A. Plot CL, CS conditional on survival ~~~~ ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 20190916: PLot Median with 40, 60 percentile bands

# 1. Merge dfs ----

sources <- c("died (cumulative)", "surviving")
sources <- factor(sources, levels = sources)
ylab <- "Number of children"

# sources <- c("died", "survived")
# ylab <- "Number of children who"

sum_cl2 <- sum_cl %>% filter(cohort %in% c(lower_year, upper_year)) 
sum_cs2 <- sum_cs %>% filter(cohort %in% c(lower_year, upper_year)) 

cl_cs <-
  rbind(
    # Child loss
    sum_cl2 %>% mutate(source = levels(sources)[1]) 
    # Child survival
    , sum_cs2 %>% mutate(source = levels(sources)[2])
    # Survived - died
    , sum_cs2 %>%
      # select(region, cohort) %>%
      mutate(
        median = sum_cs2$median - sum_cl2$median
        , low_iqr = NA
        , high_iqr = NA
        , source = 'Survived - died'
      )
    # died/Survived
    , sum_cs2 %>%
      # select(region, cohort) %>%
      mutate(
        median = sum_cl2$median/sum_cs2$median
        , low_iqr = NA
        , high_iqr = NA
        , source = 'Died/Survived'
        # , region = factor(plyr::mapvalues(region, from = new_sdg8, to = new_sdg8_short), levels = new_sdg8_short)
      )
  ) %>% 
  mutate(
    cohort2 = paste0(cohort, " birth cohort")
    )

# ! 2. Plot with facets ----

# Note that this is the most developed plot in terms of format
# having removed the space between axes, making it the smallest possible, etc

# Add facet Label
coh <- paste0(c(lower_year, upper_year), " birth cohort")

f_lab <- data.frame(
  x = rep(20, 4)
  , y = rep(4.9, 4)
  , label = c("A", "C", "B", "D")
  , source = rep(sources, 2)
  , cohort2 = sort(rep(coh, 2))
)

p_cl_cs_facet <-
  cl_cs %>% 
  filter(cohort %in% c(lower_year, upper_year)) %>% 
  filter(!region %in% regions_to_remove) %>% 
  filter(source %in% sources) %>% 
  mutate(source = factor(source, levels = sources)) %>% 
  mutate(region = factor(as.character(region), levels = regions_long)) %>% 
  ggplot() +
  geom_line(
    aes(x = age, y = median, group = region, colour = region)
    , size = region_line_size
    , show.legend = FALSE
  ) +
  # Plot ECL quantiles as bands
  geom_ribbon(
    aes(x = age, ymin = low_iqr, ymax = high_iqr, group = region, fill = region)
    , alpha = 0.4, show.legend = F
  ) +
  # Plot ECL shapes to help distinguish regions
  geom_point(
    aes(x = age, y = median, group = region, colour = region
        # , size = share
        , shape = region
    )
    , size = point_size
    , data = . %>% filter(age %in% age_br)
  ) +
  # Add facet numbers
  geom_text(aes(x = x, y = y, label = label), data = f_lab, size = 6) +
  # scale_x_continuous("Woman's age") +
  scale_x_continuous("Woman's life course (age in years)") +
  scale_y_continuous(
    ylab
    , position = "left"
    , sec.axis = dup_axis()
  ) +
  scale_color_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_fill_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_shape_discrete(col_lab, br = regions_long, labels = regions_short) +
  # scale_size_continuous("Population share") +
  facet_grid(source ~ cohort2, scales = 'fixed', switch = "y") +
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
  
p_cl_cs_facet

ggsave(paste0("../../Output/fig2.pdf"), p_cl_cs_facet, width = width, height = height, units = "cm")

print("5 - Figure 2 saved to ../../Output")