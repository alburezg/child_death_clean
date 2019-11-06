
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A. Plot first diff and weighted first diff ~~~~ 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data requirements
# sum_diff
# sum_abs
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Plotting params ----

lower_year <- 1950
upper_year <- 1999
# upper_year <- 2000

point_br <- c(seq(lower_year, upper_year, 10) , upper_year)
age_br <- c(seq(5, 100, 20), 100)
col_lab <- ""

# Add facet Label
coh <- paste0(c(lower_year, upper_year), " birth cohort")

# Facet labels eventually in the plot
# sources <- c("individual-level", "population-level")
sources <- c("for average woman", "at a population level")
sources <- factor(sources, levels = sources)

# Choose size options depending on whether image is intended for small format (e.g. PNAS).
# medium (regular draft) or large (presentation)

# 0.2. Draft paper and presentation format (large)

width <- 18
height <- 12
base_size <- 15
region_line_size <- 1
point_size <- 3

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
  mutate(
    cohort2 = paste0(cohort, " birth cohort")
    , source =  factor(source, levels = sources)
  )

# ! 2. Plot Facets A-B ----

# Note that this is the most developed plot in terms of format
# having removed the space between axes, making it the smallest possible, etc

fig3ab <-
  diff_abs %>% 
  filter(cohort %in% c(lower_year, upper_year)) %>% 
  filter(!region %in% regions_to_remove) %>% 
  filter(source %in% sources[1]) %>% 
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
        , shape = region
    )
    , size = point_size
    , data = . %>% filter(age %in% age_br)
  ) +
  scale_x_continuous("Woman's life course (age in years)") +
  scale_y_continuous(
    "Child deaths at each age"
    , breaks = scales::pretty_breaks(n = 4)
    # Make sure the lower facet shows labels in millions
    # but the upper does does not
    , labels = function(x) ifelse(x < 1, x, paste0(x, "M"))
  ) +
  scale_color_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_fill_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_shape_discrete(col_lab, br = regions_long, labels = regions_short) +
  facet_grid(. ~ cohort2, scales = 'free_y', switch = "y") +
  # Use with four measures
  theme_bw(base_size = base_size) +
  theme(
    legend.position = "bottom"
    # Remove space over legend
    , legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")
    # Remove space between legends
    , legend.key.size = unit(0.1, "cm")
    # get rid of facet boxes
    , strip.background = element_blank()
    # Move y axis closer to the plot
  )

fig3ab

ggsave(paste0("../pres/fig3ab.pdf"), fig3ab, width = width, height = height, units = "cm")

# 2.1. Animated ====

f3ab <- function(co) {
  
    diff_abs %>% 
    filter(cohort %in% co) %>%
    filter(!region %in% regions_to_remove) %>% 
    filter(source %in% sources[1]) %>% 
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
          , shape = region
      )
      , size = 5
      , data = . %>% filter(age %in% age_br)
    ) +
    scale_x_continuous("Woman's life course (age in years)") +
    scale_y_continuous(
      "Child deaths at each age"
      , breaks = scales::pretty_breaks(n = 4)
      # Make sure the lower facet shows labels in millions
      # but the upper does does not
      , labels = function(x) ifelse(x < 1, x, paste0(x, "M"))
    ) +
    scale_color_discrete(col_lab, br = regions_long, labels = regions_short) +
    scale_fill_discrete(col_lab, br = regions_long, labels = regions_short) +
    scale_shape_discrete(col_lab, br = regions_long, labels = regions_short) +
    coord_cartesian(ylim = c(0, 0.12)) +
    # Use with four measures
    theme_bw(base_size = 20) +
    theme(
      legend.position = "bottom"
      # Remove space over legend
      , legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")
      # Remove space between legends
      , legend.key.size = unit(0.1, "cm")
      , legend.text=element_text(size= 16)
      # get rid of facet boxes
      , strip.background = element_blank()
      # Move y axis closer to the plot
    ) +
    guides(colour=guide_legend(ncol=2)) +
    ggtitle(paste0("Woman's birth cohort: ", co))
  
  # ggsave(paste0("../pres/fig3ab/fig3ab_", co ,".pdf"), p, width = width, height = height, units = "cm")
  
}


pdf("../pres/fig3ab_trans.pdf", width = 6.5, height = 6.5)
lapply(1950:1999, f3ab)
dev.off()

# ! 3. Plot Facets C-D ----

# Note that this is the most developed plot in terms of format
# having removed the space between axes, making it the smallest possible, etc

fig3cd <-
  diff_abs %>% 
  filter(cohort %in% c(lower_year, upper_year)) %>% 
  filter(!region %in% regions_to_remove) %>% 
  filter(source %in% sources[2]) %>% 
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
        , shape = region
    )
    , size = point_size
    , data = . %>% filter(age %in% age_br)
  ) +
  scale_x_continuous("Woman's life course (age in years)") +
  scale_y_continuous(
    "Global burden of child death (millions)"
    , breaks = scales::pretty_breaks(n = 4)
    # Make sure the lower facet shows labels in millions
    # but the upper does does not
    , labels = function(x) ifelse(x < 1, x, paste0(x, "M"))
  ) +
  scale_color_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_fill_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_shape_discrete(col_lab, br = regions_long, labels = regions_short) +
  facet_grid(. ~ cohort2, scales = 'free_y', switch = "y") +
  # Use with four measures
  theme_bw(base_size = base_size) +
  theme(
    legend.position = "bottom"
    # Remove space over legend
    , legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")
    # Remove space between legends
    , legend.key.size = unit(0.1, "cm")
    # get rid of facet boxes
    , strip.background = element_blank()
    # Move y axis closer to the plot
  )

fig3cd

ggsave(paste0("../pres/fig3cd.pdf"), fig3cd, width = width, height = height, units = "cm")


# 3.1. Animated ====

f3cd <- function(co) {
  
    diff_abs %>% 
    filter(cohort %in% co) %>% 
    filter(!region %in% regions_to_remove) %>% 
    filter(source %in% sources[2]) %>% 
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
          , shape = region
      )
      , size = 5
      , data = . %>% filter(age %in% age_br)
    ) +
    scale_x_continuous("Woman's life course (age in years)") +
    scale_y_continuous(
      "Global burden of child death (millions)"
      , breaks = scales::pretty_breaks(n = 4)
      # Make sure the lower facet shows labels in millions
      # but the upper does does not
      , labels = function(x) ifelse(x < 1, x, paste0(x, "M"))
    ) +
    scale_color_discrete(col_lab, br = regions_long, labels = regions_short) +
    scale_fill_discrete(col_lab, br = regions_long, labels = regions_short) +
    scale_shape_discrete(col_lab, br = regions_long, labels = regions_short) +
    coord_cartesian(ylim = c(0, 3.15)) +
    # Use with four measures
    theme_bw(base_size = 20) +
    theme(
      legend.position = "bottom"
      # Remove space over legend
      , legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")
      # Remove space between legends
      , legend.key.size = unit(0.1, "cm")
      , legend.text=element_text(size= 16)
      # get rid of facet boxes
      , strip.background = element_blank()
      # Move y axis closer to the plot
    ) +
    guides(colour=guide_legend(ncol=2)) +
    ggtitle(paste0("Women's birth cohort: ", co))
  
  # ggsave(paste0("../pres/fig3cd/fig3cd_", co ,".pdf"), p, width = width, height = height, units = "cm")
  
}

pdf("../pres/fig3cd_trans.pdf", width = 6.5, height = 6.5)
lapply(1950:1999, f3cd)
dev.off()

print("6 - Figure 2 saved to ../../Output")
