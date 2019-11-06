
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

# Add facet Label
coh <- paste0(c(lower_year, upper_year), " birth cohort")

# 0.2. Draft paper and presentation format (large)

width <- 16
height <- 10
base_size <- 15
region_line_size <- 1
point_size <- 3

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A. Plot CL, CS conditional on survival ~~~~ ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 20190916: PLot Median with 40, 60 percentile bands

# 1. Merge dfs ----

sources <- c("died (cumulative)", "surviving")
sources <- factor(sources, levels = sources)


# sum_cl2 <- sum_cl %>% filter(cohort %in% c(lower_year, upper_year)) 
# sum_cs2 <- sum_cs %>% filter(cohort %in% c(lower_year, upper_year)) 

cl_cs <-
  rbind(
    # Child loss
    sum_cl %>% mutate(source = levels(sources)[1]) 
    # Child survival
    , sum_cs %>% mutate(source = levels(sources)[2])
    # Survived - died
    , sum_cs %>%
      # select(region, cohort) %>%
      mutate(
        median = sum_cs$median - sum_cl$median
        , low_iqr = NA
        , high_iqr = NA
        , source = 'Survived - died'
      )
    # died/Survived
    , sum_cs %>%
      # select(region, cohort) %>%
      mutate(
        median = sum_cl$median/sum_cs$median
        , low_iqr = NA
        , high_iqr = NA
        , source = 'Died/Survived'
        # , region = factor(plyr::mapvalues(region, from = new_sdg8, to = new_sdg8_short), levels = new_sdg8_short)
      )
  ) %>% 
  mutate(
    cohort2 = paste0(cohort, " birth cohort")
    )

# ! 2. Plot Facets A-B ----

fig2a <-
  cl_cs %>% 
  filter(cohort %in% c(lower_year, upper_year)) %>%
  filter(!region %in% regions_to_remove) %>%
  filter(source %in% sources[1]) %>%
  mutate(source = factor(source, levels = sources)) %>%
  mutate(region = factor(as.character(region), levels = regions_long)) %>%
  ggplot() +
  geom_line(
    aes(x = age, y = median, group = region, colour = region)
    , size = region_line_size
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
  scale_color_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_fill_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_shape_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_x_continuous("Woman's life course (age in years)") +
  scale_y_continuous(
    "Accumulated child deaths"
  ) +
  facet_wrap(. ~ cohort2) +
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
    )
  
# fig2a

ggsave(paste0("../pres/fig2ab.pdf"), fig2a, width = width, height = height, units = "cm")

# 2.1. Animate ====

f2ab <- function(co) {
  
  cl_cs %>% 
    filter(cohort %in% co) %>%
    filter(!region %in% regions_to_remove) %>%
    filter(source %in% sources[1]) %>%
    mutate(source = factor(source, levels = sources)) %>%
    mutate(region = factor(as.character(region), levels = regions_long)) %>%
    ggplot() +
    geom_line(
      aes(x = age, y = median, group = region, colour = region)
      , size = region_line_size
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
      , size = 5
      , data = . %>% filter(age %in% age_br)
    ) +
    scale_color_discrete(col_lab, br = regions_long, labels = regions_short) +
    scale_fill_discrete(col_lab, br = regions_long, labels = regions_short) +
    scale_shape_discrete(col_lab, br = regions_long, labels = regions_short) +
    scale_x_continuous("Woman's life course (age in years)") +
    scale_y_continuous(
      "Accumulated child deaths"
    ) +
    coord_cartesian(ylim = c(0,4.5)) +
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
    ) +
    guides(colour=guide_legend(ncol=2)) +
    ggtitle(paste0("Woman's birth cohort: ", co))
  
}

pdf("../pres/fig2ab_trans.pdf", width = 6.5, height = 6.5)
lapply(1950:1999, f2ab)
dev.off()

# ! 3. Plot Facets C-D ----

fig2cd <-
  cl_cs %>% 
  filter(cohort %in% c(lower_year, upper_year)) %>%
  filter(!region %in% regions_to_remove) %>%
  filter(source %in% sources[2]) %>%
  mutate(source = factor(source, levels = sources)) %>%
  mutate(region = factor(as.character(region), levels = regions_long)) %>%
  ggplot() +
  geom_line(
    aes(x = age, y = median, group = region, colour = region)
    , size = region_line_size
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
  scale_color_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_fill_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_shape_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_x_continuous("Woman's life course (age in years)") +
  scale_y_continuous(
    "Children currently alive"
  ) +
  facet_wrap(. ~ cohort2) +
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
  )

# fig2cd

ggsave(paste0("../pres/fig2cd.pdf"), fig2cd, width = width, height = height, units = "cm")

# 3.1. Animate ====

f2cd <- function(co) {
  
  cl_cs %>% 
    filter(cohort %in% co) %>%
    filter(!region %in% regions_to_remove) %>%
    filter(source %in% sources[2]) %>%
    mutate(source = factor(source, levels = sources)) %>%
    mutate(region = factor(as.character(region), levels = regions_long)) %>%
    ggplot() +
    geom_line(
      aes(x = age, y = median, group = region, colour = region)
      , size = region_line_size
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
      , size = 5
      , data = . %>% filter(age %in% age_br)
    ) +
    scale_color_discrete(col_lab, br = regions_long, labels = regions_short) +
    scale_fill_discrete(col_lab, br = regions_long, labels = regions_short) +
    scale_shape_discrete(col_lab, br = regions_long, labels = regions_short) +
    scale_x_continuous("Woman's life course (age in years)") +
    scale_y_continuous(
      "Accumulated child deaths"
    ) +
    coord_cartesian(ylim = c(0,5.5)) +
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
    ) +
    guides(colour=guide_legend(ncol=2)) +
    ggtitle(paste0("Woman's birth cohort: ", co))
  
}

pdf("../pres/fig2cd_trans.pdf", width = 6.5, height = 6.5)
lapply(1950:1999, f2cd)
dev.off()


print("5 - Figure 2 saved to ../../Output")

