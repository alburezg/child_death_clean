
# Evaluate findings under different assumptions of fertility

# 1. Table of differences --------------
# Is there an eventual convergence in the experience of 
# maternal bereavement between women in the Global North 
# and South, on average

ages_keep_table <- c(50, 65, 100)

# fsklmt498
cl_robust <-
  bind_rows(
    readRDS(paste0("../../Data/estimates/sum_cl_","low",".RDS")) %>% 
      mutate(variant = "low")
    , readRDS(paste0("../../Data/estimates/sum_cl_","medium",".RDS")) %>% 
      mutate(variant = "medium")
    , readRDS(paste0("../../Data/estimates/sum_cl_","high",".RDS")) %>% 
      mutate(variant = "high")
  ) %>% 
  filter(cohort == 2000) %>% 
  rename(value = median) 

cs_robust <-
  bind_rows(
    readRDS(paste0("../../Data/estimates/sum_cs_","low",".RDS")) %>% 
      mutate(variant = "low")
    , readRDS(paste0("../../Data/estimates/sum_cs_","medium",".RDS")) %>% 
      mutate(variant = "medium")
    , readRDS(paste0("../../Data/estimates/sum_cs_","high",".RDS")) %>% 
      mutate(variant = "high")
  ) %>% 
  filter(cohort == 2000) %>% 
  # filter(age %in% ages_keep) %>% 
  rename(value = median)

out_tab <-
cl_robust %>% 
  filter(age %in% ages_keep_table) %>% 
  mutate(value = round(value, 2)) %>%
  pivot_wider(names_from = variant, values_from = value) %>% 
  mutate(
    diff =  medium - low
    , diff2  = high - medium
    , diff3  = high - low
    # , diff2 = round(diff/medium, 2)
    ) %>% 
  select(-region) %>% 
  kable(
    "latex"
    , caption = "Cumulative number of child deaths experienced by a woman born in 2000 surviving to ages 50, 65, and 100 under three distinct assumptions about future fertility development (UNWPP `low', `medium', and `high' fertility variants for the 2020-2100 projection horizon)."
    , label = "robust",
    booktabs = T, escape = T, 
    # col.names = c("Woman's age", "Low", "Medium", "High", "High-Low")
    col.names = c("Woman's age", "Low", "Medium", "High","Medium-Low", "High-Medium", "High-Low")
  ) %>% 
  kable_styling() %>%
  add_header_above(c("Region" = 1, "UNWPP fertility variant" = 3, "Difference between fertility variants" = 3)) %>%
  pack_rows(
    index = c(
      "Sub-Saharan Africa" = length(ages_keep_table)
      , "North Africa & West Asia" = length(ages_keep_table)
      , "Central & South Asia" = length(ages_keep_table)
      , "East & SE Asia" = length(ages_keep_table)
      , "LATAM & Caribbean" = length(ages_keep_table)
      , "AUS & NZ" = length(ages_keep_table)
      , "Oceania (other)" = length(ages_keep_table)
      , "Europe & N America" = length(ages_keep_table)
    )
  )

write(out_tab, file = "../../Output/robustness_fertility.tex")

#2.  Fig: child surv and death by variant -----------

width <- 16
height <- 12
base_size <- 15
region_line_size <- 1
point_size <- 3.5


p_robust <-
bind_rows(
  cl_robust %>% mutate(source = "died (cumulative)")
  , cs_robust %>% mutate(source = "surviving")
) %>% 
  filter(!region %in% regions_to_remove) %>% 
  # mutate(source = factor(source, levels = sources)) %>% 
  mutate(
    region = factor(as.character(region), levels = regions_long)
    , variant = recode(variant
                       , low = "Low fertility"
                       , medium = "Medium fertility"
                       , high = "High fertility"
                       )
    , variant = factor(variant, c("Low fertility", "Medium fertility", "High fertility"))
    ) %>% 
  ggplot() +
  geom_line(
    aes(x = age, y = value, group = region, colour = region)
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
    aes(x = age, y = value, group = region, colour = region
        # , size = share
        , shape = region
    )
    , size = point_size
    , data = . %>% filter(age %in% age_br)
  ) +
  # # SCALES
  scale_x_continuous("Woman's life course (age in years)") +
  scale_y_continuous(
    "Number of children (woman born in 2000)"
    , position = "left"
    , sec.axis = dup_axis()
  ) +
  scale_color_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_fill_discrete(col_lab, br = regions_long, labels = regions_short) +
  scale_shape_discrete(col_lab, br = regions_long, labels = regions_short) +
  # scale_size_continuous("Population share") +
  facet_grid(source ~ variant, scales = 'free', switch = "y") +
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

p_robust

ggsave(paste0("../../Output/robust_fig2_cd_cs.pdf"), p_robust, width = width, height = height, units = "cm")
