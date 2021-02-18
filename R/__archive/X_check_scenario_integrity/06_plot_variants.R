
# Evaluate findings under different assumptions of fertility

# 1. Table of differences --------------
# Is there an eventual convergence in the experience of 
# maternal bereavement between women in the Global North 
# and South, on average
country_plot <- "guatemala"

width <- 16
height <- 12
base_size <- 15
region_line_size <- 1
point_size <- 1
age_br <- c(seq(5, 100, 20), 100)

mort_old <- c("Lower 95 PI", "Upper 95 PI", "constant", "Median PI")
fert_old <- c("low", "high", "constant", "medium")

# mort_levs <- c("lower 95 pi", "upper 95 pi", "constant/stable", "median pi")
mort_levs <- c("low mortality", "high mortality", "stable/constant", "medium")
fert_levs <- c("low fertility", "high fertility", "stable/constant", "medium")


readme <- list.files(
  path = "../../Data/estimates/"
  , pattern = "df_cl_m_1950to1999_15to100_fert"
  , full.names = T
  )

cl_robust <- 
  lapply(readme, readRDS) %>% 
  bind_rows() %>% 
  filter(country == country_plot) %>%
  filter(cohort == 2000) %>% 
  mutate(
    variant_mort = plyr::mapvalues(variant_mort, mort_old, mort_levs)
    , variant_mort = factor(variant_mort, mort_levs)
    , variant_fert = plyr::mapvalues(variant_fert, fert_old, fert_levs)
    , variant_fert = factor(variant_fert, fert_levs)
    ) 
  
# Get df of medium estiamtes to plor in all facets ===============

cl_medium <- 
  cl_robust %>% 
  filter(variant_fert == "medium") %>% 
  filter(variant_mort == "medium") %>% 
  pull(value)

cl_reference_medium <- cl_robust
cl_reference_medium$value <- cl_medium

cl_reference_medium <- 
  cl_reference_medium %>% 
  filter(variant_fert != "medium") %>% 
  filter(variant_mort != "medium") %>% 
  mutate(type = "reference_medium")

# Get df of stable estiamtes to plor in all facets ===============

cl_stable <- 
  cl_robust %>% 
  filter(variant_fert == "stable/constant") %>% 
  filter(variant_mort == "stable/constant") %>% 
  pull(value)

cl_reference_stable <- cl_robust
cl_reference_stable$value <- cl_stable

cl_reference_stable <- 
  cl_reference_stable %>% 
  filter(variant_fert != "stable/constant") %>% 
  filter(variant_mort != "stable/constant") %>% 
  mutate(type = "stable")


# Plot
cl_robust %>%
  filter(variant_fert != "medium") %>% 
  filter(variant_mort != "medium") %>% 
  filter(variant_fert != "stable/constant") %>% 
  filter(variant_mort != "stable/constant") %>% 
  mutate(type = "alternative") %>% 
  ggplot(aes(x = age, y = value)) +
  # Plot different scenarios
  geom_line(
    size = region_line_size
    , show.legend = FALSE
  ) +
  # Medium variant as reference
  geom_line(
    aes(colour = type)
    , size = region_line_size
    , linetype = "dashed"
    , data = cl_reference_medium %>% 
      filter(variant_fert != "stable/constant") %>% 
      filter(variant_mort != "stable/constant")
  ) +
  # Stable variant as reference
  geom_line(
    aes(colour = type)
    , size = region_line_size
    , linetype = "dashed"
    , data = cl_reference_stable %>% 
      filter(variant_fert != "medium") %>% 
      filter(variant_mort != "medium")
  ) +
  # # SCALES
  scale_x_continuous("Woman's life course (age in years)") +
  scale_y_continuous(
    "Cumulative child death"
    , position = "left"
    , sec.axis = dup_axis()
    , breaks = c(0, 0.33, 0.66, 1)
    # , breaks = scales::pretty_breaks(n=3)
  ) +
  scale_color_discrete("Reference variants", labels = c("Medium", "Stable")) +
  # scale_linetype_discrete("", breaks = "reference_medium", labels = "Medium scenario (reference)") +
  facet_grid(variant_fert ~ variant_mort, scales = 'fixed', switch = "y") +
  # Use with four measures
  theme_bw(base_size = base_size) +
  theme(
    legend.position = "bottom"
    # Remove space over legend
    , legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")
    # Remove space between legends
    , legend.key.size = unit(1, "cm")
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

# ggsave(paste0("../../Output/robust_cd_",country_plot,".pdf"), width = width, height = height-1, units = "cm")
