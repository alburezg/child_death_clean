# For background section, 
# one plot with ex in x axis and tfr in y axis for all regions
# and a line showing progress in 1950-1999 cohort

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data requirements: 
# LTCB
# ASFRC
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Plotting params ----

# Choose size options depending on whether image is intended for small format (e.g. PNAS).
# medium (regular draft) or large (presentation)

# 0.1. PNAS plotting params (small)
# base_size <- 9
# width <- 9
# height <- 6
# region_line_size <- 0.7
# country_line_size <- 0.5
# point_size <- 2
# text_size <- 2

# 0.2. Draft paper and presentation format (large)

base_size <- 17
width <- 20
height <- 14
region_line_size <- 1
country_line_size <- 1
point_size <- 5
text_size <- 4

# 1. Cohort ex ----

ex_df <- 
  LTCB %>% 
  filter(dplyr::between(Cohort, 1950, 1999)) %>%
  filter(Age == 0) %>% 
  select(country = Country, cohort = Cohort, ex)
# filter(age < 100)

# 2. Cohort TFR ----

ctfr <- 
  ASFRC %>% 
  filter(dplyr::between(Cohort, 1950, 1999)) %>%
  group_by(country, Cohort) %>% 
  summarise(tfr = sum(ASFR)/1000) %>% 
  ungroup %>% 
  rename(cohort = Cohort) 
  # mutate(source = "TFR")

# 3. Merge and get regions ----

ex_ctfr <- merge(
  ex_df
  , ctfr
  , by = c('country', 'cohort')
)

# ex_ctfr <- rbind(ex_df, ctfr)

ex_ctfr_reg <- 
  merge(
    ex_ctfr
    , un_reg
    , by.x = 'country'
    , by.y = 'level1'
    , all.x = T
    , all.y = F
  ) %>% 
  mutate(
    region = factor(default_region, levels = regions_long)
  ) %>% 
  filter(type == 'country') %>% 
  select(region, type, country, cohort, ex, tfr)

# 4. Summarise by region ====

ex_ctfr_sum <- 
  ex_ctfr_reg %>% 
  group_by(region, cohort) %>% 
  summarise(
    ex = median(ex)
    , tfr = median(tfr)
  ) %>% 
  ungroup %>% 
  filter(!region %in% regions_to_remove)

# 5. Pre-plotting params ----

# 5.1. Get df for point sizes =====

df_l <- split(ex_ctfr_sum, ex_ctfr_sum$region)

# brk <- c(1, 25, 50)
brk <- c(1, 50)
siz <- c(2,1)

points <- data.frame(do.call(rbind, lapply(df_l, function(df) {
  d <- arrange(df, ex)[brk, ]
  d$size <- siz
  d
  }) ), stringsAsFactors = F ) %>% 
  na.omit() %>% 
  mutate(region = factor(region, levels = regions_long))

# 5.2. Chose individual countries ====

# To display in plot

con <- c(
  "sweden"
  # , "south africa"
  , 'zimbabwe'
  # , 'kenya'
         )

# con_new <- c("SWE", "ZA")

country_lines <- 
  ex_ctfr_reg %>% 
  filter(country %in% con) %>% 
  arrange(country, cohort)

lab_df <- data.frame(
  text = c("Sweden", "Zimbabwe")
  , x = c(86, 49)
  , y = c(2.1, 6.3)
)

# !! 5. Plot ----

p_ex_ctfr <- 
  ex_ctfr_sum %>% 
  mutate(region = factor(as.character(region), levels = regions_long)) %>% 
  ggplot() + 
  # Region lines
  geom_line(
    aes(x = ex, y = tfr, colour = region, group = region)
    , show.legend = F
    , size = region_line_size
  ) +
  # Selected countries
  geom_line(
    aes(x = ex, y = tfr, group = country)
    , linetype = 'longdash'
    # , linetype = 'dashed'
    # , linetype = 'dotted'
    , colour = "black"
    , data = country_lines
    , show.legend = F
    , size = country_line_size
  ) +
  # COuntry names
  geom_text(
    aes(x = x, y = y, label = text)
    , size = text_size
    , data = lab_df
             ) +
  geom_point(
    aes(x = ex, y = tfr, colour = region, shape = region
        # , size = size
        )
    , size = point_size
    , data = points
  ) +
  scale_x_continuous(
    # expression(e[0])
    "Cohort life expectancy at birth (years)"
    ) +
  scale_y_continuous(
    "Cohort Total Fertility Rate"
    # , position = "left"
    # , sec.axis = dup_axis()
  ) +
  scale_color_discrete("", br = regions_long, labels = regions_short) +
  scale_shape_discrete("", br = regions_long, labels = regions_short) +
  # scale_size_continuous("", range = c(1, 3), guide = F) +
  theme_bw(base_size = base_size) +
  theme(
    legend.position = "bottom"
    # One legned under the other
    , legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")
    # Remove space between legends
    , legend.key.size = unit(0.1, "cm")
    # remove all margins
    # , plot.margin = grid::unit(c(0,1,0,1), "mm")
  )

p_ex_ctfr

ggsave(paste0("../../Output/fig1.pdf"), p_ex_ctfr, width = width, height = height, units = "cm")

print("4 - Figure 1 saved to ../../Output")