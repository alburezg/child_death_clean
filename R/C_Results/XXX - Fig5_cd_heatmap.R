
# 20200122

# 0. Parameters ----

width <- 25
height <- 25
base_size <- 18
label_size <- 5

# 1. Heatmap ~~~~ ----

# Cumulative child death CD

# df_cl_m_full is the df with these estimates by country
# sum_cl is the regional equivalent

# Add lx column

lx <- sum_abs %>% 
  arrange(region, cohort, age) %>% 
  group_by(region, cohort) %>% 
  mutate(
    shrink = lx / dplyr::first(lx)
    , id = paste(region, age, cohort, sep = "_")
  ) %>% 
  ungroup %>% 
  select(id, shrink)

# 1.1. Get median ex per region/cohort ====
# First, merge with df of reginos

ex_coh_reg <- 
  merge(
  LTCF %>% 
    filter(Age == 0) %>% 
    filter(between(Cohort, 1950, 2000)) %>% 
    select(country = Country, cohort = Cohort, ex)
  , un_reg %>% 
    filter(type %in% c("country", "un_sdg-groups")) %>% 
    select(country = level1, region = un_sdg_groups)
  , by.x = 'country'
  , all.x = T
) %>% 
  filter(!is.na(region)) %>% 
  group_by(region, cohort) %>% 
  summarise(
    median = median(ex)
    , mean = mean(ex)
  ) %>% 
  ungroup %>% 
  filter(!region %in% regions_to_remove) %>% 
  mutate(
    region = plyr::mapvalues(region, from = regions_long, to = regions_short, warn_missing = F)
         ) %>% 
  arrange(region, cohort)

# Get df of values to plot ====

df <- 
  sum_cl %>%
  arrange(region, cohort, age) %>% 
  filter(!region %in% regions_to_remove) %>% 
  select(region, cohort, age, value = median) %>% 
  mutate(
    cohort = as.numeric(cohort)
    , age = as.numeric(age)
    , id = paste(region, age, cohort, sep = "_")
    # Recode regions
    , region = plyr::mapvalues(region, from = regions_long, to = regions_short, warn_missing = F)
  )   %>% 
  merge(., lx, by = "id") %>% 
  arrange(region, cohort, age)

# 1.2. Get coordinates ====

coords <- data.frame(do.call(rbind, apply(select(df, - value), 1, lexis_coord_cohort) ))

datapoly <- merge(df %>% select(id, value, region, shrink), coords, by = "id")

# Define colour 

H <- seq(255,-60,length=6)
L <- seq(75,15,length=6) # L is luminance
L[1] <- 99 # we blend in for near-white
# cols = colors, brks = breaks, labs = labels
cols <- myfxHCLramp(H=H,L=L,N=2)
# For CD
brks <- seq(0,4,1)

# For contour lines
# min_val <- c(0.25, 0.5, 0.75, 1, 2, 3, 4)
min_val <- c(0.1, 0.5, 1, 2, 3, 4)

# 1.3. Plot ====

br <- seq(1950, 2000, 10)
# labs <- c(1950, seq(60, 90, 10), 2000)
# labs[labs == 2000] <- "'00"
labs <- c(paste0("'", seq(50 ,90, 10)) ,"'00")

# df for plotting lines

contour_df <- 
  find_min_value(min_val, df) %>% 
  mutate(
    type = "contour"
    , region = as.character(region)
    ) %>% 
  select(region, cohort, age, value, type)

lines_df <- bind_rows(
  contour_df
  , ex_coh_reg %>% 
    mutate(
      type = "ex"
      , dummy = as.character(NA)
      ) %>% 
    select(region, cohort, age = median, value = dummy, type)
)

ex_labels <- 
  lines_df %>% 
  filter(type == "ex") %>% 
  filter(cohort == 1996) %>%
  mutate(
    age = age - 4
    , label = "e[0]"
  ) %>% 
  select(region, cohort, age, label)

lex_cohort <-
  datapoly %>% 
  mutate(
    region = factor(region, levels = regions_short)
  ) %>% 
  # filter(region == "europe and northern america") %>% 
  ggplot(aes(x=cohort, y=age)) + 
  geom_polygon(aes(fill=value, group=id)) +
  # geom_polygon(aes(fill=value, group=id, alpha = shrink)) +
  scale_fill_gradientn("Cumulative child death",colors= cols, breaks=brks,labels=brks,space="Lab") +
  # scale_alpha_continuous(aes(group = region), range = c(0, 1)) +
  #  Life expectancy and contour lines
  geom_line(aes(x = cohort, y = age, group = value, linetype = type), data = lines_df) +
  # Add labels for contour plot
  geom_text(
    aes(x = cohort, y = age, group = value, label = value)
    , data = label_min_value(min_val, df, x = 1953, shift_y = 3)
    , size = label_size
    ) +
  # Add labels for e_x line
  geom_text(
    aes(x = cohort, y = age, group = label, label = label)
    , data = ex_labels
    , parse = T
    , size = label_size
  ) +
  # Grid 
  geom_vline(xintercept=seq(1950,2000,by=10), colour="#80808030") +
  geom_hline(yintercept=seq(20,100,by=10), colour="#80808030") +
  labs(x ="Cohort of women", y = "Age") +
  scale_x_continuous(br = br, labels = labs) +
  scale_y_continuous(sec.axis = dup_axis(name = "")) +
  scale_linetype("", breaks = "ex", labels = c("Life expectancy of women"), guide = F) +
  coord_cartesian(xlim = c(1950, 2000), ylim = c(15, 100), expand = FALSE) +
  facet_wrap( ~ region) +
  theme_bw(base_size = base_size) +
  # guides(
  #   fill = guide_colourbar(order = 1)
  #   , linetype = guide_legend(order = 2)
  # ) +
  theme(
    legend.position = "bottom"
    , legend.text=element_text(size=(base_size-2))
    # , legend.key.size = unit(3,"line")
    , panel.spacing = unit(1.4, "lines")
    # , plot.margin=unit(c(5.5, 5.5, 5.5, 6.5),"points")
  ) 

  

  # datapoly %>% 
  # mutate(
  #   region = factor(region, levels = regions_short)
  # ) %>% 
  # # filter(region == "europe and northern america") %>% 
  #   ggplot(aes(x=cohort, y=age)) + 
  #   geom_polygon(aes(fill=value, group=id)) +
  #   # geom_polygon(aes(fill=value, group=id, alpha = shrink)) +
  #   scale_fill_gradientn("Cumulative child deaths for a \n woman standing before us",colors= cols, breaks=brks,labels=brks,space="Lab") +
  #   # scale_alpha_continuous(aes(group = region), range = c(0, 1)) +
  #   #  Life expectancy and contour lines
  #   geom_line(aes(x = cohort, y = age, group = value, linetype = type), data = lines_df) +
  #   geom_text(aes(x = cohort, y = age, group = value, label = value), data = label_min_value(min_val, df, x = 1955)) +
  #   # Grid 
  #   geom_vline(xintercept=seq(1950,2000,by=10), colour="#80808030") +
  #   geom_hline(yintercept=seq(20,100,by=10), colour="#80808030") +
  #   labs(x ="Cohort of women", y = "Age") +
  #   scale_x_continuous(br = br, labels = labs) +
  #   scale_y_continuous(sec.axis = dup_axis(name = "")) +
  #   scale_linetype("", breaks = "ex", labels = c("Life expectancy of women")) +
  #   coord_cartesian(xlim = c(1950, 2000), ylim = c(15, 100), expand = FALSE) +
  #   facet_wrap( ~ region) +
  #   theme_bw() +
  #   theme(
  #     legend.position = "bottom"
  #     , legend.text=element_text(size=12)
  #     # , axis.text.x = element_text(hjust = -0.003)
  #     , panel.spacing.x = unit(1.2, "lines")
  #     , plot.margin=unit(c(5.5, 5.5, 5.5, 6.5),"points")
  #     ) +
  #    guides(
  #      fill = guide_colourbar(order = 1)
  #     , linetype = guide_legend(order = 2)
  #      )

# lex_cohort

ggsave(paste0("../../Output/fig5_cd_heatmap.pdf"), lex_cohort, width = width, height = height, units = "cm")

# # 2. Enhanced cohort plot ~~~~ 
# 
# 
# # It's terrible, don't use it
# # Where size of squares diminshes proportional to share of surviving population
# 
# # Quick and easy way to get lx is from sum_abs
# 
# lx <- sum_abs %>% 
#   group_by(region, cohort) %>% 
#   mutate(
#     shrink = lx / dplyr::first(lx)
#     , id = paste(region, age, cohort, sep = "_")
#   ) %>% 
#   ungroup %>% 
#   select(id, shrink)
# 
# df <- 
#   sum_cl %>%
#   arrange(region, cohort, age) %>% 
#   filter(!region %in% regions_to_remove) %>% 
#   select(region, cohort, age, value = median) %>% 
#   mutate(
#     cohort = as.numeric(cohort)
#     , age = as.numeric(age)
#     , id = paste(region, age, cohort, sep = "_")
#     # Recode regions
#     , region = plyr::mapvalues(region, from = regions_long, to = regions_short)
#   ) %>% 
#   merge(., lx, by = "id") %>% 
#   arrange(region, cohort, age)
# 
# # Now just reduce the width of the squares proportional to 
# 
# #2.1. Get coordinates 
# 
# coords <- data.frame(do.call(rbind, apply(select(df, - value), 1, lexis_coord_shrink) ))
# 
# datapoly <- merge(df %>% select(id, value, region, shrink), coords, by = "id")
# 
# # Define colour 
# 
# H <- seq(255,-60,length=6)
# L <- seq(75,15,length=6) # L is luminance
# L[1] <- 99 # we blend in for near-white
# # cols = colors, brks = breaks, labs = labels
# cols <- myfxHCLramp(H=H,L=L,N=2)
# # For CD
# brks <- seq(0,4,1)
# # For first difference
# # brks <- seq(0,0.2,0.01)
# # For Burden
# # brks <- seq(0,max(df$value, na.rm = T),1e6)
# 
# # For contour lines
# min_val <- c(0.25, 0.5, 0.75, 1, 2, 3, 4)
# # min_val <- c(0.1, 0.25, 0.5, 0.75, 1, 2, 3)
# 
# 
# # 2.2. Plot 
#   
# lex_cohort_shrink <-
#   datapoly %>% 
#   ggplot(aes(x=cohort, y=age)) + 
#   geom_polygon(aes(fill=value, group=id)) +
#   scale_fill_gradientn("Cumulative offspring deaths \n for a woman standing before us",colors= cols, breaks=brks,labels=brks,space="Lab") +
#   # Lines for specific values
#   geom_line(aes(x = cohort, y = age, group = value), data = find_min_value(min_val, df)) +
#   geom_text(aes(x = cohort, y = age, group = value, label = value), data = label_min_value(min_val, df, x = 1955)) +
#   # Grid 
#   geom_vline(xintercept=seq(1950,2000,by=10), colour="#80808030") +
#   geom_hline(yintercept=seq(15,100,by=10), colour="#80808030") +
#   coord_cartesian(xlim = c(1950, 2000), ylim = c(15, 100), expand = FALSE) +
#   facet_wrap( ~ region) +
#   theme_bw() +
#   theme(legend.position = "bottom")
# 
# lex_cohort_shrink
# 
# ggsave(paste0("fig_lexis_cohort_shrink.pdf"), lex_cohort_shrink, width = width, height = height, units = "cm")
