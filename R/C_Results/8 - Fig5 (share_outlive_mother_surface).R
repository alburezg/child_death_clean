

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data requirements: 
# ASFRC
# df_cl_m_full:
# df_cl_m_full <- readRDS('../../Data/estimates/df_cl_m_1950to1999_15to100.RDS')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Parameters ----

width <- 25
height <- 25
base_size <- 18
label_size <- 5

# 1. Estimate values ----

# 1.0. Get Life expectnacy per regio ====

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

# 1.1. Enumerator ====

# Cumulative child death up to age a

enumerator <- 
  df_cl_m_full %>%
  arrange(country, cohort, age) %>% 
  select(country, cohort, age, children_dead = value) 

# 1.2. Denominator ====

# Children born up to age 'a' by birth cohort

# Add ages over 45 for ASFR 

asfr_upper <- 
  ASFRC %>% 
  filter(dplyr::between(Cohort, 1950, 2000)) %>% 
  select(country, cohort = Cohort) %>% 
  distinct() %>% 
  mutate(age = factor(46, levels = 46:100)) %>% 
  tidyr::complete(age, country, cohort) %>% 
  select(country, cohort, age) %>% 
  arrange(country, cohort, age) %>% 
  mutate(
    age = as.numeric(as.character(age))
    , ASFR = 0
    )

asfr_full <- bind_rows(
  ASFRC %>% 
    filter(dplyr::between(Cohort, 1950, 2000)) %>% 
    select(country, cohort = Cohort, age = Age, ASFR) 
  ,   asfr_upper 
) %>% 
  arrange(country, cohort, age)

denominator <- 
  asfr_full %>% 
  group_by(country, cohort) %>% 
  mutate(value = cumsum(ASFR)/1000) %>% 
  ungroup %>% 
  select(country, cohort, age, children_born = value) 

# 1.3. DF with enumerator and denominator

share_died <- merge(
  enumerator
  , denominator
  , by = c("country", "cohort", "age")
  , all.x = T
  , all.y = F
) %>% 
  arrange(country, cohort, age) %>% 
  mutate(value = children_dead/children_born)

# 3. Regional means ====

share_died_reg <- merge(
  share_died
  , un_reg %>% 
    filter(type %in% c("country", "un_sdg-groups")) %>% 
    select(country = level1, region = un_sdg_groups)
  , by.x = 'country'
  , all.x = T
) %>% 
  filter(!is.na(region)) %>% 
  group_by(region, cohort, age) %>% 
  summarise(
    median = median(value)
    , mean = mean(value)
  ) %>% 
  ungroup

# 4. Heatmap ----

df <- 
  share_died_reg %>%
  arrange(region, cohort, age) %>% 
  filter(!region %in% regions_to_remove) %>% 
  select(region, cohort, age, value = median) %>% 
  mutate(
    cohort = as.numeric(cohort)
    , age = as.numeric(age)
    , id = paste(region, age, cohort, sep = "_")
    # Recode regions
    , region = plyr::mapvalues(region, from = regions_long, to = regions_short, warn_missing = F)
  ) 

# 4.2. Get coordinates ====

coords <- data.frame(do.call(rbind, apply(select(df, - value), 1, lexis_coord_cohort) ))

datapoly <- merge(df %>% select(id, value, region), coords, by = "id")

# Define colour 

H <- seq(255,-60,length=5)
L <- seq(75,15,length=5) # L is luminance
L[1] <- 99 # we blend in for near-white
# cols = colors, brks = breaks, labs = labels
cols <- myfxHCLramp(H=H,L=L,N=2)
# For CD
# brks <- seq(0.15,0.6,0.15)
brks <- c(0.05, 0.30, 0.6)

lab_grad <- brks*100

# For contour lines
min_val <- c(0.05, seq(0.1,0.5,0.1))
# min_val <- c(0.1, 0.25, 0.5, 0.75, 1, 2, 3)

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
    # , value = paste0(value*100, "%")
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
  

lex_share_died <-
  datapoly %>% 
  mutate(
    region = factor(region, levels = regions_short)
  ) %>% 
  # filter(region == "europe and northern america") %>% 
  ggplot(aes(x=cohort, y=age)) + 
  geom_polygon(aes(fill=value, group=id)) +
  # geom_polygon(aes(fill=value, group=id, alpha = shrink)) +
  scale_fill_gradientn("Share of children have died",colors= cols, breaks=brks,labels=lab_grad,space="Lab") +
  # scale_alpha_continuous(aes(group = region), range = c(0, 1)) +
  #  Life expectancy and contour lines
  geom_line(aes(x = cohort, y = age, group = value, linetype = type), data = lines_df) +
  # Add labels for contour plot
  geom_text(
    aes(x = cohort, y = age, group = value, label = value)
    , data = label_min_value(min_val, df, x = 1969, shift_y = -4, as_share = T)
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

# lex_share_died

ggsave(paste0("../../Output/Fig5_share_died.pdf"), lex_share_died, width = width, height = height, units = "cm")
