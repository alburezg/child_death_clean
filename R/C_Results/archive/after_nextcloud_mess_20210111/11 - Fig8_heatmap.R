
# Heatmap:
# Number of children currently alive as a share of children ever born. Solid lines
# show the ages at which we would expect various percentiles of a woman’s progeny to still
# be alive. Dotted lines mark the regional life expectancy in years for an average woman in
  # each cohort and region.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data requirements: 
# ASFRC
# df_cl_m_full <- readRDS('../../Data/estimates/df_cl_m_1950to1999_15to100.RDS')
# ex_coh_reg
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Parameters ----

width <- 25
height <- 25
base_size <- 16
label_size <- 5

y_br <- c(20, 40, 50, 60, 65, 80, 100)
y_labs <- c(20, 40, "end of\nreproduction", 60, "retirement", 80, 100)
y_labs_simple <- c(20, 40, "", 60, "", 80, 100)
 

# gradient_label <- "Children surviving to a woman aged 'a' as\na share of her current parity (%)"
# gradient_label <- "Number of children surviving to a woman\naged 'a' as a share of her current parity"
gradient_label <- "Mean number of children surviving to a woman\naged 'a' as a share of her parity at age 'a'  (%)"

# 1. Estimate values ----

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

# 3. Regional medians ====

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
  # select(region, cohort, age, value = mean) %>% 
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
brks <- c(0.05, 0.25, 0.5)

lab_grad <- (1-brks)*100
# lab_grad[1] <- paste0(lab_grad[1], "%")

# For contour lines
# min_val <- c(0.05, seq(0.1,0.5,0.1))
min_val <- c(0.01, 0.05, seq(0.1,0.5,0.1))

# 4.3 Get median ex per region/cohort ====
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

# 1.3. Plot ====

br <- seq(1950, 2000, 10)
# labs <- c(1950, seq(60, 90, 10), 2000)
# labs[labs == 2000] <- "'00"
labs <- c(paste0("'", seq(50 ,90, 10)) ,"'00")

# df for plotting lines ====

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

labels_df1 <- label_min_value(
  min_val
  , df
  , x = 1958
  , shift_y = -4
  , as_share = T
) 

remove1 <- labels_df1$region == "Central & South Asia" & labels_df1$value == "10%"
remove2 <- labels_df1$region == "Sub-Saharan Africa" & labels_df1$value %in% c("5%", "10%")
labels_df1 <- labels_df1[!(remove1|remove2), ]

reg_keep <- c("Sub-Saharan Africa", "North Africa & West Asia", "Central & South Asia")
val_keep <- c("5%", "10%")

labels_df2 <- label_min_value(
  min_val
  , df 
  , x = 1995
  , shift_y = 4
  , as_share = T
) %>% 
  filter(value %in% val_keep & region %in% reg_keep)

remove <- labels_df2$region == "North Africa & West Asia" & labels_df2$value == "10%"
labels_df2 <- labels_df2[!remove, ]

labels_df <- bind_rows(labels_df1, labels_df2)

# Fix by hand

cond <- labels_df$region == "Central & South Asia" & labels_df$value == "5%"
labels_df$cohort[cond] <- 1992
labels_df$age[cond] <- 50

cond <- labels_df$region == "East & SE Asia" & labels_df$value == "10%"
labels_df$age[cond] <- 79

cond <- labels_df$region == "East & SE Asia" & labels_df$value == "1%"
labels_df$cohort[cond] <- 1988
labels_df$age[cond] <- 21

cond <- labels_df$region == "LATAM & Caribbean" & labels_df$value == "5%"
labels_df$cohort[cond] <- 1962

cond <- labels_df$region == "LATAM & Caribbean" & labels_df$value == "1%"
labels_df$cohort[cond] <- 1990
labels_df$age[cond] <- 21

cond <- labels_df$region == "LATAM & Caribbean" & labels_df$value == "10%"
labels_df$age[cond] <- 78

cond <- labels_df$region == "North Africa & West Asia" & labels_df$value == "10%"
labels_df$age[cond] <- 80

cond <- labels_df$region == "North Africa & West Asia" & labels_df$value == "1%"
labels_df$cohort[cond] <- 1990
labels_df$age[cond] <- 21

cond <- labels_df$region == "Sub-Saharan Africa" & labels_df$value == "30%"
labels_df$cohort[cond] <- 1961

cond <- labels_df$region == "Sub-Saharan Africa" & labels_df$value == "20%"
labels_df$cohort[cond] <- 1963

cond <- labels_df$region == "Europe & N America" & labels_df$value == "1%"
labels_df$cohort[cond] <- 1980
labels_df$age[cond] <- 40


# Recode to make this about survival

old <- paste0(c(1, 5, 10, 20, 30,40), "%")
new <- paste0(c(99, 95, 90, 80, 70, 60), "%")

labels_df <- labels_df %>% 
  mutate(
    value = plyr::mapvalues(value, old, new)
  )

# For ex labels

cond <- ex_labels$region == "East & SE Asia"
ex_labels$age[cond] <- 85

cond <- ex_labels$region == "Central & South Asia"
ex_labels$age[cond] <- 77.5

# To add facet names
old_regions <- unique(datapoly$region)
new_regions <- paste(LETTERS[1:length(old_regions)], "-", old_regions)
# new_regions[1] <- "bold(A) - Central & South Asia"

# Long region names (two rows)
facet_id <-
  as_labeller(
    c("Central & South Asia" = "Central & South Asia"
      , "East & SE Asia" = "East & Southeast\nAsia"
      , "Europe & N America" = "Europe & North\nAmerica"
      , "LATAM & Caribbean" = "Latin America &\nCaribbean"
      , "North Africa & West Asia" = "North Africa & West\nAsia"
      , "Sub-Saharan Africa" = "Sub-Saharan Africa"
    )
  )

# Short region names
# facet_id <-
#   as_labeller(
#   c("Central & South Asia" = "A Central & South Asia"
#   , "East & SE Asia" = "B East & SE Asia"
#   , "Europe & N America" = "C Europe & N America"
#   , "LATAM & Caribbean" = "D LATAM & Caribbean"
#   , "North Africa & West Asia" = "E N Africa & West Asia"
#   , "Sub-Saharan Africa" = "F Sub-Saharan Africa"
# )
# )

# facet_id <- 
#   as_labeller(
#     c("C & S Asia" = "A Central & South Asia"
#       , "E & SE Asia" = "B East & SE Asia"
#       , "Europe & N America" = "C Europe & N America"
#       , "LATAM & Caribbean" = "D LATAM & Caribbean"
#       , "N Africa & W Asia" = "E N Africa & West Asia"
#       , "Sub-Sah Africa" = "F Sub-Saharan Africa"
#     )
#   )

lex_share_surv <-
  datapoly %>% 
  mutate(
    region = factor(region, levels = regions_short)
  ) %>% 
  ggplot(aes(x=cohort, y=age)) + 
  geom_polygon(aes(fill=value, group=id)) +
  #  Life expectancy and contour lines
  geom_line(aes(x = cohort, y = age, group = value, linetype = type), data = lines_df) +
  # Add labels for contour plot
  geom_text(
    aes(x = cohort, y = age, group = value, label = value)
    , data = labels_df
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
  # Retirement age
  geom_hline(yintercept = c(49, 65), linetype = "dotted") +
  # Format
  labs(x ="Cohort of women", y = "Woman's age 'a'") +
  scale_fill_gradientn(gradient_label,colors= cols, breaks= brks,labels=lab_grad,space="Lab") +
  # scale_fill_viridis(gradient_label,breaks= brks,labels=lab_grad,space="Lab", direction = -1, option = "plasma") +
  scale_x_continuous(br = br, labels = labs) +
  scale_y_continuous(sec.axis = dup_axis(name = "", labels = y_labs_simple), breaks = y_br, labels = y_labs) +
  scale_linetype("", breaks = "ex", labels = c("Life expectancy of women"), guide = F) +
  coord_fixed(xlim = c(1950, 2000), ylim = c(15, 100), expand = FALSE) +
  facet_wrap( ~ region, labeller = facet_id) +
  theme_bw(base_size = base_size) +
  theme(
    legend.position = "bottom"
    , legend.text=element_text(size=(base_size-2))
    , panel.spacing = unit(1.4, "lines")
    # Move y axis closer to the plot
    , axis.title.y = element_text(margin = margin(t = 0, r = - 3, b = 0, l = -3))
    , plot.margin = unit(c(t=0.2, r=0.25, b=0.1, l=0.1), unit="cm")
    # get rid of facet boxes
    , strip.background = element_blank()
    , strip.text = element_text(hjust = 0)
    # Remove spacing between facets
    # , panel.spacing.x=unit(0.07, "cm")
  ) +
  guides(
    fill = guide_colorbar(reverse = T)
  )

# lex_share_surv

ggsave(paste0("../../Output/fig8_heatmap.pdf"), lex_share_surv, width = width, height = height, units = "cm")