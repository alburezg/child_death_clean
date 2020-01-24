
# 20200122
# Create lexis surfaces for the measure computed in this paper
# This would be cohort Lexis surfaces showing offspring mortality by region
# inititally

# Starting point is Tom Riffe's visualisation: 
# https://sites.google.com/site/timriffepersonal/DemogBlog/lexissurfacesinggplot2
# I would like to do it with enhanced surfaces eventually:
# https://www.demographic-research.org/volumes/vol42/6/

# 0. Parameters ----

width <- 25
height <- 25

# 2. Period lexis surface ----

# x axis is period and diagonals trace cohort's experience

df <- 
  sum_cl %>%
  # sum_diff %>%
  # sum_abs %>%
  # mutate(median = value) %>% 
  arrange(region, cohort, age) %>% 
  filter(!region %in% regions_to_remove) %>% 
  mutate(
    cohort = as.numeric(cohort)
    , age = as.numeric(age)
    , year = cohort + age
    , id = paste(region, year, age, cohort, sep = "_")
  ) %>% 
  select(id, region, year, age, cohort, value = median)

# 1.1. Get coordinates ====

coords <- data.frame(do.call(rbind, apply(select(df, id, year, age), 1, lexis_coord_period) ))

datapoly <- merge(df %>% select(id, value, region), coords, by = c("id") )

# 1.2. Cohort grid ====

a <- -rev(unique(df$cohort)[unique(df$cohort)%%10==0])
# a <- unique(df$cohort)[unique(df$cohort) %% 10 == 0] - 25
# a <- -c(a, a-50)
# we pick out the cohorts and remember that a is the 'y' intercept
b <- rep(1,length(a))
DF <- data.frame(a,b)

# 1.3. Plot ====

# Define colour 

H <- seq(255,-60,length=6)
L <- seq(75,15,length=6) # L is luminance
L[1] <- 99 # we blend in for near-white
# cols = colors, brks = breaks, labs = labels
cols <- myfxHCLramp(H=H,L=L,N=2)
# For CD
brks <- seq(0,4,1)
# For first difference
# brks <- seq(0,0.2,0.01)
# For Burden
# brks <- seq(0,max(df$value, na.rm = T),1e6)

# For contour lines
min_val <- c(0.25, 0.5, 0.75, 1, 2, 3, 4)
# min_val <- c(0.1, 0.25, 0.5, 0.75, 1, 2, 3)

# Plot proper

lex_period <-
  datapoly %>% 
  ggplot(aes(x=year, y=age)) + 
  geom_polygon(aes(fill=value, group=id)) +
  scale_fill_gradientn("Cumulative offspring deaths",colors= cols, breaks=brks,labels=brks,space="Lab") +
  # Lines for specific values
  geom_line(aes(x = year, y = age, group = value), data = find_min_value(min_val, df)) +
  geom_text(aes(x = year, y = age, group = value, label = value), data = label_min_value(min_val, df, x = 1955)) +
  # Lexis grid 
  geom_vline(xintercept=seq(1950,2100,by=25), colour="#80808030") +
  geom_hline(yintercept=seq(15,100,by=25), colour="#80808030") +
  geom_abline(aes(intercept=a, slope=b),data=DF, colour="#80808030") +
  coord_cartesian(xlim = c(1965, 2100), ylim = c(15, 100), expand = FALSE) +
  facet_wrap( ~ region) +
  theme_bw() +
  theme(legend.position = "bottom")

lex_period

ggsave(paste0("fig_lexis_period.pdf"), lex_period, width = width, height = height, units = "cm")
