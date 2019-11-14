
print("Running script: 5 - ungroup_births.R")

# We require the size of annual female birth cohorts by UN region.
# However, the WPP only reports the number of births (both sexes) in 5-year bands.
# To estimate the yearly number of female births (ie "female birth cohort size"):
#  a. multiply the 5-year number of births by the sex ratio at birth, and
#  b. ungroup to year-specific estimates using linear interpolation.

visual_examination <- F
export <- T

# 0. Format ----
# ~~~~~~~~~~~~~~~~~~

# 0.1. Births df

births_5 <- bind_rows(
  format_births(df = births_obs_B)
  , format_births(df = births_pred1_B)
) %>% 
  # Fix UN overlapping periods
  mutate(
    year = change_period_labels(year)
    # UN values are given in thousands
    , value = value*1000
    ) %>% 
  arrange(country, year)

# 0.2. Sex ratio at birth df

sex_ratio_5 <- bind_rows(
  format_births(df = sex_ratio_at_birth_obs)
  , format_births(df = sex_ratio_at_birth_pred1)
) %>% 
  # Fix UN overlapping periods
  mutate(year = change_period_labels(year)) %>% 
  arrange(country, year)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A. Women only ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. 5-year number of female births ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

births_f_5 <- 
  births_5 %>% 
  mutate(
    value = value / (sex_ratio_5$value + 1)
    )

# 2. 1-year number of female births ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Interpolate female births for every calendar year

births_f_1 <- 
  interpolate_births_calendar_years(
  df_5 = births_f_5
  , method = "linear"
  ) %>% 
  mutate(
    country = as.character(country)
    , value = round(value, 0)
    )

# 3. Checks ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(visual_examination) {
  
  country_keep <- c("sweden", "guatemala", "israel", "sri Lanka")
  
  cowplot::plot_grid(
    births_f_1 %>% 
      filter(country %in% country_keep) %>% 
      ggplot(aes(x = year, y = value, group = country, colour = country)) +
      geom_line(size=0.5) +
      scale_x_continuous(breaks = seq(1950, 2100, 50)) +
      theme(legend.position = "none")
    , 
    births_f_5 %>% 
      filter(country %in% country_keep) %>% 
      mutate(year = as.numeric(str_extract(year, '^[0-9]{4}'))) %>% 
      ggplot(aes(x = year, y = value, group = country, colour = country)) +
      scale_x_continuous(breaks = seq(1950, 2100, 50), labels = seq(1950, 2100, 50)) +
      geom_line(size=0.5) +
      # facet_grid(. ~ country) +
      theme(legend.position = "right")
  )
  
}

# 4. Export ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(export) {
  write.csv(births_f_1, file = '../../Data/derived/wpp_female_births_1_1.csv', row.names = F)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# B. Men and women ~~~~ ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. 1-year number of female births ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Interpolate female births for every calendar year

births_1 <- 
  interpolate_births_calendar_years(
    df_5 = births_5
    , method = "linear"
  ) %>% 
  mutate(
    country = as.character(country)
    , value = round(value, 0)
  )

# 2. Checks ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(visual_examination) {
  
  country_keep <- c("sweden", "guatemala", "israel", "sri Lanka")
  
  cowplot::plot_grid(
    births_1 %>% 
      filter(country %in% country_keep) %>% 
      ggplot(aes(x = year, y = value, group = country, colour = country)) +
      geom_line(size=0.5) +
      scale_x_continuous(breaks = seq(1950, 2100, 50)) +
      theme(legend.position = "none")
    , 
    births_5 %>% 
      filter(country %in% country_keep) %>% 
      mutate(year = as.numeric(str_extract(year, '^[0-9]{4}'))) %>% 
      ggplot(aes(x = year, y = value, group = country, colour = country)) +
      scale_x_continuous(breaks = seq(1950, 2100, 50), labels = seq(1950, 2100, 50)) +
      geom_line(size=0.5) +
      # facet_grid(. ~ country) +
      theme(legend.position = "right")
  )
  
}

# 3. Export ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(export) {
  write.csv(births_1, file = '../../Data/derived/wpp_all_births_1_1.csv', row.names = F)
}

print("Size of female and total birth cohorts saved as 1x1 csv file to ../../Data/derived/wpp_female_births_1_1.csv")
