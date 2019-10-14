
# Tables for supplementary materials
# 20101014

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data required: 
# Child death: 
# df_cl_m_full
# Child survival
# df_cs_m_full
# cs_pop
# df_cl_diff
# abs_df
# cs_ex_pop_country
# cl_ex_pop_country
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Include full country results
# this includes estimates for all countries AND regions, including Austrialia and NZ

# 0. Params ----

# Ages for table
# ages <- c(seq(20, 90, 15), 100)
# cohorts <- c(seq(1950, 1990, 10), 1999)

ages <- c(20, 45, 100)
cohorts <- c(1950, 1975, 1999)

# Fig 2 ~~~~ ----

# 2.1	Child death (CD) ====
# ~~~~~~~~~~~~~

# Full country results for the (cumulative) number of child deaths for a woman surviving to selected ages (median and IQR).

cl_full <- merge(
  df_cl_m_full %>% 
    filter(type == "country") %>% 
    filter(cohort %in% cohorts)
  , female_births %>% 
    select(cohort = year, cohort_size = value, everything())
  , by = c('country', 'cohort')
  , all.x = T
)

cl_countries <- 
  cl_full %>% 
  filter(type == 'country') %>% 
  filter(age %in% ages) %>% 
  filter(cohort %in% cohorts) %>% 
  mutate(region = as.character(region)) %>% 
  select(country, region, cohort, age, value)
  
cl_regions <- 
  cl_countries %>% 
  group_by(region, age, cohort) %>%
  summarise(
    median = round(median(value), 2)
    , iqr = round(IQR(value), 1)
  ) %>%
  ungroup() %>% 
  mutate(
    type = "region"
    , CD = paste0(median, " (", iqr,")")
    , region = as.character(region)
    , area = as.character("")
         ) %>% 
  # mutate_each(as.character) %>% 
  select(area, region, age, cohort, CD)

cl_long <- bind_rows(
  cl_countries %>% 
    mutate(
      CD = as.character(round(value, 2))
           ) %>% 
    # mutate_each(as.character) %>% 
    select(region, area = country, age, cohort, CD)
  , cl_regions
) %>% 
  arrange(region, area, cohort, age)

# To wide

cl_w <- spread(
  cl_long %>% 
    mutate(cohort_age = paste0(cohort, "_", age)) %>% 
    select(-cohort, -age)
  , cohort_age
  , CD
  ) %>% 
  arrange(region, area)

# Order
cl_w <- cl_w[ , c("region", "area", paste0(sort(rep(cohorts, length(cohorts))), "_", ages))]

# Add region names

cl_w$region[cl_w$area != ""] <- ""

# Format 0 values

cl_w[cl_w == "0"] <- '<0.01'

# Export

write.csv(cl_w, "../../Output/tab.2.1_child_death_full.csv", row.names = F)

print("8 - tab.2.1_child_death_full saved to ../../Output")

# 2.2.	Child survival (CS) ====
# ~~~~~~~~~~~~~

# Full country results for the expected number of children surviving for a woman aged a (selected ages; median and IQR).

cs_full <- merge(
  df_cs_m_full %>% 
    filter(type == "country") %>% 
    filter(cohort %in% cohorts)
  , female_births %>% 
    select(cohort = year, cohort_size = value, everything())
  , by = c('country', 'cohort')
  , all.x = T
)

cs_countries <- 
  cs_full %>% 
  filter(type == 'country') %>% 
  filter(age %in% ages) %>% 
  filter(cohort %in% cohorts) %>% 
  mutate(region = as.character(region)) %>% 
  select(country, region, cohort, age, value)

cs_regions <- 
  cs_countries %>% 
  group_by(region, age, cohort) %>%
  summarise(
    median = round(median(value), 2)
    , iqr = round(IQR(value), 1)
  ) %>%
  ungroup() %>% 
  mutate(
    type = "region"
    , CS = paste0(median, " (", iqr,")")
    , region = as.character(region)
    , area = as.character("")
  ) %>% 
  # mutate_each(as.character) %>% 
  select(area, region, age, cohort, CS)

cs_long <- bind_rows(
  cs_countries %>% 
    mutate(
      CS = as.character(round(value, 2))
    ) %>% 
    # mutate_each(as.character) %>% 
    select(region, area = country, age, cohort, CS)
  , cs_regions
) %>% 
  arrange(region, area, cohort, age)

# To wide

cs_w <- spread(
  cs_long %>% 
    mutate(cohort_age = paste0(cohort, "_", age)) %>% 
    select(-cohort, -age)
  , cohort_age
  , CS
) %>% 
  arrange(region, area)

# Order
cs_w <- cs_w[ , c("region", "area", paste0(sort(rep(cohorts, length(cohorts))), "_", ages))]

# Add region names

cs_w$region[cs_w$area != ""] <- ""

# Format 0 values

cs_w[cs_w == "0"] <- '<0.01'

# Export

write.csv(cs_w, "../../Output/tab.2.2_child_survival_full.csv", row.names = F)

print("8 - tab.2.2_child_survival_full.csv saved to ../../Output")

# Fig 3 ~~~~ ----

# 3.1.	First difference of child death (ΔCD) ====
# ~~~~~~~~~~~~~

cohorts <- c(1950, 1975, 1999)
ages <- c(25, 70, 99)

# First difference of child death for a woman surviving to age $a$ 
# (full country results; selected ages; median and IQR).

diff_countries <- 
  df_cl_diff %>% 
  filter(type == 'country') %>% 
  filter(age %in% ages) %>% 
  filter(cohort %in% cohorts) %>% 
  mutate(region = as.character(region)) %>% 
  select(country, region, cohort, age, value = diff)

diff_regions <- 
  diff_countries %>% 
  group_by(region, age, cohort) %>%
  summarise(
    median = round(median(value), 2)
    , iqr = round(IQR(value), 1)
  ) %>%
  ungroup() %>% 
  mutate(
    type = "region"
    , CS = paste0(median, " (", iqr,")")
    , region = as.character(region)
    , area = as.character("")
  ) %>% 
  # mutate_each(as.character) %>% 
  select(area, region, age, cohort, CS)

diff_long <- bind_rows(
  diff_countries %>% 
    mutate(
      CS = as.character(round(value, 2))
    ) %>% 
    # mutate_each(as.character) %>% 
    select(region, area = country, age, cohort, CS)
  , diff_regions
) %>% 
  arrange(region, area, cohort, age)

# To wide

diff_w <- spread(
  diff_long %>% 
    mutate(cohort_age = paste0(cohort, "_", age)) %>% 
    select(-cohort, -age)
  , cohort_age
  , CS
) %>% 
  arrange(region, area)

# Order
diff_w <- diff_w[ , c("region", "area", paste0(sort(rep(cohorts, length(cohorts))), "_", ages))]

# Add region names

diff_w$region[diff_w$area != ""] <- ""

# Format 0 values

diff_w[diff_w == "0"] <- '<0.01'

# Export

write.csv(diff_w, "../../Output/tab.3.1_child_death_first_diff.csv", row.names = F)

print("8 - tab.3.1_child_death_first_diff.csv saved to ../../Output")

# 3.2.	Burden of child death ====
# ~~~~~~~~~~~~~

# IN  TENS OF THOUSENDS!

cohorts <- c(1950, 1975, 1999)
ages <- c(25, 70, 99)
round_by <- 2

abs_countries <- 
  abs_df %>% 
  filter(type == 'country') %>% 
  filter(age %in% ages) %>% 
  filter(cohort %in% cohorts) %>% 
  # Save as thousands
  mutate(absolute = absolute/1e5) %>% 
  mutate(region = as.character(region)) %>% 
  select(country, region, cohort, age, value = absolute)

abs_regions <- 
  abs_countries %>% 
  group_by(region, age, cohort) %>%
  summarise(
    median = round(sum(value), round_by)
    , iqr = round(IQR(value), round_by)
  ) %>%
  ungroup() %>% 
  mutate(
    type = "region"
    , CS = paste0(median, " (", iqr,")")
    , region = as.character(region)
    , area = as.character("")
  ) %>% 
  # mutate_each(as.character) %>% 
  select(area, region, age, cohort, CS)

abs_long <- bind_rows(
  abs_countries %>% 
    mutate(
      CS = as.character(round(value, round_by))
    ) %>% 
    # mutate_each(as.character) %>% 
    select(region, area = country, age, cohort, CS)
  , abs_regions
) %>% 
  arrange(region, area, cohort, age)

# To wide

abs_w <- spread(
  abs_long %>% 
    mutate(cohort_age = paste0(cohort, "_", age)) %>% 
    select(-cohort, -age)
  , cohort_age
  , CS
) %>% 
  arrange(region, area)

# Order
abs_w <- abs_w[ , c("region", "area", paste0(sort(rep(cohorts, length(cohorts))), "_", ages))]

# Add region names

abs_w$region[abs_w$area != ""] <- ""

abs_w[abs_w == "0"] <- '<0.01'

# Export

write.csv(abs_w, "../../Output/tab.3.2_child_death_burden.csv", row.names = F)

print("8 - tab.3.2_child_death_burden.csv saved to ../../Output")

# Fig 4 ~~~~ ----

# 4.1.	Expected children outlive mothers ====
# ~~~~~~~~~~~~~

cohorts <- c(seq(1950, 1999, 10), 1999)

csex_countries <- 
  cs_ex_pop_country %>% 
  filter(type == 'country') %>% 
  filter(cohort %in% cohorts) %>% 
  mutate(region = as.character(region)) %>% 
  select(country, region, cohort, value)
  

csex_regions <- 
  csex_countries %>% 
  group_by(region, cohort) %>%
  summarise(
    median = round(median(value), 2)
    , iqr = round(IQR(value), 1)
  ) %>%
  ungroup() %>% 
  mutate(
    type = "region"
    , value = paste0(median, " (", iqr,")")
    , region = as.character(region)
    , area = as.character("")
  ) %>% 
  select(area, region, cohort, value)

csex_long <- bind_rows(
  csex_countries %>% 
    mutate(
      value = as.character(round(value, 2))
    ) %>% 
    select(region, area = country, cohort, value)
  , csex_regions
) %>% 
  arrange(region, area, cohort)
  
# To wide

csex_w <- spread(
  csex_long 
  , cohort
  , value
) %>% 
  arrange(region, area)

# Add region names
csex_w$region[csex_w$area != ""] <- ""

# Format 0 values
csex_w[csex_w == "0"] <- '<0.01'

# Export
write.csv(csex_w, "../../Output/tab.4.1_child_outlive_expected.csv", row.names = F)

print("8 - tab.4.1_child_outlive_expected.csv saved to ../../Output")

# 4.2.	Share children outlive mothers ====
# ~~~~~~~~~~~~~

cohorts <- c(seq(1950, 1999, 10), 1999)

out_countries <-
  ecl_ctfr %>%
  filter(type == 'country') %>% 
  filter(cohort %in% cohorts) %>% 
  mutate(region = as.character(region)) %>% 
  mutate(share = 1 - value / tfr) %>% 
  select(country, region, cohort, value = share)


out_regions <- 
  out_countries %>% 
  group_by(region, cohort) %>%
  summarise(
    median = round(median(value), 2)
    , iqr = round(IQR(value), 1)
  ) %>%
  ungroup() %>% 
  mutate(
    type = "region"
    , value = paste0(median, " (", iqr,")")
    , region = as.character(region)
    , area = as.character("")
  ) %>% 
  # mutate_each(as.character) %>% 
  select(area, region, cohort, value)

out_long <- bind_rows(
  out_countries %>% 
    mutate(
      value = as.character(round(value, 2))
    ) %>% 
    select(region, area = country, cohort, value)
  , out_regions
) %>% 
  arrange(region, area, cohort)

# To wide

out_w <- spread(
  out_long 
  , cohort
  , value
) %>% 
  arrange(region, area)

# Add region names

out_w$region[out_w$area != ""] <- ""

# Format 0 values

out_w[out_w == "0"] <- '<0.01'

# Export
write.csv(out_w, "../../Output/tab.4.2_child_outlive_share_full.csv", row.names = F)

print("8 - tab.4.2_child_outlive_share_full.csv saved to ../../Output")