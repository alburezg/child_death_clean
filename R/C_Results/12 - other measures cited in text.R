
# Functions ----

find_regions <- function(df, allowed_types = "country", region_choice = "default_region", ignore_regions = T){
  
  # Save un objects in gobal envir
  format_UN_country_grouping(un_regions)
  
  out <- df %>% 
    left_join(
      un_reg %>% 
        select(type, country = level1, region = starts_with(region_choice))
      , by = c('country')
    ) %>% 
    mutate(
      # Here you chose which regions will be used
      # (default column defined in script 2_UN_country_grouping.R)
      region = factor(region, levels = regions_long)
      # level2 = factor(un_sdg_groups, levels = old_sdg)
      # , cohort2 = paste0("Women born in ", variable)
    ) %>% 
    filter(type %in% allowed_types) %>% 
    # na values in col region are regions like 'world', central america', etc
    # and can safely be ignored
    filter(!is.na(region)) %>% 
    select(-type) %>% 
    select(region, country, everything())
  
  if(ignore_regions){
    out <- out %>% filter(!region %in% regions_ignore)
    print("Ignoring AUS/NZ and Small island states")
  } 
  
  return(out)
  
}

# Decide how countries will be groupped in the analysis
# On 20191010, we had decided to group all countries by UN SDG region 
format_UN_country_grouping <- function(un_regions){
  
  # Decide how countries will be groupped in the analysis
  # On 20191010, we had decided to group all countries by UN SDG region 
  
  
  un_reg <<- un_regions %>% 
    # fix text formatting
    mutate_all(.funs = fix_un_countries) %>% 
    # Chose which regions should be used as default
    # Very important as this will shape the final analysis
    # [PREFERRED 20190814]: Use UN SDG regions but remove UAS/NZ and Ocenia (other)
    mutate(default_region = un_sdg_groups)
  
  # Define labels for using in plots later on
  
  regions_long <<- c(
    "sub-saharan africa"
    , "northern africa and western asia"
    , "central and southern asia"
    , "eastern and south-eastern asia"
    , "latin america and the caribbean"
    , "australia_new zealand"
    , "oceania (excluding australia and new zealand)"
    , "europe and northern america"
  )
  
  regions_short <<- c(
    # "SS Africa"
    "Sub-Sah Africa"
    , "N Africa & W Asia"
    , "C & S Asia"
    , "E & SE Asia"
    , "LATAM & Caribbean"
    , "AUS & NZ"
    , "Oceania (other)"
    , "Europe & N America"
  )
  
  regions_ignore <<- c(
    "australia_new zealand"
    , "oceania (excluding australia and new zealand)"
  )
  
}

# New
retirement_age <- 65
two_cohorts <- c(2020 - retirement_age, 2000)

# 1. child deaths at retirement WORLD ----
# rcode ns621

sum_cl %>% 
  filter(age == retirement_age & cohort %in% two_cohorts) %>% 
  group_by(cohort, age) %>% 
  summarise(value = mean(median))

# 2. child deats by region ----
# rcode 7lsdk6

sum_cl %>% 
  filter(age == retirement_age & cohort %in% two_cohorts) %>% 
  select(region, cohort, median)



# 2. Relative change in size of birth cohorts ----
# rcode sf20j

# For whole world
female_births %>%
  filter(year %in% two_cohorts) %>% 
  find_regions(ignore_regions = F) %>% 
  group_by(cohort = year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup %>% 
  pivot_wider(names_from = cohort, values_from = value) %>% 
  mutate(x = `2000` / `1955` )

# 4. Reduction in burden of child death 1950 -2000 ----
# rcode: 4fgr

sum_abs_temp %>% 
  filter(cohort %in% two_cohorts) %>% 
  na.omit() %>% 
  select(region, cohort, age, value) %>% 
  pivot_wider(names_from = cohort, values_from = value) %>% 
  mutate(change = (`2000` - `1950`) / `1950` + 1) %>% 
  na.omit() %>% 
  group_by(region) %>% 
  summarise(mean = mean(change)) %>% 
  arrange(mean)

# 5. Share of offspring alive at retirement age ----

# worldwide
# gjks54

df %>% 
  group_by(cohort, age) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  filter(age == retirement_age) %>% 
  filter(cohort %in% two_cohorts) %>% 
  mutate(value = (1 - value)*100) %>% 
  select(cohort, value) %>% 
  pivot_wider(names_from = cohort, values_from = value)

# By region
# rcode sdlk36

df %>% 
  filter(age == retirement_age) %>% 
  filter(cohort %in% two_cohorts) %>% 
  mutate(value = (1 - value)*100) %>% 
  select(region, cohort, value) %>% 
  pivot_wider(names_from = cohort, values_from = value)

# By country
# rcode l34du

country_keep <- c(
  # "zambia", "australia", "republic of korea"
  # , "cambodia", "vietnam", "thailand"
  "bolivia (plurinational state of)", "argentina", "paraguay", "chile", "brazil", "paraguay"
)

share_died  %>% 
  filter(age == retirement_age) %>% 
  filter(cohort %in% two_cohorts) %>% 
  filter(country %in% country_keep) %>% 
  mutate(value = (1 - value)*100) %>% 
  select(country, cohort, value) %>% 
  pivot_wider(names_from = cohort, values_from = value)


# Difference between country with highest and lowest child death ----
# rcode shd88

share_died %>% 
  filter(age == retirement_age) %>% 
  filter(cohort %in% two_cohorts) %>% 
  mutate(value = (1 - value)*100) %>% 
  group_by(cohort) %>% 
  arrange(value) %>% 
  slice(1, n()) %>% 
  mutate(diff = value / lag(value)) %>% 
  # slice(2) %>% 
  ungroup()

# 4. Generational Burden for the whole world ----

# burden_world <- 
sum_burden %>% 
  group_by(cohort) %>% 
  summarise(
    value = sum(value) / 1e6
  ) %>% 
  ungroup %>% 
  filter(cohort %in% two_cohorts)

# 4. Burden of CD for young and old women ----
# rcode jsd83

# Global generational burden 

sum_burden %>% 
  filter(cohort %in% two_cohorts) %>% 
  group_by(cohort) %>% 
  summarise(value = sum(value) / 1e6 ) %>% 
  mutate(
    change = value / lag(value)
    , times_lower = 1 / change
    )

# Times more likely to experience death in retirement than in reproductive life
# Worldwide
# % rcode 76ger
# rcode alj27

rep_age <- 15:49
ret_age <- retirement_age:99

sum_abs_temp %>% 
  mutate(
    agegr = "other"
    , agegr = ifelse(age %in% rep_age, "reproductive", agegr)
    , agegr = ifelse(age %in% ret_age, "retirement", agegr)
  ) %>% 
  # group_by(region, cohort, agegr) %>% 
  group_by(cohort, agegr) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  # filter(cohort %in% two_cohorts) %>% 
  pivot_wider(names_from = agegr, values_from = value) %>% 
  mutate(
    share_rep = reproductive / (retirement + reproductive + other) * 100
    , share_ret = retirement / (retirement + reproductive + other) * 100
    , share_sum = share_rep + share_ret
    , share_more_in_retirement = share_ret/share_rep
  ) %>% 
  select(cohort, starts_with("share")) %>% 
  data.frame()

# Burden by region
# rcode a3ka8

sum_burden %>% 
  filter(cohort %in% two_cohorts) %>% 
  mutate(value = value/1e6) %>% 
  select(region, cohort, value)

# Burden in africa ====
# rcode as18r

sum_burden %>% 
  filter(cohort %in% two_cohorts) %>% 
  mutate(region = ifelse(region != "sub-saharan africa", "other", "ssa")) %>% 
  group_by(region, cohort) %>% 
  summarise(value = sum(value) / 1e6 ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = region, values_from = value) %>% 
  mutate(share = ssa / (ssa + other) * 100)

# burdne in africa over the projection horizon
# rcode fge53j

sum_burden %>% 
  filter(cohort %in% c(1950, 2000)) %>% 
  mutate(region = ifelse(region != "sub-saharan africa", "other", "ssa")) %>% 
  group_by(region, cohort) %>% 
  summarise(value = sum(value) / 1e6 ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = region, values_from = value) %>% 
  mutate(share = ssa / (ssa + other) * 100)


# Children outlive mothesr ----

# Global decline in absolute number of children outliving mothers
# rcode jd50k

tally_share %>% 
  filter(cohort %in% two_cohorts) %>% 
  filter(level == "Expected number of children") %>% 
  filter(measure == "Child survival") %>% 
  group_by(cohort) %>% 
  summarise(value = mean(value)) %>% 
  mutate(decline =  1 - (value / lag(value))) %>% 
  ungroup() 

# Regional decline in absolute number of children outliving mothers
# rcode hds5l


tally_share %>% 
  filter(cohort %in% two_cohorts) %>% 
  filter(level == "Expected number of children") %>% 
  filter(measure == "Child survival") %>% 
  group_by(region) %>% 
  arrange(region, cohort) %>% 
  mutate(decline =  1 - (value / lag(value))) %>% 
  ungroup() %>% 
  select(region, cohort, value, decline)


# Global decline as share of tfr
# rcode k847z

tally_share %>% 
  filter(level == "Expected number as fraction of TFR") %>% 
  filter(measure == "Child survival") %>% 
  filter(cohort %in% two_cohorts) %>% 
  group_by(cohort) %>% 
  summarise(value = mean(value) * 100) %>% 
  mutate(change = value / lag(value)) %>% 
  ungroup() 


# Global decline as share of tfr
# rcode sl27k

tally_share %>% 
  filter(level == "Expected number as fraction of TFR") %>% 
  filter(measure == "Child survival") %>% 
  filter(cohort %in% two_cohorts) %>% 
  group_by(region) %>% 
  arrange(region, cohort) %>% 
  mutate(change = value / lag(value)) %>% 
  ungroup() %>% 
  select(region, cohort, value, change)

# Difference between country with highest and lowest child death ----
# rcode fhd58

ecl_ctfr %>% 
  filter(type == "country") %>% 
  filter(cohort %in% two_cohorts) %>% 
  select(-region) %>% 
  mutate(share = 1 - (value / tfr)) %>% 
  group_by(cohort) %>% 
  arrange(share) %>% 
  slice(1, n()) %>% 
  mutate(diff = share / lag(share)) %>% 
  # slice(2) %>% 
  ungroup()

# B. NEW DRAGGED FROM SUP folder ----


# MOVE TO RESULTS FOLDER!!

# 1. Reduction in child death globally and regionally ----

cl_full <- merge(
  df_cl_m_full %>% 
    filter(type == "country")
  , female_births %>% 
    select(cohort = year, cohort_size = value, everything())
  , by = c('country', 'cohort')
  , all.x = T
)

# Get estimated values for countries

cl_countries <- 
  cl_full %>% 
  filter(type == 'country') %>% 
  mutate(
    region = as.character(region)
    , cohort = as.numeric(cohort)
    # , measure = "child_death"
  ) %>% 
  select(country, region, cohort, age, value) %>% 
  arrange(country, cohort, age)

# Get summary values (median and IQR) for world

cl_world <- 
  cl_countries %>% 
  group_by(age, cohort) %>%
  summarise(
    median = median(value)
    # , low_iqr = quantile(value, quant_low, na.rm = T)
    # , high_iqr = quantile(value, quant_high, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(source = sources[1]) %>% 
  arrange(cohort, age)

# Decline in frequency at all ages ----
# rcode gjs63j

cl_world %>% 
  filter(cohort %in% two_cohorts) %>% 
  mutate(cohort = plyr::mapvalues(cohort, from = two_cohorts, to = c("low", "high"))) %>% 
  select(cohort, age, median) %>% 
  pivot_wider(names_from = cohort, values_from = median) %>% 
  mutate(change = (high - low) / low ) %>% 
  na.omit() %>% 
  pull(change) %>% 
  mean()

# At age retirement_age
cl_world %>% 
  filter(age == retirement_age) %>%
  filter(cohort %in% two_cohorts) %>% 
  select(cohort, age, median) %>% 
  pivot_wider(names_from = cohort, values_from = median) %>% 
  mutate(change = (`2000` - `1950`) / `1950` + 1)

# By region 

cl_reg <- 
  cl_countries %>% 
  group_by(age, cohort, region) %>%
  summarise(
    median = median(value)
  ) %>% 
  ungroup() 

# Relative change

cl_reg %>% 
  filter(cohort %in% two_cohorts) %>% 
  select(region, cohort, age, median) %>% 
  pivot_wider(names_from = cohort, values_from = median) %>% 
  mutate(change = (`2000` - `1950`) / `1950` + 1) %>% 
  na.omit() %>% 
  group_by(region) %>% 
  summarise(mean = mean(change)) %>% 
  arrange(mean)

# Absolute change at retirement

cl_reg %>% 
  filter(cohort %in% two_cohorts) %>% 
  select(region, cohort, age, median) %>% 
  pivot_wider(names_from = cohort, values_from = median) %>% 
  mutate(change = `2000` - `1950`) %>%
  filter(age == retirement_age)

# Absolute change reproductive age

cl_reg %>% 
  filter(cohort %in% two_cohorts) %>% 
  select(region, cohort, age, median) %>% 
  pivot_wider(names_from = cohort, values_from = median) %>% 
  mutate(change = `2000` - `1950`) %>%
  filter(age == 50)

# Difference between region with highest and lowest child death

cl_reg %>% 
  filter(cohort %in% two_cohorts) %>% 
  group_by(cohort, age) %>% 
  arrange(median) %>% 
  slice(1, n()) %>% 
  mutate(diff = median / lag(median)) %>% 
  slice(2) %>% 
  ungroup() %>% 
  na.omit() %>% 
  # filter(age <= retirement_age) %>%
  filter(age == 50) %>%
  group_by(cohort) %>% 
  summarise(diff = mean(diff))

# Difference between country with highest and lowest child death ----
# rcode sldk65

cl_countries %>% 
  filter(cohort %in% two_cohorts) %>% 
  filter(age == retirement_age) %>%
  select(-age, -region) %>% 
  group_by(cohort) %>% 
  arrange(value) %>% 
  slice(1, n()) %>% 
  mutate(diff = value / lag(value)) %>% 
  # slice(2) %>% 
  ungroup()

# 2. First difference of chlid deaths ----

# 2.1. Reduction in reproductive and retirement age
# rcode ks324

# WORLD 

cl_world %>% 
  filter(cohort %in% two_cohorts) %>% 
  filter(age == 49) %>% 
  select(cohort, `49` = median) %>% 
  left_join(
    cl_world %>% 
      filter(cohort %in% two_cohorts) %>% 
      filter(age == 100) %>% 
      select(cohort, `100` = median)  
  ) %>% 
  left_join(
    cl_world %>% 
      filter(cohort %in% two_cohorts) %>% 
      filter(age == retirement_age - 1) %>% 
      select(cohort, retirement_low = median) 
  ) %>% 
  mutate(
    reproductive = `49`
    , retirement = `100` - retirement_low
  ) %>% 
  select(cohort, reproductive, retirement) %>% 
  # Distributino of deaths between age groups
  mutate(
    times = retirement / reproductive
    , share = reproductive / (reproductive + retirement)
  )  


# By region
# rcode l5763

cl_reg %>% 
  filter(cohort %in% two_cohorts) %>% 
  filter(age == 49) %>% 
  select(cohort, region, `49` = median) %>% 
  left_join(
    cl_reg %>% 
      filter(cohort %in% two_cohorts) %>% 
      filter(age == 100) %>% 
      select(cohort, region, `100` = median)  
    , by = c("cohort", "region")
  ) %>% 
  left_join(
    cl_reg %>% 
      filter(cohort %in% two_cohorts) %>% 
      filter(age == retirement_age - 1) %>% 
      select(cohort, region, retirement_low = median) 
    , by = c("cohort", "region")
  ) %>% 
  mutate(
    reproductive = `49`
    , retirement = `100` - retirement_low
  ) %>% 
  select(cohort, region, reproductive, retirement) %>% 
  # Distributino of deaths between age groups
  mutate(
    times = retirement / reproductive
    , share = reproductive / (reproductive + retirement)
  )  %>% 
  split(., .$cohort)


# Other (unused?) ----

# 1. Children surviving to women reaching retirement age 

share_died_reg %>% 
  filter(age == 65 & cohort == 2000) %>% 
  mutate(value = 1 - median)