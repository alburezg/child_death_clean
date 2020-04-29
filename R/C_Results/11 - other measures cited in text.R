
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
  
  # print(paste("Objects saved in global envir: un_reg, regions_short, regions_long"))
  
}


# 1. CHildren surviving to women reaching retirement age

share_died_reg %>% 
  filter(age == 65 & cohort == 2000) %>% 
  mutate(value = 1 - median)

# 2. Relative change in size of birth cohorts

# For whole world
female_births %>%
  filter(year %in% c(1950, 2000)) %>% 
  find_regions(ignore_regions = F) %>% 
  group_by(cohort = year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup %>% 
  pivot_wider(names_from = cohort, values_from = value) %>% 
  mutate(x = `2000` / `1950` )

# 5. Share of offspring alive at retirement age ----

# By region

df %>% 
  filter(age == 70) %>% 
  filter(cohort %in% c(1950, 2000)) %>% 
  mutate(value = (1 - value)*100) %>% 
  select(region, cohort, value) %>% 
  pivot_wider(names_from = cohort, values_from = value)

# By country

country_keep <- c(
  # "zambia", "australia", "republic of korea"
  # , "cambodia", "vietnam", "thailand"
  "bolivia (plurinational state of)", "argentina", "paraguay", "chile", "brazil", "paraguay"
)

share_died  %>% 
  filter(age == 70) %>% 
  filter(cohort %in% c(1950, 2000)) %>% 
  filter(country %in% country_keep) %>% 
  mutate(value = (1 - value)*100) %>% 
  select(country, cohort, value) %>% 
  pivot_wider(names_from = cohort, values_from = value)

# 3. Share of women surviving to retirement age 


# 4. Generational Burden for the whole world ----

# burden_world <- 
sum_burden %>% 
  group_by(cohort) %>% 
  summarise(
    value = sum(value) / 1e6
  ) %>% 
  ungroup %>% 
  filter(cohort %in% c(1950, 2000))

95.4/160

# 4. Burden of CD for young and old women ----

# ie for tos in reproductive and retirement age

rep_age <- 15:49
ret_age <- 70:99

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
  filter(cohort %in% c(1950, 2000)) %>% 
  pivot_wider(names_from = agegr, values_from = value) %>% 
  mutate(
    share_rep = reproductive / (retirement + reproductive + other) * 100
    , share_ret = retirement / (retirement + reproductive + other) * 100
    , share_sum = share_rep + share_ret
    , share_more_in_rep = share_ret/share_rep
  ) %>% 
  select(cohort, starts_with("share"))

