# 
# # MOVE TO RESULTS FOLDER!!
# 
# # 1. Reduction in child death globally and regionally ----
# 
# cl_world <- 
#   cl_countries %>% 
#   group_by(age, cohort) %>%
#   summarise(
#     median = median(value)
#     , low_iqr = quantile(value, quant_low, na.rm = T)
#     , high_iqr = quantile(value, quant_high, na.rm = T)
#   ) %>% 
#   ungroup() %>% 
#   mutate(source = sources[1]) %>% 
#   arrange(cohort, age)
# 
# # Decline in frequency at all ages
# cl_world %>% 
#   # filter(age == 70) %>% 
#   filter(cohort %in% c(1950, 2000)) %>% 
#   select(cohort, age, median) %>% 
#   pivot_wider(names_from = cohort, values_from = median) %>% 
#   mutate(change = (`2000` - `1950`) / `1950` + 1) %>% 
#   na.omit() %>% 
#   pull(change) %>% 
#   mean()
# 
# # At age 70
# cl_world %>% 
#   filter(age == 70) %>%
#   filter(cohort %in% c(1950, 2000)) %>% 
#   select(cohort, age, median) %>% 
#   pivot_wider(names_from = cohort, values_from = median) %>% 
#   mutate(change = (`2000` - `1950`) / `1950` + 1)
# 
# # By region 
# 
#   cl_reg <- 
#     cl_countries %>% 
#   group_by(age, cohort, region) %>%
#   summarise(
#     median = median(value)
#   ) %>% 
#   ungroup() 
# 
#   # Relative change
#     
#   cl_reg %>% 
#     filter(cohort %in% c(1950, 2000)) %>% 
#     select(region, cohort, age, median) %>% 
#     pivot_wider(names_from = cohort, values_from = median) %>% 
#     mutate(change = (`2000` - `1950`) / `1950` + 1) %>% 
#     na.omit() %>% 
#     group_by(region) %>% 
#     summarise(mean = mean(change)) %>% 
#     arrange(mean)
#   
#   # Absolute change at retirement
#   
#   cl_reg %>% 
#     filter(cohort %in% c(1950, 2000)) %>% 
#     select(region, cohort, age, median) %>% 
#     pivot_wider(names_from = cohort, values_from = median) %>% 
#     mutate(change = `2000` - `1950`) %>%
#     filter(age == 70)
#   
#   # Absolute change reproductive age
# 
#     cl_reg %>% 
#     filter(cohort %in% c(1950, 2000)) %>% 
#     select(region, cohort, age, median) %>% 
#     pivot_wider(names_from = cohort, values_from = median) %>% 
#     mutate(change = `2000` - `1950`) %>%
#     filter(age == 50)
#   
# # Difference between region with highest and lowest child death
#     
# cl_reg %>% 
#   filter(cohort %in% c(1950, 2000)) %>% 
#   group_by(cohort, age) %>% 
#   arrange(median) %>% 
#   slice(1, n()) %>% 
#   mutate(diff = median / lag(median)) %>% 
#   slice(2) %>% 
#   ungroup() %>% 
#   na.omit() %>% 
#   # filter(age <= 70) %>%
#   filter(age == 50) %>%
#   group_by(cohort) %>% 
#   summarise(diff = mean(diff))
# 
# # Difference between country with highest and lowest child death
# 
# cl_countries %>% 
#   filter(cohort %in% c(1950, 2000)) %>% 
#   # filter(age == 50) %>%
#   filter(age == 70) %>%
#   select(-age, -region) %>% 
#   group_by(cohort) %>% 
#   arrange(value) %>% 
#   slice(1, n()) %>% 
#   mutate(diff = value / lag(value)) %>% 
#   # slice(2) %>% 
#   ungroup()
# 
# # 2. First difference of chlid deaths ----
# 
# # 2.1. Reduction in reproductive and retirement age
# 
#   cl_world %>% 
#   filter(cohort %in% c(1950, 2000)) %>% 
#   filter(age == 49) %>% 
#   select(cohort, `49` = median) %>% 
#   left_join(
#     cl_world %>% 
#       filter(cohort %in% c(1950, 2000)) %>% 
#       filter(age == 100) %>% 
#       select(cohort, `100` = median)  
#   ) %>% 
#   left_join(
#     cl_world %>% 
#       filter(cohort %in% c(1950, 2000)) %>% 
#       filter(age == 69) %>% 
#       select(cohort, `69` = median) 
#   ) %>% 
#   mutate(
#     reproductive = `49`
#     , retirement = `100` - `69`
#   ) %>% 
#   select(cohort, reproductive, retirement) %>% 
# # Distributino of deaths between age groups
#    mutate(
#      times = retirement / reproductive
#      , share = reproductive / (reproductive + retirement)
#      )  
# 
# 
# # By region
# 
#   cl_reg %>% 
#   filter(cohort %in% c(1950, 2000)) %>% 
#   filter(age == 49) %>% 
#   select(cohort, region, `49` = median) %>% 
#   left_join(
#     cl_reg %>% 
#       filter(cohort %in% c(1950, 2000)) %>% 
#       filter(age == 100) %>% 
#       select(cohort, region, `100` = median)  
#     , by = c("cohort", "region")
#   ) %>% 
#   left_join(
#     cl_reg %>% 
#       filter(cohort %in% c(1950, 2000)) %>% 
#       filter(age == 69) %>% 
#       select(cohort, region, `69` = median) 
#     , by = c("cohort", "region")
#   ) %>% 
#   mutate(
#     reproductive = `49`
#     , retirement = `100` - `69`
#   ) %>% 
#   select(cohort, region, reproductive, retirement) %>% 
#   # Distributino of deaths between age groups
#   mutate(
#     times = retirement / reproductive
#     , share = reproductive / (reproductive + retirement)
#   )  %>% 
#     split(., .$cohort)
