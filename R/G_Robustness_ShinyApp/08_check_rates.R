
age_keep <- 15:21
cohort_keep <- 1995:2005

# Fertility ------------------

ASFRC_low %>% 
  filter(country == "guatemala") %>% 
  filter(Cohort %in% cohort_keep) %>% 
  filter(Age %in% age_keep) %>% 
  rename(low = ASFR) %>% 
  left_join(
    ASFRC_medium %>% 
      filter(country == "guatemala") %>% 
      filter(Cohort %in% cohort_keep) %>% 
      filter(Age %in% age_keep) %>% 
      rename(medium = ASFR)
    , by = c("country", "Cohort", "Age")
  ) %>% 
  left_join(
    ASFRC_constant %>% 
      filter(country == "guatemala") %>% 
      filter(Cohort %in% cohort_keep) %>% 
      filter(Age %in% age_keep) %>% 
      rename(constant = ASFR)
    , by = c("country", "Cohort", "Age")
  ) %>% 
  mutate(
    same = (low == medium) & (medium == constant)
    ) %>% 
  filter(!same) %>% 
  group_by(Cohort) %>% 
  slice(1) %>% 
  select(-country) 


# Survival array ------------------

cohort_keep <- 1950:2000

lx_low <-
  get_lx_array_robust(
  c = "guatemala"
  , reference_years = cohort_keep
  , sex_keep = F
  , path = "../../Data/derived"
  , variant_mort = "Lower 95 PI"
)[ 1, , ] %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  pivot_longer(-rowname, values_to = "low")

lx_medium <- get_lx_array_robust(
  c = "guatemala"
  , reference_years = cohort_keep
  , sex_keep = F
  , path = "../../Data/derived"
  , variant_mort = "Median PI"
)[ 1, , ] %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  pivot_longer(-rowname, values_to = "medium")

lx_high <- get_lx_array_robust(
  c = "guatemala"
  , reference_years = cohort_keep
  , sex_keep = F
  , path = "../../Data/derived"
  , variant_mort = "Upper 95 PI"
)[ 1, , ] %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  pivot_longer(-rowname, values_to = "high")

lx_low %>% 
  left_join(lx_medium) %>% 
  left_join(lx_high) %>% 
  mutate(
    same = (low == medium) & (medium == high)
  ) %>% 
  filter(!same) %>% 
  group_by(name) %>%
  slice(1)  

# Mortality ---------------

method = "spline"

# Get LTC

LTC_low <- 
  ungroup_mortality_from_mx_robust(
    lt_per = lt_lower95 %>% filter(country %in% "Guatemala")
    , sex = "F"
    , parallel = T
    , numCores = numCores
    , export = F
  ) %>% 
  # Convert to cohort
  convert_period_LT_to_cohort_LT_robust(
    lt_1_1 = .
    , sex = "F"
    , export = F
    , years = 1950:2100
    , ages = 1:100
    , parallel = T
    , numCores = numCores
  ) %>% 
  mutate(variant = "low")

LTC_medium <- 
  ungroup_mortality_from_mx_robust(
    lt_per = lt_median %>% filter(country %in% "Guatemala")
    , sex = "F"
    , parallel = T
    , numCores = numCores
    , export = F
  ) %>% 
# Convert to cohort
  convert_period_LT_to_cohort_LT_robust(
    lt_1_1 = .
    , sex = "F"
    , export = F
    , years = 1950:2100
    , ages = 1:100
    , parallel = T
    , numCores = numCores
  ) %>% 
  mutate(variant = "medium")

LTC_high <- 
  ungroup_mortality_from_mx_robust(
    lt_per = lt_upper95 %>% filter(country %in% "Guatemala")
    , sex = "F"
    , parallel = T
    , numCores = numCores
    , export = F
  ) %>% 
  # Convert to cohort
  convert_period_LT_to_cohort_LT_robust(
    lt_1_1 = .
    , sex = "F"
    , export = F
    , years = 1950:2100
    , ages = 1:100
    , parallel = T
    , numCores = numCores
  ) %>% 
  mutate(variant = "high")

# Find difference ===============

# df <- 
  bind_cols(
  LTC_low %>% select(cohort = Cohort, age = Age, low = mx)
  , LTC_medium %>% select(medium = mx)
  , LTC_high %>% select(high = mx)
    ) %>% 
  filter(cohort %in% cohort_keep) %>% 
  mutate(
    same = (abs(low-medium) < 1e-5) & (abs(high-medium) < 1e-5)
  ) %>% 
  mutate(period = cohort + age) %>% 
  filter(period == 2010)

  filter(!same) %>% 
  group_by(cohort) %>% 
  slice(1) 
  


