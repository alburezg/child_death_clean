
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
