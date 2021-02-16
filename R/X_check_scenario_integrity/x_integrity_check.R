
# Compare 'variant' estiamtes to old code --------

# OK: Cohort ASFR ======

var <- 
  ASFRC_medium %>% 
  filter(country == "guatemala") %>% 
  select(Cohort, Age, var = ASFR)

ASFRC <- read.csv(file = paste0("../../Data/derived/","ASFRC.csv"), stringsAsFactors = F)

old <-
  ASFRC %>% 
  filter(country == "guatemala") %>% 
  select(Cohort, Age, old = ASFR)


  var %>% 
  filter(Cohort %in% 1950:2000) %>% 
  left_join(old) %>% 
  mutate(
    same = old - var < 1e-6
    ) %>% 
  filter(!same)

# If this returns an empty table, then both methods are equivalent

# LTC_var <-   read.csv(paste0("../../Data/derived/LTC_","Median PI", ".csv"), stringsAsFactors = F)
  
# OK: Period aggregated female life table ===========

# This is the 'new' estimates
lt_var <- 
  lt_median %>% 
  filter(country == "Guatemala") %>% 
  select(year, age, var = mx)


# These are the 'old' estimates

lt_per_obs_F <- readxl::read_xlsx(
  path= "../../Data/wpp_data/WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.xlsx"
  , sheet = "ESTIMATES"
  , skip = 16
)

# Predicted values for 2020-2050
lt_per_pred1_F <- readxl::read_xlsx(
  path= "../../Data/wpp_data/WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.xlsx"
  , sheet = "MEDIUM 2020-2050"
  , skip = 16
)

# Predicted values for 2050-2100
lt_per_pred2_F <- readxl::read_xlsx(
  path= "../../Data/wpp_data/WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.xlsx"
  , sheet = "MEDIUM 2050-2100"
  , skip = 16
)

lt_old_temp <-
  bind_rows(
  lt_per_obs_F
  , lt_per_pred1_F
  , lt_per_pred2_F
) 

new_names <- c("id", "variant", "country" , "notes", "country_code", "type", "parent_code",
               "year", "age", "interval"
               , "mx", "qx", "px", "lx", "dx", "Lx", "Sx", "Tx", "ex", "ax")

# Change colnames
colnames(lt_old_temp) <- new_names

lt_old <- 
  lt_old_temp %>% 
  filter(country == "Guatemala") %>% 
  mutate(mx = as.numeric(mx)) %>% 
  select(year, age, old = mx)

# Compare 

left_join(lt_var, lt_old) %>% 
  mutate(
    same = old - var < 1e-6
  ) %>% 
  filter(!same)  

# Discussion:
# There are some differnces in period lifetables but they 
# largely the same

# OK: compare cohort 1x1 LT -------

# Get new variant-based lt

lt_per = lt_median

variant_current <- unique(lt_per$variant)

lt_1_1 <- 
  ungroup_mortality_from_mx_robust(
    lt_per = lt_per
    , sex = "F"
    , parallel = T
    , numCores = numCores
    , export = F
  )

# Convert to cohort
LTC_var_temp <- 
  convert_period_LT_to_cohort_LT_robust(
    lt_1_1 = lt_1_1
    , sex = "F"
    , export = F
    , years = 1950:2100
    , ages = 1:100
    , parallel = T
    , numCores = numCores
  ) %>% 
  mutate(variant = "Median PI")

LTC_var <- 
  LTC_var_temp %>% 
  select(Cohort, Age, var = mx)

# Get old estiamtes
LTC_old <- 
  data.table::fread(file = paste0("../../Data/derived/","LTCF.csv"), stringsAsFactors = F) %>% 
  data.frame %>% 
  filter(Country == "guatemala") %>% 
  select(Cohort, Age, old = mx)

# Compare 

left_join(LTC_var, LTC_old) %>% 
  mutate(
    same = old - var < 1e-6
  ) %>% 
  filter(!same) 

# They are the same!

# FAIL! Compare lx_arr ========

y_small <- 1950

# This must be the same as the age groups in the asfr data
cos <- c(1950:2100) # cohorts
# cos <- c(2000:2001) # cohorts
xs <- c(15:49) # reproductive age
mas<-c(15:100) # mother ages

# Scenario-based

worker_survival_probs_robust(life_table = LTC_var_temp, cos = cos, xs = xs, mas = mas)
file_var <- paste0(paste0("../../Data/derived", "/lx.kids.arr_", "GTM","_","Median PI", ".RDS"))

# worker_survival_probs(LTC_var_temp, cos = cos, xs = xs, mas = mas)
# file_var <- paste0(paste0("../../Data/derived", "/lx.kids.arr_", "GTM",".RDS"))

lx.kids.arr_var <- readRDS(file_var)
lx_array_var <- lx.kids.arr_var[ , , paste(y_small)]


# Old one (main paper)
file <- paste0(paste0("../../Data/derived", "/lx.kids.arr_", "guatemala",".RDS"))
lx.kids.arr_old <- readRDS(file)
lx_array_old <- lx.kids.arr_old[1:35 , , paste(y_small)]

# Mean absolute difference at woman's age 15

lapply(1:35, function(a){
  mean(lx_array_var[a,] - lx_array_old[a,], na.rm = T)  
}) %>% 
  unlist() %>% 
  plot()

# DIAGNOSIS: something weird is going on with the function that creates the 
# lx.kids.arr