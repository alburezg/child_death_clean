
# Solution:
# There were severeal errors in the code:
# a. It used female life tables

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
    same = old - var < 1e-4
    ) %>% 
  filter(Cohort == 2000)
  filter(!same)

# If this returns an empty table, then both methods are equivalent

# LTC_var <-   read.csv(paste0("../../Data/derived/LTC_","Median PI", ".csv"), stringsAsFactors = F)
  
# OK: Period aggregated female life table ===========

# This is the 'new' estimates
lt_var <- 
  lt_median_B %>% 
  filter(country == "Guatemala") %>% 
  select(year, age, var = mx)


# These are the 'old' estimates

lt_per_obs_B <- readxl::read_xlsx(
  path= "../../Data/wpp_data/WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx"
  , sheet = "ESTIMATES"
  , skip = 16
)

# Predicted values for 2020-2050
lt_per_pred1_B <- readxl::read_xlsx(
  path= "../../Data/wpp_data/WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx"
  , sheet = "MEDIUM 2020-2050"
  , skip = 16
)

# Predicted values for 2050-2100
lt_per_pred2_B <- readxl::read_xlsx(
  path= "../../Data/wpp_data/WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx"
  , sheet = "MEDIUM 2050-2100"
  , skip = 16
)

lt_old_temp <-
  bind_rows(
  lt_per_obs_B
  , lt_per_pred1_B
  , lt_per_pred2_B
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

lt_per = lt_median_B

variant_current <- unique(lt_per$variant)

lt_1_1 <- 
  ungroup_mortality_from_mx_robust(
    lt_per = lt_per
    , sex = "Total"
    , parallel = T
    , numCores = numCores
    , export = F
  )

# Convert to cohort
LTC_var_temp <- 
  convert_period_LT_to_cohort_LT_robust(
    lt_1_1 = lt_1_1
    , sex = "Both"
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
LTC_old_temp <- 
  data.table::fread(file = paste0("../../Data/derived/","LTCB.csv"), stringsAsFactors = F) %>% 
  data.frame %>% 
  filter(Country == "guatemala")

LTC_old <- 
  LTC_old_temp %>% 
  select(Cohort, Age, old = mx)

# Compare 

left_join(LTC_var, LTC_old) %>% 
  mutate(
    same = old - var < 1e-6
  ) %>% 
  filter(!same) 

# They are the same!
# Expect for age 100

# OK: Compare lx_arr ========

y_small <- 2000

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

# OK:  Re-do lx.kids.arr using same function --------
# Re-do lx.kids.arr from 'variant' data using the
# same code

# life_table = LTC_old_temp
temp_fun <- function(life_table, xs, mas, cos) {
  # browser()
  
  pais <- unique(life_table$Country)
  life_table$Country <- NULL
  
  # Create array of 2x2 matrices
  # one matrix for every cohort
  # each matrix has rows = xs and cols = mas
  
  lx.kids.arr <- array(
    NA
    , dim=c(length(xs), length(mas), length(cos))
    , dimnames=list(paste(xs), paste(mas), paste(cos))
  )
  
  for (co in cos){ 
    # For every column
    for (ma in mas){
      lx.kids <- c()
      # for every row
      for (x in xs){
        tmp <- c()
        
        # This is the important bit: 
        # It only applies for cases where ma >= x
        
        # Get the survival probability for a child given:
        child_age <- ma - x # because 
        child_cohort <- co + x
        
        tmp <- life_table$lx[life_table$Age == child_age & life_table$Cohort == child_cohort]
        
        if (length(tmp) > 0){
          lx.kids<-c(lx.kids, tmp)
        }
        
      }
      if (length(lx.kids)>0)
        lx.kids.arr[1:length(lx.kids),paste(ma),paste(co)] <- lx.kids
    }
  }
  
  lx.kids.arr
  
}

lx_var_temp <- temp_fun(life_table = LTC_var_temp, xs = xs, mas = mas, cos = cos)

lx_old_temp <- temp_fun(life_table = LTC_old_temp, xs = xs, mas = mas, cos = cos)

# y_keep <- seq(1950, 2000, 10)
y_keep <- c(1950, 1975 ,2000)

lx_var <- lx_var_temp[,,paste(y_keep)]
lx_old <- lx_old_temp[,,paste(y_keep)]

dim(lx_var)
dim(lx_old)

# Compare to each other: they are identical
lapply(1:35, function(a){
  value <- colMeans(lx_var[a,,] - lx_old[a,,], na.rm = T) 
    data.frame(
      value = value
      , cohort = names(value)
      , a = a
    ) 
}) %>% 
  bind_rows() %>% 
  ggplot(aes(x = a, y = value, colour = cohort, group = cohort)) +
  geom_line()


# Diagnosis: they are identical if estimated using the same procedure. 
# Therefore, the difference between the lx arrays that I observed
# originated in the A_Data_formatting scripts
# To confirm this, let's compare the 'old' estimates
# using the lx arrays that I had saved before and the
# ones that I just recomputed in this script using the same method:

lx_old_old <- lx.kids.arr_old[1:35 , , paste(y_keep)]

# Compare to old version other: they are identical
lapply(1:35, function(a){
  value <- colMeans(lx_old[a,,] - lx_old_old[a,,], na.rm = T) 
  data.frame(
    value = value
    , cohort = names(value)
    , a = a
  ) 
}) %>% 
  bind_rows() %>% 
  ggplot(aes(x = a, y = value, colour = cohort, group = cohort)) +
  geom_line()

# Solution: 
# See why the lx array in A_Data_format is different

# OK: child loss -----------

cl_old_temp <- readRDS(paste0("../../Data/estimates/df_cl_m_1950to1999_15to100.RDS"))

cl_old <- 
  cl_old_temp %>% 
  filter(
    country == "guatemala"
    # , cohort == 2000, age == 100
  ) %>% 
  select(cohort, age, old = value)

cl_var_temp <- readRDS("../../Data/estimates/df_cl_m_1950to1999_15to100_fert_medium_mort_Median PI.RDS")

cl_var <- 
  cl_var_temp %>% 
  filter(
    country == "guatemala"
    # , cohort == 2000, age == 100
  ) %>% 
  select(cohort, age, var = value)

left_join(cl_var, cl_old) %>% 
  mutate(
    same = old - var < 1e-6
  ) %>% 
  filter(!same) 

#  dfgnsdgd

cl_robust %>% 
  filter(variant_fert == "medium") %>% 
  filter(variant_mort == "medium") %>% 
  filter(cohort == 2000)

eclc[,'2000']



x <- eclc %>% 
  reshape2::melt(id = c("age")) %>% 
  dplyr::mutate(
    variable = as.character(variable)
    , value = as.numeric(value)
    , country = c
    , variant_fert = variant_fert
    , variant_mort = variant_mort
  )


x %>% 
  filter(variable == 2000)

ASFRSC %>% 
  filter(Cohort == co) 
  filter(Age <= ma) 

  ASFR_df %>% 
    filter(Cohort == co) 
  
  
  cl_robust %>% 
    filter(variant_fert == "stable/constant") %>% 
    filter(variant_mort == "stable/constant") %>% 
    filter(cohort == 2000)
  