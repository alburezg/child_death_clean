
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Required data from WPP
# https://population.un.org/wpp/Download/Standard/CSV/
# 1. Fertility data
# 1.1. 2019 WPP ASFR (including observed and projected)
# 2. Abriged life tables 2019
# 2.1. LT women
# 2.2. LT both sexes 
# 3. Yearly births BOTH SEXES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Period fertility data ----
# UN fertility projections
# These are groups in 5-age groups for every 5 calendar-year interva
# 
# 1. Fertility data: 2019 WPP ASFR ----

asfr_obs <- readxl::read_xlsx(
  path ="../../Data/wpp_data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx"
  , sheet = "ESTIMATES"
  , skip=16
  ) %>% 
  filter(`Region, subregion, country or area *` %in% country_keep)

asfr_pred_medium <- readxl::read_xlsx(
  path ="../../Data/wpp_data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx"
  , sheet = "MEDIUM VARIANT"
  , skip=16
) %>% 
  filter(`Region, subregion, country or area *` %in% country_keep)

asfr_pred_low <- readxl::read_xlsx(
  path ="../../Data/wpp_data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx"
  , sheet = "LOW VARIANT"
  , skip=16
) %>% 
  filter(`Region, subregion, country or area *` %in% country_keep)

asfr_pred_high <- readxl::read_xlsx(
  path ="../../Data/wpp_data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx"
  , sheet = "HIGH VARIANT"
  , skip=16
) %>% 
  filter(`Region, subregion, country or area *` %in% country_keep)

# Constant fertility

# Constant fert measnt that 2015-baseline are repeated every year
years_project <- unique(asfr_pred_high$Period)
times_years <- length(years_project)

asfr_baseline <- 
  asfr_obs %>% 
  filter(Period == baseline_year_constant_rates)

# Now, repeat this times:

asfr_constant <- 
  asfr_baseline[ rep(row.names(asfr_baseline), times_years), ] %>% 
  # arrange(country) %>% 
  group_by(`Region, subregion, country or area *`) %>% 
  mutate(Period = years_project) %>% 
  ungroup() %>% 
  arrange(`Region, subregion, country or area *`, Period)


print("WPP 2019 ASFR data loaded (low, med, high variants)")

# 2. Abriged life tables 2019 ----

# 2.1. LT both sexes  ====

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mort_variants <- c("Median PI", "Upper 80 PI", "Lower 80 PI", "Upper 95 PI", "Lower 95 PI")

lt_per_obs_B <- readxl::read_xlsx(
  path= "../../Data/wpp_data/WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx"
  , sheet = "ESTIMATES"
  , skip = 16
)

lt_per_obs_B[lt_per_obs_B == "…"] <- NA

# Change column names of life tables

old_names <- colnames(lt_per_obs_B)

new_names <- c("id", "variant", "country" , "notes", "country_code", "type", "parent_code",
               "year", "age", "interval"
               , "mx", "qx", "px", "lx", "dx", "Lx", "Sx", "Tx", "ex", "ax")

# Change colnames
colnames(lt_per_obs_B) <- new_names

lt_per_obs_B2 <-
  lt_per_obs_B %>%
  filter(country %in% country_keep) %>%
  mutate(
    mx = as.numeric(mx)
    , qx = as.numeric(qx)
    , px = as.numeric(px)
    , lx = as.numeric(lx)
    , dx = as.numeric(dx)
    , Lx = as.numeric(Lx)
    , Sx = as.numeric(Sx)
    , Tx = as.numeric(Tx)
    , ex = as.numeric(ex)
    , ax = as.numeric(ax)
  ) %>%
  select(country, variant, year, age, interval, mx, qx, ax, lx, dx, Lx, Tx, ex)

# This large dataset needs to be downloaded from the interweb:

if(!file.exists("../../Data/wpp_data/WPP2019_Life_Table_OtherVariants.csv")){
  download.file(
    "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_Life_Table_OtherVariants.csv"
    , "../../Data/wpp_data/WPP2019_Life_Table_OtherVariants.csv"
    )
}

lt_per_pred_B <-
  data.table::fread("../../Data/wpp_data/WPP2019_Life_Table_OtherVariants.csv", stringsAsFactors = F) %>%
  data.frame() %>%
  filter(Sex == "Total") %>%
  filter(Location %in% country_keep) %>%
  select(
    country = Location, variant = Variant
    , year = Time, age = AgeGrpStart, interval = AgeGrpSpan
    , mx, qx, ax, lx, dx, Lx, Tx, ex
  )

# Create full dfs with historical and projected by type of scenario

lt_median_B <-
  bind_rows(
    lt_per_obs_B2
    , lt_per_pred_B %>%
      filter(variant == "Median PI")
  ) %>%
  mutate(variant = "Median PI")

lt_upper95_B <-
  bind_rows(
    lt_per_obs_B2
    , lt_per_pred_B %>%
      filter(variant == "Upper 95 PI")
  )  %>%
  mutate(variant = "Upper 95 PI")

lt_lower95_B <-
  bind_rows(
    lt_per_obs_B2
    , lt_per_pred_B %>%
      filter(variant == "Lower 95 PI")
  )  %>%
  mutate(variant = "Lower 95 PI")

# Constant fert measnt that baseline are repeated every year

years_project <- unique(lt_per_pred_B$year)
times_years <- length(years_project)
times_new_years <- length(unique(lt_per_obs_B2$age))

new_years <- sort(rep(years_project, times_new_years))

lt_baseline <-
  lt_per_obs_B2 %>%
  filter(year == baseline_year_constant_rates)

# Now, repeat this times:

lt_expanded <-
  lt_baseline[ rep(row.names(lt_baseline), times_years), ] %>%
  group_by(country) %>%
  mutate(year = new_years) %>%
  ungroup()

lt_constant_B <-
  bind_rows(
    lt_per_obs_B2
    , lt_expanded
  ) %>%
  mutate(variant = "constant")


print("WPP data loaded")
#*********************************