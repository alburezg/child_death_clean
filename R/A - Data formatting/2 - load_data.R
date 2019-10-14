
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
# list.files("../../Data/CSVProjectionFiles/")
list.files("../../Data/wpp_data/")

# 1. Fertility data: 2019 WPP ASFR ----

asfr_obs <- readxl::read_xlsx(
  path ="../../Data/wpp_data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx"
  , sheet = "ESTIMATES"
  , skip=16
  )

asfr_pred_medium <- readxl::read_xlsx(
  path ="../../Data/wpp_data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx"
  , sheet = "MEDIUM VARIANT"
  , skip=16
)

asfr_wpp <- bind_rows(
  asfr_obs
  , asfr_pred_medium
)

print("WPP 2019 ASFR data loaded")

# 2. Abriged life tables 2019 ----

# 2.1. LT women ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

lt_per_F <- bind_rows(
  lt_per_obs_F
  , lt_per_pred1_F
  , lt_per_pred2_F
)

rm(  "lt_per_obs_F" , "lt_per_pred1_F", "lt_per_pred2_F")

# 2.2. LT both sexes  ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

lt_per_B <- bind_rows(
  lt_per_obs_B
  , lt_per_pred1_B
  , lt_per_pred2_B
)

rm("lt_per_obs_B" , "lt_per_pred1_B", "lt_per_pred2_B")

# 3. Yearly births BOTH SEXES ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

births_obs_B <- readxl::read_xlsx(
  path= "../../Data/wpp_data/WPP2019_FERT_F01_BIRTHS_BOTH_SEXES.xlsx"
  , sheet = "ESTIMATES"
  , skip = 16
)

# Predicted values for 2020-2100
births_pred1_B <- readxl::read_xlsx(
  path= "../../Data/wpp_data/WPP2019_FERT_F01_BIRTHS_BOTH_SEXES.xlsx"
  , sheet = "MEDIUM VARIANT"
  , skip = 16
)

# 4. Sex ratio at birh ----

sex_ratio_at_birth_obs <- readxl::read_xlsx(
  path= "../../Data/wpp_data/WPP2019_FERT_F02_SEX_RATIO_AT_BIRTH.xlsx"
  , sheet = "ESTIMATES"
  , skip = 16
)

# Predicted values for 2020-2100
sex_ratio_at_birth_pred1 <- readxl::read_xlsx(
  path= "../../Data/wpp_data/WPP2019_FERT_F02_SEX_RATIO_AT_BIRTH.xlsx"
  , sheet = "MEDIUM VARIANT"
  , skip = 16
)

# 5. Population totals ----

# popMF<-read.csv(file="../../Data/CSVProjectionFiles/popMF.csv", skip=3, header=TRUE)

print("WPP data loaded")
#*********************************