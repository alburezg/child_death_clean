
print("Running script: 5 - ungroup_births.R")

# We require the size of annual female birth cohorts by UN region.
# However, the WPP only reports the number of births (both sexes) in 5-year bands.
# To estimate the yearly number of female births (ie "female birth cohort size"):
#  a. multiply the 5-year number of births by the sex ratio at birth, and
#  b. ungroup to year-specific estimates using linear interpolation.

# visual_examination <- F
export <- T

# 1.1. Low variant  ==================

ungroup_births(
  births_obs_B = births_obs_B
  , births_pred_B = births_low_B
  , sex_ratio_5 = sex_ratio_at_birth_low
  , variant_name = "low"
  , export = export
)

# 1.2. Medium variant ==================

ungroup_births(
  births_obs_B = births_obs_B
  , births_pred_B = births_medium_B
  , sex_ratio_5 = sex_ratio_at_birth_medium
  , variant_name = "medium"
  , export = export
)

# 1.3. HIGH variant ==================

ungroup_births(
  births_obs_B = births_obs_B
  , births_pred_B = births_high_B
  , sex_ratio_5 = sex_ratio_at_birth_high
  , variant_name = "high"
  , export = export
)

print("Size of female and total birth cohorts saved as 1x1 csv file to ../../Data/derived/wpp_female_births_1_1.csv")
