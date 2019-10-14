(files <- list.files(pattern = ".R$"))

# for(f in files) file.edit(f)
# 
# Load functions and data
for(f in files[1:2]) source(f)

# Format fertility data
# "2.1_ungroup_fertility_ASFR.R"
# "2.2_convert_period_asfr_to_cohort_asfr.R"
for(f in files[3:4]) source(f)

# Format birth data
# 3._ungroup_births.R
for(f in files[5]) source(f)

# Format mortality data
# "4.1_ungroup_mortality_from_mx.R"
# "4.2_convert_period_LT_to_cohort_LT.R"
# "4.3_matrix_of_survival_probabilities.R"
for(f in files[6:8]) source(f)

# Session info ----

sessionInfo()

# R version 3.6.0 Patched (2019-06-11 r76697)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows Server >= 2012 x64 (build 9200)
# 
# Matrix products: default
# 
# locale:
#   [1] C
# 
# attached base packages:
#   [1] parallel  splines   stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] data.table_1.12.2  DemoTools_01.01.04 ungroup_1.1.1      gridExtra_2.3      scales_1.0.0       reshape2_1.4.3     forcats_0.4.0     
# [8] stringr_1.4.0      dplyr_0.8.2        purrr_0.3.2        readr_1.3.1        tidyr_0.8.3        tibble_2.1.3       ggplot2_3.2.0     
# [15] tidyverse_1.2.1   
