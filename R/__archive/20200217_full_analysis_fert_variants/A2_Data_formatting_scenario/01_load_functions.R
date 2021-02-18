source("../functions.R")

# Data wrangling
wrangling <- c(
  "tidyverse", "tidyr", "reshape2"
  , "scales", "gridExtra", "ungroup"
  , "splines", "data.table", "parallel"
  , "readxl"
)

library2(wrangling)

# Please note that you will need an updated version of devtools in order to install this package
# For more info and troubleshoot, please see
# github.com/timriffe/DemoTools
# if(!require("DemoTools")) {
#   devtools::install_github("timriffe/DemoTools")
#   library(DemoTools)
# }