source("../functions.R")

# Data wrangling
wrangling <- c(
  "tidyverse", "tidyr", "reshape2"
  , "scales", "gridExtra", "ungroup"
  , "splines", "data.table", "parallel"
  , "readxl"
)

library2(wrangling)

if(!require("DemoTools")) {
  devtools::install_github("timriffe/DemoTools")
  library(DemoTools)
}