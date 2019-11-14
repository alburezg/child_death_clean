# Reproducible analysis

Scripts to reproduce figures and tables of paper "Women's experience of child death over the life course: a global demographic perspective".

## A - Structure

The directory `R` has four sub-directories, which are needed to transform the data to the right format, produce the model estimates
, produce the figures and tables from the main text, and create the resources for the SI Appendix. They are:

[A_Data_formatting](R/A_Data_formatting)

[B_Analysis](R/B_Analysis)

[C_Results](R/C_Results)

[D_Supplementary](R/D_Supplementary)

Each directory in `R` has the same structure, which includes:

1. An R project prefaced by `___` (e.g. `___A_Data_formatting.Rproj`)
1. A script called `__run_code.R` which should be used to execute all scripts in the directory
1. Several R scripts ordered numerically.
 
 
## B - Runing the scripts to reproduce the results

In order to run the code, please do the following:

1. Download this repository as a zip file (aprox 150MB) and extract its content
1. Open the `R/A_Data_formatting` directory in order to start with the data wrangling
1. Open the `___A_Data_formatting.Rproj` R project in RStudio
1. Open the `__run_code.R` script and run the code line by line to execute all the scripts in the directory
1. When finished, exit RStudio
1. Repeat for the other three directories in the `R` directory.

The final results of the analysis are stored in the `Output` directory, including the figures as .pdf files and full country and regional estimates.

## C - Data availability

This repository already includes all the raw data needed to reproduce the results. UN World Population Prospect data was downloaded by hand in advance and stored in the
[Data/wpp_data](Data/wpp_data) directory. All data come from: [https://population.un.org/wpp/Download/](https://population.un.org/wpp/Download/), downloaded on 14 October 2019. 
See the *Supporting Information* for more details about the data and estimation.  

## D - A note on parallelisation

If possible, the R scripts in the [A_Data_formatting](R/A_Data_formatting) directory (used to re-format the UN WPP data) should be run on a High Performance Computing (HPC) unit
as they involve many calculations. These would take many hours in a normal PC, but under one hour on a HPC running 25 cores. 

We ran the scripts on a Windows server:

`> sessionInfo()`

```
R version 3.6.0 Patched (2019-06-11 r76697)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows Server >= 2012 x64 (build 9200)

Matrix products: default

locale:
[1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                   
[5] LC_TIME=German_Germany.1252    

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] directlabels_2018.05.22 gridExtra_2.3           scales_1.0.0            reshape2_1.4.3          data.table_1.12.2       forcats_0.4.0          
 [7] stringr_1.4.0           dplyr_0.8.2             purrr_0.3.2             readr_1.3.1             tidyr_0.8.3             tibble_2.1.3           
[13] ggplot2_3.2.0           tidyverse_1.2.1        

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.1       plyr_1.8.4       cellranger_1.1.0 pillar_1.4.2     compiler_3.6.0   tools_3.6.0      digest_0.6.19    jsonlite_1.6    
 [9] lubridate_1.7.4  gtable_0.3.0     nlme_3.1-140     lattice_0.20-38  pkgconfig_2.0.2  rlang_0.4.0      cli_1.1.0        rstudioapi_0.10 
[17] yaml_2.2.0       haven_2.1.0      withr_2.1.2      xml2_1.2.0       httr_1.4.0       generics_0.0.2   hms_0.4.2        grid_3.6.0      
[25] tidyselect_0.2.5 glue_1.3.1       R6_2.4.0         readxl_1.3.1     modelr_0.1.4     magrittr_1.5     backports_1.1.4  rvest_0.3.4     
[33] assertthat_0.2.1 colorspace_1.4-1 labeling_0.3     quadprog_1.5-7   stringi_1.4.3    lazyeval_0.2.2   munsell_0.5.0    broom_0.5.2     
[41] crayon_1.3.4    
```