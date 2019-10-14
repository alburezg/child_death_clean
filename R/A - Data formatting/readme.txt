Note [20181014]:

If possible, the R scripts to re-format UN WPP data should be preferably run on a High Performance Computing (HPC) unit
as involve many calculations and would take many hours in a normal PC, but only about 2 hours on a HPC running 
on 25 cores. 

All the formatted data produced by the scripts in these folders is saved to "../../Data/derived" and is loaded
by the scripts in the folder "../2 - Analysis"

UN WPP data can be downloaded from:
https://population.un.org/wpp/Download/Standard/CSV/

The scripts were ran on a Windows server:

> sessionInfo()
R version 3.6.0 Patched (2019-06-11 r76697)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows Server >= 2012 x64 (build 9200)

Matrix products: default

locale:
[1] C

attached base packages:
[1] parallel  splines   stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] data.table_1.12.2  DemoTools_01.01.04 ungroup_1.1.1      gridExtra_2.3      scales_1.0.0       reshape2_1.4.3     forcats_0.4.0     
 [8] stringr_1.4.0      dplyr_0.8.2        purrr_0.3.2        readr_1.3.1        tidyr_0.8.3        tibble_2.1.3       ggplot2_3.2.0     
[15] tidyverse_1.2.1   


> detectCores()
[1] 144
