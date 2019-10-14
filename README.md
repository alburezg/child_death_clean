# Reproducible analysis

*Last updated: 14 October 2019*

Scripts to reproduce figures and tables of paper "Women's experience of child death: a global demographic perspective".

## Structure

The folder `R` has the following folders, which are needed to transform the data to the right format, produce the model estimates
, and produce the figures and tables, respectively. 

A - Data formatting

B - Analysis

C - Results

UN World Population Prospect data was downloaded by hand before running the scripts in 'A - Data formatting' and stored in the folder
Data/wpp_data. All data come from: https://population.un.org/wpp/Download/, downloaded on 14 October 2019. See the *Supporting Information*
for more details about the data and estimation.

In order to run the code, please download this repository as a zip file, extract it and run in R.
The scripts in all three folders needs to be executed in order for the figures and tables to be produced and stored 
in the folder Output.

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
```