
# DS6306 - Analyzing US Craft Beers and Breweries - Case Study
# Author(s): Ahmad Salama & Almuhannad Qneis

# Data Description
# 
# The Beers dataset contains a list of 2410 US craft beers and the Breweries dataset contains 558 US breweries. 
# The datasets descriptions are as follows.
# 
# Beers Dataset:
#
# ./data/Beers.csv:
# 
# Name: Name of the beer.
# 
# Beer_ID: Unique identifier of the beer.
# 
# ABV: Alcohol by volume of the beer.
# 
# IBU: International Bitterness Units of the beer.
# 
# Brewery_ID: Brewery id associated with the beer.
# 
# Style: Style of the beer.
# 
# Ounces: Ounces of beer.
# 
# Breweries Dataset:
#
# ./data/Breweries.csv:
# 
# Brew_ID: Unique identifier of the brewery.
# 
# Name: Name of the brewery.
# 
# City: The city where the brewery is located.
# 
# State: U.S. State where the brewery is located.

# files directory: ./data
# rmarkdown file: ./rmarkdown.Rmd
# R Script file: ./fearlesss_ds6306_eda.R

# Session Info:
# R version 4.3.1 (2023-06-16 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19045)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
# [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.utf8    
# 
# time zone: America/Los_Angeles
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#  [1] caret_6.0-94    lattice_0.21-8  class_7.3-22    usdata_0.2.0    maps_3.4.1      ggthemes_4.2.4  GGally_2.1.2   
#  [8] lubridate_1.9.2 forcats_1.0.0   stringr_1.5.0   dplyr_1.1.2     purrr_1.0.2     readr_2.1.4     tidyr_1.3.0    
# [15] tibble_3.2.1    ggplot2_3.4.3   tidyverse_2.0.0
# 
# loaded via a namespace (and not attached):
#  [1] gtable_0.3.4         recipes_1.0.8        tzdb_0.4.0           vctrs_0.6.3          tools_4.3.1         
#  [6] generics_0.1.3       stats4_4.3.1         parallel_4.3.1       fansi_1.0.4          ModelMetrics_1.2.2.2
# [11] pkgconfig_2.0.3      Matrix_1.5-4.1       data.table_1.14.8    RColorBrewer_1.1-3   lifecycle_1.0.3     
# [16] farver_2.1.1         compiler_4.3.1       munsell_0.5.0        codetools_0.2-19     prodlim_2023.08.28  
# [21] crayon_1.5.2         pillar_1.9.0         MASS_7.3-60          gower_1.0.1          iterators_1.0.14    
# [26] rpart_4.1.19         foreach_1.5.2        parallelly_1.36.0    nlme_3.1-162         lava_1.7.2.1        
# [31] tidyselect_1.2.0     digest_0.6.33        stringi_1.7.12       future_1.33.0        reshape2_1.4.4      
# [36] listenv_0.9.0        labeling_0.4.2       splines_4.3.1        grid_4.3.1           colorspace_2.1-0    
# [41] cli_3.6.1            magrittr_2.0.3       survival_3.5-5       utf8_1.2.3           future.apply_1.11.0 
# [46] withr_2.5.0          scales_1.2.1         timechange_0.2.0     globals_0.16.2       nnet_7.3-19         
# [51] timeDate_4022.108    hms_1.1.3            hardhat_1.3.0        rlang_1.1.1          Rcpp_1.0.11         
# [56] glue_1.6.2           pROC_1.18.4          ipred_0.9-14         rstudioapi_0.15.0    reshape_0.8.9       
# [61] R6_2.5.1 