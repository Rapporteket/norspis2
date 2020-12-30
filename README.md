
<!-- README.md is generated from README.Rmd. Please edit that file -->
norspis2
========

<!-- badges: start -->
<!-- badges: end -->
The goal of norspis2 is to make standardized plots, tables and documents, that fits the norms of Norwegian medical quality registries.

Installation
------------

This package is made for use in Norwegian Quality Registry for Eating Disorders (NorSpis), and has little utility outside the ecosystem of Norwegian medical quality registries. Still we aim to make the functions so that is should be possible for anyone to input standardized anonymous/summary data frames to the plot and table functions, and get plots and tables on our standardized format.

So far the package contains functions to prepare:

-   five differently structured datasets (RegData, RegDataBeh, RegDataNatVal, RegDataStartEnd, RegDataStartEndNatVal)
-   three tables (DQ\_missing ,DQ\_regCount, and patChar)
-   two figures (one simple distribution figure and one two compare proportions at different hospital units).
-   The figures have functions for tables belonging to them (figTable), which must be run first and used as input to the figure functions.

You can install the released version/development version of norspis2 from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Rapporteket/norspis2")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(norspis2)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub!
