
<!-- README.md is generated from README.Rmd. Please edit that file -->

# norspis2

<!-- badges: start -->

[![Version](https://img.shields.io/github/v/release/rapporteket/norspis2?sort=semver)](https://github.com/rapporteket/norspis2/releases)
[![R build
status](https://github.com/Rapporteket/norspis2/workflows/R-CMD-check/badge.svg)](https://github.com/Rapporteket/norspis2/actions)
[![codecov.io](https://codecov.io/github/Rapporteket/norspis2/norspis2.svg?branch=main)](https://codecov.io/github/Rapporteket/norspis2?branch=main)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Doc](https://img.shields.io/badge/Doc--grey.svg)](https://rapporteket.github.io/norspis2/)
<!-- badges: end -->

The goal of norspis2 is to make standardized plots, tables and
documents, that fits the norms of Norwegian medical quality registries.

## Installation

This package is made for use in Norwegian Quality Registry for Eating
Disorders (NorSpis), and has little utility outside the ecosystem of
Norwegian medical quality registries. Still we aim to make the functions
so that is should be possible for anyone to input standardized
anonymous/summary data frames to the plot and table functions, and get
plots and tables on our standardized format.

So far the package contains functions to prepare:

  - five differently structured datasets (RegData, RegDataBeh,
    RegDataNatVal, RegDataStartEnd, RegDataStartEndNatVal)
  - three tables (DQ\_missing ,DQ\_regCount, and patChar)
  - two figures (one simple distribution figure and one to compare
    proportions at different hospital units).
  - The figures have functions for tables belonging to them (figTable),
    which must be run first and used as input to the figure functions.

Preliminary example that works (but only locally at NLSH)

``` r
library(dplyr)

RegData <- norspis2::fun1_1_import_data_FEA()
RegData <- norspis2::fun2_1_1_RegData_newVarGlobal()
RegData <- norspis2::fun2_1_2_RegData_newVarFrmt()
RegData <- norspis2::fun2_1_3_RegData_newVarMAsNA()
RegData <- norspis2::fun2_1_4_RegData_newVarDich()
  

RegDataNatVal <- norspis2::fun2_3_RegDataNatVal()

tab <- norspis2::make_figTable_unitCompar(RegDataNatVal, rlang::quo(PROP_PO10Pasientsikkerhet))
fig <- norspis2::make_figFig_unitCompar(tab)
fig
```

You can install the released version/development version of norspis2
from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Rapporteket/norspis2")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(norspis2)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

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

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
