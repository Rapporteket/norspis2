
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
documents, that fit the norms of Norwegian medical quality registries.

## Installation

This package is made for use in Norwegian Quality Registry for Eating
Disorders (NorSpis), and has little utility outside the ecosystem of
Norwegian medical quality registries. The functions may be made to have
greater utility for others in the future, in particular other medical
quality registries.

So far the package contains functions to prepare:

  - five differently structured datasets (RegData, RegDataBeh,
    RegDataNatVal, RegDataStartEnd, RegDataStartEndNatVal)
  - three tables (DQ\_missing ,DQ\_regCount, and patChar)
  - two figures (one simple distribution figure and one to compare
    proportions at different hospital units).
  - The figures have functions for tables belonging to them (figTable),
    which must be run first and used as input to the figure functions.
  - four functions used to make maps with overview of units and tables
    to go along with the maps
  - …

Some functions belongs together. For instance the function make_figFig_unitCompar.R 
belongs to make_figTable_unitCompar.R in the following way: The function
make_figFig_unitCompar.R depends on make_figTable_unitCompar.R. One must first make
the table by running make_figTable_unitCompar., which can then put into 
make_figFig_unitCompar.R to make a figure for that table. 

A function for making parameterized reports for each hospital unit,
norspis2::make\_report(), in connection to a a .Rmd document,
inst/norspis-periodisk-rapport.Rmd, has also been made.

A document, recipe-implement-maps-in-R-package, describes how map
graphics were included in the norspis2 package, and can be found in the
folder norspis2/inst.

## Installation

You can install the released version/development version of norspis2
from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Rapporteket/norspis2")
```

## Example

<!-- This is a basic example which shows you how to solve a common problem: -->

<!-- ```{r example} -->

<!-- library(norspis2) -->

<!-- ## basic example code -->

<!-- ``` -->

<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->

<!-- ```{r cars} -->

<!-- summary(cars) -->

<!-- ``` -->

<!-- You may render `README.Rmd` manually, to keep `README.md` up-to-date. However, you may also leave this to the ci process at GitHub. -->

<!-- You can also embed plots, for example: -->

<!-- ```{r pressure, echo = FALSE} -->

<!-- plot(pressure) -->

<!-- ``` -->

<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub! -->

## Other useful information

## Working with development of the package locally
A script to load data, for development of the package locally, can be found in the NorSpisEkstra project 
under Rapporteket at GitHub.

## Annual report
The functions in the norspis2 package is used in NorSpis' annual report. An annual report .Rmd document
can be found in the NorSpisEkstra project under Rapporteket at GitHub. NorSpisEkstra also contain other 
documents used in the administration of NorSpis. 

## Registration types:

The variable regRegtype gives eight different types of registrations for the patients.
Four of these registrations types are start registrations, and four are end 
registrations. These are (value of regRegtype in parenthesis):

Start registrations:
   - Examination only ("kun utredning") - adults (value 1)
   - Examination only ("kun utredning") - children/adolescents (value 2)
   - Start - adults (value 3)
   - Start - children/adolescents (value 4)

End registrations
   - End - adults (value 5)
   - End - children/adolescents (value 6)
   - Discontinued - adults (value 98)
   - Discontinued - children/adolescents (value 99)

In addition, as of version 1.4 of the NorSpis registry, october 2021, NorSpis will
contain two more registrations types. These are follow-up ("oppfølging") registations one
year after ended treatment. 

  - Follow-up children/adolescents (value undecided)
  - Follow-up children/adolescents (value undecided) 

Note also that the "examination only" registration type is no longer in use in NorSpis. In 
addition, as of version 1.4 of the NorSpis registry, october 2021, the examination only 
registration types will no longer be used. But all registration types will still be 
kept in historical data, and must thus be taken into account in the code.

### Colors

Three colors from …?

  - ‘\#c6dbef’,
  - ‘\#2171b5’
  - ‘\#c6dbef’

**HEX codes for colors (from the Norwegian national profile handbook for
the specialist health care service):**

*Codes gotten directly from the handbook:*

  - \#003283 - dark blue (PMS 287)
  - \#81A9E1 - light blue (PMS 284)

*The following colors have been collected through color sampling from
the handbook and may be imprecise:* *Brown*

  - \#eacbaa - light brown (PMS 4665)
  - \#b68069 - dark brown (PMS 4715)
  - \#781d00 - reddish brown (PMS483)

*Red*

  - \#dd052b - red (PMS1795)

*Yellow*

  - \#f6e48c - light yellow (PMS 459)
  - \#fbba00 - yellow (PMS 130)

*Grey*

  - \#bbc2c6 - grey (PMS 7543)

*Green*

  - \#86b29e - green-grey (PMS 624)
  - \#4da32f - green (PMS 362)
  - \#00684b - dark green(PMS 342)
