% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun1_2_import_data_B.R
\name{fun1_2_import_data_B}
\alias{fun1_2_import_data_B}
\title{Import data with information about treatmeant "tiltak" (this also is available in _num format, but we import the non-"num" .csv)}
\usage{
fun1_2_import_data_B(
  path_data = "F:/2020-28-12_data-til-utvikling/",
  date_data = "2020-12-28"
)
}
\arguments{
\item{path_data}{path of you data (B - named NorSpis_BehandlingNum_datadump_DATE)}

\item{date_data}{date on the format "2020-12-31" (the names of your data files will end in with date in current download set up from Rapporteket)}
}
\value{
a tibble with the data (invisible)
}
\description{
B refers to "behandling"
}
\details{
Note: This function's name when writing aarsrapport 2019 (in 2020) was: import12_dataBeh )

Use this function like this:
RegData <- fun2_import_data_B()
}
