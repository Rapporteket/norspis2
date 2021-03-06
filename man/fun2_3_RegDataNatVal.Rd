% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun2_3_RegDataNatVal.R
\name{fun2_3_RegDataNatVal}
\alias{fun2_3_RegDataNatVal}
\title{Make RegData with a set of rows that represent national values}
\usage{
fun2_3_RegDataNatVal(myInData)
}
\arguments{
\item{myInData}{RegData}
}
\value{
myInData_NatVal (invisible)
}
\description{
Note: This function's name when writing aarsrapport 2019 (in 2020) was: make25_dataNatVal
}
\details{
Making data with with national values.
* Double the size (vertically/number of rows) where half will represent national data.
* This allows for summarizing with dplyr and getting national values when we chage department name and ID to a national one.
indata can be RegData (there is an own function to make start-end data with national values)
}
