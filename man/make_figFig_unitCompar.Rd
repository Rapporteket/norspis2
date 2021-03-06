% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_figFig_unitCompar.R
\name{make_figFig_unitCompar}
\alias{make_figFig_unitCompar}
\title{Figure with comparisons of hospital units}
\usage{
make_figFig_unitCompar(
  my_proptable_hospitals,
  my_y_lab = "Andel (\%)",
  my_title = "",
  YellowGoal = "mean",
  GreenGoal = "mean"
)
}
\arguments{
\item{my_proptable_hospitals}{my_proptable_hospitals <- RegData_NatVal_summarized #with NATIONA vulues!}

\item{my_y_lab}{"Andel", unless you change it}

\item{my_title}{'' - empty, unless you change it}

\item{YellowGoal}{can be give the values 1)'' 2)'mean' or 3) A character value which will be converted to na , for instance '60' if the goal value is 60 percent}

\item{GreenGoal}{can be give the values 1)'' 2)'mean' or 3) A character value which will be converted to na , for instance '60' if the goal value is 60 percent}
}
\value{
figure
}
\description{
Note: This function's name when writing aarsrapport 2019 (in 2020) was: make_compfig_hospitals
}
