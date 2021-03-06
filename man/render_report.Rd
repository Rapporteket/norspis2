% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render_report.R
\name{render_report}
\alias{render_report}
\title{render_report in norspis2}
\usage{
render_report(reshID = 700821)
}
\arguments{
\item{reshID}{}
}
\value{

}
\description{
This function allows you to render a parameterized report in norspis2, as
described in R Markdown: The Definive Guide by Yihui Xie.
}
\details{
The most important usecase is to quickly make one report for each hospital
department that use NorSpis. This can be implemented in a Shiny application,
so that either a defined user role prints all reports and/or the respective
hospital department logs in to retrieve their own report and/or with a
function that sends the report(s) to each hospital department at chosen
points in time.
}
