#' render_report in norspis2
#'
#' This function allows you to render a parameterized report in norspis2, as
#' described in R Markdown: The Definive Guide by Yihui Xie.
#'
#' The most important usecase is to quickly make one report for each hospital
#' department that use NorSpis. This can be implemented in a Shiny application,
#' so that either a defined user role prints all reports and/or the respective
#' hospital department logs in to retrieve their own report and/or with a
#' function that sends the report(s) to each hospital department at chosen
#' points in time.
#'
#'
#' @param reshID
#'
#' @return
#' @export
#'
#' @examples

render_report = function(reshID = 700821 ){

    #random file name suffix to make sure saving the file is possible (make
    #sure that the file name do not already exist):
    random_suffix <- paste0(sample(c(0:9, LETTERS[1:6]), 6, T), collapse = '')

  rmarkdown::render(
    "inst/norspis-periodisk-rapport.Rmd",
    params = list(
      reshID = reshID
    ),
    output_file = paste0("Rapport-for-enhet",
                         reshID,
                         "-",
                         "DATO",
                         "-",
                         random_suffix,
                         ".pdf")
  )

}
