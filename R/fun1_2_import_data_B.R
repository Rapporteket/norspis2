#' Import data with information about treatmeant "tiltak" (this also is available in _num format, but we import the non-"num" .csv)
#'
#' B refers to "behandling"
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: import12_dataBeh )
#'
#' Use this function like this:
#' RegData <- fun2_import_data_B()
#'
#' @return a tibble with the data (invisible)
#' @export
#'
#' @examples

fun1_2_import_data_B <- function(){

  output <- norspis::queryBehandlingNum("norspis") %>% tibble::as_tibble()

  return(invisible(output))

}
