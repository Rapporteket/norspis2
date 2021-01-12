#' Make RegDatabeh with new global variables (Global refers to that the varaibel(s) made her will be used globally/often)
#'
#' fun2_1_1.. must be run first.
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make211_dataBeh_newVarGlobal
#'
#' @param myInData1 RegData
#' @param myInData2 RegDataBeh
#'
#' @return output (invisible)
#' @export
#'
#' @examples

fun2_2_RegDataBeh_newVarGlobal <- function(myInData1, myInData2){

  #Join:
  RegDataBeh <- dplyr::left_join(myInData2, myInData1[, c("AvdNavn", "HovedDato", "ForlopsID")], by="ForlopsID")

  output <- RegDataBeh
  return(invisible(output))
}
