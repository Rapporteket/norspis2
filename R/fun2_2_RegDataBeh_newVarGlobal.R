#' Make RegDatabeh with new global variables (Global refers to that the varaibel(s) made her will be used globally/often)
#'
#' fun2_1_1.. must be run first.
#'
#' @param myInData1 RegData
#' @param myInData2 RegDataBeh
#'
#' @return
#' @export
#'
#' @examples

fun2_2_RegDataBeh_newVarGlobal <- function(myInData1=RegData, myInData2=RegDataBeh){

  #Join:
  RegDataBeh <- dplyr::left_join(RegDataBeh, RegData[, c("AvdNavn", "HovedDato", "ForlopsID")], by="ForlopsID")

  output <- RegDataBeh
  return(invisible(output))
}
