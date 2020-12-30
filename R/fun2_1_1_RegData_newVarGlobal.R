#' Make variable with hospital department names (Global refers to that the varaibel(s) made her will be used globally/often)
#'
#' @param myInData  RegData
#'
#' Output is data, with new var. that will be used extensively (like AvdNavn which is name of hospital unit)
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make21_data_newVarGlobal
#'
#' @return RegData (invisible)
#' @export
#'
#' @examples

fun2_1_1_RegData_newVarGlobal <- function(myInData=RegData){

  # Hospital department names
  RegData$AvdNavn <- RegData$AvdRESH

  RegData$AvdNavn[RegData$AvdNavn==105806] <- 'Levanger (regional)'
  RegData$AvdNavn[RegData$AvdNavn==109979] <- 'Oslo (regional)'
  RegData$AvdNavn[RegData$AvdNavn==110361] <- 'Oslo, Gaustad (spes.pol.)'
  RegData$AvdNavn[RegData$AvdNavn==700698] <- 'Troms? (regional, BU)'
  RegData$AvdNavn[RegData$AvdNavn==700821] <- 'Bod? (regional, V)'
  RegData$AvdNavn[RegData$AvdNavn==707383] <- 'Ski (DPS, pol.)'
  RegData$AvdNavn[RegData$AvdNavn==4204191] <-'Fredrikstad (Capio, d?gn)'
  RegData$AvdNavn[RegData$AvdNavn==4207041] <- 'Ski (DPS, d?gn)'
  RegData$AvdNavn[RegData$AvdNavn==4209009] <- 'T?nsberg (BUP, s.team)'
  RegData$AvdNavn[RegData$AvdNavn==4210562] <- 'Bod? (BUP, s.team)'
  RegData$AvdNavn[RegData$AvdNavn==4210626] <- 'Mosj?en (BUP)'
  RegData$AvdNavn[RegData$AvdNavn==4210825] <-  'Mosj?en (DPS)'
  RegData$AvdNavn[RegData$AvdNavn==107026] <- 'Bergen (regional)'
  RegData$AvdNavn[RegData$AvdNavn==4210303] <- 'Tr.heim (spes.pol.)'


  output <- RegData
  return(invisible(output))
}
