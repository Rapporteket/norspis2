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

fun2_1_1_RegData_newVarGlobal <- function(myInData){

  # Hospital department names
  myInData$AvdNavn <- myInData$AvdRESH

  myInData$AvdNavn[myInData$AvdNavn==105806] <- 'Levanger (regional)'
  myInData$AvdNavn[myInData$AvdNavn==109979] <- 'Oslo (regional)'
  myInData$AvdNavn[myInData$AvdNavn==110361] <- 'Oslo, Gaustad (spes.pol.)'
  myInData$AvdNavn[myInData$AvdNavn==700698] <- 'Tromsø (regional, BU)'
  myInData$AvdNavn[myInData$AvdNavn==700821] <- 'Bodø (regional, V)'
  myInData$AvdNavn[myInData$AvdNavn==707383] <- 'Ski (DPS, pol.)'
  myInData$AvdNavn[myInData$AvdNavn==4204191] <-'Fredrikstad (Capio, døgn)'
  myInData$AvdNavn[myInData$AvdNavn==4207041] <- 'Ski (DPS, døgn)'
  myInData$AvdNavn[myInData$AvdNavn==4209009] <- 'Tønsberg (BUP, s.team)'
  myInData$AvdNavn[myInData$AvdNavn==4210562] <- 'Bodø (BUP, s.team)'
  myInData$AvdNavn[myInData$AvdNavn==4210626] <- 'Mosjøen (BUP)'
  myInData$AvdNavn[myInData$AvdNavn==4210825] <-  'Mosjøen (DPS)'
  myInData$AvdNavn[myInData$AvdNavn==107026] <- 'Bergen (regional)'
  myInData$AvdNavn[myInData$AvdNavn==4210303] <- 'Tr.heim (spes.pol.)'
  myInData$AvdNavn[myInData$AvdNavn==4207697] <- 'Stjørdal (DPS)'


  myInData
}
