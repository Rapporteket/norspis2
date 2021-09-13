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

  # Hospital department NAMES
  myInData$AvdNavn <- myInData$AvdRESH

  myInData$AvdNavn[myInData$AvdNavn==105806] <- 'Levanger (regional)'
  myInData$AvdNavn[myInData$AvdNavn==109979] <- 'Oslo (regional)'
  myInData$AvdNavn[myInData$AvdNavn==110361] <- 'Oslo, Gaustad (spes.pol.)'
  myInData$AvdNavn[myInData$AvdNavn==700698] <- 'Tromso (regional, BU)'
  myInData$AvdNavn[myInData$AvdNavn==700821] <- 'Bodo (regional, V)'
  myInData$AvdNavn[myInData$AvdNavn==707383] <- 'Ski (DPS, pol.)'
  myInData$AvdNavn[myInData$AvdNavn==4204191] <-'Fredrikstad (Capio, dogn)'
  myInData$AvdNavn[myInData$AvdNavn==4207041] <- 'Ski (DPS, dogn)'
  myInData$AvdNavn[myInData$AvdNavn==4209009] <- 'Tonsberg (BUP, s.team)'
  myInData$AvdNavn[myInData$AvdNavn==4210562] <- 'Bodo (BUP, s.team)'
  myInData$AvdNavn[myInData$AvdNavn==4210626] <- 'Mosjoen (BUP)'
  myInData$AvdNavn[myInData$AvdNavn==4210825] <-  'Mosjoen (DPS)'
  myInData$AvdNavn[myInData$AvdNavn==107026] <- 'Bergen (regional)'
  myInData$AvdNavn[myInData$AvdNavn==4210303] <- 'Tr.heim (spes.pol.)'
  myInData$AvdNavn[myInData$AvdNavn==4207697] <- 'Stjordal (DPS)'
  myInData$AvdNavn[myInData$AvdNavn==4204275] <- 'Halden/Sarpsborg (DPS)'
  myInData$AvdNavn[myInData$AvdNavn==104083] <- 'Arendal (dogn)'

  # Hospital department CATEGORY
  myInData$AvdKategori <- myInData$AvdRESH

  myInData$AvdKategori[myInData$AvdKategori==105806] <- 'regional'
  myInData$AvdKategori[myInData$AvdKategori==109979] <- 'regional'
  myInData$AvdKategori[myInData$AvdKategori==110361] <- 'special_outpatient'
  myInData$AvdKategori[myInData$AvdKategori==700698] <- 'regional'
  myInData$AvdKategori[myInData$AvdKategori==700821] <- 'regional'
  myInData$AvdKategori[myInData$AvdKategori==707383] <- 'outpatient'
  myInData$AvdKategori[myInData$AvdKategori==4204191] <-'inpatient'
  myInData$AvdKategori[myInData$AvdKategori==4207041] <- 'inpatient'
  myInData$AvdKategori[myInData$AvdKategori==4209009] <- 'outpatient'
  myInData$AvdKategori[myInData$AvdKategori==4210562] <- 'outpatient'
  myInData$AvdKategori[myInData$AvdKategori==4210626] <- 'outpatient'
  myInData$AvdKategori[myInData$AvdKategori==4210825] <-  'outpatient'
  myInData$AvdKategori[myInData$AvdKategori==107026] <- 'regional'
  myInData$AvdKategori[myInData$AvdKategori==4210303] <- 'spec_outpaitent'
  myInData$AvdKategori[myInData$AvdKategori==4207697] <- 'outpatient'
  myInData$AvdNavn[myInData$AvdNavn==4204275] <- 'inpatient'
  myInData$AvdNavn[myInData$AvdNavn==104083] <- 'inpatient'

  # Hospital department AGE CATEGORY
  myInData$AvdAlder <- myInData$AvdRESH

  myInData$AvdAlder[myInData$AvdAlder==105806] <- 'adult'
  myInData$AvdAlder[myInData$AvdAlder==109979] <- 'adult'
  myInData$AvdAlder[myInData$AvdAlder==110361] <- 'adult'
  myInData$AvdAlder[myInData$AvdAlder==700698] <- 'children'
  myInData$AvdAlder[myInData$AvdAlder==700821] <- 'adult'
  myInData$AvdAlder[myInData$AvdAlder==707383] <- 'adult'
  myInData$AvdAlder[myInData$AvdAlder==4204191] <-'children'
  myInData$AvdAlder[myInData$AvdAlder==4207041] <- 'adult'
  myInData$AvdAlder[myInData$AvdAlder==4209009] <- 'children'
  myInData$AvdAlder[myInData$AvdAlder==4210562] <- 'children'
  myInData$AvdAlder[myInData$AvdAlder==4210626] <- 'children'
  myInData$AvdAlder[myInData$AvdAlder==4210825] <-  'adult'
  myInData$AvdAlder[myInData$AvdAlder==107026] <- 'adult'
  myInData$AvdAlder[myInData$AvdAlder==4210303] <- 'adult'
  myInData$AvdAlder[myInData$AvdAlder==4207697] <- 'adult'
  myInData$AvdNavn[myInData$AvdNavn==4204275] <- 'adult'
  myInData$AvdNavn[myInData$AvdNavn==104083] <- 'adult'

  myInData
}
