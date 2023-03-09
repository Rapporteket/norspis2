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

  myInData$AvdNavn[myInData$AvdNavn==105806] <- 'HNT: Reg.V.'
  # myInData$AvdNavn[myInData$AvdNavn==109979] <- 'OUS: Reg.'
  myInData$AvdNavn[myInData$AvdNavn==109979 & myInData$ForlopsType1Num %in% c(99,8,6,4,2)] <- 'OUS: Reg.BU.'
  myInData$AvdNavn[myInData$AvdNavn==109979 & myInData$ForlopsType1Num %in% c(98,1,3,5,7)] <- 'OUS: Reg.V.'
  myInData$AvdNavn[myInData$AvdNavn==110361] <- 'OUS: Spes.Pol.V.'
  myInData$AvdNavn[myInData$AvdNavn==700698] <- 'UNN: Reg.Døgn.BU.'
  myInData$AvdNavn[myInData$AvdNavn==700821] <- 'NLSH: Reg.V.'
  myInData$AvdNavn[myInData$AvdNavn==707383] <- 'AHUS: EFS Spes.Pol.V.'
  myInData$AvdNavn[myInData$AvdNavn==4204191] <-'CAPIO: Spes.Døgn.BU'
  myInData$AvdNavn[myInData$AvdNavn==4207041] <- 'AHUS: EFS Spes.Døgn.V.'
  myInData$AvdNavn[myInData$AvdNavn==4209009] <- 'SIV: Spes.Pol.BU.'
  myInData$AvdNavn[myInData$AvdNavn==4210562] <- 'NLSH: Spes.Pol.BU.'
  myInData$AvdNavn[myInData$AvdNavn==4210626] <- 'HSYK: Allm.Pol.BU.Mosjøen'
  myInData$AvdNavn[myInData$AvdNavn==4210825] <-  'HSYK: Allm.Pol.V.Mosjøen'
  myInData$AvdNavn[myInData$AvdNavn==107026] <- 'HB: Reg.V.'
  myInData$AvdNavn[myInData$AvdNavn==4210303] <- 'ST.OLAVS: Spes.Pol.V.'
  myInData$AvdNavn[myInData$AvdNavn==4207697] <- 'HNT: MHOBY.V.'
  myInData$AvdNavn[myInData$AvdNavn==4204275] <- 'SOHF: Spes.Døgn.V.'
  myInData$AvdNavn[myInData$AvdNavn==104083] <- 'SSHF: Spes.Døgn.V.'
  myInData$AvdNavn[myInData$AvdNavn==104364] <-'SIV: Spis.Pol.V.'
  myInData$AvdNavn[myInData$AvdNavn==4208300] <-'SIHF: Spes.Døgn.V.'
  myInData$AvdNavn[myInData$AvdNavn==4208548] <-'Diakonsyk: Allm.Pol.V.'
  myInData$AvdNavn[myInData$AvdNavn==102152] <-'HSYK: Allm.Pol.V. Sandnessjøen'
  myInData$AvdNavn[myInData$AvdNavn==102154]<-'HSYK: Allm.Pol.BU. Sandnessjøen'
  myInData$AvdNavn[myInData$AvdNavn==105008] <-'SSHF: Allm.Pol.V.'


  # Hospital department CATEGORY
  myInData$AvdKategori <- myInData$AvdRESH

  myInData$AvdKategori[myInData$AvdKategori==105806] <- 'Reg.'
  myInData$AvdKategori[myInData$AvdKategori==109979] <- 'Reg.'
  myInData$AvdKategori[myInData$AvdKategori==110361] <- 'Spes.Pol.'
  myInData$AvdKategori[myInData$AvdKategori==700698] <- 'Reg.Døgn.'
  myInData$AvdKategori[myInData$AvdKategori==700821] <- 'Reg.'
  myInData$AvdKategori[myInData$AvdKategori==707383] <- 'Spes.Pol.'
  myInData$AvdKategori[myInData$AvdKategori==4204191] <-'Spes.Døgn.'
  myInData$AvdKategori[myInData$AvdKategori==4207041] <- 'Spes.Døgn.'
  myInData$AvdKategori[myInData$AvdKategori==4209009] <- 'Spes.Pol.'
  myInData$AvdKategori[myInData$AvdKategori==4210562] <- 'Spes.Pol.'
  myInData$AvdKategori[myInData$AvdKategori==4210626] <- 'Allm.Pol.'
  myInData$AvdKategori[myInData$AvdKategori==4210825] <-  'Allm.Pol.'
  myInData$AvdKategori[myInData$AvdKategori==107026] <- 'Reg.'
  myInData$AvdKategori[myInData$AvdKategori==4210303] <- 'Spes.Pol.'
  myInData$AvdKategori[myInData$AvdKategori==4207697] <- 'Spes.Pol.'
  myInData$AvdKategori[myInData$AvdKategori==4204275] <- 'Spes.Døgn.'
  myInData$AvdKategori[myInData$AvdKategori==104083] <- 'Spes.Døgn.'
  myInData$AvdKategori[myInData$AvdKategori==104364] <-'Spes.Pol.'
  myInData$AvdKategori[myInData$AvdKategori==4208300] <-'Spes.Døgn.'
  myInData$AvdKategori[myInData$AvdKategori==4208548] <-'Allm.Pol.'
  myInData$AvdKategori[myInData$AvdKategori==102152] <- 'Allm.Pol.'
  myInData$AvdKategori[myInData$AvdKategori==102154] <-'Allm.Pol.'
  myInData$AvdKategori[myInData$AvdKategori==105008] <- 'Allm.Pol.'


  # Hospital department AGE CATEGORY
  myInData$AvdAlder <- myInData$AvdRESH

  myInData$AvdAlder[myInData$AvdAlder==105806] <- 'VOP'
  # myInData$AvdAlder[myInData$AvdAlder==109979] <- 'VOP'
  myInData$AvdAlder[myInData$AvdAlder==109979 & myInData$ForlopsType1Num %in% c(99,8,6,4,2)] <- 'BUP'
  myInData$AvdAlder[myInData$AvdAlder==109979 & myInData$ForlopsType1Num %in% c(98,1,3,5,7)] <- 'VOP'
  myInData$AvdAlder[myInData$AvdAlder==110361] <- 'VOP'
  myInData$AvdAlder[myInData$AvdAlder==700698] <- 'BUP'
  myInData$AvdAlder[myInData$AvdAlder==700821] <- 'VOP'
  myInData$AvdAlder[myInData$AvdAlder==707383] <- 'VOP'
  myInData$AvdAlder[myInData$AvdAlder==4204191] <-'BUP'
  myInData$AvdAlder[myInData$AvdAlder==4207041] <- 'VOP'
  myInData$AvdAlder[myInData$AvdAlder==4209009] <- 'BUP'
  myInData$AvdAlder[myInData$AvdAlder==4210562] <- 'BUP'
  myInData$AvdAlder[myInData$AvdAlder==4210626] <- 'BUP'
  myInData$AvdAlder[myInData$AvdAlder==4210825] <-  'VOP'
  myInData$AvdAlder[myInData$AvdAlder==107026] <- 'VOP'
  myInData$AvdAlder[myInData$AvdAlder==4210303] <- 'VOP'
  myInData$AvdAlder[myInData$AvdAlder==4207697] <- 'VOP'
  myInData$AvdAlder[myInData$AvdAlder==4204275] <- 'VOP'
  myInData$AvdAlder[myInData$AvdAlder==104083] <- 'VOP'
  myInData$AvdAlder[myInData$AvdAlder==104364] <-'VOP'
  myInData$AvdAlder[myInData$AvdAlder==4208300] <-'VOP'
  myInData$AvdAlder[myInData$AvdAlder==4208548] <-'VOP'
  myInData$AvdAlder[myInData$AvdAlder==102152] <- 'VOP'
  myInData$AvdAlder[myInData$AvdAlder==102154]<-'BUP'
  myInData$AvdAlder[myInData$AvdAlder==105008] <-'VOP'

  myInData
}
