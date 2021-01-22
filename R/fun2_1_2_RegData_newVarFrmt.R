#' Make variables with new formats from old variables (Frmt refers to formatted)
#'
#' Output is data, with new var. which is similar to old, but with new format and "FRMT" suffix.
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make22_data_newVarFrmt
#'
#' @param myInData RegData
#'
#' @return RegData (invisible)
#' @export
#'
#' @examples

fun2_1_2_RegData_newVarFrmt <- function(myInData) {
  #Made/formatted:
  #HovedDato_FRMT
  #AvdodDato_FRMT
  #PasientAlder
  #EDEQ60GlobalScore
  #CIA30GlobalScore
  #MedBMI

  #Point of departure:
  # All imported variables are automatically imported as either characters or integers:
  # - Variables where all values of the variables are integers in.csv is automaticallt imported as integers.
  # - Variables with values other than integers in .csv file are imported as character variables.
  # - Variabels with integers and some "null" values in .csv file are imported as characters.

  #date formats (give the the suffix _FRMT just to make it easy to identify later, as working with dates often are subject to errors)
  myInData$HovedDato_FRMT <- as.Date(myInData$HovedDato)#,format="%d.%m.%Y") #HovedDato
  myInData$AvdodDato_FRMT <- as.Date(myInData$AvdodDato, format="%Y-%m-%d") #AvdodDato

  #numeric formats ("null" values will here become NA, and you will get a warning: "NAs intorduced by coercion")
  myInData$PasientAlder <- as.numeric(myInData$PasientAlder)
  myInData$EDEQ60GlobalScore <- as.numeric(myInData$EDEQ60GlobalScore)
  myInData$CIA30GlobalScore <- as.numeric(myInData$CIA30GlobalScore)
  myInData$MedBMI <- as.numeric(myInData$MedBMI)

  myInData$PasOppSikkerhet <- as.numeric(myInData$PasOppSikkerhet) #want similiar format as database

  myInData
}
