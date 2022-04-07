#' Make RegData with a set of rows that represent national values
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make25_dataNatVal
#'
#' Making data with with national values.
#' * Double the size (vertically/number of rows) where half will represent national data.
#' * This allows for summarizing with dplyr and getting national values when we chage department name and ID to a national one.
#' indata can be RegData (there is an own function to make start-end data with national values)
#'
#'
#' @param myInData RegData
#'
#' @return myInData_NatVal (invisible)
#' @export
#'
#' @examples

fun2_3_RegDataNatVal <- function(myInData){
  # Duplicating the data myInData but exchange AvdRESH and SykehusNavn - that will be the data that represent the national results:
  NatVal <- myInData %>%
    dplyr::mutate(AvdRESH ='99999', SykehusNavn='Nasjonal', AvdNavn='Nasjonal') #set national RESH-id to 99999 and name to "Nasjonal"

  # Attaching national data myInData (so that it become double the size with half having AvdRESH.x and SykehusNavn 99999 and national)
  # rbind(myInData, NatVal)
  dplyr::bind_rows(myInData, NatVal)
}
