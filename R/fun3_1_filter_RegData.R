#' Filter function for RegData (NOT start-end data):
#'
#' This function is also used to filter RegData with national values (NatVal) (as long as they are NOT start-end data)
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make29_data_filtered
#'
#' @param RegData myInData
#' @param regStatus c(0,1,-1)
#' @param regType c(0,1,2,3,4,5,6,98,99)
#' @param dateFrom "2012-01-01",
#' @param dateTo "2100-12-31"
#' @param ageFrom 0
#' @param ageTo 200
#'
#' @return RegData_filtered
#' @export
#'
#' @examples

fun3_1_filter_RegData <- function(RegData = myInData, #OBS this function does not work with StartEndData as the date filter is named with a suffix ,HovedDato_FRMT.x there
                                 regStatus = c(0,1,-1),
                                 regType = c(0,1,2,3,4,5,6,98,99),
                                 dateFrom = "2012-01-01",
                                 dateTo = "2100-12-31",
                                 ageFrom = 0,
                                 ageTo = 200,
                                 BUP=99){

  RegData_filtered <- RegData %>%
    filter(BasisRegStatus %in% regStatus) %>% #this var has no NAs
    filter(RegRegtype %in% regType) %>% #this var has no NAs
    filter(HovedDato_FRMT >= dateFrom  &  HovedDato_FRMT <= dateTo) %>% #some dates are NA, and will observations will thus be reomved even if we filter on the lowest and highest available date.
    filter(as.numeric(PasientAlder) >= ageFrom  &  as.numeric(PasientAlder) <= ageTo) #FIX format on data earlier, not here

  if (BUP %in% c(0, 1)) {
    if (BUP == 1) {
      RegData_filtered <- RegData_filtered %>%
        filter(ForlopsType1Num %in% c(99, 8, 6, 4, 2))
    } else {
      RegData_filtered <- RegData_filtered %>%
        filter(ForlopsType1Num %in% c(98, 1,3, 5, 7))
    }
  }


  output <- RegData_filtered
  return(invisible(output))
}

#```
