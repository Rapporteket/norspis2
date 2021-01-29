#' Filter function for start-end registrations (RegDataStartEnd):
#'
#' We use a separate filter function for start-end data. Star-data have the suffix x, and end-data have the suffix y.
#' We can thus for instance filter on dateFrom.x and dateTo.x, if we want to filter on dates of the start registrations,
#' or dateFrom.y and dateTo.y if we want to filter on dates of end registrations.
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make291_data_filtered
#'
#' @param RegDataStartEnd myInData
#' @param BasisRegStatus.x c(0,1,-1)
#' @param RegRegType.x c(0,1,2,3,4,5,6,98,99)
#' @param RegRegType.y c(5,6,98,99,NA)
#' @param dateFrom.x "2012-01-01"
#' @param dateTo.x "2100-12-31"
#' @param dateFrom.y "2012-01-01"
#' @param dateTo.y "2100-12-31"
#' @param ageFrom.x 0
#' @param ageTo.x 200
#' @param ageFrom.y 0
#' @param ageTo.y 200
#'
#' @return RegData_filtered
#' @export
#'
#' @examples

fun3_2_filter_RegDataStartEnd <- function(RegDataStartEnd = myInData,
                                  BasisRegStatus.x = c(0,1,-1),
                                  BasisRegStatus.y = c(0,1,-1,NA),
                                  RegRegType.x = c(0,1,2,3,4,5,6,98,99),
                                  RegRegType.y = c(5,6,98,99,NA),
                                  dateFrom.x = "2012-01-01",
                                  dateTo.x = "2100-12-31",
                                  dateFrom.y = "2012-01-01",
                                  dateTo.y = "2100-12-31",
                                  ageFrom.x = 0,
                                  ageTo.x = 200,
                                  ageFrom.y = 0,
                                  ageTo.y = 200){

  RegData_filtered <- RegDataStartEnd %>%
    filter(BasisRegStatus.x %in% BasisRegStatus.x) %>% #this var has no NAs
    filter(BasisRegStatus.y %in% BasisRegStatus.y) %>%
    filter(RegRegtype.x %in% RegRegType.x) %>%
    filter(RegRegtype.y %in% RegRegtype.y) %>% #this var has no NAs
    filter(HovedDato_FRMT.x >= dateFrom.x & HovedDato_FRMT.x <= dateTo.x) %>%#some dates are NA, and will observations will thus be reomved even if we filter on the lowest and highest available date.
    filter(HovedDato_FRMT.y >= dateFrom.y  & HovedDato_FRMT.y <= dateTo.y) %>%
    filter(PasientAlder.x >= ageFrom.x  &  PasientAlder.x <= ageTo.x) %>% #FIX data format du as. numeric, but earlier,not here
    filter(PasientAlder.y >= ageFrom.y  &  PasientAlder.y <= ageTo.y)

  output <- RegData_filtered
  return(invisible(output))
}
