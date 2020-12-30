#' Make start-end data with national values#'
#'
#' Data, start-end, with national values, double the size of the start-end data, where half will represent national data,
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make28_dataStartEnd_NatVal
#'
#' @param myInData RegDataStartEnd
#'
#' @return RegDataStartEnd_NatVal
#' @export
#'
#' @examples

fun2_5_RegDataStartEndNatVal <- function(myInData = RegDataStartEnd){
  # Duplicating the data RegData but exchange AvdRESH and SykehusNavn - that will be the data that represent the national results:
  NatValStartEnd <- RegDataStartEnd %>%
    mutate(AvdRESH.x ='99999', SykehusNavn.x='Nasjonal', AvdNavn.x='Nasjonal') #snational RESH-id set to99999 and name to "Nasjonal".

  # Attaching national data RegData (so that it become double the size with half having AvdRESH.x and SykehusNavn 99999 and national)
  RegDataStartEnd_NatVal <- rbind(RegDataStartEnd,NatValStartEnd)


  output <- RegDataStartEnd_NatVal
  return(invisible(output))
}
