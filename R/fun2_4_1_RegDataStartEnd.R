#' Make start-end data:
#'
#' * We still want one line per patient, and thus move some of the variables from the final registration to the patients start registration.
#' * The End registrations will appear twice in this data. Once as in the normal data and in addition added to the start registrations as new variables
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make26_dataStartEnd()
#'
#' @param myInData RegData
#'
#' @return RegDataStartEnd
#' @export
#'
#' @examples

fun2_4_1_RegDataStartEnd <- function(myInData){

  #Transform to tibble
  RegData <- as_tibble(myInData)

  # Make the primary data set:
  ## Make one dataset with only the end registrations
  RegDataEnd <- RegData[which(RegData$RegRegtype %in% c(5,6,98,99)), ]
  #note: some observations have RegRegType == 0, which is because these registrations are not yet fully made

  ## Join the full data with the end registratons, to get one dataset with on line per patient, with all scores from start and end  registrations
  ## Make new columm called id, in each dataset to merge/join by:
  RegData$id <- RegData$ForlopsID
  RegDataEnd$id <- as.numeric(RegDataEnd$RegTilhorendeStartReg)
  ##Join:
  RegDataStartEnd <- dplyr::full_join(RegData, RegDataEnd, by = "id")#OBS -all combinations of multiple matches are returned: For every end registration that is connected to one and the same start registration (which should not happen, but still does happen because people register/choose wrong start registration to connect the end registrations to) - additional   start-registrations are made, because of how we choose to do this merge.
  #we remove multiple matches. Of the multiple matches, we keep the matches with the oldest end date (HovedDato_FRMT.y) (by use of arrange first):
  RegDataStartEnd <- RegDataStartEnd %>%
    arrange(ForlopsID.x, HovedDato_FRMT.y) %>%# Together with distinct below: to sort and keep olderst end registration
    distinct(ForlopsID.x, .keep_all = TRUE)

  RegDataStartEnd
}
