#' Make data quality table with proportions of missing values for chosen variables.
#'
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make_missingTab
#'
#' @param RegData myFilteredData
#' @param varsInMissTab myvarStringMiss <- myvarStringMiss <- c(quo(variablename_miss), quo(variablename_missStart))
#'
#' @return output_missingTibble
#' @export
#'
#' @examples

make_table_DQ_missing <- function(RegData = myFilteredData,
                            varsInMissTab = myvarStringMiss){
  output_missingTibble <- tibble()

  for(myvar in varsInMissTab){
    missingtable_variable <- RegData %>%
      summarize(NA_ = sum(is.na(!!myvar)),
                NotNA = sum(!is.na(!!myvar)),
                Null := sum(!!myvar %in% c('null')),# "null" is rows that should be excluded from  missing calculations according to how I have coded the *_miss variables
                Valide = (NotNA - Null),
                NA_pluss_Valide = (NA_ + Valide),
                Totalt = NA_+NotNA)%>%
      mutate(Variabel := wrapr::qc(!!myvar))%>%#mutate(!!myvar :=as.character(!!myvar))
      mutate(Kompletthet = round(Valide/(NA_+Valide)*100, 1))%>%
      relocate(Variabel, NA_,Valide, NA_pluss_Valide, Kompletthet, NotNA, Null, Totalt, before=1) #place the variable "Variabel" first


    #Here we just merge the summary tables made above (one table/variable is added for each new running of the loop)
    output_missingTibble <- dplyr::bind_rows(output_missingTibble,
                                             missingtable_variable)
  }
  return(output_missingTibble)
}
