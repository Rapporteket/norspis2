#' Make data quality table with proportions of missing values for chosen variables.
#'
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make_missingTab
#'
#' @param RegData myFilteredData
#' @param varsInMissTab myvarStringMiss <- myvarStringMiss <- c(quo(variablename_miss), quo(variablename_missStart))
#' @param comparison
#' @param RegDataComparison
#'
#' @return output_missingTibble
#' @export
#'
#' @examples

make_table_DQ_missing <- function(RegData,
                                  varsInMissTab,
                                  #comparison
                                  comparison = FALSE,
                                  RegDataComparison
){

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
      relocate(Variabel, NA_,Valide, NA_pluss_Valide, Kompletthet, NotNA, Null, Totalt, .before=1) #place the variable "Variabel" first


    #Here we just merge the summary tables made above (one table/variable is added for each new running of the loop)
    output_missingTibble <- dplyr::bind_rows(output_missingTibble,
                                             missingtable_variable)
  }

  if(comparison == TRUE){

    output_missingTibble_comparison <- tibble()

    for(myvar in varsInMissTab){
      missingtable_variable <- RegDataComparison %>%
        summarize(NA_ = sum(is.na(!!myvar)),
                  NotNA = sum(!is.na(!!myvar)),
                  Null := sum(!!myvar %in% c('null')),# "null" is rows that should be excluded from  missing calculations according to how I have coded the *_miss variables
                  Valide = (NotNA - Null),
                  NA_pluss_Valide = (NA_ + Valide),
                  Totalt = NA_+NotNA)%>%
        mutate(Variabel := wrapr::qc(!!myvar))%>%#mutate(!!myvar :=as.character(!!myvar))
        mutate(Kompletthet = round(Valide/(NA_+Valide)*100, 1))%>%
        relocate(Variabel, NA_,Valide, NA_pluss_Valide, Kompletthet, NotNA, Null, Totalt, .before=1) #place the variable "Variabel" first


      #Here we just merge the summary tables made above (one table/variable is added for each new running of the loop)
      output_missingTibble_comparison <- dplyr::bind_rows(output_missingTibble_comparison,
                                                          missingtable_variable)
    }


    output_missingTibble_merged <-
      full_join(output_missingTibble,
                output_missingTibble_comparison,
                by = c("Variabel"),
                #add suffix only to merged data
                suffix = c("",".compare"))%>%
      #compute differences:
      mutate(
        #N.diff = N.compare-N,
        `Kompletthet.diff` = `Kompletthet.compare`- `Kompletthet` )


    output_missingTibble <- output_missingTibble_merged


  }

  return(output_missingTibble)
}
