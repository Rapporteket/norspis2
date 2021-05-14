#' Function to make data table for plot with comparison of hospitals
#' Making a small summary table which will go into the plot with  percentages (proportions) within each group (hospital):
#'
#' This funcition works with both ordinary data (RegData) and data with added national values (RegDataNatVal)
#'
#' @param myIndata_NatVal myIndata_NatVal <- RegData_NatVal
#' @param myInvar01 myVar01 <- PROP_PO10Pasientsikkerhet
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was:
#' make_proptable_hospitals(), and we then had to pass myVar01 on the format:
#' myVar01 <- quo(PROP_PO10Pasientsikkerhet). This had to be fixed when
#' implemented function in Shiny application, so that we "quo()" the variable
#' name after it has been passed/inside our make_figTable_unitCompar function.
#'
#' @return summary table with propoprtions at each unit/hosptital and one national value to go into make_figFig_unitCompar
#' @export
#'
#' @examples

make_figTable_unitCompar <- function(myIndata_NatVal,
                                     myInvar01 = myVar01) {

  myInvar01 <- sym({{noquote(myInvar01)}})
  myInvar01 <- quo({{myInvar01}})


  myInData_NatVal_summarized <- myIndata_NatVal %>%
  #group_by together with summarize (see Wickham, R for Data Science)
  dplyr::group_by(AvdNavn)%>%
    dplyr::summarize(perc := mean(!!myInvar01, na.rm = T),#mean() of 0-1 gives
                                                          #proportion
                     n := sum(!is.na(!!myInvar01))) %>%
 ## TODO: make axixs label depend on presented variable (avarage or proportion)
 #try1
 # mutate(my_y_lab = case_when(
 #  perc > 1 ~ "Gjennomsnittlig sk?re", #When perc is above 1: Label is
                                        #"Gjennomsnittlig sk?re"
 # perc <= 1 ~ "Andel (%)")) %>% #if less than or eq. to one it is
                                 #between 0-1 and "Andel (%)"
 #try2:
 #...stringr::str_detect(my_proptable_hospitals["PROP_PO09Utbytte.x", ],"PROP")
 #try#.
 #mutate(!!myInvar01 := case_when(
 # grepl("PROP",
 #     .[,4]) ~ "Andel (%)", #when PROP is in the variable's values (column 4)
 #                            TRUE ~ "Gjennomsnittlig sk?re")) %>%
    #When perc is between 0-1 we multiply by 100, else we keep the perc value
    #(which will then be a mean ordinary mean value):
    dplyr::mutate(perc = dplyr::case_when(perc >= 0 &
                                            perc <= 1 ~ perc*100,
                                          TRUE ~perc)) %>%
    #make extra hospital name that include "n":
    dplyr::mutate(AvdNavnNavn = AvdNavn,#first,keep variable with just name
                  AvdNavn = paste0(AvdNavn, ' (', n,')' )) %>%
    #removes hospitals w n<20 (choose 0.00000123 as easy identifiable value
    dplyr::mutate(perc = ifelse(n<5, 0.00000123, perc)) %>%
    dplyr::mutate(!!myInvar01 := 0)%>%
    #create new variable with name same as myinVar name, and set the values
    #equal to name of 4. columns whic is the new myInvar01 column:
    dplyr::mutate(!!myInvar01 := (colnames(.[,4])))%>%
    #recode nan - choose 0.00000123 (as above for units with N<5):
    dplyr::mutate(perc = dplyr::case_when(is.nan(perc) ~ 0.00000123,
                            TRUE ~ perc))

  #SORT (from smallest to largest proportions)
  #remember that plot will sort by AvdNavn ,so we must reorder this factor
  ## forcats needed to reorder the levels (as a means to SORT the variable):
  myInData_NatVal_summarized$AvdNavn <-
    myInData_NatVal_summarized$AvdNavn %>%
    forcats::fct_reorder(myInData_NatVal_summarized$n,
                         .desc=F) %>%
    forcats::fct_reorder(myInData_NatVal_summarized$perc,
                         .desc=F)#T for the other order

  ## Then we first sort by N and then by percentage
  myInData_NatVal_summarized <- myInData_NatVal_summarized %>%
    dplyr::arrange(n) %>%
    dplyr::arrange(perc)

}
