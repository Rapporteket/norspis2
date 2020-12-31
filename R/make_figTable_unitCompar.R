#' Function to make data table for plot with comparison of hospitals
#' Making a small summary table which will go into the plot with  percentages (proportions) within each group (hospital):
#'
#' @param myIndata_NatVal myIndata_NatVal <- RegData_NatVal
#' @param myInvar01 myVar01 <- quo(PROP_PO10Pasientsikkerhet)
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make_proptable_hospitals
#'
#' @return summary table with propoprtions at each unit/hosptital and one national value to go into make_figFig_unitCompar
#' @export
#'
#' @examples

make_figTable_unitCompar <- function(myIndata_NatVal, myInvar01 = myVar01) {
  # We use dplyr for this (group_by and summarize combined - see H.W. book)
  myInData_NatVal_summarized <- myIndata_NatVal %>%
    #a series of mutations to make the 0-1 variable (to go into the calculations in summarize()):
    #BUT WE DO NOT USE IT HERE, as we have already made the variable PROP_PO10Pasientsikkerhet.x earlier.
    # mutate(PROP_PO10Pasientsikkerhet.x = recode(PO10Pasientsikkerhet.x,
    #                                            "0"="1",
    #                                            "1"="0",
    #                                            "2"="0",
    #                                            "3"="0",
    #                                            "4"="0"))%>%
    #                                            # "9"="0",
    #                                            # "99"="0"
    # ##regarding recode function, be aware of  following (seee ?recode): "recode() is questioning because the                                     arguments are in the wrong order. We have new <- old, mutate(df, new = old), and rename(df, new = old) but                                   recode(x, old = new). We don't yet know how to fix this problem, but it's likely to involve creating a new function                          then retiring or deprecating recode().""
  # mutate(PROP_PO10Pasientsikkerhet.x=replace(PROP_PO10Pasientsikkerhet.x, PROP_PO10Pasientsikkerhet.x %in% c("9","99","null"), NA))%>%
  # mutate(PROP_PO10Pasientsikkerhet.x=as.numeric(PROP_PO10Pasientsikkerhet.x))#to enable calculations



  #group_by together with summarize is what we need to make the summary we want(see Wickham, R for Data Science)
  dplyr::group_by(AvdNavn)%>%
    dplyr::summarize(perc := mean(!!myInvar01, na.rm = T),#mean() of a 0-1 variable gives proportion
              n := sum(!is.na(!!myInvar01))
              #perc_missing = mean(MISSING_PO10Pasientsikkerhet, na.rm = T)*100,
              #n_missing = sum(MISSING_PO10Pasientsikkerhet>=1, na.rm = T),
              #perc_uaktuell = mean(UAKTUELL_PO10Pasientsikkerhet, na.rm = T)*100,
              #n_uaktuell = sum(UAKTUELL_PO10Pasientsikkerhet>=1, na.rm = T)
    ) %>%
    ## make axixs label depend on which variable is presented (avarage or proportion)
    #try1
    # mutate(my_y_lab = case_when(perc > 1 ~ "Gjennomsnittlig sk?re", #When perc is above 1: Label is "Gjennomsnittlig sk?re"
    #                             perc <= 1 ~ "Andel (%)")) %>% #if less than or eq. to one it is between 0-1 and "Andel (%)"
    #try2:
    #...stringr::str_detect(my_proptable_hospitals["PROP_PO09Utbytte.x", ], "PROP")
    #try#.
    #mutate(!!myInvar01 := case_when(grepl("PROP", .[,4]) ~ "Andel (%)", #when PROP is in the variable's values (column 4)
    #                            TRUE ~ "Gjennomsnittlig sk?re")) %>%



  dplyr::mutate(perc = dplyr::case_when(perc >= 0 & #When perc is between 0-1 we multiply by 100, else we keep the perc value (which will then be a mean ordinary mean value)
                            perc <= 1 ~ perc*100,
                          TRUE ~perc)) %>%
    dplyr::mutate(AvdNavn = paste0(AvdNavn, ' (', n,')' )) %>% #makes hospital names include N
    dplyr::mutate(perc = ifelse(n<5, 0.00000123, perc)) %>% #removes hospitals w n<20 (choose 0.00000123 as an easy identifiable value and same as
    dplyr::mutate(!!myInvar01 := 0)%>%
    dplyr::mutate(!!myInvar01 := (colnames(.[,4])))%>%#create new variable with name same as myinVar name, and set thevalues equal to name of 4. columns whic is the new myInvar01 column

    dplyr::mutate(perc = dplyr::case_when(is.nan(perc) ~ 0.00000123, #recode nan - choose 0.00000123 (as above for units with N<5)
                            TRUE ~ perc))
  #mutate(my_bar_lab = factor(ifelse(perc == 0.00000123,'',paste0(round(perc,1),'%'))))
  #<-make bar label (inside/above bar, e.g. percentages), but not for those which should be emtpy, i.e those with 0.00000123 values. I did this in fig function in earlier version, but moved it here to


  #Fix NaN values (MOVED INTO PIPE ABOVE:  ("mutate(perc = case_when(is.nan(perc) ~ 0.00000123))%>%" )
  #myInData_NatVal_summarized[is.nan(myInData_NatVal_summarized$perc),]$perc <- 0.00000123 # choose 0.00000123 (as above for units with N<20)
  #myInData_NatVal_summarized[is.nan(myInData_NatVal_summarized$perc_missing),]$perc_missing <- 0.00000123 # choose 0.00000123 (as above for units with N<20)
  #myInData_NatVal_summarized[is.nan(myInData_NatVal_summarized$perc_uaktuell),]$perc_uaktuell <- 0.00000123 # choose 0.00000123 (as above for units with N<20)

  #Sort from smallest to largest proportions
  ## Here we must first use forcats to reorder the leves, as a means to SORT the variable:
  myInData_NatVal_summarized$AvdNavn <- myInData_NatVal_summarized$AvdNavn%>%
    forcats::fct_reorder(myInData_NatVal_summarized$n, .desc=F) %>%
    forcats::fct_reorder(myInData_NatVal_summarized$perc, .desc=F) #CHANGE to T for opposite order
  ## Then we first sort by N and then by percentage
  myInData_NatVal_summarized <- myInData_NatVal_summarized %>%
    dplyr::arrange(n) %>%
    dplyr::arrange(perc)

}
