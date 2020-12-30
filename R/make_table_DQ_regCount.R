#' Make data table with count (n) of registrations (regCount) at the different hospitals and nationally
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make42_nregtab_hospitals
#'
#' @param RegData myInData. Function works with natval data.
#'
#' @return output (table)
#' @export
#'
#' @examples

make_table_DQ_regCount <- function(RegData = myInData){

  #main function:
  RegData_NatVal_overview <- RegData %>%
    group_by(AvdNavn)%>%
    summarize(n_reg_utred_BU= sum(RegRegtype==2),
              n_reg_start_BU = sum(RegRegtype==4),
              n_reg_end_BU = sum(RegRegtype==6),
              n_reg_avbr_BU = sum(RegRegtype==99),
              n_reg_utred_V = sum(RegRegtype==1),
              n_reg_start_V= sum(RegRegtype==3),
              n_reg_end_V = sum(RegRegtype==5),
              n_reg_avbr_V = sum(RegRegtype==98),
              n_reg_all = sum(!is.na(BasisRegStatus))) %>%
    #n_patients_unique = n_distinct(PasientID)) %>%#OBS: this gives unique with start OR end registrations #(Also this one, but seems simliar: sum(!is.na(unique(PasientID.x))))
    arrange(AvdNavn) #sort/arrage by hospital name
  ##If you want to add a sum column manually (instead of using a "najonal column"):
  #rbind(c("Sum", sum(.[,2]), sum(.[,3]),sum(.[,4]),sum(.[,5]),sum(.[,6]),sum(.[,7]),sum(.[,8]),sum(.[,9]),sum(.[,10]),sum(.[,11])))

  #Adding "Nasjonal" at bottom
  ##We first remove the row and then bind it back to the table
  #remove national:
  RegData_NatVal_overview_without_national <-  RegData_NatVal_overview %>%
    subset(AvdNavn !="Nasjonal")
  #extract national
  RegData_NatVal_overview_only_national <-  RegData_NatVal_overview %>% #only national numbers
    subset(AvdNavn =="Nasjonal")
  #add national back again
  RegData_NatVal_overview <- rbind(RegData_NatVal_overview_without_national, RegData_NatVal_overview_only_national ) #hospitals AND nationa
  #Call it SUM instead of "Nasjonal":
  RegData_NatVal_overview$AvdNavn <- dplyr::recode(RegData_NatVal_overview$AvdNavn,
                                                   "Nasjonal"="SUM")

  output <- RegData_NatVal_overview
  return(output)
}
