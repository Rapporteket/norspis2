#' Make data table with count (n) of registrations (regCount) at the different hospitals and nationally
#' (both start and end registrations is counted)
#'
#' The output here is a table and a flextable, and in addition you can save the
#' flextable as a word-document and or image at to a chosen path.
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make42_nregtab_hospitals
#'
#'
#' @param RegData myInData. Function works with natval data.
#'
#' @return output (table)
#' @export
#'
#' @examples

make_table_DQ_regCount <- function(RegData = myInData,
                                   saveAsImage=F,
                                   saveAsDoc=F,
                                   pathToSaveTableFile = "F:/"
                                   ){

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

  table_output <- RegData_NatVal_overview
  #<- now we have have the table we need

  ##WE ALSO MAKE A FLEXTABLE out of it, so that we can save to docx or image:

    #remove 0s, replace with NA, so that we later (in fletabale easily can switch to "-")
    RegData_NatVal_overview[RegData_NatVal_overview==0] <- NA

    #present RegData_NatVal_overview as ft
    border <- officer::fp_border() #this is just the border we plot with flextable::vline() further down

    ft <- flextable::flextable(RegData_NatVal_overview) %>%
      #endre kolonnenavn

      flextable::set_header_labels(AvdNavn = "Behandlingsenhet",
                                   n_reg_utred_BU = "Kun utredning",
                                   n_reg_start_BU = "Start",
                                   n_reg_end_BU = "Slutt",
                                   n_reg_avbr_BU = "Avbrudd",
                                   n_reg_utred_V = "Kun utredning",
                                   n_reg_start_V= "Start",
                                   n_reg_end_V =  "Slutt",
                                   n_reg_avbr_V = "Avbrudd",
                                   n_reg_all = "SUM")%>%
      #n_patients_unique = "Unike pasienter")%>%
      #flextable::theme_zebra()%>%
      flextable::add_header_row(values = c("","Startregistreringer", "","Sluttregistreringer", "", "Startregisteringer", "", "Sluttregistreringer","",""))%>% #extra column names
      flextable::add_header_row(values = c("","Barn/unge", "","", "", "Voksne", "", "","",""),top=TRUE)%>% #extra column names
      flextable::merge_at(i = 1, j=2:5, part = "header") %>% #merging new header cells, before centering them
      flextable::merge_at(i = 1, j=6:9, part = "header") %>% #merging 1.2
      flextable::merge_at(i = 2, j=2:3, part = "header") %>%  #merging 2.1
      flextable::merge_at(i = 2, j=4:5, part = "header") %>%  #merging 2.2
      flextable::merge_at(i = 2, j=6:7, part = "header") %>%  #merging 2.3
      flextable::merge_at(i = 2, j=8:9, part = "header") %>%  #merging 2.4
      flextable::align(align="center", part= "header") %>% #aligning header
      flextable::bold(i=1:2,part = "header")%>% #bold header
      #flextable::add_header_lines(values = "Tabell 1: Antall registreringer innenfor hver av registreringstypene, hos de ulike behandlingsenhetene og totalt, i NorSpis i 2019.", top=TRUE) %>% # Table title (adding this last becuase to not want to center it)
      #flextable::vline(j=c("n_reg_avbr_BU","n_reg_avbr_V"), border=border, part="body" )
      flextable::hline(i=11, part = "body", border=border) %>%
      flextable::colformat_int(na_str = "-")#remove NA from visual
      #flextable::colformat_char(j=1, i=12,suffix="!")

      ft

  #WE ALSO SAVE THE FILE:
  #If save file...
  if(saveAsImage==T){
    flextable::save_as_image(ft, path = paste0(pathToSaveTableFile,"Nreg",Sys.Date(),".png" ))
  }

  if(saveAsDoc==T){
    flextable::save_as_docx("Nreg2019" = ft, path = paste0(pathToSaveTableFile,"Nreg",Sys.Date(),".png" ))

  }

  #return list with table and flextable
  output <- list(table_output,
                 ft)

  return(output)
}
