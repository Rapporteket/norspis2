#' Make table with number of registrations per department per year
#'
#' The data must be filtered first, to contain the registration types
#' you want to present, if not the table will show a mix of registration types.
#'
#' @param RegDataNatVal data with own rows representing national values
#'
#' @return flexable of number of registrations per year per department
#' @export
#'
#' @examples

make_table_DQ_regCountYearly <- function(RegDataNatValFiltered,
                                         saveAsImage=F,
                                         saveAsDoc=F,
                                         width_first_column=2,
                                         pathToSaveTableFile = "F:/",
                                         fileNameSuffix= "_2021" #any text
                                         ){

  #make tab
  RegData_NatVal_overview <- RegDataNatValFiltered %>%
    group_by(AvdNavn, Year) %>% #, RegRegtype)%>%
    summarize(counts = sum(!is.na(PasientID))) %>%
    tidyr::spread(key = Year, value = counts) %>%
    # pivot_wider(id_cols = AvdNavn,
    #             names_from = c("RegRegtype", "Year"),
    #             values_from = c("counts"))
    rowwise() %>%
    mutate("Total" = sum(c_across(), na.rm = T))





  #Adding "Nasjonal" at bottom
  ##We first remove the row and then bind it back to the table
  #remove national:
  RegData_NatVal_overview_without_national <-  RegData_NatVal_overview %>%
    subset(AvdNavn !="Nasjonal")
  #extract national
  RegData_NatVal_overview_only_national <-
    RegData_NatVal_overview %>% #only national numbers
    subset(AvdNavn =="Nasjonal")
  #add national back again
  RegData_NatVal_overview <-
    rbind(RegData_NatVal_overview_without_national,
          RegData_NatVal_overview_only_national ) #hospitals AND nationa
  #Call it SUM instead of "Nasjonal":
  RegData_NatVal_overview$AvdNavn <-
    dplyr::recode(RegData_NatVal_overview$AvdNavn,
                  "Nasjonal"="Total")

  RegData_NatVal_overview <- RegData_NatVal_overview %>%
    rename(" "= "AvdNavn")#remove AvdNavn as title,first column (Set it empty)

  #present tab as ft

  #this is just the border we plot with flextable::vline() further down
  border <- officer::fp_border()

  ft3 <- flextable::flextable(RegData_NatVal_overview) %>%
    flextable::hline(i=nrow(RegData_NatVal_overview)-1, part = "body", border=border)%>%
    flextable::colformat_int(na_str = "-")%>%#remove NA from visual
    flextable::autofit() %>%
    flextable::width(j=1, width =width_first_column)

  # Print/save as .png and .docx
  # flextable::save_as_image(ft3, path = "F:/Nreg2012_2019start.png" )
  # flextable::save_as_docx("Nreg2012_2019start" = ft3,
  # path = "F:/Nreg2012_2019start.docx")

  #If save file....as .png and .docx
  if(saveAsImage==T){
    flextable::save_as_image(ft3, path = paste0(pathToSaveTableFile,"Nreg",Sys.Date(), fileNameSuffix ,".png" ))
  }

  if(saveAsDoc==T){
    flextable::save_as_docx("Nreg2019" = ft3, path = paste0(pathToSaveTableFile,"Nreg",Sys.Date(), fileNameSuffix,".docx" ))

  }

  return(list(RegData_NatVal_overview, ft3))

}
