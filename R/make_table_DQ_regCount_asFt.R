#' Make table of count of registrations into a nicely formatted Flextable
#'
#' This functionality did not have its own function in aarsrapport 2019
#' (in 2020) but the code was included in the .Rmd file for the
#' "aarsrapport"
#'
#' @param tab this is the table made with the function make_table_DQ_regCount
#'
#' @return
#' @export
#'
#' @examples

make_table_DQ_regCount_asFt <- function(tab){

  #remove 0s, replace with NA, so that we later (in fletabale easily can switch to "-")
  tab[tab==0] <- NA

  #present tab as ft
  border <- officer::fp_border() #this is just the border we plot with flextable::vline() further down

  ft <- flextable::flextable(tab) %>%
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

  #You can save the variable as .png or .docx with the following commands
  #flextable::save_as_image(ft, path = "F:/Nreg2019.png" )
  #flextable::save_as_docx("Nreg2019" = ft, path = "F:/Nreg2019.docx")

}


