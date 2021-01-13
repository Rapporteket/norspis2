#' Table with number of registrations
#'
#' Table with number of registrations within different registration types
#' at the different hospital units as well as the sum  nationally
#'
#' @param RegData Registry data
#' @param userRole
#' @param reshID
#' @param datoFra
#' @param datoTil
#'
#' @return A DT::datatable
#' @export
#'
#' @examples

NorSpis1TabRegStatus <- function(RegData,
                                 userRole,
                                 reshID,
                                 datoFra,
                                 datoTil) {

  #Pre-process data:
  RegData <- NorSpis1_1_Preprosess(RegData=RegData)
  #blant annet velger man her i preprosess-funksjonen kun
  #ferdigstilte registreringer

  #------- Gjøre utvalg
  NorSpisUtvalg <- NorSpis1_3_Utvalg(RegData=RegData,
                                     datoFra=datoFra,
                                     datoTil=datoTil)

  RegData <- NorSpisUtvalg$RegData
  utvalgTxt <- NorSpisUtvalg$utvalgTxt

  # Endre datoformat (as.tibble krever dato på POSIXct-format)
  # FIKS: burde ligge i Preprosseseringsfila, men kan p.t. (mars 2019) ikke
  #endres der, da andre funksjoner krever andre datoformat
  RegData$HovedDato <- as.POSIXct(RegData$HovedDato, format="%Y-%m-d")
  #format="%Y-%m-%d") #23.mai 2018: måtte endre linje pga. bug - antagelig har
  #HNIKT endret datoformat i datadump siden sist
  RegData$Dato <- as.POSIXct(RegData$Dato, format="%Y-%m-d")#format="%Y-%m-%d")
  #23.mai 2018: måtte endre linje pga. bug - antagelig har HNIKT
  #endret datoformat i datadump siden sist

  #Lage tabell
  TabRegStatus <-
    RegData %>%
    #to count only unique patients :#distinct(PasientID, .keep_all = TRUE)%>%
    count(SykehusAvdNavn, RegRegtype) %>% #Counts number of registrations
    #Make new variables to make sure all categories/columns of reg.type
    #is included when "spreading", in case of unused reg.types (commands
    #further down will fail if not included):
    mutate('1'= NA,'2'=NA,'3'=NA,'4'=NA,
           '5'= NA, '6'=NA,'98'=NA, '99'=NA)%>%
    tidyr::spread(RegRegtype,n) %>% #Makes crosstab: (spreading to make
    #a cross tabulation/contingency table (n column used below was created
    #by count function above))
    #see: analyticswithr.com/contingencytables.html
    rename('Behandlingsenhet'="SykehusAvdNavn",
           'Utredning voksen'='1',
           'Utredning ungdom/barn'='2',
           'Startregistrering voksen'='3',
           'Startregistrering ungdom/barn'='4',
           'Sluttregistrering voksen'= '5',
           'Sluttregistrering ungdom/barn'='6',
           'Avbrutt behandling voksen'='98',
           'Avbrutt behandling ungdom/barn'='99') %>%
    select('Behandlingsenhet',
           'Utredning ungdom/barn',
           'Startregistrering ungdom/barn',
           'Sluttregistrering ungdom/barn',
           'Avbrutt behandling ungdom/barn',
           'Utredning voksen',
           'Startregistrering voksen',
           'Sluttregistrering voksen',
           'Avbrutt behandling voksen') %>%#Rearranges the order of the columns
    tibble::as_tibble() %>%
    #Make sum column:
    replace(is.na(.),0) %>%
    mutate(SUM = rowSums(.[2:9]))%>%
    #Make sum row
    janitor::adorn_totals("row")%>%
    #Change name of sum row from "Total" (default in Janitor) to "SUM"
    mutate(Behandlingsenhet=replace(
      Behandlingsenhet, Behandlingsenhet=='Total', 'SUM'))

  #knitr::kable(TabRegStatus)
  DT::datatable(TabRegStatus, options = list(pageLength = 50))


}
