#' Tabell med tellinger, per behandlingsenhet/sykehusavdeling
#'
#' @param RegData
#' @param userRole
#' @param reshID
#' @param datoFra
#' @param datoTil
#'
#' @return Tabell med tellinger, per behandlingsenhet/sykehusavdeling
#' - unike pasienter, per avdeling (ferdigstilte)
#' - startregistreringer (ferdigstilte)
#' - Manglende sluttregistreringer (til ferdigstilte startregistreringer)
#' - registrering startet men ikke fullført
#'
#' @export

NorSpis1TabRegStatusUtvidet <- function(RegData = RegData,
                                        userRole,
                                        reshID,
                                        datoFra,
                                        datoTil) {

  # DL is not available in function environment. Remove for now (Mads?)
  #RegData <- DL$RegData2
  #Lage kolonne 1:
  kolonne1_UnikePasienter <-
    RegData %>%
    filter(HovedDato >= datoFra & HovedDato <=datoTil)%>%
    #startregistreringer, kun ferdigstilte:
    filter(RegRegtype %in% c(1,2,3,4),
           BasisRegStatus==1) %>%
    #make PasientID_Enhet variabel to count only patients that are distinct
    #on each hospital (is done underneath)
    mutate(AvdRESH_PasientID= stringr::str_c(AvdRESH, PasientID)) %>%
    #removes patient if occur twice at a treatment unit:
    distinct(AvdRESH_PasientID, .keep_all = TRUE) %>%
    #make new variable with TRUE for startregi:
    mutate(RegRegtypeStart= RegRegtype %in% c(1,2,3,4)) %>%
    count(AvdRESH, RegRegtypeStart) %>%
    tidyr::spread(RegRegtypeStart,n) %>%
    rename(Unike_pasienter_per_avd = 'TRUE') %>%
    select(AvdRESH, Unike_pasienter_per_avd)

  #flextable::flextable(kolonne1_UnikePasienter)

  #Lage kolonne 2:
  kolonne2_StartReg <-
    RegData %>%
    filter(HovedDato >= datoFra & HovedDato <=datoTil,
           BasisRegStatus==1) %>%
    #make new variable with TRUE for startregi:
    mutate(RegRegtypeStart= RegRegtype %in% c(1,2,3,4)) %>%
    count(AvdRESH, RegRegtypeStart) %>%
    tidyr::spread(RegRegtypeStart,n) %>%
    rename('Startregistreringer_ferdig' = 'TRUE') %>%
    select(AvdRESH, Startregistreringer_ferdig)

  #flextable::flextable(kolonne2_StartReg)

  #Lage kolonne 3:
  kolonne3_SluttregMangler <-
    RegData %>%
    filter(HovedDato>=datoFra & HovedDato<=datoTil, BasisRegStatus==1) %>%
    #make new variable with TRUE if registration has a belonging sluttreg:
    mutate(HarStartRegSluttReg= ForlopsID %in% RegTilhorendeStartReg) %>%
    count(AvdRESH, HarStartRegSluttReg) %>%
    tidyr::spread(HarStartRegSluttReg, n) %>%
    rename(Manglende_slutt_start_ferdig = 'TRUE') %>%
    select(AvdRESH, Manglende_slutt_start_ferdig)

  #flextable::flextable(kolonne3_SluttregMangler)

  #Lage kolonne 4 og 5:
  kolonne4og5_ikke_ferdigstilte <-
    RegData %>%
    filter(HovedDato>=datoFra & HovedDato<=datoTil) %>%
    filter(BasisRegStatus!=1) %>%
    count(AvdRESH, BasisRegStatus) %>%
    tidyr::spread(BasisRegStatus, n) %>%
    rename(Opprettet_ikke_ferdig1 = '-1',
           Opprettet_ikke_ferdig2='0')

  #flextable::flextable(kolonne4og5_ikke_ferdigstilte)

  #Slå sammen  ("merge") alle kolonnene(tabellene):
  TabRegStatusUtvidet <-
    full_join(kolonne1_UnikePasienter, kolonne2_StartReg) %>%
    full_join(.,kolonne3_SluttregMangler)%>%
    full_join(., kolonne4og5_ikke_ferdigstilte)#%>%
  #janitor::adorn_totals("row") #Make sum row

  TabRegStatusUtvidet$AvdRESH[TabRegStatusUtvidet$AvdRESH==105806] <-
    'RKSF, Levanger, Helse Nord-Trøndelag HF'
  TabRegStatusUtvidet$AvdRESH[TabRegStatusUtvidet$AvdRESH==109979] <-
    'RASP, OUS HF'
  TabRegStatusUtvidet$AvdRESH[TabRegStatusUtvidet$AvdRESH==110361] <-
    'Spiseforstyrrelsespoliklinikken, Gaustad, OUS HF'
  TabRegStatusUtvidet$AvdRESH[TabRegStatusUtvidet$AvdRESH==700698] <-
    'RSS, UNN HF'
  TabRegStatusUtvidet$AvdRESH[TabRegStatusUtvidet$AvdRESH==700821] <-
    'RESSP, NLSH HF'
  TabRegStatusUtvidet$AvdRESH[TabRegStatusUtvidet$AvdRESH==707383] <-
    'Follo, enhet for SF, poliklinikk, Akershus u. HF'
  TabRegStatusUtvidet$AvdRESH[TabRegStatusUtvidet$AvdRESH==4204191] <-
    'Capio Anoreksi Senter'
  TabRegStatusUtvidet$AvdRESH[TabRegStatusUtvidet$AvdRESH==4207041] <-
    'Follo, Enhet for SF, døgn, Akershus u. HF'
  TabRegStatusUtvidet$AvdRESH[TabRegStatusUtvidet$AvdRESH==4209009] <-
    'BUPA poliklinikk Nordre Vestfold, SiV HF'
  TabRegStatusUtvidet$AvdRESH[TabRegStatusUtvidet$AvdRESH==4210562] <-
    'BUP, Bodø, NLSH HF'
  TabRegStatusUtvidet$AvdRESH[TabRegStatusUtvidet$AvdRESH==4210626] <-
    'BUP, Mosjøen, NLSH HF'
  TabRegStatusUtvidet$AvdRESH[TabRegStatusUtvidet$AvdRESH==4210825] <-
    'DPS,Mosjøen, NLSH HF'
  TabRegStatusUtvidet$AvdRESH[TabRegStatusUtvidet$AvdRESH==107026] <-
    'Seksjon for SF, Helse Bergen HF'

  #flextable::flextable(TabRegStatusUtvidet)

  #TO TEST/VIEW, RUN THIS LINE:
  DT::datatable(TabRegStatusUtvidet, options = list(pageLength = 50))

  #return(invisible(TabRegStatusUtvidet))

}
