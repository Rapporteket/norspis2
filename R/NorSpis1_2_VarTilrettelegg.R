#' Funksjon for å tilrettelegge variable for beregning.
#'
#' Denne funksjonen gjør utvalg og tilrettelegger variable (gitt ved valgtVar) til videre bruk.
#' Videre bruk kan eksempelvis være beregning av AggVerdier eller gjennomsnitt.
#' Funksjonen gjør også filtreringer som å fjerne ugyldige verdier for den valgte variabelen, samt ta høyde for avhengigheter med
#' andre variable. Det er også her man angir aksetekster og titler for den valgte variabelen.
#' Her kan mye hentes til analysebok
#'
#' @param RegData
#' @param valgtVar
#' @param grVar
#' @param figurtype
#'
#' @return Definisjon av valgt variabel.
#' @export
#'
#' @examples

NorSpis1_2_VarTilrettelegg  <- function(RegData, valgtVar, grVar='SykehusNavn', figurtype='andeler'){

  grtxt <- 0
  xAkseTxt <- ''
  KImaal <- NA
  KImaaltxt <- NA
  retn='V'
  tittel <- ''
  flerevar <- 0
  sortAvtagende <- T
  variable <- 'Ingen'
  deltittel <- ''
  tittel2 <- ''
  tittel3 <- ''
  RegData$Variabel <- 0

  #--------Variabler  - tilrettelagt og sortert alfabetisk
  #--------(fire figurtyper er representert:                   andeler, andelGrVar, gjsnGrVar, andelTid (NorSpisFigAndeler, NorSpisFigAndelerGrVar, NorSpisFigGjsnGrVar, NorSpisAndelTid))

  #NESTE/TODO:
  #Fra FRM 07.06.18, sak 27-2018
  #KI2:Spiseforstyrrelsessymtomer målt vha. EDE-Q og CIA
  #KI3:Bortfall av undervekt med mål om at 80 % av denne gruppen av pasienter skal ha redusert sin undervekt
  #    ved behandlingsslutt, dvs. at man oppnår en BMI på minst 18,5 og med tilsvarende persentil for barn og unge.
  #KI4.1:Pasientvurdering av utbytte/utfall ved behandlingsslutt:
  #    Spm. 9 i Pasienterfaringer i spes.h.tj: Hvilket utbytte har du hatt alt i alt av behandlingen på behandlingsenheten?
  #KI4.2:Pasientvurdering av utbytte/utfall ved behandlingsslutt:
  #    Spm. 3 i skjemaet Pasienttilfredshet: Hvordan vurderer du utfallet av mottatt behandling?



  #FIX på sikt (GJELDER HELE PAKKEN): Legge til unique-filter i applikasjonen etter hvert (p.t. telles en pasient med to startreg. to ganger)
  #Å velge unike pasienter kan gjøres slik:
  ##indDum2 <- which(unique(RegData$PasientID, incomparables = FALSE) > 0)
  ##RegData <- RegData[indDum2, ]
  #RegData <- RegData[!duplicated(RegData$PasientID), ]



  if (valgtVar %in% c('Alder','B08StartAldrProbl', 'B12cAldrForsteBeh')) {                  #BRUKES I: Andeler
    #OM: Omfatter alle pasienter. Hvis en pasient er registrert flere ganger, enten ved samme eller ulike
    #behandlingssted kommer pasienten med to ganger i statistikken.Pasienten telles imidlertid kun én gang per
    #registreringstype per enhet (for eksempel telles en pasient med sluttregistrering kun med sin startregistrering
    #eller utredning

    #Foreløpig fjernet:
    #RegData <- RegData[which(RegData$RegRegtype == 1 | RegData$RegRegtype ==2 | RegData$RegRegtype ==3 | RegData$RegRegtype ==4), ]#velger kun startregistreringer og utredninger fordi ellers får vi dobbelt opp med pasienter (1=Utredning voksen, 2=Utredning ungdom/barn, 3=Startregistrering voksen, 4=Startregistrering ungdom/barn)
    ##feil1: teller en pasient flere ganger hvis mottar behandling to ganger - ev. fjerne duplikat
    ##feil2: teller også en pasient flere ganger hvis mottar behandling samme sted to ganger - må fjerne også denne formen for duplikat
    ##feil3: hvis retter feil 1 og 2 kan utvalget "egen enhet mot resten av landet" bli feil - for hvor skal pasienten telles? Egen enhet: ja. Landet: ja, hvis utvalget er "hele landet", men usikkert hvis mottar beh. to egen enhet pluss landet (alternativene blir å telle pasoenten kun ved egen enhet eller både ved egen enhet og "hele landet").
    ##riktig blir kanskje: Telle hver PasientID(variabelnavn) kun én gang for hvert behandlingssted(altså 2 ganger hvis mottar behandling 2 steder)
    ##...men kun engang i statistikken for hele landet

    #velge unike pasienter
    #FEIL?(SER SLIK UT):
    #indDum2 <- which(unique(RegData$PasientID, incomparables = FALSE) > 0)
    #RegData <- RegData[indDum2, ]

    #BEDRE:
    #RegData <- RegData[!duplicated(RegData$PasientID), ]
    ##FIKS: Sorter på alder eller hoveddato først, og fjern deretter den siste (ikke første) av duplikasjonene
    ##MERKNAD/HJELPETEKST TIL FIKS OVER SOM MÅ MED: "Unike pasienter/pasient telles kun én gang hvis flere registreringer. Pasienter som har mottatt behandling flere ganger telles med med laveste alder/første alder."

    #ENDA BEDRE:
    #ALTERNATIV FIKS: Fjerne duplikasjonsfilter og heller la bruker filtrere selv på registreringstype (en pasient med f.eks. to startregistreringer i perioden, vil da komme med to ganger, men det kan være en kommentar til figuren)
    #MERKNAD TIL ALTERNATIV FIKS (tilrettelegg slik at "kommentar" sendes ut av funksjon og tas inn i Figuren":
    kommentar <- 'Merknad: Pasienter med flere registreringer telles med en gang for hver registrering. Filtrer på registreringstype for å unngå at pasienter
    med både start- og slutt-registreringer kommer med to ganger. Pasienten kan da likevel komme med to ganger i nasjonal oversikt,
    hvis pasienten har mottatt behandling flere behandlingssteder i perioden.'
    kommentar <- ''

    gr <- c(0,seq(5,30,5),40,50,150) #c(0,seq(10,20,2),30,40,50,150)#c(0,seq(6,30,4),40,50,150) # c(0,seq(10,22,2),30,40,50,150)
    #indDum <- which(RegData[ ,valgtVar] %in% c(1:150))
    #RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
    RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '50+')	#c(names(AndelLand)[-length(gr)], '90+')
    grtxt <- gsub("\\,", "-", grtxt) #erstatter komma med bindestrek
    subtxt <- 'Aldersgruppe'
    xAkseTxt <- 'Alder (år)'
    tittel <- switch(valgtVar,
                     Alder = 'Aldersfordeling',
                     B08StartAldrProbl = 'Alder da problemene startet',
                     B12cAldrForsteBeh = 'Tidligere behandling: Alder ved start av første behandling') #Deaktivert til årsr.17
    RegData$VariabelGj <- RegData[ ,valgtVar] #til FigGjsnGrVar
    deltittel <- 'alder'
  }


  if (valgtVar=='alder_u18') {        #BRUKES I: AndelerGrVar, AndelTid
    RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
    RegData$Variabel[which(RegData$Alder<18)] <- 1
    tittel <- 'Pasienter under 18 år'
  }

  if (valgtVar=='alder_o25') {        #BRUKES I: AndelerGrVar, AndelTid
    RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
    RegData$Variabel[which(RegData$Alder>25)] <- 1
    tittel <- 'Andel pasienter over 25 år'
    kommentar <- ''
  }


  if (valgtVar=='B01Sivilstatus') {   #BRUKES I: Andeler
    #OM: 1)Variabelen måles kun ved start/utredning. Man trenger dermed ikke ha en filtreringslinje
    #hvor man filterer på start/utredning, slik som i aldersfiguren. N vil bli lik som for alder, fordi selv om
    #sivilstatus kun spørres de over 15 år, så autoutfylles den for de under 15 år med verdien 1 (enslig).
    #2)Pasienten telles også her flere ganger ved behandling flere ganger, enten samme sted eller ulike steder

    retn <- 'V'
    grtxt <- c('Enslig','Samboer','Gift','Skilt','Enke/\nenkemann','Annen')

    #sortere bort ugyldige rader
    koder <- c(1:5,9)
    indDum <- which(RegData$B01Sivilstatus %in% koder)#
    RegData <- RegData[indDum,] #velger de gyldige radene/fjerner de ugyldige

    ##velge unike pasienter
    ##Rettet 16.07.18. OBS ved bruk av unique: Følgende som indDum2 ga feil i andeler: indDum2 <- which(unique(RegData$PasientID, incomparables = FALSE) > 0)
    #RegData <- RegData[!duplicated(RegData$PasientID), ]

    RegData$VariabelGr <- factor(RegData$B01Sivilstatus, levels = koder)
    xAkseTxt <- 'Sivilstatus'
    tittel <- 'Sivilstatus'
    tittel2 <- ''
    kommentar <- '' #'Selvrapportert ved start av behandling/utredning. De under 15 år spørres ikke, men får automatisk verdien enslig.'
  }

  #enkelt (men feil?) kodesnutt for B01Sivilstatus:
  #if (valgtVar=='B01Sivilstatus') {                                                         #BRUKES I: Andeler
  #      grtxt <- c('Enslig','Samboer','Gift','Skilt','Enke/\nenkemann','Annen')
  #      RegData$VariabelGr <- factor(RegData$B01Sivilstatus, levels = koder)
  #      tittel <- 'Sivilstatus'
  #}

  if (valgtVar=='B02EgneBarn') {                                                                  #BRUKES I: Andeler

    #sortere bort ugyldige rader
    koder <- c(0:15)
    indDum <- which(RegData$B02EgneBarn %in% koder)#
    RegData <- RegData[indDum,] #velger de gyldige radene/

    grtxt <- c(0:4, '5+')	#c(names(AndelLand)[-length(gr)], '90+')
    gr <- c(0:5,15)
    RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)

    xAkseTxt <- 'Antall barn'
    tittel <- 'Antall egne barn'
  }

  if (valgtVar=='B03Bosituasjon') {                                                                #BRUKES I: Andeler
    #Alternativer: 1	Hos en av foreldrene, 2 Hos begge foreldre, 3 Bor alene, 4  Med partner,
    #5 Med partner og barn, 6 Uten partner med barn, 9 Annen
    retn <- 'H'
    grtxt <- c ('Hos en av foreldrene', 'Hos begge foreldre', 'Bor alene', 'Med partner',
                'Med partner og barn', 'Uten partner med barn', 'Annen')
    #RegData$VariabelGr <- 99

    #Velge kun gyldige rader:
    koder <- c (1:6,9) #definerer gyldige koder/kategorier, bukes til å lage indDum under, som videre brukes til å velge gyldige rader to linjer ned
    indDum <- which(RegData$B03Bosituasjon %in% koder)#
    RegData <- RegData[indDum,] #velger de gyldige radene/fjerner de ugyldige

    #lager en variabel kalt VariabelGr som skal brukes i NorSpisFigAndeler.R. Fordi vi har fjernet ugyldige rader i kodelinjene ovenfor,
    #kan vi bare lage variabelen lik B03Bosituasjon. Vi fester variabelen til RegData. Deretter gjør vi den om til en faktor:
    RegData$VariabelGr <- RegData$B03Bosituasjon  #Endret fordi følgende gav feilmeld: RegData$VariabelGr[indDum] <- RegData$B03Bosituasjon[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder) #Trenger strengt tatt ikke levels,
    #men har den med fordi den forsikrer oss om at kategorier som kan være tomme (e.g. ingen bodde alene, kategori 3) likevel kommer med i figur.
    #xAkseTxt <- 'Bosituasjon'
    tittel <- 'Bosituasjon'
  }

  if (valgtVar %in% c('B04PabegyntUtd', 'B05FullfortUtd')) {                                       #BRUKES I: Andeler
    #OBS - se over/korrigere kategoriene
    retn <- 'H'
    grtxt <- switch(valgtVar,
                    B04PabegyntUtd=c('Grunnskole','Videregående skole (1-3 år)',
                                     'Høgskole eller universitet, \nmindre enn 4 år',
                                     'Høgskole eller universitet, \n4 år eller mer','Ukjent'),
                    B05FullfortUtd=c('Ikke fullført grunnskole','Grunnskole','Videregående skole (1-3 år)',
                                     'Høgskole eller universitet, \nmindre enn 4 år',
                                     'Høgskole eller universitet, \n4 år eller mer','Ukjent'))

    #Velge kun gyldige rader:
    koder <- switch(valgtVar,
                    B04PabegyntUtd=c(1:4,9),
                    B05FullfortUtd=c(1:5,9))
    indDum <- which(RegData[ ,valgtVar] %in% koder)
    RegData <- RegData[indDum, ]

    RegData$VariabelGr <- RegData[ ,valgtVar]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
    tittel <- switch(valgtVar,
                     B04PabegyntUtd ='Høyeste påbegynte utdanning',
                     B05FullfortUtd ='Høyeste fullførte utdanning')
  }


  if (valgtVar=='B06Hovedaktivitet') {                                                            #BRUKES I: Andeler
    # 1=Heltidsarbeid, 2=Deltidsarbeid, 3=På arbeidsmarkedstiltak, 4=Vernepliktig, 5=Skoleelev/lærling, 6=Student, 7=Sykemeldt, 8=Ufør, 9=Annen
    retn <- 'H'
    grtxt <- c('Heltidsarbeid', 'Deltidsarbeid', 'På arbeidsmarkedstiltak', 'Vernepliktig', 'Skoleelev/lærling', 'Student', 'Sykemeldt', 'Ufør', 'Annen')

    #Velge kun gyldige rader:
    koder <- c(1:9)
    #RegData$VariabelGr <- 99
    indDum <- which(RegData$B06Hovedaktivitet %in% koder)
    RegData <- RegData[indDum, ]

    RegData$VariabelGr<- RegData$B06Hovedaktivitet #RegData$VariabelGr[indDum] <- RegData$B06Hovedaktivitet[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
    tittel <- 'Hovedaktivitet'
  }

  if (valgtVar=='B07Hovedinntekt') {                                                               #BRUKES I: Andeler
    retn <- 'H'
    grtxt <- c('Arbeidsinntekt', 'Sykepenger/trygd/pensjon', 'Blir forsørget', 'Sosialhjelp', 'Stipend/lån', 'Kursstønad/\nlønn i arbeidsmarkedstiltak', 'Andre inntekter')

    #Velge kun gyldige rader:
    koder <- c(1:6,9)
    #RegData$VariabelGr <- 99
    indDum <- which(RegData$B07Hovedinntekt %in% koder)

    RegData$VariabelGr <- RegData$B07Hovedinntekt #RegData$VariabelGr[indDum] <- RegData$B07Hovedinntekt[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:6,9))
    tittel <- 'Pasientenes hovedinntekt'
  }

  #if (valgtVar=='B08StartAldrProbl') {
  #  retn <- 'H'
  #  grtxt <- c('Arbeidsinntekt', 'Sykepenger/trygd/pensjon', 'Blir forsørget', 'Sosialhjelp', 'Stipend/lån', 'Kursstønad/lønn i arbeidsmarkedstiltak', 'Andre inntekter', 'Ikke registrert')
  #  RegData$VariabelGr <- 99
  #  indDum <- which(RegData$B07Hovedinntekt %in% c(1:6))
  #  RegData$VariabelGr[indDum] <- RegData$B07Hovedinntekt[indDum]
  #  RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:6,9,99))
  #  tittel <- 'Pasientenes alder da problemene startet'
  #}

  #if (valgtVar =='B08StartAldrProbl'){                                                      #BRUKES I: GjsnGrVar;
  #      #Velge gyldige observasjoner:
  #      RegData <- RegData[which(RegData[ ,valgtVar] >0), ]
  #
  #      RegData$Variabel <- RegData[ ,valgtVar]
  #      deltittel <- 'alder ved start av problematikk'
  #      xaksetxt <- 'Alder (år)'
  #
  #}

  #if (valgtVar %in% c('B08StartAldrProbl', 'B12cAldrForsteBeh')) {                                 #BRUKES I: Andeler
  #      #  RegData <- RegData[which(RegData$ErOppflg == 0), ] #LENA? Hjelpeargument?
  #      gr <- c(0,seq(5,30,5),150)
  #      #indDum <- which(RegData[ ,valgtVar] %in% c(1:150))
  #      #RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
  #      RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
  #      grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '30+')	#c(names(AndelLand)[-length(gr)], '90+')
  #     xAkseTxt <- 'Aldersgruppe'
  #      tittel <- switch(valgtVar,
  #                      B08StartAldrProbl = 'Alder da problemene startet',
  #                       B12cAldrForsteBeh = 'Tidligere behandling: Alder ved start av første behandling')
  #}

  if (valgtVar=='B11FamilieSF') {                                                                  #BRUKES I: Andeler
    retn <- 'V'
    grtxt <- c('Nei', 'Ja', 'Vet ikke')

    #Velge kun gyldige rader:
    koder <- c(0,1,9)
    #RegData$VariabelGr <- 99
    indDum <- which(RegData$B11FamilieSF %in% koder)
    RegData <- RegData[indDum, ]#RegData$VariabelGr[indDum] <- RegData$B11FamilieSF[indDum]

    RegData$VariabelGr <- RegData$B11FamilieSF
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9))
    tittel <- 'Spiseforstyrrelse hos andre i familien?'
  }

  if (valgtVar=='B12TidlBehSF') {                                                                  #BRUKES I: Andeler
    grtxt <- c('Nei', 'Ja')

    #Velge kun gyldige rader:
    koder <- c(0,1)
    #RegData$VariabelGr <- 99
    indDum <- which(RegData$B12TidlBehSF %in% koder)
    RegData <- RegData[indDum, ]#RegData$VariabelGr[indDum] <- RegData$B12TidlBehSF[indDum]

    RegData$VariabelGr <- RegData$B12TidlBehSF
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1))
    tittel <- 'Tidligere behandling for spiseforstyrrelser?'
  }




  #if (valgtVar=='B12dArTilBehstart') {

  #B12dMndTilBehstart OG B12dMndTilBehstart

  #if (valgtVar=='B17FysMishandl') {
  #  retn <- 'H'
  #  grtxt <- c('Nei', 'Ja', 'Ukjent')
  #  RegData$VariabelGr <- 99
  #  indDum <- which(RegData$B17FysMishandl %in% c(0,1,9))
  #  RegData$VariabelGr[indDum] <- RegData$B17FysMishandl[indDum]
  #  RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,99))
  #  tittel <- 'Negative hendelse: Tidligere fysisk mishandling'
  #}

  #if B17FysMishandl, B18PsykMishandl, B19Overgrep, B20Mobbing -> 0,1,2,3,4 negative hendelser
  #alle negative hendelsene i én figur


  #if (valgtVar =='B12cAldrForsteBeh'){                                                      #BRUKES I: GjsnGrVar;
  #      #Velge gyldige observasjoner:
  #      RegData <- RegData[which(RegData[ ,valgtVar] >0), ]
  #
  #      RegData$Variabel <- RegData[ ,valgtVar]
  #      deltittel <- 'alder ved første behandling'
  #      xaksetxt <- 'Alder (år)'
  #}



  if (valgtVar %in% c('B17FysMishandl', 'B18PsykMishandl', 'B19Overgrep', 'B20Mobbing',
                      'B21SelvskadTidl', 'B22SelvskadSisteAr', 'B23SelvmordFTidl', 'B24SelvmordFSisteAr',
                      'B25Avhengighet')) {                                                        #BRUKES I: Andeler
    retn <- 'V'
    #  RegData <- RegData[which(RegData$ErOppflg == 0), ] #LENA? Hjelpeargument?
    grtxt <- c('Nei', 'Ja', 'Ukjent')

    #Velge kun gyldige rader:
    koder <- c(0,1,9)
    #RegData$VariabelGr <- 99
    indDum <- which(RegData[ ,valgtVar] %in% koder)
    RegData <- RegData[indDum, ]#RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]

    RegData$VariabelGr <- RegData[ ,valgtVar]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9))
    tittel <- switch(valgtVar,
                     B17FysMishandl = 'Negativ hendelse: Tidligere fysisk mishandling',
                     B18PsykMishandl = 'Negativ hendelse: Tidligere psykisk mishandling',
                     B19Overgrep = 'Negativ hendelse: Tidligere misbruk/overgrep',
                     B20Mobbing = 'Negativ hendelse: Tidligere mobbing',
                     B21SelvskadTidl = 'Selvskading tidligere',
                     B22SelvskadSisteAr = 'Selvskading siste år',
                     B23SelvmordFTidl = 'Selvmordsforsøk tidligere',
                     B24SelvmordFSisteAr = 'Selvmordsforsøk siste år',
                     B25Avhengighet = 'Misbruk/avhengighet av alkohol, illegale rusmidler eller medikamenter')
  }

  if (valgtVar=='BehDodUnderBeh') {	                                                      #BRUKES I: AndelerGrVar
    #Velge bort ugyldige (NA) observasjoner
    koder <- c(0,1)
    RegData <- RegData[which(RegData[ ,valgtVar] %in% koder), ]
    #RegData <- RegData[which(RegData$BehDodUnderBeh>=0), ]    #tar bort eventuelle verdier som er <0
    #RegData$Variabel[which(RegData$BehDodUnderBeh==1)] <- 1

    RegData$Variabel <- RegData$BehDodUnderBeh
    tittel <- 'Dødsfall under behandling'
  }


  if (valgtVar=='BehUtfallsvurdSamlet') {                                                          #BRUKES I: Andeler
    grtxt <- c('Ikke noe\n problem lenger', 'Klar bedring', 'Noe bedring', 'Uendret', 'Forverring', 'Ikke aktuelt')

    #Velge kun gyldige rader:
    koder <- c(1:6)
    #RegData$VariabelGr <- 99
    indDum <- which(RegData$BehUtfallsvurdSamlet %in% koder)
    RegData <- RegData[indDum, ]#RegData$VariabelGr[indDum] <- RegData$BehUtfallsvurdSamlet[indDum]

    RegData$VariabelGr <- RegData$BehUtfallsvurdSamlet

    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:6))
    tittel <- 'Behandlers samlede utfallsvurdering'
  }

  if (valgtVar=='BehVidereBeh') {                                                                  #BRUKES I: Andeler
    grtxt <- c('Nei', 'Ja')

    #Velge kun gyldige rader:
    koder <- c(0,1)
    #RegData$VariabelGr <- 99
    indDum <- which(RegData$BehVidereBeh %in% koder)
    RegData <- RegData [indDum, ]

    RegData$VariabelGr <- RegData$BehVidereBeh
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
    tittel <- 'Videre behandling annen instans?'
  }



  if (valgtVar %in% c('CIA30GlobalScore',
                      'CIA30Personlig',
                      'CIA30Sosial',
                      'CIA30Kognitiv'))
  {                                                                                         #BRUKES I: GjsnGrVar; Andeler
    #FIX - legge formatfiks (fjerne null/NA) til preprosess-fila
    #Velge gyldige observasjoner
    #Fjerne null-verdier ved omkoding til NA - for å unngå warning (NAs introduced by coercion) når fikser bruker as.num..(as.char...) i kommando for valg av gyldige observasjoner
    # RegData[ ,valgtVar][RegData[ ,valgtVar]=='null']=NA
    # #Fjerne NA-verdier
    # RegData <- na.omit(RegData, cols=RegData[ ,valgtVar])
    # #Velge gyldige observasjoner:
    RegData <- RegData[which(as.numeric(as.character(RegData[ ,valgtVar])) >=0), ] # 23.05.18: må bruke as,numeric + as.character for å endre fra factor (antagelig kan det at variable nå er "factor" skyldes en endring i datadump)

    kommentar <- '1:Hver pasient kan være med flere ganger hvis pasienten har mottatt behandling flere steder. N kan være derfor høyere enn antall unike pasienter.
    2:"Husk å bruke filteret "registreringstype". Uten filter vil hver pasient telles flere ganger - én gang for hver registreringstype'
    kommentar <- ''

    RegData$Variabel <- RegData[ ,valgtVar]
    deltittel <- switch(valgtVar,
                        CIA30GlobalScore = 'CIA global skår',
                        CIA30Personlig = 'personlig svekkelse, CIA 3.0',
                        CIA30Sosial = 'sosial svekkelse, CIA 3.0',
                        CIA30Kognitiv = 'kognitiv svekkelse, CIA 3.0')
    xAkseTxt <- switch(valgtVar,
                       CIA30GlobalScore = 'CIA global skår',
                       CIA30Personlig = 'Personlig svekkelse, CIA 3.0',
                       CIA30Sosial = 'Sosial svekkelse, CIA 3.0',
                       CIA30Kognitiv = 'Kognitiv svekkelse, CIA 3.0')
    #til andelsfiguren:
    tittel <- switch(valgtVar,
                     CIA30GlobalScore =  'CIA global skår',
                     CIA30Personlig = 'personlig svekkelse, CIA 3.0',
                     CIA30Sosial = 'sosial svekkelse, CIA 3.0',
                     CIA30Kognitiv = 'kognitiv svekkelse, CIA 3.0')  #Deaktivert ifm. årsr.17
    tittel2 <- switch(valgtVar,
                      CIA30GlobalScore =  'En skår over 16 anses som uttrykk for at spiseforstyrrelsen påvirker hverdagslivet i betydelig grad',
                      CIA30Personlig = '',
                      CIA30Sosial = '',
                      CIA30Kognitiv = '')                             #Deaktivert ifm. årsr.17
    gr <- switch(valgtVar,
                 CIA30GlobalScore = c(0,10,20,30,40,48),#c(0,16,48),
                 CIA30Personlig = c(0,8.2,18),
                 CIA30Sosial = c(0,2.3,15),
                 CIA30Kognitiv = c(0,3.3,15))
    RegData$VariabelGr <- cut(as.numeric(as.character(RegData[ ,valgtVar])), breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[-(length(gr))]) #grtxt <- c('Under klinisk cut-off','Over klinisk cut-off')
    grtxt <- gsub("\\,", "-", grtxt) #erstatter komma med bindestrek
    subtxt <- 'Over/under klinisk cut-off'
    #til gjennomsnittsfigur (pre/post)
    RegData$VariabelGj <- RegData[ ,valgtVar]
  }

  if (valgtVar == 'CIA30GlobalScoreRCI') { #basert verdier gitt av Ø.Rø i e-post 06.05.2019
    retn <- 'V'
    grtxt <- c('Verre', 'Uendret', 'Bedring', 'Stor forbedring')

    RegDataStart <- RegData[which(RegData$RegRegtype %in% c(3,4)), ] #lager dataramme med kun startregistreringer
    RegDataSlutt <- RegData[which(RegData$RegRegtype %in% c(5,6)), ] #lager dataramme med kun slttregistreinger ((5=Sluttregistrering voksen, 6=Sluttregistrering ungdom/barn, 98=Avbrutt behandling voksen, 99=Avbrutt behandling ungdom/barn).)
    RegData <- merge(RegDataStart, RegDataSlutt, by.x ="ForlopsID", by.y = "RegTilhorendeStartReg", suffixes = c('','.y'))

    RegData$ciatemp <- (as.numeric(RegData$CIA30GlobalScore.y) - as.numeric(RegData$CIA30GlobalScore))
    RegData$ciatemp[RegData$ciatemp >7] <- 1
    RegData$ciatemp[RegData$ciatemp <=7 & RegData$ciatemp >=(-7)] <- 2
    RegData$ciatemp[RegData$ciatemp <(-7) & RegData$CIA30GlobalScore >=16] <- 3
    RegData$ciatemp[RegData$ciatemp <(-7) & RegData$CIA30GlobalScore <16] <- 4

    #sortere bort ugyldige rader
    koder <- c(1:4)
    indDum <- which(RegData$ciatemp %in% koder)#
    RegData <- RegData[indDum,] #velger de gyldige radene/fjerner de ugyldige

    RegData$VariabelGr <- factor(RegData$ciatemp, levels = koder)

    tittel <-'Endring CIA30GlobalScore'
    kommentar <- ''#'Bergeningen baserer seg på normadata fra ... og tar hensyn til måleusikkerheten...'

    #Til AndelerGrVar:
    RegData$Variabel <- recode_factor(RegData$VariabelGr,'1'='0', '2'='0','3'='1','4'='1')
    kommentarAndelerGrVar <- ''
  }



  if (valgtVar=='CIA_oX') {        #BRUKES I: AndelerGrVar, AndelTid
    RegData$CIA30GlobalScore <- as.numeric(as.character(RegData$CIA30GlobalScore))  # 23.05.18: må bruke as,numeric + as.character for å endre fra factor (antagelig kan det at variable nå er "factor" skyldes en endring i datadump)
    RegData <- RegData[which(RegData$CIA30GlobalScore >=0), ]
    RegData$Variabel[which(RegData$CIA30GlobalScore>16)] <- 1
    tittel <- 'Andel pasienter med CIA global skår over 16'
  }


















  if (valgtVar=='DiagVDiabetes') {                                                          #BRUKES I: AndelerGrVar
    #Velge bort ugyldige (NA) observasjoner
    koder <- c(0,1)
    RegData <- RegData[which(RegData$DiagVDiabetes %in% koder), ]

    RegData$Variabel <- RegData$DiagVDiabetes
    tittel <- 'Diabetes'
    sortAvtagende <- FALSE
  }


  if (valgtVar=='DiagVSF') {    #OBS Navnet er misvisende - ikke lenger bare voksene. 17.06.2019 ble barndediag(SF) lagt til, men det er kodet i preprosesser-fila. Må fikses generisk senere, se PreProsesser.R       #BRUKES I: Andeler
    #FIKS: Sy inn DiagBUAkse1 når det har tilkommet noen barn (det vil egentlig si: når HN-IKT har fikset variabel som bestilt vår 2018)
    retn <- 'V'

    #Velge bortugyldige rader tomme celler. Hvis ikke legger blir den en egen "level"/kategori.
    #Gjør dette ved bruk av droplevels, ved å først gi tomme celler verdien NA.
    RegData$DiagVSF <- as.factor(RegData$DiagVSF) #gjøres om til factor fordi den er character på server, og droplevels kan ikke brukes på character (19.07.18)
    RegData$DiagVSF[RegData$DiagVSF=='']=NA
    RegData$DiagVSF[RegData$DiagVSF=='null']=NA # linje måtte legges til fordi noen fikk verdien "null" i PROD-data
    RegData$DiagVSF = droplevels(RegData$DiagVSF)

    grtxtA <-c('F500','F501','F502','F503','F504','F505','F508','F509','F982','F983') #de 10 diagnosene som inngår i inklusjonskriteriene
    grtxtB <- levels(RegData$DiagVSF) #Lagt til mulighet for at nye diagnosekoder kan komme til å bli brukt. Se alternativt løsning fra NGER jmf. e-post 15.02.2017.
    grtxtC <- setdiff(grtxtB, grtxtA)
    grtxtA <- paste(substr(grtxtA,1,3),".", substr(grtxtA,4,nchar(grtxtA)), sep="") #legger til et punktum i A-vektor(".")
    grtxt <- append(grtxtA,grtxtC)

    grtxt <- grtxtA #endring 27.11.19 (Slett linje hvis endrer tilbake)linje lagt til i sammenheng med endring 4 linjer under,
    #endring fra koderB til koderA, etter at fagrådet ønsket å fjerne stolpen med z-diagnoser i FRM høstem 2018.

    koderA <- c("F500", "F501", "F502", "F503", "F504","F505", "F508", "F509","F982","F983")
    koderB <- levels(RegData$DiagVSF)#henter de diagnosekodene (kategoriene) som faktisk er registrert
    #c("F500", "F501", "F502", "F503", "F504","F505", "F508", "F509","F98.2","F98.3", "Z004")
    koder <- union(koderA, koderB)


    mz <- RegData[which(RegData$DiagVSF %in% koderB), ]
    lengthMZ <- length(mz$DiagVSF)                        #ikke i bruk enda...
    #laget etter endring 27.11.18, for å telle N som ikke er med i figuren (z-diag o.a)

    indDum <- which(RegData$DiagVSF %in% koderA) #endring 27.11.19 fra koderB til koderA, etter at fagrådet ønsket å fjerne stolpen med z-diagnoser i FRM høstem 2018.
    RegData <- RegData[indDum, ] #  RegData <- RegData[!(RegData$DiagVSF==""),]   #Veldig bra

    #FJERNET
    ##velge unike pasienter (OBS: Hold et øye med. Slår antaelig ikke ut før om en stund (først når en pasient har levert sin andre sluttregistrering)). Dvs at filteret er betydningsløst inntil da.
    #indDum2 <- which(unique(RegData$PasientID, incomparables = FALSE) > 0)
    #RegData <- RegData[indDum2, ]
    #FIKS: legge til generisk unique-filter i applikasjonen etter hvert.

    RegData$DiagVSF <- as.character(RegData$DiagVSF)



    RegData$VariabelGr <-RegData$DiagVSF
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koderA) #endring 27.11.19: Endret fra "koder"(uten A, kun "koder") til koderA, jmf. 2 andre endringer 27.11.18
    tittel <- 'Spiseforstyrrelsesdiagnose ICD-10' #'Spiseforstyrrelsesdiagnose ICD-10 (voksne)'
    xAkseTxt <- 'Diagnoser (ICD-10)'

    lengthUZ <- length(RegData$DiagVSF)
    Nandre <- lengthMZ-lengthUZ                             #gir N som ikke er med i figuren (z-diag o.a)
    #ikke i bruk enda...
    #laget etter endring 27.11.18.

    kommentar<- 'Registrerte pasienter som er kodet på annen måte enn med en spiseforstyrrelsesdiagnose, er ikke med i figuren.
    F.eks. kan dette gjelde pasienter som ble utredet for spiseforstyrrelser, men ikke endte opp med en SF-diagnose.
    I slike tilfeller er andre koder, som z-diagnoser benyttet, men de er altså ikke med her. '
    kommentar <- ''
  }

  if (valgtVar=='DiagVDSM5Hoved') {                                                                       #BRUKES I: Andeler
    #FIKS: Sy inn DiagBUDSM5Hoved
    retn <- 'V'
    #sortere bort ugyldige rader
    koder <- c(0:7)
    indDum <- which(RegData$DiagVDSM5Hoved %in% koder)
    RegData <- RegData[indDum,]

    grtxt <- c('307.52 \n Pica \n (ICD-10: F50.8)','307.53 \n Drøvtygging \n (ICD-10: F98.21)','307.59 \n Unnv./restr. \n (ICD-10: F50.8)','307.1 \n Anorexia N. \n (ICD-10: F50.0)','307.51 \n Bulimia N. \n (ICD-10: F50.2)','307.51 \n Overspisingsl. \n (ICD-10: F50.8)','307.59 \n Annen spesifisert \n (ICD-10: F50.8)','307.50 \n Uspesifisert \n (ICD-10: F50.9)') #klokebok sier "overspisningsforstyrrelse", nasjonale retn.linjer *lidelse, derfor forkortet til "*l."
    #RegData$DiagVDSM5Hoved <- as.character(RegData$DiagVDSM)

    RegData$VariabelGr <-RegData$DiagVDSM5Hoved
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
    tittel <- 'Spiseforstyrrelsesdiagnose DSM-5 (voksne)'
    xAkseTxt <- 'Diagnoser (DSM-5)'
    kommentar <- ''
  }


  if (valgtVar %in% c('EDEQ60GlobalScore',
                      'EDEQ60Restriksjon',
                      'EDEQ60Kroppsform',
                      'EDEQ60Spising',
                      'EDEQ60Vekt'))
  {                                                                                         #BRUKES I: GjsnGrVar; Andeler

    RegData[,valgtVar] <- as.numeric(gsub(",",".",RegData[,valgtVar])) #2020-04-27, bytter ut  , med . etter ny feil/endring i denne variabel i datadump(?)

    #FIX - legge formatfiks (fjerne null/NA) til preprosess-fila
    #Velge gyldige observasjoner
    #Fjerne null-verdier ved omkoding til NA - for å unngå warning (NAs introduced by coercion) når fikser bruker as.num..(as.char...) i kommando for valg av gyldige observasjoner
    # RegData[ ,valgtVar][RegData[ ,valgtVar]=='null']=NA
    # #Fjerne NA-verdier
    # RegData <- na.omit(RegData, cols=RegData[ ,valgtVar])
    #Velge gyldige observasjoner:
    RegData <- RegData[which(as.numeric(as.character(RegData[ ,valgtVar])) >=0), ] # 23.05.18: må bruke as,numeric + as.character for å endre fra factor (antagelig kan det at variable nå er "factor" skyldes en endring i datadump)

    kommentar <- '1:Hver pasient kan være med flere ganger hvis pasienten har mottatt behandling flere steder. N kan være derfor høyere enn antall unike pasienter.
    2:"Husk å bruke filteret "registreringstype". Uten filter vil hver pasient telles flere ganger - én gang for hver registreringstype'
    kommentar <- ''
    RegData$Variabel <- RegData[ ,valgtVar]
    deltittel <- switch(valgtVar,
                        EDEQ60GlobalScore = 'symptomtrykk: Global skår, EDE-Q 6.0',
                        EDEQ60Restriksjon = 'symptomtrykk: Restriksjon, EDE-Q 6.0',
                        EDEQ60Kroppsform = 'symptomtrykk: Kroppsform, EDE-Q 6.0',
                        EDEQ60Spising = 'symptomtrykk: Spising, EDE-Q 6.0',
                        EDEQ60Vekt = 'symptomtrykk: Vekt,EDE-Q 6.0')
    xAkseTxt <- switch(valgtVar,
                       EDEQ60GlobalScore = 'EDE-Q global skår',
                       EDEQ60Restriksjon = 'Restriksjon',
                       EDEQ60Kroppsform = 'Kroppsform',
                       EDEQ60Spising = 'Spising',
                       EDEQ60Vekt = 'Vekt')
    #til andelsfiguren:
    gr <- c(0,1,2,3,4,5,6)#c(0,2.5,6)
    RegData$VariabelGr <- cut(as.numeric(as.character(RegData[ ,valgtVar])), breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[-(length(gr))])#grtxt <- c('Under cut-off','Over cut-off')
    #til gjennomsnittsfigur (pre/post):
    RegData$VariabelGj <- RegData[ ,valgtVar] #henter f.eks. alle EDE-Q-global-verdiene
    kommentarPrePost <- 'Kun de pasientene som har levert både start og slutt-registreringer, kommer med i figuren.
    I tillegg er pasienter med manglende skårer på enten start- eller sluttregistreringen fjernet helt, selv om de faktisk hadde
    skårer på én av registreringstypene. Datofilteret fungerer slik på start/slutt-figurene, at.... ' #Årsaken til at intensiv hadde ulikt antall var i pre og post var at det uansett ikke var de samme individene/pårørende som ble målt i pre og post-gruppen.

    grtxt <- gsub("\\,", "-", grtxt) #erstatter komma med bindestrek
    subtxt <- 'Over/under klinisk cut-off'
    tittel <- switch(valgtVar,
                     EDEQ60GlobalScore = 'EDE-Q global skår',
                     EDEQ60Restriksjon = 'EDE-Q restriksjon',
                     EDEQ60Kroppsform = 'EDE-Q kroppsform',
                     EDEQ60Spising = 'EDE-Q spising',
                     EDEQ60Vekt= 'EDE-Q vekt')

    tittel2 <- switch(valgtVar,
                      EDEQ60GlobalScore = 'En skår over 2,5 anses som sannsynlig spiseforstyrrelse',
                      EDEQ60Restriksjon = '',
                      EDEQ60Kroppsform = '',
                      EDEQ60Spising = '',
                      EDEQ60Vekt= '')                           #Deaktivert ifm. årsr.17
  }

  if (valgtVar == 'EDEQ60GlobalScoreRCI') { #basert verdier gitt av Ø.Rø i e-post 06.05.2019
    retn <- 'V'

    RegDataStart <- RegData[which(RegData$RegRegtype %in% c(3,4)), ] #lager dataramme med kun startregistreringer
    RegDataSlutt <- RegData[which(RegData$RegRegtype %in% c(5,6)), ] #lager dataramme med kun slttregistreinger ((5=Sluttregistrering voksen, 6=Sluttregistrering ungdom/barn, 98=Avbrutt behandling voksen, 99=Avbrutt behandling ungdom/barn).)
    RegData <- merge(RegDataStart, RegDataSlutt, by.x ="ForlopsID", by.y = "RegTilhorendeStartReg", suffixes = c('','.y'))

    RegData$edeqtemp <- (as.numeric(RegData$EDEQ60GlobalScore.y) - as.numeric(RegData$EDEQ60GlobalScore))
    RegData$edeqtemp[RegData$edeqtemp >0.9] <- 1
    RegData$edeqtemp[RegData$edeqtemp <=0.9 & RegData$edeqtemp >=(-0.9)] <- 2
    RegData$edeqtemp[RegData$edeqtemp <(-0.9) & RegData$EDEQ60GlobalScore >=2.5] <- 3
    RegData$edeqtemp[RegData$edeqtemp <(-0.9) & RegData$EDEQ60GlobalScore <2.5] <- 4


    # #For å sørge for at utvalgsfunksjonen sorterer på sluttdato (i FigAndeler),
    # #endrer derfor HovedDato.y (sluttdato) til HovedDato, i to linjer:
    # RegData$HovedDato.z <- RegData$HovedDato
    # RegData$HovedDato <- RegData$HovedDato.y

    #sortere bort ugyldige rader
    koder <- c(1:4)
    indDum <- which(RegData$edeqtemp %in% koder)#
    RegData <- RegData[indDum,] #velger de gyldige radene/fjerner de ugyldige

    RegData$VariabelGr <- factor(RegData$edeqtemp, levels = koder)
    grtxt <- c('Verre', 'Uendret', 'Bedring', 'Symptomfri')

    tittel <-'Endring EDEQ60GlobalScore'
    kommentar <- ''#Beregeningen baserer seg på normadata fra ... og tar hensyn til måleusikkerheten...'

    #Til AndelerGrVar:
    RegData$Variabel <- recode_factor(RegData$VariabelGr,'1'='0', '2'='0','3'='1','4'='1')
    kommentarAndelerGrVar <- 'Figuren viser andelen pasienter som kan kategoriseres som friske ved behandlingslutt basert på endring i EDE-Q-skåre (nærmere definsjon
    hvor stor endring som regnes som relevant for å angi bedring, finnes i årsrapporten).
    Noen svakheter ved figuren er:
    1. Noen pasienter kan i hht. instrumenentet ha vært friske ved behandlingsstart , og hvis de i hht. instrumentet også var friske ved behandlingsslutt telles de likevel ikke med som friske.
    2. Generelt: Resultatene kan ikke regnes som representative. Det er lav dekningssgrad for sluttregistreringene i registeret.
    3. Slutt er ikke alltid slutt for hele forløpet, men ved det enkelte behanlingssted før overføring til nytt behandlingssted.
    F.eks. kan en pasient ha noe bedring ved Follo Døgn, og overføres til Follo DPS og ha noe bedring ved der, og selv om de totalt sett har blit bedre
    og friske, så kategoriseres de som uendret begge gangene.
    KImaal <- 90 # andel på 90%'
  }



  if (valgtVar=='EDEQ_oX') {        #BRUKES I: AndelerGrVar, AndelTid
    RegData$EDEQ60GlobalScore <- as.numeric(as.character(RegData$EDEQ60GlobalScore))  # 23.05.18: må bruke as,numeric + as.character for å endre fra factor (antagelig kan det at variable nå er "factor" skyldes en endring i datadump)
    RegData <- RegData[which(RegData$EDEQ60GlobalScore >=0), ]
    RegData$Variabel[which(RegData$EDEQ60GlobalScore>5)] <- 1
    tittel <- 'Andel pasienter med EDE-Q global skår over 5'
  }

  if (valgtVar == 'erMann')       {               #BRUKES I: AndelerGrVar, Andeler (muligens flytte til AndelerStablet når den er klar)
    grtxt <- c('Kvinne', 'Mann')
    #Velge kun gyldige rader:
    koder <- c(0,1)
    #RegData$VariabelGr <- 99
    indDum <- which(RegData$erMann %in% koder)
    RegData <- RegData[indDum, ]#RegData$VariabelGr[indDum] <- RegData$B12TidlBehSF[indDum]

    #FJERNET (heller bruke filter i applikasjon)
    ##velge unike pasienter
    #RegData <- RegData[which(RegData$RegRegtype == 1 | RegData$RegRegtype ==2 | RegData$RegRegtype ==3 | RegData$RegRegtype ==4), ]#velger kun startregistreringer og utredninger fordi ellers får vi dobbelt opp med pasienter (1=Utredning voksen, 2=Utredning ungdom/barn, 3=Startregistrering voksen, 4=Startregistrering ungdom/barn)
    #indDum2 <- which(unique(RegData$PasientID, incomparables = FALSE) > 0)
    #RegData <- RegData[indDum2, ]

    kommentar <- 'Pasienter med f.eks. både start- og slutt-registrering i perioden, telles med to ganger. du kan unngå dette ved å filtrere på registreringstype.
    Pasienten kan da likevel komme med to ganger i nasjonalt oversikt, hvis pasienten har mottatt behandling flere behandlingssteder i perioden'
    kommentar <- ''

    RegData$VariabelGr <- RegData$erMann
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1))
    tittel <- 'Kjønn'
    xAkseTxt <- 'Kjønn'

    #RegData$Variabel <- RegData$erMann
    RegData$Variabel[RegData$erMann==1] <- 0 #snur om på 0 og 1 til AndelerPerEnhet-figuren
    RegData$Variabel[RegData$erMann==0] <- 1
  }


  if (valgtVar %in% c('HCA01Atferd', 'HCA02Aktivitetsniva', 'HCA03Selvskade', 'HCA04Rusmisbruk', 'HCA05SkoleSprak',
                      'HCA06FysiskProblem', 'HCA07Hallusinasjoner', 'HCA08SomatiskSymp', 'HCA09EmosjonelleSymp','HCA10JevnaldrProbl',
                      'HCA11Egenomsorg', 'HCA12FamilieProbl', 'HCASkoleframmote', 'HCA14ProblKunnskap', 'HCA15Mangelinfo', 'H01Atferd',
                      'H02Selvskade','H03Rusmisbruk', 'H04KognitiveProbl','H05FysiskeProbl', 'H06Hallusinasjoner','H07Stemningsleie',
                      'H08AndreProbl','H09ForhAndre','H10ADLProbl','H11BoligProbl','H12YrkeProbl')) {
    #BRUKES I: Andeler
    retn <- 'H'
    grtxt <- c('Ingen problem', 'Lite problem som ikke \n krever tiltak', 'Mildt problem, \n men klart tilstede',
               'Moderat alvorlig problem', 'Alvorlig til svært alvorlig\n problem', 'Ukjent')

    #Velge kun gyldige rader:
    koder <- c(0:4,9)
    #RegData$VariabelGr <- 99
    indDum <- which(RegData[ ,valgtVar] %in% koder)
    RegData <- RegData[indDum, ]

    RegData$VariabelGr <- RegData[ ,valgtVar]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
    tittel <- switch(valgtVar,
                     HCA01Atferd = 'HONOSCA: 1. Problemer med forstyrrende,\n antisosial eller aggressiv atferd',
                     HCA02Aktivitetsniva = c('HONOSCA: 2.Problemer med høyt aktivitetsnivå,', 'oppmerksomhet eller konsentrasjon'),
                     HCA03Selvskade = 'HONOSCA: 3.Selvskade som ikke skyldes uhell',
                     HCA04Rusmisbruk = 'HONOSCA: 4.Problemer med alkohol, stoff eller løsemiddelmisbruk',
                     HCA05SkoleSprak = 'HONOSCA: 5.Problemer med skole- eller språkferdigheter',
                     HCA06FysiskProblem = 'HONOSCA: 6. Problemer pga fysisk sykdom eller funksjonshemming',
                     HCA07Hallusinasjoner = 'HONOSCA: 7.Problemer knyttet til hallusinasjoner, \n vrangforestillinger eller unormale persepsjoner',
                     HCA08SomatiskSymp = 'HONOSCA: 8. Problemer med somatiske symptomer \n uten kjent organisk grunnlag',
                     HCA09EmosjonelleSymp = 'HONOSCA: 9. Problemer med emosjonelle \n og relaterte symptomer',
                     HCA10JevnaldrProbl = 'HONOSCA: 10. Problemer med forhold til jevnaldrende',
                     HCA11Egenomsorg = 'HONOSCA: 11.Problemer med egenomsorg og uavhengighet',
                     HCA12FamilieProbl = 'HONOSCA: 12.Problemer med familieliv og forhold til andre',
                     HCASkoleframmote = 'HONOSCA: 13.Dårlig skoleframmøte',
                     HCA14ProblKunnskap = 'HONOSCA: 14.Problemer med kunnskap eller forståelse av \n egenarten av barnets/ungdommens vanskeligheter ',
                     HCA15Mangelinfo = 'HONOSCA: 15.Problemer med mangel på informasjon om \n tilbud eller behandling av barnets/ungdommens vanskeligheter',
                     H01Atferd = 'HoNOS: 1.Overaktiv, aggressiv, forstyrrende eller agitert atferd',
                     H02Selvskade = 'HoNOS: 2.Selvskade som ikke skyldes uhell',
                     H03Rusmisbruk = 'HoNOS: 3.Problemdrikking eller bruk av rusmiddel',
                     H04KognitiveProbl = 'HoNOS: 4.Kognitive problemer',
                     H05FysiskeProbl = 'HoNOS: 5.Problemer med fysisk sykdom eller funksjonshemming',
                     H06Hallusinasjoner = 'HoNOS: 6.Problemer forbundet med \n hallusinasjoner og vrangforestillinger',
                     H07Stemningsleie = 'HoNOS: 7.Problem med senket stemningsleie',
                     H08AndreProbl = 'HoNOS: 8.Andre mentale eller atferdsmessige problem',
                     H09ForhAndre = 'HoNOS: 9.Problemer med forhold til andre',
                     H10ADLProbl = 'HoNOS: 10. Problemer med dagliglivets aktiviteter',
                     H11BoligProbl = 'HoNOS: 11. Problemer med boligforhold',
                     H12YrkeProbl = 'HoNOS: 12. Problemer med yrke og aktiviteter')
  }


  if (valgtVar == 'H08aVelgTypeProbl') {                                                          #BRUKES I: Andeler
    retn <- 'H'
    grtxt <- c('Fobi', 'Angst', 'Tvangslidelse', 'Mentalt stress/spenninger', 'Dissosiativ', 'Somatoform',
               'Spiseproblemer', 'Søvnvansker', 'Seksuelt problem', 'Annet problem (Spesifiser)')

    #Velge kun gyldige rader:
    koder <- c(1:9,99)
    indDum <- which(RegData[ ,valgtVar] %in% koder)
    RegData <- RegData[indDum, ]

    RegData$VariabelGr <- RegData[ ,valgtVar]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
    tittel <- 'HoNOS: 8a Velg type problem'
  }


  if (valgtVar %in% c('MedAntidepressiva', 'MedBenzodiazepiner', 'MedNevroleptika', 'MedAnnenMedBeh')) {
    #BRUKES I: Andeler
    #FORBEDRE 2: Én søyle med "ja" er nok (trenger ikke egen søyle for "nei").

    #FORBEDRE 3: Alle medisinene i én figur.
    #FORBEDRET med riktig Teller 25.08.2017, ved å  først velge RegData med kun de som har svart 0 eller 1 på MedPsykofarmaka
    koder <- c(0,1)
    indDum1<- which(RegData[ ,'MedPsykofarmaka'] %in% koder)
    RegData <- RegData[indDum1, ]

    #for variablen, e.g. MedAntideptressiva har vi da (antagelig) verdiene NA, 0,1
    #kod da/erstatt NA med verdien 0

    #RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ] #LENA? Hjelpeargument?
    grtxt <- c('Nei', 'Ja')

    #Velge kun gyldige rader:
    koder <- c(0,1)
    indDum2 <- which(RegData[ ,valgtVar] %in% koder)
    RegData <- RegData[indDum2, ]

    RegData$VariabelGr <- RegData[ ,valgtVar]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
    tittel <- switch(valgtVar,
                     MedAntidepressiva = 'Psykofarmaka: Antidepressiva',
                     MedBenzodiazepiner = 'Psykofarmaka: Benzodiazepiner' ,
                     MedNevroleptika = 'Psykofarmaka: Nevroleptika',
                     MedAnnenMedBeh = 'Psykofarmaka: Annen medisinsk behandling')
  }

  if (valgtVar == 'MedBeintetthMaling') {
    grtxt <- c('Nei', 'Ja')
    koder <- c(0,1)
    indDum <- which(RegData[, valgtVar] %in% koder)
    RegData <- RegData[indDum, ]
    RegData$VariabelGr <- RegData[ ,valgtVar]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
    tittel <- '"Er beintetthetsmåling utført?"'
    KImaal <- 5
    KImaaltxt <- '>50'
  }


  if (valgtVar =='MedPsykofarmaka') {                                                             #BRUKES I: Andeler
    grtxt <- c('Nei', 'Ja')
    koder <- c(0,1)
    indDum <- which(RegData[, valgtVar] %in% koder)
    RegData <- RegData[indDum, ]
    RegData$VariabelGr <- RegData[ ,valgtVar]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
    tittel <- '"Psykofarmakologisk behandling?"'
  }

  if (valgtVar == 'MedPsykofarmakaAlle' ) {                                                                    #BRUKES I: Andeler
    #For flerevar=1 må vi omdefinere variablene slik at alle gyldige registreringer
    #(dvs. alle registreringer som skal telles med) er 0 eller 1. De som har oppfylt spørsmålet
    # er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
    # som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de
    # som 0.
    # Vi kan velge å sende tilbake alle variable som indikatorvariable, dvs. med 0,1,NA
    # Eller vi kan gjøre beregninga her og sende tilbake teller og nevner for den sammensatte variabelen
    koder <- c(0,1)
    indDum <- which(RegData[, 'MedPsykofarmaka'] %in% koder)
    RegData <- RegData[indDum, ]
    flerevar <- 1
    #Omkorder variabelen MedPsykofarmaka for å bruke den til en søyle for de som ikke har fått noen medikamentell beh.
    RegData$MedPsykofarmaka <- ifelse(RegData$MedPsykofarmaka ==1, 0, 1)

    variable <- c('MedAntidepressiva', 'MedBenzodiazepiner', 'MedNevroleptika', 'MedAnnenMedBeh', 'MedPsykofarmaka')
    #Sjekk <- RegData[,variable]
    retn <- 'V'
    grtxt <- c('Antidepressiva', 'Benzodiazepiner', 'Nevroleptika', 'Annen', 'Ingen')
    ind01 <- which(RegData[ ,variable] < 2, arr.ind = T) #Alle ja/neiz
    ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
    RegData[ ,variable] <- NA
    #RegData[,variable] <-
    RegData[ ,variable][ind01] <- 0
    RegData[ ,variable][ind1] <- 1
    #Beregne direkte:
    #apply(RegData[,variable], MARGIN=2, FUN=function(x) sum(x %in% 0:1))
    tittel <- 'Medikamentell behandling'
  }


  if (valgtVar  %in% c('MedBMI','MedIsoBMIBGS','MedIsoBMICDC')) {            #BRUKES I: Andeler, GjsnGrVar
    #Merknad: Ikke kjørt den rutinemessige "velg gyldige rader, deretter sørg for at tomme kategorier kommer med", men ser ut til å fungere likevel

    #SLETT DENNE KODESNUTTEN NÅR ALT KJØRER OK, FØR LANSERING AV SHINY-VERSJON:
    # #Format-fiks - for å unngå warning (NAs introduced by coercion) når fikser formatet med as.num..(as.char...).
    # #PS:Måtte kommenteres ut nov2018 etter at dette slo ut i feil i testversjonen med serverdata (gjelder GAMLE RAPPORTEKET)
    # #PS2: feb 2019,kommentert inn igjen ved arbeid lokalt med Rapporteket Shiny
    #       #Fjerne null-verdier ved omkoding til NA.
    #      RegData$MedBMI[RegData$MedBMI=='null']=NA
    #       #Fjerne NA-verdier
    #      RegData2 <- na.omit(RegData[,'MedBMI'])
    #      #RegData <- na.omit(RegData, cols="MedBMI")
    #       #Riktig format
    #       #RegData$MedBMI <- as.numeric(as.character(RegData$MedBMI))
    RegData[,valgtVar] <- as.numeric(gsub(",",".",RegData[,valgtVar])) #2020-04-27, bytter ut  , med . etter endring i denne variabel i datadump(?)
    RegData <- RegData[which(as.numeric(as.character(RegData[ ,valgtVar])) >=0), ]
    # #FIX - legge format-fikset ovenfor i preprossess-fila

    gr <- c(0,10,12.5,15,17.5,20,25,30,9999) #c(0,seq(10,30,2),150)
    #RegData$VariabelGr <- cut(as.numeric(RegData[ ,valgtVar]), breaks=gr, include.lowest= TRUE, right=FALSE) ## 23.05.18: må bruke as,numeric + as.character for å endre fra factor (antagelig kan det at variable nå er "factor" skyldes en endring i datadump)
    RegData$VariabelGr <- cut(as.numeric(as.character(RegData[ ,valgtVar])), breaks=gr, include.lowest= TRUE, right=FALSE) ## 23.05.18: må bruke as,numeric + as.character for å endre fra factor (antagelig kan det at variable nå er "factor" skyldes en endring i datadump)
    grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '30+')
    grtxt <- gsub("\\,", "-", grtxt) #erstatter komma med bindestrek
    xAkseTxt <- switch(valgtVar,
                       MedBMI = 'Kroppsmasseindeks (BMI)',
                       MedIsoBMIBGS = 'iso-BMI (BGS)',
                       MedIsoBMICDC = 'iso-BMI (CDC)')
    tittel <- switch(valgtVar,
                     MedBMI = 'BMI',
                     MedIsoBMIBGS = 'Iso-BMI \n (normdata: Bergen Growth Study (BGS))',
                     MedIsoBMICDC = 'Iso-BMI \n (normdata: Centers for Disease Control and Prevention (CDC))') #Deaktivert til årsr.17

    kommentar <- 'Husk å filtrere på registreringstype i venstre marg for at figuren skal gi mening. Pasienter med f.eks. både start- og slutt-registrering i perioden, vil ellers telles med to ganger.
    Pasienten kan da likevel komme med to ganger i nasjonal oversikt, hvis pasienten har mottatt behandling flere behandlingssteder i perioden.'
    kommentar <- ''
    kommentar2 <- 'Denne figuren benytter IKKE iso-BMI-verdier for de som er under 19 år (slik IkkeUndervektslutt gjør).'
    #til GjsnGrVar
    deltittel <- 'BMI'
    RegData$VariabelGj <- RegData$MedBMI #OBS, FIKS/KONTROLLER: Tar denne med seg feilverdier (9999) til beregningen i GjsnGrVar?
  }


  if(valgtVar == 'IkkeUndervektSlutt') {                                #BRUKES I: Andeler, AndelerPerEnhet.
    #KI (under utvikling)
    ##Format-fiks av MedBMI (FIKS - legge format-fiks i preprossess-fila):
    #Først:Fjerne null-verdier ved omkoding til NA - for å unngå warning (NAs introduced by coercion) når fikser formatet med as.num..(as.char...)
    RegData$MedBMI[RegData$MedBMI=='null']=NA
    #Så:Fjerne NA-verdier
    RegData <- RegData %>% setNames(make.names(names(.),unique = TRUE)) #Needed line because of error:"Error: Can't bind data because some arguments have the same name"
    RegData <- RegData %>% tidyr::drop_na(MedBMI)
    #RegData <- na.omit(RegData, cols="MedBMI") (na.omit fungerte ikke - fjernet for mange rader (alle som hadde NA på noen var))
    #Endelig:Riktig format
    RegData$MedBMI <- as.numeric(as.character(RegData$MedBMI))

    #FIX - legge datoformatfiks i preprosess-fila
    #Datoformat-fiks (Dplyr krever dato på POSIXct-format):
    #RegData$HovedDato <- as.POSIXct(RegData$HovedDato, format="%Y-%m-%d")#format="%Y-%m-d") #23.mai 2018: måtte endre linje pga. bug - antagelig har HNIKT endret datoformat i datadump siden sist
    #RegData$Dato <- as.POSIXct(RegData$Dato, format="%Y-%m-%d")#format="%Y-%m-d") #23.mai 2018: måtte endre linje pga. bug - antagelig har HNIKT endret datoformat i datadump siden sist

    #Velger å bruke IsoBMIBGS for de pasientene som har den beregnet (dvs pas. opp til 19 år (de med data for iso-BMI(BGS))) (rekoder MedBMI for spesifiserte rader)
    RegData$MedBMI <- as.numeric(as.character(RegData$MedBMI)) #Må først gjøre om til numeric
    RegData$MedIsoBMIBGS <- as.numeric(as.character(RegData$MedIsoBMIBGS)) #Må først gjøre om til numeric
    RegData$MedBMI[!is.na(RegData$MedIsoBMIBGS)] <- RegData$MedIsoBMIBGS[!is.na(RegData$MedIsoBMIBGS)] #De med data på MedIsoBMIBGS(Ev. bruke alder: RegData$MedBMI[RegData$PasientAlder<=19] <- RegData$MedIsoBMIBGS[RegData$PasientAlder<=19]

    ###Mål her: Finne FID på sluttregistreringene til de med BMI/IsoBMIBGS under 18.5 ved start, og bruke FID til å velge disses sluttreg
    #De med BMI u 18,5 ved start
    RegDataBMIu185Start <- RegData %>% filter(MedBMI <18.5, RegRegtype %in% c(1,2,3,4))
    FIDBMIu185start <- RegDataBMIu185Start$ForlopsID #deres FID
    #Sluttregistreringene til de med BMI u 18.5 ved start:
    RegDataSlutt_BMIu185Start <- RegData %>% filter(RegTilhorendeStartReg %in% FIDBMIu185start)
    RegData <- RegDataSlutt_BMIu185Start

    gr <- c(0,18.5,9999)
    RegData$VariabelGr <- cut(as.numeric(as.character(RegData$MedBMI)), breaks= gr, include.lowest= TRUE, right=FALSE)
    #RegData$VariabelGr <- factor(RegData$VariabelGr, levels = levels(RegData$VariabelGr))

    grtxt <- c("Undervektig", 'Ikke undervektig')
    xAkseTxt <- 'Status ved behandlingsslutt'
    tittel <- "Undervektstatus ved behandlingsslutt"
    tittel2 <- 'For pasienter under 19 år er iso-BMI (BGS) benyttet'

    #Til AndelerGrVar
    RegData$Variabel <- recode(RegData$VariabelGr, "[0,18.5)"="0","[18.5,1e+04]"="1")#,levels(RegData$VariabelGr)[2]="1")
    RegData$Variabel <- as.numeric(as.character(RegData$Variabel))
    tittel3 <- 'Andel pasienter uten undervekt (BMI på 18.5 eller mer)'
    KImaal <- 60 # andel på 60%

    kommentar <- ''#'Figuren viser andelen paseinter som var undervektig ved behandlingsstart, mensom ikke lenger var undervektig ved behandlingsslutt (bortfall av undervekt).
    # Kun pasienter som ved start som var undervektig er inkludert, mens de som hadde BMI over 18.5 allerede ved start ikke er med
    # Fravær av undervekt er for voksne definert som en BMI på 18.5 eller høyere. NB! "Ved behandlingsslutt" innebærer i denne figuren kun registreringer av
    # typen "slutt" og ikke registreringer av typen "avbrudd", ettersom det ikke registreres vekt i registreringer av typen "avbrudd".'
    ###Mulig feilkilde, vurdere hvordan slår inn: En pasient kan være under 19 år ved start og ha Iso-BMI-verdi. Ved slutt kan pasient ha blitt over 19 år og dermed benytte vanlig ISO-BMI-verdi.
    ##Det kan medføre at BMI-verdiene ikke nødvendigvis er sammenlignbare (den kan f.eks. tilsynelatende gå ned, selv om BMI-verdien har vært uforandret)
    kommentar2 <- 'Kvalitetsindikator. Mål for andel som ikke skal være undervektige ved behandlingsslutt er ikke satt'
    kommentar3 <- 'For pasienter yngre enn 19 år, er iso-BMI-verdier benyttet'
  }

  if (valgtVar == 'NegHend' ) {                                                                    #BRUKES I: Andeler
    #MERK: Ikke kontrollert figurens korrekthet enda.
    #For flerevar=1 må vi omdefinere variablene slik at alle gyldige registreringer
    #(dvs. alle registreringer som skal telles med) er 0 eller 1. De som har oppfylt spørsmålet
    # er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
    # som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de
    # som 0.
    #Vi kan velge å sende tilbake alle variable som indikatorvariable, dvs. med 0,1,NA
    #Eller vi kan gjøre beregninga her og sende tilbake teller og nevner for den sammensatte variabelen
    flerevar <- 1
    variable <- c('B17FysMishandl', 'B18PsykMishandl', 'B19Overgrep', 'B20Mobbing')
    #Sjekk <- RegData[,variable]
    retn <- 'H'
    grtxt <- c('Fysisk mishandl.', 'Psykisk mishandl.', 'Overgrep', 'Mobbing')
    ind01 <- which(RegData[ ,variable] < 2, arr.ind = T) #Alle ja/nei
    ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
    RegData[ ,variable] <- NA
    #RegData[,variable] <-
    RegData[ ,variable][ind01] <- 0
    RegData[ ,variable][ind1] <- 1
    #Beregne direkte:
    #apply(RegData[,variable], MARGIN=2, FUN=function(x) sum(x %in% 0:1))
    tittel <- 'Negative hendelser'
  }

  #UNDER UTARBEIDING: flere/antall negative hendelser

  #if (valgtvar == 'AntallNegHend') {
  #      flerevar <- 1
  #      variable <- c('B17FysMishandl', 'B18PsykMishandl', 'B19Overgrep', 'B20Mobbing')
  #      retn <- 'V'
  #      ind01 <- which(RegData[ ,variable] < 2, arr.ind = T) #Alle ja/nei
  #      ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
  #      RegData[ ,variable] <- NA
  #
  #      RegData[ ,variable][ind01] <- 0
  #      RegData[ ,variable][ind1] <- 1

  #     grtxt <- c('0','1','2','3','4')
  #      tittel <- 'Antall negative hendelser'

  #}


  if (valgtVar=='Norsktalende') {                                                                  #BRUKES I: Andeler
    #REPARERE (hvis vi ønsker denne variabelen): Kan være feil, og må sjekke om prod data har kodet denne numerisk (gamle testdata brukt når laget figur)
    #0=Nei, 1=Ja, 2= Delvis, 9=Ukjent
    grtxt <- c('Nei','Ja', 'Delvis', 'Ukjent')

    #Velge kun gyldige rader:
    koder <- c('Nei','Ja', 'Delvis', 'Ukjent')
    indDum <- which(RegData$Norsktalende %in% koder)
    RegData <- RegData[indDum, ]

    RegData$Norsktalende <- as.character(RegData$Norsktalende)

    RegData$VariabelGr <- RegData$Norsktalende
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
    tittel <- 'Norsktalende'
  }



  if (valgtVar == 'pasienttilfredshet') {                                                          #BRUKES I: Andeler
    #IKKE KLAR. Startet på, men må endre til en figur med Pasienttilfredshet (flerevar=1)
    #Her har vi ulik N for de ulike variablene.
    flerevar <- 1
    variable <- c('PT01OnsketInvolv', 'PT02BleInvolv', 'PT04KontaktBrukerorg',
                  'PT05OrientertBrukerorg')
    grtxt <- c('Ønske om andre nære i beh.', 'Nære involvert i beh.', 'Kontakt med brukerorg.?',
               'Fått info om brukerorg.')
    indDum <- which(RegData[ ,valgtVar] %in% c(0,1))
    RegData <- RegData[indDum, ]
    RegData$VariabelGr <- factor(RegData[ ,valgtVar], levels = c(0,1))
    tittel <- 'Pasienttilfredshet'
  }

  #Endre til en figur med Pasienttilfredshet (flerevar=1)
  if (valgtVar %in% c('PT01OnsketInvolv', 'PT02BleInvolv', 'PT04KontaktBrukerorg',
                      'PT05OrientertBrukerorg')) {                                                #BRUKES I: Andeler
    #Merknad: Ikke kjørt den rutinemessige "fjern gyldige rader, deretter sørg for at tomme kategorier kommer med"
    #  RegData <- RegData[which(RegData$ErOppflg == 0), ] #LENA? Hjelpeargument?
    grtxt <- c('Nei', 'Ja')
    indDum <- which(RegData[ ,valgtVar] %in% c(0,1))
    RegData <- RegData[indDum, ]
    RegData$VariabelGr <- factor(RegData[ ,valgtVar], levels = c(0,1))
    kommentar<-''#Måles ved slutt'
    tittel <- switch(valgtVar,
                     PT01OnsketInvolv = 'Pasienttilfredshet: Ønske om involvering av andre nære i behandlingen',
                     PT02BleInvolv = 'Pasienttilfredshet: Ble nære involvert i behandlingen?',
                     PT04KontaktBrukerorg = 'Pasienttilfredshet: Noen gang kontak med brukerorganisasjoner?',
                     PT05OrientertBrukerorg = 'Pasienttilfredshet: Informasjon om brukerorganisasjoner ila. behandlingen?')


  }

  if (valgtVar %in% c('PO01Forstod',	'PO02Tillit',	'PO03InfoDiagnose',	'PO04Tilpasset',	'PO05Involvert', 'PO06Organisert',
                      'PO07Tilfredsstillende'))
  {                                                      #BRUKES I: Andeler
    grtxt <- c('Ikke i det hele tatt','I liten grad', 'I noen grad', 'I stor grad', 'I svært stor grad', 'Ikke aktuelt') # OBS:'Ikke aktuelt' med  verdien 9 er fjernet
    indDum <- which(RegData[ ,valgtVar] %in% c(0:4,9))
    RegData <- RegData[indDum, ]
    RegData$VariabelGr <- factor(RegData[ ,valgtVar], levels = c(0:4,9))
    kommentar <- ''#'Måles ved slutt'
    tittel <- switch(valgtVar,
                     PO01Forstod = '1. Snakket behandlerne til deg slik at du forstod dem?',
                     PO02Tillit =  '2. Har du tillit til behandlernes faglige dyktighet?',
                     PO03InfoDiagnose = '3. Fikk du tilstrekkelig informasjon om din diagnose/dine plager?',
                     PO04Tilpasset = '4. Opplevde du at behandlingen var tilpasset din situasjon?',
                     PO05Involvert = '5. Var du involvert i avgjørelser som angikk din behandling?',
                     PO06Organisert = '6. Opplevde du at behandlingenhetens arbeid var godt nok organisert?',
                     PO07Tilfredsstillende = '7. Var hjelpen og behandlingen du fikk på behandlingsenheten, alt i alt, tilfredstillende?')
    # #Til FigAndelerGrVar:
    # RegData$Variabel <- recode_factor(RegData$VariabelGr,'0'='0', '1'='0','2'='1','3'='1','4'='1') # ''
    # kommentarAndelerGrVar <- ''
    # KImaal <- 50 # andel på 90%
  }

  if (valgtVar == 'PO08Tilgjengelighet')
  {                                                      #BRUKES I: Andeler
    grtxt <- c('Nei','Ja, men ikke lenge', 'Ja, ganske lenge', 'Ja, alt for lenge')
    indDum <- which(RegData[ ,valgtVar] %in% c(1:4))
    RegData <- RegData[indDum, ]
    RegData$VariabelGr <- factor(RegData[ ,valgtVar], levels = c(1:4))
    kommentar <- ''#'Måles ved slutt'
    tittel <- 'Måtte du vente for å få tilbud ved behandlingsenheten'
  }

  if (valgtVar == 'PO09Utbytte'){                                                      #BRUKES I: Andeler
    grtxt <- c('Ikke noe utbytte','Lite utbytte', 'En del utbytte', 'Stort utbytte', 'Svært stort utbytte') # OBS:'Ikke aktuelt' med  verdien 9 er fjernet
    indDum <- which(RegData[ ,valgtVar] %in% c(0:4))
    RegData <- RegData[indDum, ]
    RegData$VariabelGr <- factor(RegData[ ,valgtVar], levels = c(0:4))
    tittel <- 'Hvilket utbytte har du hatt, alt i alt, av behandlingen på behandlingsenheten?'
    kommentar <- ''#'Måles ved slutt'

    #Til FigAndelerGrVar:
    RegData$Variabel <- recode_factor(RegData$VariabelGr,'0'='0', '1'='0','2'='1','3'='1','4'='1') # ''
    kommentarAndelerGrVar <- 'Figuren viser andelen pasienter med "stort utbytte" eller "svært stort utbytte".
    Resterende andel har da rapportert "ikke noe utbytte", "lite utbytte" eller "en del utbytte".
    Også de som har rapportert "ikke aktuelt" er kategorisert sammen med resterende andel.'
    KImaal <- 50 # andel på 90%
  }

  if (valgtVar == 'PO10Pasientsikkerhet')
  {                                                      #BRUKES I: Andeler
    grtxt <- c('Ikke i det hele tatt','I liten grad', 'I noen grad', 'I stor grad', 'I svært stor grad', 'Ikke aktuelt')
    indDum <- which(RegData[ ,valgtVar] %in% c(0:4,9))
    RegData <- RegData[indDum, ]
    RegData$VariabelGr <- factor(RegData[ ,valgtVar], levels = c(0:4,9))
    kommentar <- ''#'Måles ved slutt'
    tittel <- 'Mener du at du på noen måte ble feilbehandlet (etter det du selv kan bedømme)?'
  }


  if (valgtVar == 'PO09UtbytteDikotom') { #vurderes som KI           #Brukes I: Andeler, AndelerGrVar
    grtxt <- c('Ikke noe,lite, en del utbytte', 'Stort/svært stort utbytte')
    #Velge kun gyldige rader:
    koder <- c(0:4)#koder <- c(0:4,9) #OBS/VURDER: verdien 9, ikke aktuelt er fjernet her, fordi kan ikke inngå i dikotomiseringen.
    #                                               Vurder hvordan det kan/bør hensyntas.
    indDum <- which(RegData$PO09Utbytte %in% koder)
    RegData <- RegData[indDum, ]#RegData$VariabelGr[indDum] <- RegData$B12TidlBehSF[indDum]

    #rekode til 0,1 (0,1,2 = 0, og 3,4 = 1)

    RegData$VariabelGr <- recode(RegData$PO09Utbytte,"c(0:2)=0; c(3,4)=1") #FIX(gir error per 21.08.2018: Argument 2 must be named, not unnamed)#rekoder til 0,1 (0,1,2 = 0, og 3,4 = 1)
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1))
    tittel <- 'Kjønn'
    xAkseTxt <- 'Kjønn'

    RegData$Variabel <- RegData$VariabelGr
  }

  if (valgtVar=='PT03Utfallsvurd') {                                                   #BRUKES I: Andeler, GjsnGrVar(KI)
    #Merknad: Ikke kjørt den rutinemessige "fjern gyldige rader, deretter sørg for at tomme kategorier kommer med"

    ##Bruk følgende to linjer om ønsker å snu på rekkefølgen på kategoriene (OBS indikatoren må da også endres (RegData$Variabel lenger ned) slik at den korresponderer.)
    # RegData$PT03Utfallsvurd <- recode(RegData$PT03Utfallsvurd, '1'='5', '2'='4', '3'='3', '4'='2', '5'='1')
    # grtxt <- c('Forverring','Uendret', 'Noe bedring', 'Klar bedring', 'Ikke noe problem lenger')

    grtxt <- c('Ikke noe problem lenger', 'Klar bedring', 'Noe bedring', 'Uendret', 'Forverring')
    # RegData$VariabelGr <- 99
    indDum <- which(RegData$PT03Utfallsvurd %in% c(1:5))
    RegData <- RegData[indDum, ]
    RegData$VariabelGr <- factor(RegData$PT03Utfallsvurd, levels = c(1:5))


    #Til GjsnGrVar:
    RegData$VariabelGj <- RegData$VariabelGr
    deltittel <- 'pasienttilfredshet'
    kommentarGjsnGrVar<- 'Bedringen er rapportert i fem deskriptive kategorier som er gjort om til en numerisk
    skala fra 1 til 5 som gjennomsnittet er beregnet ut fra'
    #Til AndelerGrVar:
    RegData$Variabel <- recode_factor(RegData$VariabelGr,'1'='1', '2'='1','3'='1','4'='0','5'='0')
    kommentarAndelerGrVar <- 'Figuren viser andelen pasienter med "noe bedring", "klar bedring" eller "ikke noe problem lenger". Resterende andel har da rapportert "uendret" eller "forverret".'
    KImaal <- 90 # andel på 90%
    kommentar<-''#Måles ved slutt'
    tittel <- 'Pasienttilfredshet: "Hvordan vurderer du utfallet av mottatt behandling?"'

  }


  if (valgtVar %in% c('RAND36FysFunk',
                      'RAND36RollebegFys',
                      'RAND36RollebegEmo',
                      'RAND36Tretthet',
                      'RAND36MentalHelse',
                      'RAND36SosialFunk',
                      'RAND36Smerte',
                      'RAND36GenHelse',
                      'RAND36EndringHelse'))
  {                                                                                         #BRUKES I: GjsnGrVar;
    #Merknad: Ikke kjørt den rutinemessige "fjern ugyldige rader" først
    RegData <- RegData[which(RegData[ ,valgtVar] >0), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    deltittel <- switch(valgtVar,
                        RAND36FysFunk = 'skåre: Global skåre, RAND-36',
                        RAND36RollebegFys = 'skåre: Rollefungering (fysisk), RAND-36',
                        RAND36RollebegEmo = 'skåre: Rollefungering (emosjonelt), RAND-36',
                        RAND36Tretthet = 'skåre: Vitalitet, RAND-36',
                        RAND36MentalHelse = 'skåre: Mental helse, RAND-36',
                        RAND36SosialFunk = 'skåre: Sosial fungering, RAND-36',
                        RAND36Smerte = 'skåre: Smerte, RAND-36',
                        RAND36GenHelse = 'skåre: Generell helse, RAND-36',
                        RAND36EndringHelse ='skåre: Endring i helse, RAND-36')
    xaksetxt <- switch(valgtVar,
                       RAND36FysFunk = 'Global skåre',
                       RAND36RollebegFys = 'Rollefungering (fysisk)',
                       RAND36RollebegEmo = 'Rollefungering (emosjonelt)',
                       RAND36Tretthet = 'Vitalitet',
                       RAND36MentalHelse = 'Mental helse',
                       RAND36SosialFunk = 'Sosial fungering',
                       RAND36Smerte = 'Smerte',
                       RAND36GenHelse = 'Generell helse',
                       RAND36EndringHelse ='Endring i helse')
  }


  if (valgtVar == 'RCIEDEQ60GlobalScore') {
    retn <- 'V'
    grtxt <- c('Reliabel \nforverring', 'Sannsynlig \nforverring', 'Ingen endring', 'Sannsynlig\npositiv endring', 'Reliabel \npositiv endring')

    RegDataStart <- RegData[which(RegData$RegRegtype >= 3 & RegData$RegRegtype <=4), ] #lager dataramme med kun startregistreringer
    RegDataSlutt <- RegData[which(RegData$RegRegtype >= 5 & RegData$RegRegtype <=6), ] #lager dataramme med kun slttregistreinger ((5=Sluttregistrering voksen, 6=Sluttregistrering ungdom/barn, 98=Avbrutt behandling voksen, 99=Avbrutt behandling ungdom/barn).)
    RegData <- merge(RegDataStart, RegDataSlutt, by="PasientID", suffixes = c('','.y'))

    RegData$edeqtemp <- (as.numeric(RegData$EDEQ60GlobalScore.y) - as.numeric(RegData$EDEQ60GlobalScore))/0.7405 #basert på Riksät-syntaks. Endre standard error of diff til norsk.
    RegData$edeqtemp[RegData$edeqtemp >=1.96] <- 1
    RegData$edeqtemp[RegData$edeqtemp <1.28 & RegData$edeqtemp >=-1.28] <- 2
    RegData$edeqtemp[RegData$edeqtemp < 1.96 & RegData$edeqtemp >-1.28] <- 3
    RegData$edeqtemp[RegData$edeqtemp <= -1.28 & RegData$edeqtemp > -1.96] <- 4
    RegData$edeqtemp[RegData$edeqtemp <= -1.96] <- 5

    #sortere bort ugyldige rader
    koder <- c(1:5)
    indDum <- which(RegData$edeqtemp %in% koder)#
    RegData <- RegData[indDum,] #velger de gyldige radene/fjerner de ugyldige

    RegData$VariabelGr <- factor(RegData$edeqtemp, levels = koder)

    tittel <-'RCI EDEQ60GlobalScore'
    kommentar <- ''
  }

  if (valgtVar == 'RCICIA30GlobalScore') {
    retn <- 'V'
    grtxt <- c('Reliabel \nforverring', 'Sannsynlig \nforverring', 'Ingen endring', 'Sannsynlig\npositiv endring', 'Reliabel \npositiv endring')

    RegDataStart <- RegData[which(RegData$RegRegtype >= 3 & RegData$RegRegtype <=4), ] #lager dataramme med kun startregistreringer
    RegDataSlutt <- RegData[which(RegData$RegRegtype >= 5 & RegData$RegRegtype <=6), ] #lager dataramme med kun slttregistreinger ((5=Sluttregistrering voksen, 6=Sluttregistrering ungdom/barn, 98=Avbrutt behandling voksen, 99=Avbrutt behandling ungdom/barn).)
    RegData <- merge(RegDataStart, RegDataSlutt, by="PasientID", suffixes = c('','.y'))

    RegData$ciatemp <- (as.numeric(RegData$CIA30GlobalScore.y) - as.numeric(RegData$CIA30GlobalScore))/4.9475 #basert på Riksät-syntaks. Endre standard error of diff til norsk.
    RegData$ciatemp[RegData$ciatemp >=1.96] <- 1
    RegData$ciatemp[RegData$ciatemp<1.28 & RegData$ciatemp >=-1.28] <- 2
    RegData$ciatemp[RegData$ciatemp < 1.96 & RegData$ciatemp >-1.28] <- 3
    RegData$ciatemp[RegData$ciatemp <= -1.28 & RegData$ciatemp > -1.96] <- 4
    RegData$ciatemp[RegData$ciatemp <= -1.96] <- 5

    #sortere bort ugyldige rader
    koder <- c(1:5)
    indDum <- which(RegData$ciatemp %in% koder)#
    RegData <- RegData[indDum,] #velger de gyldige radene/fjerner de ugyldige

    RegData$VariabelGr <- factor(RegData$ciatemp, levels = koder)

    tittel <-'RCI CIA30GlobalScore'
  }

  if (valgtVar=='RegHenvInstans') { #1 Pasienten selv, 2 Fastlege/primærlege, 3	Øvrig primærhelsetjenste, 4 Spesialisthelsetjenesten,
    # 5	Barnehage / skolesektor/PPT, 6 Sosialtjeneste / barnevern, 7 Politi/fengsel/rettsvesen,
    # 8 Rehabiliteringsinstitusjon/sykehjem, 9Privatpraktiserende spesialister, 99 Annet
    #BRUKES I: Andeler
    # MANGLER: (Rettet 28.06 slik at tom kat. kommer med, men...) kategoriene kommer ikke i den rekkefølgen de bør(slik de er listet i spørreskjemaene)
    retn <- 'H'
    grtxt <- c('Pasienten selv', 'Fastlege/primærlege', 'Øvrig primærhelsetjeneste', 'Spesialisthelsetjenesten',
               'Barnehage/skolesektor/PPT', 'Sosialtjeneste/barnevern', 'Politi/fengsel/rettsvesen', 'Rehabiliteringsinstitusjon/\nsykehjem', 'Privatpraktiserende \nspesialister', 'Annet')

    #sortere bort ugyldige rader:
    koder <- c(1:9,99)
    indDum <- which(RegData$RegHenvInstans %in% koder)
    RegData <- RegData[indDum,]

    RegData$VariabelGr <- RegData$RegHenvInstans
    RegData$VariabelGr <- factor(RegData$RegHenvInstans,levels=koder)

    xAkseTxt <- 'Henvisende instans'
    tittel <- 'Henvisende instans'
  }

  if (valgtVar %in% c('SCL90TGSI',
                      'SCL90TSomatisering',
                      'SCL90TTvang',
                      'SCL90TSensitivitet',
                      'SCL90TDepresjon',
                      'SCL90TAngst',
                      'SCL90TFiendlighet',
                      'SCL90TFobi',
                      'SCL90TParanoia',
                      'SCL90TPsykotisk'))
  {                                                                                         #BRUKES I: GjsnGrVar;

    RegData <- RegData[which(RegData[ ,valgtVar] >0), ]                                 #T viser til T-skår
    RegData$Variabel <- RegData[ ,valgtVar]
    deltittel <- switch(valgtVar,
                        SCL90TGSI = 'symptomtrykk: Global Severity Index, SCL-90-R',
                        SCL90TSomatisering = 'symptomtrykk: Somatisering, SCL-90-R,',
                        SCL90TTvang = 'symptomtrykk: Tvang, SCL-90-R',
                        SCL90TSensitivitet = 'symptomtrykk: Sensitivitet, SCL-90-R',
                        SCL90TDepresjon = 'symptomtrykk: Depresjon, SCL-90-R',
                        SCL90TAngst = 'symptomtrykk: Angst, SCL-90-R',
                        SCL90TFiendlighet = 'symptomtrykk: Fiendtlighet, SCL-90-R',
                        SCL90TFobi = 'symptomtrykk: Fobi, SCL-90-R',
                        SCL90TParanoia = 'symptomtrykk: Paranoia, SCL90-R',
                        SCL90TPsykotisk = 'symptomtrykk: Psykotisisme, SCL90-R')
    xaksetxt <- switch(valgtVar,
                       SCL90TGSI = 'Global Severity Index (T-skår; mean=50, std=10)',
                       SCL90TSomatisering = 'Somatisering (T-skår; mean=50, std=10)',
                       SCL90TTvang = 'Tvang (T-skår; mean=50, std=10)',
                       SCL90TSensitivitet = 'Sensitivitet (T-skår; mean=50, std=10)',
                       SCL90TDepresjon = 'Depresjon (T-skår; mean=50, std=10)',
                       SCL90TAngst = 'Angst (T-skår; mean=50, std=10)',
                       SCL90TFiendlighet = 'Fiendtlighet (T-skår; mean=50, std=10)',
                       SCL90TFobi = 'Fobi (T-skår; mean=50, std=10)',
                       SCL90TParanoia = 'Paranoia (T-skår; mean=50, std=10)',
                       SCL90TPsykotisk = 'Psykotisisme (T-skår; mean=50, std=10)')

    #til andelsfiguren:
    gr <- c(0,2.5,6)
    RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c('Under cut-off','Over cut-off')
    subtxt <- 'Over/under klinisk cut-off'
    tittel <- switch(valgtVar,
                     EDEQ60GlobalScore = 'EDE-Q global-skåre',
                     EDEQ60Restriksjon = 'EDE-Q restriksjon',
                     EDEQ60Kroppsform = 'EDE-Q kroppsform',
                     EDEQ60Spising = 'EDE-Q spising',
                     EDEQ60Vekt= 'EDE-Q vekt')                 #Deaktivert til årsr.17




  }


  if (valgtVar %in% c('SCL90GSI',
                      'SCL90Somatisering',
                      'SCL90Tvang',
                      'SCL90Sensitivitet',
                      'SCL90Depresjon',
                      'SCL90Angst',
                      'SCL90Fiendlighet',
                      'SCL90Fobi',
                      'SCL90Paranoia',
                      'SCL90Psykotisk'))
  {                                                                                         #BRUKES I: PrePost

    RegData[,valgtVar] <- as.numeric(gsub(",",".",RegData[,valgtVar])) #2020-04-27, bytter ut  , med . etter endring i denne variabel i datadump(?)

    RegData <- RegData[which(as.numeric(as.character(RegData[ ,valgtVar])) >=0), ]

    RegData$Variabel <- RegData[ ,valgtVar]
    deltittel <- switch(valgtVar,
                        SCL90GSI = 'symptomtrykk: Global Severity Index, SCL-90-R',
                        SCL90Somatisering = 'symptomtrykk: Somatisering, SCL-90-R,',
                        SCL90Tvang = 'symptomtrykk: Tvang, SCL-90-R',
                        SCL90Sensitivitet = 'symptomtrykk: Sensitivitet, SCL-90-R',
                        SCL90Depresjon = 'symptomtrykk: Depresjon, SCL-90-R',
                        SCL90Angst = 'symptomtrykk: Angst, SCL-90-R',
                        SCL90Fiendlighet = 'symptomtrykk: Fiendtlighet, SCL-90-R',
                        SCL90Fobi = 'symptomtrykk: Fobi, SCL-90-R',
                        SCL90Paranoia = 'symptomtrykk: Paranoia, SCL90-R',
                        SCL90Psykotisk = 'symptomtrykk: Psykotisisme, SCL90-R')
    xaksetxt <- switch(valgtVar,
                       SCL90GSI = 'Global Severity Index',
                       SCL90Somatisering = 'Somatisering',
                       SCL90Tvang = 'Tvang',
                       SCL90Sensitivitet = 'Sensitivitet',
                       SCL90Depresjon = 'Depresjon',
                       SCL90Angst = 'Angst',
                       SCL90Fiendlighet = 'Fiendtlighet',
                       SCL90Fobi = 'Fobi',
                       SCL90Paranoia = 'Paranoia',
                       SCL90Psykotisk = 'Psykotisisme')
    tittel <- switch(valgtVar,
                     SCL90GSI = 'Gjennomsnittlig symptomtrykk: Global Severity Index, SCL-90-R',
                     SCL90Somatisering = 'Gjennomsnittlig symptomtrykk: Somatisering, SCL-90-R,',
                     SCL90Tvang = 'Gjennomsnittlig symptomtrykk: Tvang, SCL-90-R',
                     SCL90Sensitivitet = 'Gjennomsnittlig symptomtrykk: Sensitivitet, SCL-90-R',
                     SCL90Depresjon = 'Gjennomsnittlig symptomtrykk: Depresjon, SCL-90-R',
                     SCL90Angst = 'Gjennomsnittlig symptomtrykk: Angst, SCL-90-R',
                     SCL90Fiendlighet = 'Gjennomsnittlig symptomtrykk: Fiendtlighet, SCL-90-R',
                     SCL90Fobi = 'Gjennomsnittlig symptomtrykk: Fobi, SCL-90-R',
                     SCL90Paranoia = 'Gjennomsnittlig symptomtrykk: Paranoia, SCL90-R',
                     SCL90Psykotisk = 'Gjennomsnittlig symptomtrykk: Psykotisisme, SCL90-R')

    #Til andelsfiguren:
    gr <- c(0,1,2,3,4)#c(0,2.5,6)
    RegData$VariabelGr <- cut(as.numeric(as.character(RegData[ ,valgtVar])), breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[-(length(gr))])


    kommentar <- ''

    #Til gjennomsnittsfigur (pre/post):
    RegData$VariabelGj <- RegData[ ,valgtVar] #henter f.eks. alle EDE-Q-global-verdiene
    kommentarPrePost <- ''


  }

  if (valgtVar %in% c('SDQGlobalScore',
                      'SDQEmoProb',
                      'SDQHyperaktivitet',
                      'SDQVennerProb',
                      'SDQSosialProb',
                      'SDQEksternScore',
                      'SDQInternScore',
                      'SDQImpactScore',
                      'SDQAtferdProb',
                      'SDQPGlobalScore',
                      'SDQPEmoProb',
                      'SDQPHyperaktivitet',
                      'SDQPVennerProb',
                      'SDQPSosialProb',
                      'SDQPEksternScore',
                      'SDQPInternScore',
                      'SDQPImpactScore',
                      'SDQPAtferdProb'
  ))
  {                                                                                   #Brukes i: Andeler, PrePost
    RegData <- RegData[which(as.numeric(as.character(RegData[ ,valgtVar])) >=0), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    deltittel <- switch(valgtVar,
                        SDQGlobalScore = '',
                        SDQEmoProb = '',
                        SDQHyperaktivitet = '',
                        SDQVennerProb = '',
                        SDQSosialProb = '',
                        SDQEksternScore = '',
                        SDQInternScore = '',
                        SDQImpactScore = '',
                        SDQAtferdProb = '',
                        SDQPGlobalScore = '',
                        SDQPEmoProb = '',
                        SDQPHyperaktivitet = '',
                        SDQPVennerProb = '',
                        SDQPSosialProb = '',
                        SDQPEksternScore = '',
                        SDQPInternScore = '',
                        SDQPImpactScore = '',
                        SDQPAtferdProb = ''
    )

    xaksetxt <- switch(valgtVar,
                       SDQGlobalScore = '',
                       SDQEmoProb = '',
                       SDQHyperaktivitet = '',
                       SDQVennerProb = '',
                       SDQSosialProb = '',
                       SDQEksternScore = '',
                       SDQInternScore = '',
                       SDQImpactScore = '',
                       SDQAtferdProb = '',
                       SDQPGlobalScore = '',
                       SDQPEmoProb = '',
                       SDQPHyperaktivitet = '',
                       SDQPVennerProb = '',
                       SDQPSosialProb = '',
                       SDQPEksternScore = '',
                       SDQPInternScore = '',
                       SDQPImpactScore = '',
                       SDQPAtferdProb = ''
    )

    kommentar <- ''#'En skåre på 20-40, regnes som svært høy (Goodman et al. 1998) (se youthinmind.org)'
    #Til andelsfiguren:
    gr <- c(0,10,20,30,40)#c(0,2.5,6)
    RegData$VariabelGr <- cut(as.numeric(as.character(RegData[ ,valgtVar])), breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[-(length(gr))])

    #Til gjennomsnittsfigur (pre/post):
    RegData$VariabelGj <- RegData[ ,valgtVar] #henter f.eks. alle EDE-Q-global-verdiene
    kommentarPrePost <- ''

  }

  if (valgtVar=='TidSykBehandling'){  #BRUKES I: Andeler    #KI("hvor godt/tidlig fanger man opp de syke?"). Interessant med tidsserie og sammenligning mellom enheter.)
    #Merknad: Ikke kjørt den rutinemessige "fjern ugyldige rader" først
    RegData$TidSykBehandling<-RegData$B12dArTilBehstart*12+RegData$B12dMndTilBehstart #lager variabel som gir år + måneder fra sykdom til behandlingsstart
    gr <- c(0,6,12,18,24,30,36,500)
    RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '36+')
    xAkseTxt <-'Måneder'
    tittel <- '"Hvor lang tid gikk det fra du ble syk til du fikk behandling?"'#'Tid fra sykdomsdebut til behandlingsstart (selvrapportert)'
  }

  if (valgtVar == 'BehTvang'){ #BRUKES I: AndelGrVar, AndelTid
    RegData$BehTvang <- as.numeric(as.character(RegData$BehTvang))
    RegData <- RegData[which(RegData$tvungenBeh >=0), ]



    grtxt <- c('Nei', 'Ja')
    #Velge kun gyldige rader:
    koder <- c(0,1)
    #RegData$VariabelGr <- 99
    indDum <- which(RegData$BehTvang %in% koder)
    RegData <- RegData[indDum, ]#RegData$VariabelGr[indDum] <- RegData$B12TidlBehSF[indDum]

    kommentar <- 'Pasienter med f.eks. både start- og slutt-registrering i perioden, telles med to ganger - med mindre du filtrerer på registreringstype.
    Pasienten kan da likevel komme med to ganger i nasjonalt oversikt, hvis pasienten har mottatt behandling flere behandlingssteder i perioden'

    RegData$VariabelGr <- RegData$BehTvang
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1))
    tittel <- 'Tvungent psykisk helsevern? (nei, ja)'
    xAkseTxt <- 'Tvungent psykisk helsevern'

    RegData$Variabel <- RegData$BehTvang #FIKS: Måtte legge denne til for å få FigANDELERGrVar til å kjøre. Hvorfor bruke både Variabel og VariabelGr? Kanskje endre i til VariabelGr i funksjonen FigAndelerGrVar.R

  }


  if (valgtVar=='VentetidKat') {                                                                  #BRUKES I: Andeler
    #Merknad: Ikke kontrollert korrekthet og ikke kjørt den rutinemessige "fjern ugyldige rader" først
    #Kvalitetsindikatorområdet ventetid?
    RegData$Ventetid <- difftime(strptime(RegData$RegHendelsesdato, format = "%Y-%m-%d"),
                                 strptime(RegData$RegHenvMottattDato, format = "%Y-%m-%d"),units="weeks")    # tid fra henvisning til start av behandlings eller utredning - variabel kalkulert av Mads: Forskjellem "henvisning mottatt dato" og "hendelsesdato"
    RegData$Ventetid <- as.numeric(RegData$Ventetid, units="weeks") #must make the atomic vector Ventetid numeric
    gr <- c(0,seq(2,16,2),150)
    RegData$VariabelGr <- cut(RegData$Ventetid, breaks=gr, include.lowest= TRUE, right=FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '16+')
    xAkseTxt <- 'Uker'
    tittel <- 'Ventetid: Tid fra "henvisning mottatt" til utredningdato/behandlingsstart'
  }

  if (valgtVar=='VentetidOver2Uker') {                                                            #BRUKES I: AndelerGrVar       #Kvalitetsindikator
    #Merknad: Ikke kjørt den rutinemessige "fjern ugyldige rader" først
    RegData$Ventetid <- difftime(strptime(RegData$RegHendelsesdato, format = "%Y-%m-%d"),
                                 strptime(RegData$RegHenvMottattDato, format = "%Y-%m-%d"),units="weeks")    # tid fra henvisning til start av behandlings eller utredning - variabel kalkulert av Mads: Forskjellem "henvisning mottatt dato" og "hendelsesdato"
    RegData$Ventetid <- as.numeric(RegData$Ventetid, units="weeks") #must make the atomic vector Ventetid numeric
    RegData$Variabel <- 99
    RegData$Variabel[which(RegData$Ventetid>2)] <- 1
    tittel <- 'Ventetid over 2 uker'
  }

  UtData <- list(RegData=RegData, grtxt=grtxt, xAkseTxt=xAkseTxt, retn=retn, KImaal=KImaal, KImaaltxt=KImaaltxt,
                 tittel=tittel, deltittel=deltittel,tittel2=tittel2, tittel3=tittel3, variable= variable, flerevar=flerevar, kommentar=kommentar)  #, sortAvtagende=sortAvtagende
  #RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
  return(invisible(UtData))

}
