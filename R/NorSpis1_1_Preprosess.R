#' Preprosesser data fra NorSpis
#'
#' Denne funksjonen gir nytt navn til variabler og beregner eventuelt nye.
#'
#' @param RegData
#'
#' @return Data En liste med det filtrerte datasettet (og sykehusnavnet som tilsvarer reshID, ikke pt)
#' @export
#'
#' @examples

NorSpis1_1_Preprosess <- function(RegData=RegData)	#, reshID=reshID)
{
  #Kun ferdigstilte registreringer:
  # Rapporteket skal få kun levert ferdigstilte registreringer fra MRS/NHN. Men dette stemmer ikke alltid.
  RegData <- RegData[which(RegData$BasisRegStatus==1), ]
  #Kjønn
  # RegData$ErMann <- NULL

  #Riktig navn på regions-variabel:
  #Mangler regionsvariabel!!!
  #RegData$Region <- RegData$RHF

  # Endre variabelnavn:
  names(RegData)[which(names(RegData) == 'PasientAlder')] <- 'Alder'

  # Riktig format
  RegData$EnhNavn <- as.character(RegData$SykehusNavn)
  RegData$ReshId <- as.character(RegData$AvdRESH)

  #TODO: se om får lagt inn en logiskt test her, som avhengig av dataformatet på datoene velger den av
  #de to aktuelle as.Date-kallene under som er aktuell
  # #Brukes av server(også brukt 04.01.2021 for å få pakken og applikasjon til å kjøre):
  RegData$HovedDato <- as.Date(RegData$HovedDato, tz='UTC')#, format = "%d.%m.%y %H:%M")   #OBS! Følg med: usikker på hvorfor "as.Date" her, og om det slår ut.
  # Brukt med datadump-data (sist ifm. årsrapport 2019 (i 2020))
  #RegData$HovedDato <- as.Date(RegData$HovedDato, format="%d.%m.%y")

  #OBS -FEIL -  DATO UNDER BLIR NA, OG DERMED BLIR MND, KVARTAL, HALVAAR, AAR FEIL LENGER NED #(FIKS NÅR TRENGER VARIABLENE MND, KVARTAL, AAR UNDER)
  RegData$Dato <- as.POSIXlt(RegData$RegHendelsesdato, format="%Y-%m-%d")#format="%d.%m.%y %H:%M" #as.Date(RegData$RegHendelsesdato, tz='UTC', format="%d.%m.%y %H:%M")
  #RegData$Dato <- as.POSIXlt(RegData$RegHendelsesdato, format="Y-%m-%d")


  #slett?# RegData$HovedDato <- as.POSIXlt(RegData$HovedDato, format= "Y-%m-%d") #%d.%m.%y %H:%M    11. juni:Måtte endre tilbake #23.mai 2018: måtte endre linje pga. bug - antagelig har HNIKT endret datoformat i datadump siden sist
  #slett?# RegData$HovedDato <- as.Date(RegData$HovedDato, format='Y-%m-%d')#%d.%m.%y %H:%M   #OBS! Følg med: usikker på hvorfor "as.Date" her, og om det slår ut.
  #slett?# RegData$Dato <- as.POSIXlt(RegData$RegHendelsesdato, format="Y-%m-%d")  #d.%m.%y %H:%M

  #Brukes lokalt:
  # RegData$HovedDato <- as.POSIXlt(RegData$HovedDato, format= "%d.%m.%y %H:%M ")
  # RegData$HovedDato <- as.Date(RegData$HovedDato, format='%d.%m.%y %H:%M')
  # RegData$Dato <- as.POSIXlt(RegData$RegHendelsesdato, format="%d.%m.%y %H:%M")  #d.%m.%y %H:%M


  #	RegData$HovedDato <- as.POSIXlt(RegData$HovedDato, format= "Y-%m-%d") #%d.%m.%y %H:%M    11. juni:Måtte endre tilbake #23.mai 2018: måtte endre linje pga. bug - antagelig har HNIKT endret datoformat i datadump siden sist
  #Brukes på server:
  #	RegData$HovedDato <- as.Date(RegData$HovedDato, tz='UTC')#%d.%m.%y %H:%M   #OBS! Følg med: usikker på hvorfor "as.Date" her, og om det slår ut.

  #RegData$InnDato <- strptime(RegData$DateAdmittedIntensive, format="%Y-%m-%d") # %H:%M:%S" )  #"%d.%m.%Y"	"%Y-%m-%d"
  #RegData$Aar <- 1900 + strptime(RegData$DateAdmittedIntensive, format="%Y")$year

  #RegData$Dato <- as.Date(RegData$RegHendelsesdato, format="%d.%m.%y %H:%M")
  #RegData$Dato <- as.POSIXlt(RegData$RegHendelsesdato, format="Y-%m-%d") #format="%Y-%m-%d" #brukes for å lage mnd, kvartal, halvaar og aar (se under)

  RegData$Alder <- as.numeric(gsub(",",".",RegData$Alder)) #omgjør fra faktor etter at feilmelding oppstod ved nedlastning av datadump 08.06.18
  #2020-04-22, bytter ut  , med . etter ny feil/endring i denne variabel i datadump(?)
  #RegData$Alder <- as.numeric(as.character(RegData$Alder))

  # Nye variable:

  RegData$Mnd <- RegData$Dato$mon +1
  RegData$Kvartal <- ceiling(RegData$Mnd/3)
  RegData$Aar <- 1900 + RegData$Dato$year #strptime(RegData$Innleggelsestidspunkt, format="%Y")$year
  RegData$Halvaar <- ceiling(RegData$Mnd/6)
  #MedBMIStart til bruk i filteret (BMI ved start) (må først plukke ut kun start- og utredningsregistreringer,
  #og deretter endre verdiene på disse plasseringene slik at de blir lik MedBMI-verdiene):
  #indDumTEMP <- which(RegData$RegRegtype %in% c(1,2,3,4))
  #RegData$MedBMIStart <- as.numeric('')

  RegDataStart <- RegData[which(RegData$RegRegtype %in% c(1,2,3,4)), ] #lager dataramme med kun startregistreringer
  RegDataStart$MedBMIStart <- RegDataStart$MedBMI #lager ny variabel,MedBMIStart
  RegDataStart <- RegDataStart[,c("MedBMIStart", "ForlopsID")] #tar ut MedBMIStart i egen dataramme,
  #sammen med FID for å merge inn i RegData
  #by RegTilhorendestartreg

  #RegData <- merge(RegData, RegDataStart, by.x = "RegTilhorendeStartReg", by.y = "ForlopsID", all.x=TRUE, sort=FALSE) #OBS: følg med- det ser ut som datarammen blir sortert på et vis,
  #fordi alle med skårer på MedBMI start kommer først, deretter kommer NA
  #kan sees ved inspeksjon RegData$MedBMIStart
  #Finner dog ikke at det har noen innvirkning på resultater (enda)

  #gi nytt navn til kolonner for å merge/joine by, for å kunne bruke "join"
  RegDataStart$id <-RegDataStart$ForlopsID
  RegData$id <-RegData$RegTilhorendeStartReg
  #bruke join:
  RegData <- plyr::join(RegData, RegDataStart, by = "id", type="left")
  #RegData <- dplyr::left_join(RegData, RegDataStart, by = "id")

  RegData$MedBMIStart[which(RegData$RegRegtype %in% c(1,2,3,4))] <- RegData$MedBMI[which(RegData$RegRegtype %in% c(1,2,3,4))]
  RegData$MedBMIStart <- as.numeric(as.character(RegData$MedBMIStart))

  # #inspect:
  #RegData[,c("MedBMIStart", "MedBMI", "ForlopsID","RegTilhorendeStartReg", "RegRegtype")]
  # RegData4 <- RegData3[,c("MedBMIStart", "MedBMI", "ForlopsID","RegTilhorendeStartReg", "RegRegtype")]

  #Make column names unique. Must do because join function above create duplicted rows/row names, which creates trouble in NorSpisTabRegStatus
  colnames(RegData) <- make.unique(names(RegData))

  #RegData$MedBMIStart[which(RegData$RegRegtype %in% c(5,6,98,99))] <- 15 #koder om sluttreg til å få BMI -1
  #RegData$MedBMIStart[which(is.na(RegData$MedBMIStart))] <- 15 #koder om alle BMIStart=NA til -1

  #RegData$MedBMIStart[indDumTEMP] <-RegData$MedBMI[indDumTEMP] #ser OK ut, men OBS de som har verdien 0 på MedBMIStart er pasienter med andre reg.typer enn start og utredning, og de som har verdien NA er de med NA på start/utredningsreg. (altså antagelig "missing"/ikke besvart)


  #Variabelen UnikePasPerEnhet
  RegData$UnikePasPerEnh <- stringr::str_c(RegData$PasientID,RegData$AvdRESH)
  # ##UNIKE PASIENTER PER ENHET
  # #Velger først kun startreg/utredn:
  # RegData <- RegData[which(RegData$RegRegtype %in% c(1,2,3,4)), ]
  # <---OBS DENNE GÅR VEL IKKE, DA DEN ØDELEGGER HELE REGDATA-DATASETTET....
  # RegData$Variabel[which(RegData$Alder<18)] <- 1
  #
  # #En kombinasjon av PasientID og AvdRESH. Får dermed en koding som gir ulik ID for en pasient som har fått behandling på flere enheter,
  # #og lik ID hvis pasienten har fått behandling flere ganger samme sted. Kan bruke det til å sortere ut unike pasienter per enhet.
  # RegData$UnikPasPerEnh <- stringr::str_c(RegData$PasientID,RegData$AvdRESH)
  # RegData <- unique(RegData)
  # #FIKS senere: Tar p.t. mars 2019, ikke hensyn til hvilken av flere registreringer som skal beholdes (første eller siste), om en pasient
  # #har flere registreringer på en enhet.

  #Avdelingsnavn (legge til flere etterhvert som de kommer til):
  RegData$SykehusAvdNavn <- RegData$AvdRESH
  RegData$SykehusAvdNavn[RegData$SykehusAvdNavn==105806] <- 'RKSF, Levanger, Helse Nord-Trøndelag HF'
  RegData$SykehusAvdNavn[RegData$SykehusAvdNavn==109979] <- 'RASP, OUS HF'
  RegData$SykehusAvdNavn[RegData$SykehusAvdNavn==110361] <- 'Spiseforstyrrelsespoliklinikken, Gaustad, OUS HF'
  RegData$SykehusAvdNavn[RegData$SykehusAvdNavn==700698] <- 'RSS, UNN HF'
  RegData$SykehusAvdNavn[RegData$SykehusAvdNavn==700821] <- 'RESSP, NLSH HF'
  RegData$SykehusAvdNavn[RegData$SykehusAvdNavn==707383] <- 'Follo, enhet for SF, poliklinikk, Akershus u. HF'
  RegData$SykehusAvdNavn[RegData$SykehusAvdNavn==4204191] <-'Capio Anoreksi Senter'
  RegData$SykehusAvdNavn[RegData$SykehusAvdNavn==4207041] <- 'Follo, Enhet for SF, døgn, Akershus u. HF'
  RegData$SykehusAvdNavn[RegData$SykehusAvdNavn==4209009] <- 'BUPA poliklinikk Nordre Vestfold, SiV HF'
  RegData$SykehusAvdNavn[RegData$SykehusAvdNavn==4210562] <- 'BUP, Bodø, NLSH HF'
  RegData$SykehusAvdNavn[RegData$SykehusAvdNavn==4210626] <- 'BUP, Mosjøen, NLSH HF'
  RegData$SykehusAvdNavn[RegData$SykehusAvdNavn==4210825] <-  'DPS,Mosjøen, NLSH HF'
  RegData$SykehusAvdNavn[RegData$SykehusAvdNavn==107026] <- 'Seksjon for SF, Helse Bergen HF'


  #ENHETSTYPEVARIABLER:
  #hver enhet må tilordnes en verdi på følgende fem variabler:
  #(variablene eksiterer ikke i registeret fra før av, men skisseres under,
  #slik at man kan tilordne verdier derunder igjen, hvor variablene lages)
  #1: enhetstypeDogn
  #2: enhetstypeRegional
  #3: enhetstypeSpesialisert
  #4: enhetstypeVoksen
  #5: enhetstypeN
  #Av antall kategorier under, gir dette totalt mulighet til å filtere på alt fra 1 til  3*2*3*4*n= 72*n kategorier av enhetstyper, etter behov,
  #eller ved kombinasjon å lage ny enhetstype-variabel med tilsvarende antall kategorier, etter behov.
  #Ytterligere muligheter man eventuelt kan legge til rette for senere:
  #6: enhetstypeTvang
  #7: enhetstypeNSenger
  #8: enhetstypePrivat
  #9: enhetstypeAkutt
  #10: tilbudDagDognPol
  #11: tilbudGruppeIndividuell


  #FIKS: Kontroller at det stemmer at
  #1: Alle regionale enheter, minus Bergen, har poliklinisk tilbud (og at disse også inkluderes i NorSpis)
  #2: BUPA, poliklinikk, Nordre Vestfold, kun har poliklinisk behandlingstilbud, og ikke døgn. I motsatt fall,
  #hvor mange sengeplasser?

  #1:avdelingstypeDogn
  #c(0,1,2)
  #c('Poliklinikk', 'Døgnbehandling', 'Poliklinikk og døgnbehandling'))
  RegData$enhetstypeDogn <- RegData$AvdRESH
  RegData$enhetstypeDogn[RegData$enhetstypeDogn==105806] <- 2#'RKSF'
  RegData$enhetstypeDogn[RegData$enhetstypeDogn==109979] <- 2#'RASP'
  RegData$enhetstypeDogn[RegData$enhetstypeDogn==110361] <- 0#'Spiseforstyrrelsespoliklinikken, Gaustad'
  RegData$enhetstypeDogn[RegData$enhetstypeDogn==700698] <- 2#'RSS'
  RegData$enhetstypeDogn[RegData$enhetstypeDogn==700821] <- 2#'RESSP'
  RegData$enhetstypeDogn[RegData$enhetstypeDogn==707383] <- 0#'Follo, enhet for SF, poliklinikk'
  RegData$enhetstypeDogn[RegData$enhetstypeDogn==4204191] <- 1#'Capio Anoreksi Senter'
  RegData$enhetstypeDogn[RegData$enhetstypeDogn==4207041] <- 1#'Follo, Enhet for SF, døgn'
  RegData$enhetstypeDogn[RegData$enhetstypeDogn==4209009] <- 0#'BUPA poliklinikk Nordre Vestfold'
  RegData$enhetstypeDogn[RegData$enhetstypeDogn==4210562] <- 0#'BUP, Bodø'
  RegData$enhetstypeDogn[RegData$enhetstypeDogn==4210626] <- 0#'BUP, Mosjøen'
  RegData$enhetstypeDogn[RegData$enhetstypeDogn==4210825] <- 0#'DPS,Mosjøen'
  RegData$enhetstypeDogn[RegData$enhetstypeDogn==107026] <- 2#'Seksjon for SF, Helse Bergen HF'

  #2:enhetstypeRegional
  #c(0,1)
  #c('Ikke regional', 'Regional')
  RegData$enhetstypeRegional  <- RegData$AvdRESH
  RegData$enhetstypeRegional [RegData$enhetstypeRegional ==105806] <- 1#'RKSF'
  RegData$enhetstypeRegional [RegData$enhetstypeRegional ==109979] <- 1#'RASP'
  RegData$enhetstypeRegional [RegData$enhetstypeRegional ==110361] <- 0#'Spiseforstyrrelsespoliklinikken, Gaustad'
  RegData$enhetstypeRegional [RegData$enhetstypeRegional ==700698] <- 1#'RSS'
  RegData$enhetstypeRegional [RegData$enhetstypeRegional ==700821] <- 1#'RESSP'
  RegData$enhetstypeRegional [RegData$enhetstypeRegional ==707383] <- 0#'Follo, enhet for SF, poliklinikk'
  RegData$enhetstypeRegional [RegData$enhetstypeRegional ==4204191] <- 0#'Capio Anoreksi Senter'
  RegData$enhetstypeRegional [RegData$enhetstypeRegional ==4207041] <- 0#'Follo, Enhet for SF, døgn'
  RegData$enhetstypeRegional [RegData$enhetstypeRegional ==4209009] <- 0#'BUPA poliklinikk Nordre Vestfold'
  RegData$enhetstypeRegional [RegData$enhetstypeRegional ==4210562] <- 0#'BUP, Bodø'
  RegData$enhetstypeRegional [RegData$enhetstypeRegional ==4210626] <- 0#'BUP, Mosjøen'
  RegData$enhetstypeRegional [RegData$enhetstypeRegional ==4210825] <- 0#'DPS,Mosjøen'
  RegData$enhetstypeRegional [RegData$enhetstypeRegional ==107026] <- 1  #'Seksjon for SF, Helse Bergen HF'

  #3: enhetstypeSpesialisert
  #c(0,1,2,3,4)
  #c('Allmennpsykiatrisk uten eget tilbud SF', 'Allmennpsykiatrisk med eget tilbud SF','Ambulant tjeneste SF', 'Spesialpoliklinikk', )
  # "Allmennpsykiatrisk uten eget tilbud SF" innebærer at behandlingstilbudet ved enheten ikke er organisert med spesialsiert/eget tilbud til pas. med SF,
  # men at man jobber etter en generalismodell. Selv om tilbudet på enhetsnivå ikke er spesialisert, kan behandler likevel gi en spesialisert behandling (e.g. CBT-E).
  # "Allmennpsykiatrisk med eget tilbud SF'", innebærer dedikert tilbud til pas. med SF, f.eks. spiseteam eller gruppeterapi til pas. med SF.
  # Ambulant tjeneste SF
  # Spesialpoliklinikk - behandler kun pasienter med SF
  # Regional enhet

  RegData$enhetstypeSpesialisert  <- RegData$AvdRESH
  RegData$enhetstypeSpesialisert [RegData$enhetstypeSpesialisert ==105806] <- 2#'RKSF'
  RegData$enhetstypeSpesialisert [RegData$enhetstypeSpesialisert ==109979] <- 2#'RASP'
  RegData$enhetstypeSpesialisert [RegData$enhetstypeSpesialisert ==110361] <- 2#'Spiseforstyrrelsespoliklinikken, Gaustad'
  RegData$enhetstypeSpesialisert [RegData$enhetstypeSpesialisert ==700698] <- 2#'RSS'
  RegData$enhetstypeSpesialisert [RegData$enhetstypeSpesialisert ==700821] <- 2#'RESSP'
  RegData$enhetstypeSpesialisert [RegData$enhetstypeSpesialisert ==707383] <- 2#'Follo, enhet for SF, poliklinikk'
  RegData$enhetstypeSpesialisert [RegData$enhetstypeSpesialisert ==4204191] <- 2#'Capio Anoreksi Senter'
  RegData$enhetstypeSpesialisert [RegData$enhetstypeSpesialisert ==4207041] <- 2#'Follo, Enhet for SF, døgn'
  RegData$enhetstypeSpesialisert [RegData$enhetstypeSpesialisert ==4209009] <- 1#'BUPA poliklinikk Nordre Vestfold'
  RegData$enhetstypeSpesialisert [RegData$enhetstypeSpesialisert ==4210562] <- 1#'BUP, Bodø'
  RegData$enhetstypeSpesialisert [RegData$enhetstypeSpesialisert ==4210626] <- 0#'BUP, Mosjøen'
  RegData$enhetstypeSpesialisert [RegData$enhetstypeSpesialisert ==4210825] <- 0#'DPS,Mosjøen'

  #4: enhetstypeVoksen
  #c(0,1,2,3)
  #c('Barn', 'Voksen', 'Barn og voksen', 'Annen')
  #"Annen" kan f.eks. være egendefinerte, hvor man har pasienter fra 16 år og oppover, altså ikke behandling som er rent for barn, rent for voksen, eller rent begge deler
  RegData$enhetstypeVoksen <- RegData$AvdRESH
  RegData$enhetstypeVoksen[RegData$enhetstypeVoksen==105806] <- 1#'RKSF'
  RegData$enhetstypeVoksen[RegData$enhetstypeVoksen==109979] <- 2#'RASP'
  RegData$enhetstypeVoksen[RegData$enhetstypeVoksen==110361] <- 1#'Spiseforstyrrelsespoliklinikken, Gaustad'
  RegData$enhetstypeVoksen[RegData$enhetstypeVoksen==700698] <- 0#'RSS'
  RegData$enhetstypeVoksen[RegData$enhetstypeVoksen==700821] <- 1#'RESSP'
  RegData$enhetstypeVoksen[RegData$enhetstypeVoksen==707383] <- 1#'Follo, enhet for SF, poliklinikk'
  RegData$enhetstypeVoksen[RegData$enhetstypeVoksen==4204191] <- 0#'Capio Anoreksi Senter'
  RegData$enhetstypeVoksen[RegData$enhetstypeVoksen==4207041] <- 1#'Follo, Enhet for SF, døgn'
  RegData$enhetstypeVoksen[RegData$enhetstypeVoksen==4209009] <- 0#'BUPA poliklinikk Nordre Vestfold'
  RegData$enhetstypeVoksen[RegData$enhetstypeVoksen==4210562] <- 0#'BUP, Bodø'
  RegData$enhetstypeVoksen[RegData$enhetstypeVoksen==4210626] <- 0#'BUP, Mosjøen'
  RegData$enhetstypeVoksen[RegData$enhetstypeVoksen==4210825] <- 1#'DPS,Mosjøen'

  #5: enhetstypeN <- ---
  #c('0-9', '10-49', '50-99', '100-149', '150-199', '>200')
  # i utagangspunktet kontinuerlig, men kan lage kategorier som f.eks. de som er skissert i linjen ovenfor
  #grunnlag p.t. er NPR-telling med tall fra år 2014/2015 alt ettersom hva man hadde tall for.
  RegData$enhetstypeN <- RegData$AvdRESH
  RegData$enhetstypeN[RegData$enhetstypeN==105806] <- 163#'RKSF'
  RegData$enhetstypeN[RegData$enhetstypeN==109979] <- 318#'RASP'
  RegData$enhetstypeN[RegData$enhetstypeN==110361] <- NA#'Spiseforstyrrelsespoliklinikken, Gaustad'
  RegData$enhetstypeN[RegData$enhetstypeN==700698] <- 19#'RSS'
  RegData$enhetstypeN[RegData$enhetstypeN==700821] <- 52#'RESSP'
  RegData$enhetstypeN[RegData$enhetstypeN==707383] <- 58/2#'Follo, enhet for SF, poliklinikk'
  RegData$enhetstypeN[RegData$enhetstypeN==4204191] <- 36#'Capio Anoreksi Senter'
  RegData$enhetstypeN[RegData$enhetstypeN==4207041] <- 58/2#'Follo, Enhet for SF, døgn'
  RegData$enhetstypeN[RegData$enhetstypeN==4209009] <- 41#'BUPA poliklinikk Nordre Vestfold'
  RegData$enhetstypeN[RegData$enhetstypeN==4210562] <- 44#'BUP, Bodø'
  RegData$enhetstypeN[RegData$enhetstypeN==4210626] <- 10#'BUP, Mosjøen'
  RegData$enhetstypeN[RegData$enhetstypeN==4210825] <- 11#'DPS,Mosjøen'



  ##6: enhetstypeGrVarAarsr2017 (TIL GRVAR-FIGURER I ÅRSRAPPORTEN 2017):
  #RegData$enhetstypeGrVarAars2017 <- RegData$AvdRESH
  #RegData$enhetstypeGrVarAars2017[RegData$enhetstypeGrVarAars2017==105806] <- 1#'RKSF'
  #RegData$enhetstypeGrVarAars2017[RegData$enhetstypeGrVarAars2017==109979] <- 1#'RASP'
  #RegData$enhetstypeGrVarAars2017[RegData$enhetstypeGrVarAars2017==110361] <- 0#'Spiseforstyrrelsespoliklinikken, Gaustad'
  #RegData$enhetstypeGrVarAars2017[RegData$enhetstypeGrVarAars2017==700698] <- 1#'RSS'
  #RegData$enhetstypeGrVarAars2017[RegData$enhetstypeGrVarAars2017==700821] <- 1#'RESSP'
  #RegData$enhetstypeGrVarAars2017[RegData$enhetstypeGrVarAars2017==707383] <- 0#'Follo, enhet for SF, poliklinikk'
  #RegData$enhetstypeGrVarAars2017[RegData$enhetstypeGrVarAars2017==4204191] <- 0#'Capio Anoreksi Senter'
  #RegData$enhetstypeGrVarAars2017[RegData$enhetstypeGrVarAars2017==4207041] <- 0#'Follo, Enhet for SF, døgn'
  #RegData$enhetstypeGrVarAars2017[RegData$enhetstypeGrVarAars2017==4209009] <- 0#'BUPA poliklinikk Nordre Vestfold'
  #RegData$enhetstypeGrVarAars2017[RegData$enhetstypeGrVarAars2017==4210562] <- 0#'BUP, Bodø'
  #RegData$enhetstypeGrVarAars2017[RegData$enhetstypeGrVarAars2017==4210626] <- 0#'BUP, Mosjøen'
  #RegData$enhetstypeGrVarAars2017[RegData$enhetstypeGrVarAars2017==4210825] <- 0#'DPS,Mosjøen'


  ##ÅRSRAPPORTEN - ha med denne linjen når kjører GRVAR-figurene
  #RegData <- RegData [which(RegData$enhetstypeRegional==1), ]
  ##ÅRSRAPPORTEN#



  return(invisible(RegData))
}

