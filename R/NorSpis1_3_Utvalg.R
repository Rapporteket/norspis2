#' Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.
#'
#' @param RegData
#' @param fargepalett
#' @param aar
#' @param datoFra
#' @param datoTil
#' @param datoFraSluttreg
#' @param datoTilSluttreg
#' @param minald
#' @param maxald
#' @param minbmistart
#' @param maxbmistart
#' @param regType
#' @param enhetstypeDogn
#' @param enhetstypeRegional
#' @param erMann
#' @param enhetsUtvalg
#' @param reshID
#' @param diagnose
#'
#' @return UtData En liste bestående av det filtrerte datasettet (RegData), utvalgstekst for
#' figur (UtvalgTxt), fargepalett, indekser for hovedgruppe og sammenlikningsgruppe (ind),
#' tekststreng som angir fargepalett,  om vi har sammenlikning eller ikke (medSml), tekst som angir
#' hovedgruppa (hovedgrTxt) og gruppa det evt. sammenliknes mot (smltxt)
#' @export
#'
#' @examples

NorSpis1_3_Utvalg <- function(RegData, fargepalett='BlaaOff', aar=0,
                          datoFra, datoTil,
                          datoFraSluttreg='', datoTilSluttreg='',
                          minald=0, maxald=130,
                          minbmistart=0, maxbmistart=99999,
                          regType='',
                          enhetstypeDogn='',
                          enhetstypeRegional='',
                          erMann='',
                          enhetsUtvalg=0,
                          reshID=0,
                          diagnose ='')  #grType=99,
{
  #Legge inn feilmelding hvis reshID=0 og enhetsUtvalg ulik 0.

  # Definer intersect-operator
  "%i%" <- intersect

  #Enhetsutvalg:
  #Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
  #trengs ikke data for hele landet:
  indEgen1 <- match(reshID, RegData$ReshId)
  if (enhetsUtvalg %in% c(2,3,4,6,7)) {
    RegData <- switch(as.character(enhetsUtvalg),
                      '2' = RegData[which(RegData$ReshId == reshID),],	#kun egen enhet
                      '3' = subset(RegData,EnhType==EnhType[indEgen1]),
                      '4' = RegData[which(RegData$EnhType == RegData$EnhType[indEgen1]),],	#kun egen shgruppe
                      '6' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),],	#sml region
                      '7' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),])	#kun egen region
  }

  RegData$EnhNavn <- as.factor(RegData$EnhNavn) #gjøres her om til faktor for å forsikre oss om at også tomme sykehus kommer med, e.g. i gjsngrvar
  Ninn <- dim(RegData)[1]
  indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)

  #for å lage indbmi, må jeg først lage og legge til RegData en variabel MedBMI start, siden denne ikke finnes (variabelen heter i registeret MedBMI uavh. av reg.type)
  #<- dette er gjort i preprosseseringsfila
  #indbmi <- which(RegData$MedBMIStart >= minbmistart & RegData$MedBMIStart <= maxbmistart)
  indbmi <- which(RegData$MedBMIStart >= minbmistart & RegData$MedBMIStart <= maxbmistart | is.na(RegData$MedBMIStart) )
  #indDumBMI <- which(RegData$RegRegtype %in% c(1,2,3,4))
  #indbmi <- which(as.numeric(as.character(RegData$MedBMIStart[indDumBMI])) >= minbmistart & as.numeric(as.character(RegData$MedBMIStart[indDumBMI])) <= maxbmistart & )

  #indRegType <- if (regType %in% 1:99) {which(RegData$RegRegtype == regType)} else {indRegType <- 1:Ninn}
  #indRegType <- if (regType[1] %in% 1:99) {which(RegData$RegRegType %in% as.numeric(regType))
  #} else {indRegType <- 1:Ninn}
  indRegType <- if (as.numeric(regType[1]) %in% 1:99) {which(RegData$RegRegtype %in% as.numeric(regType))} else {indRegType <- 1:Ninn}

  indEnhetstypeDogn <- if (enhetstypeDogn[1] %in% 0:2) {which(RegData$enhetstypeDogn %in% enhetstypeDogn)} else {indEnhetstypeDogn <- 1:Ninn}
  indEnhetstypeRegional <- if (enhetstypeRegional[1] %in% 0:1) {which(RegData$enhetstypeRegional %in% enhetstypeRegional)} else {indEnhetstypeRegional <- 1:Ninn}

  #FIKS filteret under slik at inkluderer barn også, når HN-IKT har fikset datadump med akse 1 fra BU diag:
  indDiagnose <- if (diagnose[1] %in% levels(RegData$DiagVSF)) {which( (RegData$DiagVSF %in% diagnose) | (RegData$ForlopsID %in% as.numeric(as.character(RegData$RegTilhorendeStartReg) )))}  else {indDiagnose <- 1:Ninn}
  #Over er føglende kodebit med for at man skal få med startregistreringene til pasientene med diagnose (RegData$ForlopsID %in% as.numeric(as.character(RegData$RegTilhorendeStartReg) ))
  #FIKS(TEST): Test at diagnosefilteret fungerer slik det skal - at de pasientene/reg.typene som skal med kommer med.

  indDato <- which(RegData$HovedDato >= as.POSIXlt(datoFra) & RegData$HovedDato <= as.POSIXlt(datoTil))

  #since the above often fails due to chaning date formats, we run another code for indDato if it is empty above:
  # if (length(indDato) == 0){}
  #   indDato <- which(RegData$HovedDato >= as.Date(datoFra, tz='UTC') & RegData$HovedDato <= as.Date(datoTil, tz='UTC'))
  # }

  if(datoFraSluttreg[1]>0) { #til pre/post-figur. Skal sørge for å få med sluttreg. innenfor datoområdet det er filtrert for, men også startreg. som tilhører men som kan være utenfor datområdet (f.eks. fra året før)
    #Lager en kompleks indeks in tre trinn:
    #1) markerer pasienter med regtype sluttreg og dato for disse innen valgt datointervall,
    indComplex1 <- which(RegData$RegRegtype %in% c(5,6,98,99)
                         & RegData$HovedDato >= as.Date(datoFraSluttreg, tz='UTC')
                         & RegData$HovedDato <= as.Date(datoTilSluttreg, tz='UTC')
    )
    #2)Bruker RegTilhorendeStartreg, kun fra de som er indeksert over, til å indeksere startregistreringer som skal være med
    indComplex2 <- which(RegData$ForlopsID %in% RegData[indComplex1,]$RegTilhorendeStartReg)
    #- samt ForlopsID som tilsvarer RegTilhorendeStartreg for de samme (altså sluttreg inne valgt datointervall)
    #3)Legger sammen de to indeksene,til en endelig indeks, som
    indDatoSlutt <- c(indComplex1,indComplex2)

  } else {indDatoSlutt <- 1:Ninn}


  #SLETT inDatoStart og indDatoSlutt, under, hvis ikke blir brukt i prePost
  #indDatoStart <- which(RegData$HovedDatoStartreg >= as.Date(datoFraStartreg, tz='UTC')
  #                      & RegData$HovedDatoStartreg <= as.Date(datoTilStartreg, tz='UTC'))
  #indDatoSlutt <- which(RegData$HovedDatoSluttreg >= as.Date(datoFraSluttreg, tz='UTC')
  #                      & RegData$HovedDatoSluttreg <= as.Date(datoTilSluttreg, tz='UTC'))

  indKj <- if (erMann %in% 0:1) {which(RegData$erMann == erMann)} else {indKj <- 1:Ninn}

  #FIX: Feil p.t:
  #indUnike <- if(Unike %in% 0:1) {which(RegData$RegRegtype %in% c(1,2,3,4) & RegData$UnikePasPerEnh == unique(RegData$UnikePasPerEnh))}

  indMed <- indAld %i% indKj %i% indDato %i% indRegType %i% indbmi %i%
    indEnhetstypeDogn %i% indEnhetstypeRegional %i% indDiagnose %i%
    indDatoSlutt

  RegData <- RegData[indMed,]

  N <- dim(RegData)[1]	#N=0 gir feilmelding

  utvalgTxt <- c(paste0(
    'Registreringsperiode: ', if (N>0) {min(RegData$HovedDato, na.rm=T)} else {datoFra},
    ' til ', if (N>0) {max(RegData$HovedDato, na.rm=T)} else {datoTil}),
    if ((minald>0) | (maxald<130)) {
      paste0('Pasienter fra ', if (N>0) {sprintf('%.1f',round(as.numeric(min(RegData$Alder, na.rm=T)),0))} else {minald}, #avrunder alder for å anonymisere(slik at den ikke skal kunne sammenholdes med regdato/registreringsperiode)
             ' til ', if (N>0) {sprintf('%.1f',round(as.numeric(max(RegData$Alder, na.rm=T)),0))} else {maxald}, ' år')},
    if (erMann %in% 0:1) {paste0('Kjønn: ', c('Kvinner', 'Menn')[erMann+1])},

    if ((minbmistart>0) | (maxbmistart<200)) {
      paste0('BMI ved start fra ', if (N>0) {sprintf('%.1f',min(RegData$MedBMIStart, na.rm=T))} else {minbmistart},
             ' til ', if (N>0) {sprintf('%.1f',max(RegData$MedBMIStart, na.rm=T))} else {maxbmistart})},

    if (regType[1] %in% c(1:6,98,99)) {paste0('Registreringstype: ',
                                              paste0(c('Kun utredning V', 'Kun utredning B/U',
                                                       'Start V', 'Start B/U',
                                                       'Slutt V', 'Slutt B/U',
                                                       rep('',91),
                                                       'Avbrudd V',
                                                       'Avbrudd B/U')[as.numeric(regType)], collapse=', '))}
  )
  #Enhetsutvalg:
  indEgen1 <- match(reshID, RegData$ReshId)
  if (enhetsUtvalg %in% c(1,2,3,6)) {	#Involverer egen enhet
    hovedgrTxt <- as.character(RegData$EnhNavn[indEgen1]) } else {
      hovedgrTxt <- switch(as.character(enhetsUtvalg),
                           '0' = 'Hele landet',
                           '4' = grTypetextstreng[RegData$EnhType[indEgen1]],
                           '5' = grTypetextstreng[RegData$EnhType[indEgen1]],
                           '7' = as.character(RegData$Region[indEgen1]),
                           '8' = as.character(RegData$Region[indEgen1]))
    }


  ind <- list(Hoved=0, Rest=0)
  smltxt <- ''
  if (enhetsUtvalg %in% c(0,2,4,7)) {		#Ikke sammenlikning
    medSml <- 0
    ind$Hoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
    ind$Rest <- NULL
  } else {						#Skal gjøre sammenlikning
    medSml <- 1
    if (enhetsUtvalg %in% c(1,3,6)) {	#Involverer egen enhet
      ind$Hoved <-which(RegData$ReshId==reshID) } else {
        ind$Hoved <- switch(as.character(enhetsUtvalg),
                            '5' = which(RegData$EnhType == RegData$EnhType[indEgen1]),	#shgr
                            '8' = which(RegData$Region == RegData$Region[indEgen1]))}	#region
    smltxt <- switch(as.character(enhetsUtvalg),
                     '1' = 'landet forøvrig',
                     '3' = paste0('andre ', grTypetextstreng[RegData$EnhType[indEgen1]]),	#RegData inneh. kun egen shgruppe
                     '5' = 'andre typer sykehus',
                     '6' = paste0(RegData$Region[indEgen1], ' forøvrig'),	#RegData inneh. kun egen region
                     '8' = 'andre regioner')
    ind$Rest <- switch(as.character(enhetsUtvalg),
                       '1' = which(RegData$ReshId != reshID),
                       '3' = which(RegData$ReshId != reshID),	#RegData inneh. kun egen shgruppe
                       '5' = which(RegData$EnhType != RegData$EnhType[indEgen1]),
                       '6' = which(RegData$ReshId !=reshID),	#RegData inneh. kun egen region
                       '8' = which(RegData$Region != RegData$Region[indEgen1]))
  }



  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett, ind=ind,
                 medSml=medSml, hovedgrTxt=hovedgrTxt,smltxt=smltxt)

  #      ind <- NorSpisUtvalg$ind
  #      medSml <- NorSpisUtvalg$medSml


  return(invisible(UtData))
}
