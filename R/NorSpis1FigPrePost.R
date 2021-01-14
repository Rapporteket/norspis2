#' Søylediagram som viser sentralmå, gjennomsnitt/median, ved to ulike tidspunkter (før og etter/ved starten og slutten av behandlingen)
#'
#' Pre/post-sammenligningsfigur har litt anndre datofiltre enn f.eks. figur for andelsfordelingsfigur. Dette fordi:
#' - datofilter for f.eks. andelsfordelingsfigur brukes i kombinasjon med filter for registreingstype
#' - mens datofilter for pre/post-figur ikke kan brukes i kombinasjon med registreringstypefilter,
#'  men må ta hensyn til at både start-og sluttregistrenger skal være med i figur.
#' Se ellers figurtekst som kommer ut til PrePost-figur i Shiny-applikasjonen for å forstå hvilke datoer som er sortert på.
#'
#'
#'
#' @param RegData
#' @param reshID
#' @param outfile
#' @param hentData
#' @param preprosess
#' @param enhetsUtvalg
#' @param valgtMaal Sentralmål 'Med' gir median, alt annet gir gjennomsnitt
#' @param valgtVar Variabelen det skal vises resultat for
#' @param datoFra
#' @param datoTil
#' @param datoFraSluttreg
#' @param datoTilSluttreg
#' @param minbmistart
#' @param maxbmistart
#' @param minald
#' @param maxald
#' @param erMann
#' @param regType
#' @param diagnose
#'
#' @return Figur med gjennomsnitt eller median ved to ulike tidspunkt.
#' @export
#'
#' @examples

NorSpis1FigPrePost <- function(RegData,
                               reshID,
                               outfile='',
                               hentData=0,
                               preprosess=1,
                               enhetsUtvalg=1,
                               valgtMaal='Gjsn',
                               valgtVar,
                               datoFra='2012-04-01',
                               datoTil='2050-12-31',
                               datoFraSluttreg='',
                               datoTilSluttreg='',
                               minbmistart=0,
                               maxbmistart=200,
                               minald=0,
                               maxald=130,
                               erMann='',
                               regType='',
                               diagnose ='')
#OBS datoFraSluttreg og datoTilSluttreg må sendes inn tomme,
#ellers så vil filteret kjøres i utvalgsfila slik at kun sluttreg
#(med tilhørende startreg kommer med) Parametrene (datoFraSluttreg og
#datoTilSluttreg) er kun med fordi de de kreves å inngå i utvalgsfila,
#som er felles for alle figurtypene. Men de må altså her settes tomme.
{
  #------ Hente data
  # if (hentData == 1) {
  #   RegData <- NorSpisELAlleScorData(datoFra, datoTil)
  # }

  #------ Preprosessere data
  if (preprosess){
    RegData <- NorSpis1_1_Preprosess(RegData=RegData)
  }

  #------- Tilrettelegge variable
  NorSpisVarSpes <- NorSpis1_2_VarTilrettelegg(RegData=RegData,
                                               valgtVar=valgtVar)

  #-------og hente inn RegData og parametre fra tilretteleggingen
  RegData <- NorSpisVarSpes$RegData
  deltittel <- NorSpisVarSpes$deltittel
  xaksetxt <- NorSpisVarSpes$tittel

  #------- Gjøre utvalg
  NorSpisUtvalg <- NorSpis1_3_Utvalg(RegData=RegData,
                                     datoFra=datoFra,
                                     datoTil=datoTil,
                                     datoFraSluttreg=datoFraSluttreg,
                                     datoTilSluttreg=datoTilSluttreg,
                                     aar=aar,
                                     minald=minald,
                                     maxald=maxald,
                                     minbmistart = minbmistart,
                                     maxbmistart = maxbmistart,
                                     erMann=erMann,
                                     regType=regType,
                                     reshID=reshID,
                                     enhetsUtvalg=enhetsUtvalg,
                                     diagnose=diagnose)

  RegData <- NorSpisUtvalg$RegData
  utvalgTxt <- NorSpisUtvalg$utvalgTxt

  #Sette noen parametre
  #Skal sammenligne start- og sluttregistreringer
  AggVerdier <- list(Pre = 0, Post = 0)
  N <- list(Pre = 0, Post = 0)
  Ngr <- list(Pre = 0, Post = 0)
  TotSkaar <- list(Pre = 0, Post = 0)

  #VELGER hvilke reg-typer som skal ligge i de ulike gruppene (before/pre
  #og after/post).
  after <- RegData[which(RegData$RegRegtype %in% c(5,6,98,99)), ]
  FIDBefore <- after$RegTilhorendeStartReg #(vector of) forlopsID of the
                                           #patients start reg. (of patients
                                           #that also is in after group)
  before <- RegData[which(RegData$RegRegtype %in% c(1,2,3,4)) , ]
  before <- before[which(before$ForlopsID %in% FIDBefore) , ] #before with only
                                                              #patients from
                                                              #after group

  #TEST if same patients in borth groups: before$PasientID %in% after$PasientID
  #TEST if size of before and after grop is the same:
  #length(before$PasientID) == length(after$PasientID)

  #REMOVE patients that have a missing score, on chosen variable, in either
  #before or after group
  before <- before[which(before$PasientID %in% after$PasientID), ]
  after <- after[which(after$PasientID %in% before$PasientID), ]

  #TEST if size of before and after grop is the same: length(before$PasientID)
  #== length(after$PasientID)

  #INDEKSERER registreringene i de ulike gruppene
  FIDAfter <- after$ForlopsID #(vector of) forlopsID of "slutt"/after reg.
  ind <- list(Pre = which(RegData$ForlopsID %in% FIDBefore),
              Post = which(RegData$ForlopsID %in% FIDAfter))

  N$Pre <- length(ind$Pre)
  #Gj.snittsverdi PRE #FIKSE:bruke mean() i stedet? Kan også beregne gj.snitt
  #bare en gang, ikke igjen i "tekst" under.
  AggVerdier$Pre <- sum(as.numeric(as.character(before$VariabelGj)))/N$Pre
  #Gj.snittsverdi POST #FIKSE:bruke mean() i stedet? Kan også beregne gj.snitt
  #bare en gang, ikke igjen i "tekst" under.
  N$Post <- length(ind$Post)
  AggVerdier$Post <- sum(as.numeric(as.character(after$VariabelGj)))/N$Post
  #Gjennomsnittsskårer (i tekst)
  varSkaar <- ifelse(grepl('Sum', valgtVar),
                     valgtVar,
                     paste0(valgtVar,'Skaaring'))
  TotSkaar$Pre <- sprintf(
    '%.1f',
    mean(as.numeric(as.character(RegData[ind$Pre, valgtVar]), na.rm = T)))
  TotSkaar$Post <- sprintf(
    '%1f',
    mean(as.numeric(as.character(RegData[ind$Post, valgtVar], na.rm = T))))

  grtxt <- c('Start','Slutt') #NorSpisVarSpes$grtxt
  grtxt2 <- ''#c(sprintf('%.1f', AggVerdier$Pre),
              #' / ',
              #sprintf('%.1f',AggVerdier$Post),
              #'%')
  #grtxt2[match('', grtxt)] <- ''
  tittel <- NorSpisVarSpes$tittel

  NPre <- N$Pre
  NPost <- N$Post
  AndelerPP <- cbind(AggVerdier$Pre, AggVerdier$Post)


  #-----------Figur---------------------------------------
  #Felles, uavhengig av om for få observasjoner eller ikke
  FigTypUt <- rapFigurer::figtype(outfile, fargepalett='BlaaOff')
  NutvTxt <- length(utvalgTxt)

  # #Hvis for få observasjoner..
  if (NPre < 5 | NPost < 5){#Endre til 10 hvis ønskelig (0 under testing)
    FigTypUt <- rapFigurer::figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    title(tittel)	#, line=-6)
    mtext(utvalgTxt,
          side=3,
          las=1,
          cex=0.9,
          adj=0,
          col=farger[1],
          line=c(3+0.8*((NutvTxt-1):0)))
    text(0.5, 0.6, 'For få registreringer',
         cex=1.2)
    if ( outfile != '') {dev.off()}

  } else {

    #Plottspesifikke parametre:
    vmarg <- 0 #switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure',
               #                        cex=cexgr)*0.7))
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid
                                                                #datoutvalg med

    farger <- FigTypUt$farger
    antGr <- length(grtxt)
    #Ngr <- matrix(c(AntPre, AntPost), antGr, 2)
    lwdPost <- 3	#tykkelse på linja som repr. landet
    cexleg <- 0.9	#Størrelse på legendtekst
    cexpt <- 2	#Størrelse på punkter (resten av landet)

    #Vertikale søyler eller linje
    ymax <- min(max(AndelerPP,na.rm=T)*1.5, 110)
    pos <- barplot(t(AndelerPP), beside=TRUE, las=1,
                   ylab="Skåre (gjennomsnitt)",
                   cex.names=0.8, col=farger[c(1,3)], names.arg=grtxt,
                   border='white', ylim=c(0, ymax), space = c(0.05,1))
    # names.arg=rep('', length(grtxt))
    # pos <- barplot(as.numeric(AggVerdier$Hoved), beside=TRUE, las=1,
    #ylab=yAkseTxt,
    #                sub=xAkseTxt,	col=fargeHoved, border='white',
    #ylim=c(0, ymax))
    posKI <- pos[1:antGr]
    #mtext(at=pos, grtxt2, side=1, las=1, cex=0.75, adj=0.2, line=0)
    #mtext(at=pos, grtxt2, side=1, las=1, cex=0.75, adj=0.2, line=0,
    #font = 2) #opprinnelig line=0.5

    #grtxt <- delTekst(grtxt, 13)
    #mtext(at=pos[1,], grtxt, side=1, las=1, cex=0.75, adj=0.2, line=2)
    legend('top', c(paste0('Ved start, N=', NPre),
                    paste0('Gj.sn. skår: ',
                           round(as.numeric(as.character(TotSkaar$Pre)),
                                 digits=2)),
                    paste0('Ved slutt, N=', NPost),
                    paste0('Gj.sn. skår: ',
                           round(as.numeric(as.character(TotSkaar$Post)),
                                 digits=2))),
           bty='n', fill=farger[c(1,NA,3,NA)], border=NA, ncol=2, cex=cexleg)

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1],
          line=c(3+0.8*((NutvTxt-1):0)))

    title(tittel, font.main=1)	#line=0.5,

    options(warn = -1)	#Unngå melding om KI med lengde 0 - fungerer av
                        #en eller annen grunn ikke i pdf.


    #-----------KI beregninger
    Gjsn <- c(AggVerdier$Pre, AggVerdier$Post)
    #SE <- tapply(as.numeric(as.character(RegData$VariabelGj)),
    #RegData[ ,grVar], sd, na.rm=T)/sqrt(Ngr)
    SEpre <- sd(as.numeric(as.character(before$VariabelGj)),
                na.rm=T)/sqrt(length(before$VariabelGj))
    SEpost <- sd(as.numeric(as.character(after$VariabelGj)),
                 na.rm=T)/sqrt(length(after$VariabelGj))
    SE <- c(SEpre,SEpost)
    # Gjsn[indGrUt] <- dummy0
    # SE[indGrUt] <- 0
    # sortInd <- order(Gjsn, decreasing=TRUE)
    # Midt <- as.numeric(Gjsn[sortInd])
    # KIned <- Gjsn[sortInd] - 2*SE[sortInd]
    KIopp <- Gjsn + 1.96*SE #FIKS/UNDERSØK: 2 eller 1,96
                            #(noen bruker avrunding til 2 for enkelthets skyld)
    KIned <- Gjsn - 1.96*SE #FIKS/UNDERSØK: 2 eller 1,96
                            #(noen bruker avrunding til 2 for enkelthets skyld)

    # MidtHele <- round(mean(RegData$VariabelGj),1)
    # KIHele <- MidtHele + sd(RegData$VariabelGj)/sqrt(N)*c(-2,2)
    #
    # indGrUtPlot <- antGr+(1:length(indGrUt))

    # #upper confidence limit (CL)
    arrows(x0=posKI, y0=Gjsn, x1=posKI, y1=KIopp, #x0=Midt[-indGrUtPlot]*0.999,
                                                  #x1=KIopp[-indGrUtPlot]
      length=0.5/max(pos), code=2, angle=90, lwd=1, col='black')#col=farger[1]
    # #nedre KI
    arrows(x0=posKI, y0=Gjsn, x1=posKI, y1=KIned, #y0=Midt[-indGrUtPlot]*1.001,
                                             #x1=posKI, y1=KIned[-indGrUtPlot]
      length=0.5/max(pos), code=2, angle=90, lwd=1, col='black')#col=farger[1]

    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}

  }
}
