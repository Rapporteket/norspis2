#' Title
#'
#' @param RegData
#' @param valgtVar
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
#' @param outfile
#' @param hentData
#' @param preprosess
#' @param reshID
#' @param enhetsUtvalg
#'
#' @return Figure
#' @export

NorSpis1FigAndeler  <- function(RegData, valgtVar,
                               datoFra='2012-01-01',
                               datoTil='3000-12-31',
                               datoFraSluttreg = '',
                               datoTilSluttreg='',
                               minald=0,
                               maxald=130,
                               minbmistart=0,
                               maxbmistart=200,
                               regType='',
                               enhetstypeDogn = '',
                               enhetstypeRegional = '',
                               erMann='',
                               outfile='',
                               hentData=0,
                               preprosess=1,
                               reshID,
                               enhetsUtvalg=1)
{

  # PREPROSESSERE DATA
  if (preprosess==1){
    RegData <- NorSpis1_1_Preprosess(RegData=RegData)
  }

  #FIGURER SATT SAMMEN AV FLERE VARIABLE, ULIKT TOTALUTVALG.
  #EKSEMPLER FOR ULIKE SITUASJONER.DENNE DELEN VIL VI TRENGE FOR F.EKS.
  #"HONOS, ENKELTLEDD". Vi har en "interessekonflikt" for figurer
  #satt sammen av fler variable: Utvalg må kjøres etter all variabelfiltrering
  #for å få riktig utvalgstekst i figuren. Vi må kjøre utvalg før
  #variabeldefinisjon for å få riktige indekser -
  #Løsning: Må tilrettelegge generelle variable som representerer de
  #sammensatte variablene.


  #3.DEFINERE VARIABLE: Henter tilrettelagte variable fra Vartilrettelegg-----
  NorSpisVarSpes <- NorSpis1_2_VarTilrettelegg(RegData=RegData,
                                               valgtVar=valgtVar)
  RegData <- NorSpisVarSpes$RegData
  flerevar <- NorSpisVarSpes$flerevar

  #4.UTVALG
  NorSpisUtvalg <- NorSpis1_3_Utvalg(RegData=RegData,
                                     datoFra=datoFra,
                                     datoTil=datoTil,
                                     datoFraSluttreg = datoFraSluttreg,
                                     datoTilSluttreg = datoTilSluttreg,
                                     minald=minald,
                                     maxald=maxald,
                                     minbmistart=minbmistart,
                                     maxbmistart=maxbmistart,
                                     regType=regType,
                                     enhetstypeDogn=enhetstypeDogn,
                                     enhetstypeRegional=enhetstypeRegional,
                                     erMann=erMann,
                                     enhetsUtvalg=enhetsUtvalg,
                                     reshID=reshID)

  RegData <- NorSpisUtvalg$RegData
  ind <- NorSpisUtvalg$ind
  medSml <- NorSpisUtvalg$medSml
  hovedgrTxt <- NorSpisUtvalg$hovedgrTxt
  smltxt <- NorSpisUtvalg$smltxt
  utvalgTxt <- NorSpisUtvalg$utvalgTxt
  fargepalett=NorSpisUtvalg$fargepalett
  medSml=NorSpisUtvalg$medSml
  msdtxt<-'msd'

  #----------------------BEREGNINGER
  #Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
  # Når vi har figurer som viser andel av flere variable (flerevar=1),
  # må vi omdefinere variablene slik at alle gyldige registreringer
  # (dvs. alle registreringer som skal telles med) er 0 eller 1. De som har
  # oppfylt spørsmålet er 1, mens ugyldige registreringer er NA. Det betyr
  # at hvis vi skal ta bort registreringer som i kategorier av typen "Ukjent"
  # kodes disse som NA, mens hvis de skal være med kodes de som 0.

  Andeler <- list(Hoved = 0, Rest =0)
  N <- list(Hoved = 0, Rest =0)   #Nevner
  Ngr <- list(Hoved = 0, Rest =0) #Teller
  Nfig <- list(Hoved = 0, Rest =0) #figurtekst: N i legend
  ind <- NorSpisUtvalg$ind
  variable <- NorSpisVarSpes$variable

  Ngr$Hoved <- switch(as.character(flerevar),
                      '0' = table(RegData$VariabelGr[ind$Hoved]),
                      # '1' = colSums(sapply(RegData[ind$Hoved ,variable],
                                            #as.numeric),
                                     #na.rm=T))
                      '1' = apply(RegData[ind$Hoved,variable], MARGIN=2,
                                  FUN=function(x) sum(x == 1, na.rm=T)))
  #N$ gjelder selv om totalutvalget er ulikt for de ulike variablene i flerevar
  N$Hoved <- switch(as.character(flerevar),
                    '0' = sum(Ngr$Hoved),	#length(ind$Hoved)- Kan inneholde NA
                    #      '1' = length(ind$Hoved)
                    '1' = apply(RegData[ind$Hoved,variable], MARGIN=2,
                                FUN=function(x) sum(x %in% 0:1, na.rm=T)))
  Andeler$Hoved <- 100*Ngr$Hoved/N$Hoved



  if (NorSpisUtvalg$medSml==1) {
    Ngr$Rest <- switch(as.character(flerevar),
                       '0' = table(RegData$VariabelGr[ind$Rest]),
                       # '1' = colSums(sapply(RegData[ind$Rest ,variable],
                                              #as.numeric),
                                      #na.rm=T))
                       '1' = apply(RegData[ind$Rest,variable], MARGIN=2,
                                   FUN=function(x) sum(x == 1, na.rm=T)))
    N$Rest <- switch(as.character(flerevar),
                     '0' = sum(Ngr$Rest),
                     '1' = apply(RegData[ind$Rest,variable], MARGIN=2,
                                 FUN=function(x) sum(x %in% 0:1, na.rm=T)))
    Andeler$Rest <- 100*Ngr$Rest/N$Rest
  }

  if(flerevar==1) {
    Nfig$Hoved <- ifelse(min(N$Hoved)==max(N$Hoved),
                         min(N$Hoved[1]),
                         paste0(min(N$Hoved),'-',max(N$Hoved)))
    Nfig$Rest <- ifelse(min(N$Rest)==max(N$Rest),
                        min(N$Rest[1]),
                        paste0(min(N$Rest),'-',max(N$Rest)))
  } else {
    Nfig <- N}

  grtxt2 <- paste0('(', sprintf('%.1f',Andeler$Hoved), '%)')
  yAkseTxt='Andel pasienter (%)'

  FigDataParam <- list(AggVerdier = Andeler,
                       N=N,
                       Ngr=Ngr,
                       KImaal <- NorSpisVarSpes$KImaal,
                       #soyletxt=soyletxt,
                       grtxt2=NorSpisVarSpes$grtxt2,
                       grtxt=NorSpisVarSpes$grtxt,
                       tittel=NorSpisVarSpes$tittel,
                       retn=NorSpisVarSpes$retn,
                       xAkseTxt=NorSpisVarSpes$xAkseTxt,
                       yAkseTxt=yAkseTxt,
                       utvalgTxt=NorSpisUtvalg$utvalgTxt,
                       fargepalett=NorSpisUtvalg$fargepalett,
                       medSml=NorSpisUtvalg$medSml,
                       hovedgrTxt=NorSpisUtvalg$hovedgrTxt,
                       smltxt=NorSpisUtvalg$smltxt,
                       msdtxt='msd')

  #Definerer opp variable siden vi fortsatt genererer figuren i denne funksjonen.
  grtxt=NorSpisVarSpes$grtxt
  tittel=NorSpisVarSpes$tittel
  retn=NorSpisVarSpes$retn
  xAkseTxt=NorSpisVarSpes$xAkseTxt
  hovedgrTxt=NorSpisUtvalg$hovedgrTxt
  tittel2 = NorSpisVarSpes$tittel2
  kommentar = NorSpisVarSpes$kommentar

  #
  # lagFig <- 0
  # if (lagFig == 1) {
  #   #cexgr <- 1-ifelse(AntGr>20, 0.25*AntGr/60, 0)
  #   NorSpisFigSoyler(RegData,
  #                    AggVerdier,
  #                    Ngr,
  #                    tittel=NorSpisVarSpes$tittel,
  #                    hovedgrTxt=NorSpisUtvalg$hovedgrTxt,
  #                    smltxt=NorSpisUtvalg$smltxt,
  #                    msdtxt='msd',
  #                    Ngr = Ngr,
  #                    KImaal <- NorSpisVarSpes$KImaal,
  #                    N=N,
  #                    retn='V',
  #                    utvalgTxt,
  #                    grtxt=NorSpisVarSpes$grtxt,
  #                    grtxt2=grtxt2,
  #                    medSml=NorSpisUtvalg$medSml,
  #                    xAkseTxt=NorSpisVarSpes$xAkseTxt,
  #                    yAkseTxt=yAkseTxt,
  #                    outfile=outfile)
  #   #ENDRE så figurparametrene skrives fullt ut i parameterkallet
  # }



  #-----------FIGUR---------------------------------------
  #-----------Figur---------------------------------------
  #Hvis for få observasjoner..
  NutvTxt <- length(utvalgTxt)
  if (dim(RegData)[1] < 5 | (max(N$Hoved)< 5 ) |
      (max(N$Rest<5) && enhetsUtvalg==1))
  {
    #KAn endres til 10 hvis ønskelig, eller 0 under testing
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
    text(0.5, 0.6, 'For få registreringer', cex=1.2)
    if ( outfile != '') {dev.off()}

  } else {
    #-----------Figur---------------------------------------
    #Innparametre: xAkseTxt, grtxt, grtxt2, tittel,
    #              Andeler, utvalgTxt, retn, cexgr
    cexgr <- 0.85	#Kan endres for enkeltvariable
    antDes <- 1

    #Plottspesifikke parametre:
    FigTypUt <- rapFigurer::figtype(outfile,
                                    fargepalett=NorSpisUtvalg$fargepalett)
    #Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    antDesTxt <- paste0('%.', antDes, 'f')
    grtxtpst <- paste0(grtxt, ' (', sprintf(antDesTxt, Andeler$Hoved), '%)')
    vmarg <- switch(retn,
                    V=0,
                    H=max(0, strwidth(grtxtpst, units='figure',cex=cexgr)*0.7))
    #Har alltid datoutvalg med #default på mar er c(5, 4, 4, 2) + 0.1:
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)), mar=c(8.1,4.1,4.1,2.1))

    farger <- FigTypUt$farger
    fargeHoved <- farger[1]
    fargeRest <- farger[3]
    antGr <- length(grtxt)
    lwdRest <- 1	#tykkelse på linja som repr. landet
    cexleg <- 1	#Størrelse på legendtekst

    #Horisontale søyler
    if (retn == 'H') {
      xmax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
      pos <- barplot(as.numeric(Andeler$Hoved),
                     horiz=TRUE,
                     beside=TRUE,
                     las=1,
                     xlab="Andel pasienter (%)", #main=tittel,
                     col=fargeHoved,
                     border='white',
                     font.main=1,
                     xlim=c(0, xmax),
                     ylim=c(0.05,1.4)*antGr)	#
      if (N$Hoved>0) {mtext(at=pos+0.05,
                            text=grtxtpst,
                            side=2,
                            las=1,
                            cex=cexgr,
                            adj=1,
                            line=0.25)}

      if (medSml == 1) {
        points(as.numeric(Andeler$Rest),
               pos,
               col=fargeRest,
               cex=2,
               pch=18) #c("p","b","o"),
        legend('top', c(paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'),
                        paste0(smltxt, ' (N=', Nfig$Rest,')')),
               border=c(fargeHoved,NA),
               col=c(fargeHoved,fargeRest),
               bty='n',
               pch=c(15,18),
               pt.cex=2,
               lty=NA,
               ncol=1,
               lwd=lwdRest,
               cex=cexleg) #
      } else {
        legend('top',
               paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'),
               border=NA,
               fill=fargeHoved,
               bty='n',
               ncol=1,
               cex=cexleg)
      }
    }


    if (retn == 'V' ) {
      #Vertikale søyler eller linje
      if (length(grtxt2) == 1) {
        grtxt2 <- paste0('(', sprintf(antDesTxt, Andeler$Hoved), '%)')}
      ymax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
      pos <- barplot(as.numeric(Andeler$Hoved),
                     beside=TRUE,
                     las=1,
                     ylab="Andel pasienter (%)",
                     xlab='',
                     col=fargeHoved,
                     border='white',
                     ylim=c(0, ymax))
      #, font.lab=2) #font.lab setter font-type på aksene (2=bold)
      mtext(at=pos,
            grtxt,
            side=1,
            las=1,
            cex=cexgr,
            adj=0.5,
            line=1.5,
            font = 2) #opprinnelig line=0.5
      mtext(at=pos,
            grtxt2,
            side=1,
            las=1,
            cex=cexgr,
            adj=0.5,
            line=2.5) #opprinnelig line=1.5
      mtext(side=1,
            adj= 0,
            line=6.85,
            cex=0.75,
            #Denne linjen kan inkluderes for å få kommentartekst nederst:
            kommentar)
      title(xlab=xAkseTxt,
 #tilført for å flytte akseteksten ned. Måtte da sette xlab='' i barplot() over
            line=4)
      if (medSml == 1) {
        points(pos,
               as.numeric(Andeler$Rest),
               col=fargeRest,
               cex=2,
               pch=18) #c("p","b","o"),
        legend('top', c(paste0(hovedgrTxt, ' (N=', N$Hoved,')'),
                        paste0(smltxt, ' (N=', N$Rest,')')),
               border=c(fargeHoved,NA),
               col=c(fargeHoved,fargeRest),
               bty='n',
               pch=c(15,18),
               pt.cex=2,
               lty=c(NA,NA),
               lwd=lwdRest,
               ncol=2,
               cex=cexleg)	#
      } else {
        legend('top',
               paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'),
               border=NA,
               fill=fargeHoved,
               bty='n',
               ncol=1,
               cex=cexleg)
      }


      #KAN BRUKES HVIS/NÅR ØNSKER Å FÅ INN GJ.SNITT OG STD.AVVIK
      #            {
      # den følgende linjen måtte legges til for å få til kombinasjonen av
      #tekst og expressions (x-bar) under:
      #hovedgrTxtAndN<-paste0(hovedgrTxt, ' (N=', N$Hoved,') ')
      #legend('top',
      #       as.expression(bquote(.(hovedgrTxtAndN) ~ bar(x))),
             #(paste0(hovedgrTxt, ' (N=', N$Hoved,') ', msdtxt='sdkjah')),
      #       border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
      #}
    }

    title(tittel, line=1, font.main=1)
    title(tittel2, line=0, font.main=1, cex.main=0.75)

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt,
          side=3,
          las=1,
          cex=0.9,
          adj=0,
          col=farger[1],
          line=c(3+0.8*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}
  }

  #Beregninger som returneres fra funksjonen.
  AndelerUt <- rbind(Andeler$Hoved, Andeler$Rest)
  rownames(AndelerUt) <- c('Hoved', 'Rest')
  AntallUt <- rbind(N$Hoved, N$Rest)
  rownames(AntallUt) <- c('Hoved', 'Rest')

  UtData <- list(paste0(toString(tittel),'.'),
                 AndelerUt,
                 AntallUt,
                 grtxt,
                 kommentar)
  names(UtData) <- c('Tittel', 'Andeler', 'Antall', 'GruppeTekst')
  return(invisible(UtData))

}
