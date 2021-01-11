library(shiny)

shinyServer(function(input, output, session) {

  # Hide/show tabs
  hideTab(inputId = "tabs", target = "FIGUR: Sammenligninger (sykehus)")

  # Navbar user widget
  output$appUserName <- renderText(getUserFullName(session))
  output$appOrgName <- renderText(getUserReshId(session))

  # Pop-up user info
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE, confirmButtonText = "Den er grei!")
  })

  # Gjenbrukbar funksjon for å bearbeide Rmd til in-line html
  htmlRenderRmd <- function(srcFile, params = list()) {
    system.file(srcFile, package="norspis") %>%
      knitr::knit() %>%
      markdown::markdownToHTML(.,
                               options = c('fragment_only',
                                           'base64_images',
                                           'highlight_code'),
                               encoding = "utf-8") %>%
      shiny::HTML()
  }

  # Gjenbrukbar funksjon for å bearbeide Rmd til nedlastbar pdf eller html
  contentFile2 <- function(file, srcFile, type, title) {
    src <- normalizePath(system.file(srcFile, package="norspis"))

    rmarkdown::render(src, output_format = switch(
      type,
      pdf = "pdf_document",
      html = "html_document"), output_file = file,
      params = list(tableFormat=switch(
        type,
        PDF = "latex",
        HTML = "html"),
        author=author,
        reshID=reshID,
        title=title)
    )
  }

  # Data
  if (rapbase::isRapContext()) {
    raplog::appLogger(session = session, msg = "Starting NorSpis application")

    # Parameters that will remain throughout the session
    ## setting values that do depend on a Rapporteket context
    reshID <- rapbase::getUserReshId(session)
    userFullName <- rapbase::getUserFullName(session)
    userRole <- rapbase::getUserRole(session)
    author <- paste0(userFullName, "/", "Rapporteket")

    # RegData <- NorSpisELAlleScorData(datoFra = '2015-01-01', datoTil = '2099-01-01',
    #                                  session = session)
    RegData <- NULL

    # if (userRole != "SC") {
    #   hideTab(inputId = "tabs", target = "OVERSIKT: Registreringer")
    # }

  } else {
    print("Make sure that all necessary data are loaded locally - the script to
          import data locally is located locally at NLSH (on NorSpis' disk)")

    reshID = '700821' #TESTNO" , Oslo: 109979, Bodø: 700821
    userRole <-'SC'
  }

  # ----The different outputs----
  # Administrasjons/Nøkkeltall
  output$antallPas <- renderText({
    NorSpisNokkeltall(RegData,
                      enhetsUtvalgEgenNasjonal=input$valgtEnhetNokkeltall,
                      reshID = reshID)
                      #enhetsUtvalg = userRole
  })

  output$plotAntallRegTid <- renderPlot({
    if (dim(RegData)[1] > 0) {
      NorSpisNokkeltallTid(RegData,
                           enhetsUtvalgEgenNasjonal=input$valgtEnhetNokkeltall,
                           reshID)
    } else {
      NULL
    }

  })
  #End tab Administrasjon/Nøkkeltall

  output$fordelinger <- renderPlot({
    NorSpisFigAndeler(RegData=RegData, datoFra=input$datovalg[1], valgtVar=input$valgtVar, datoTil=input$datovalg[2],
                      #erMann=as.numeric(input$erMann), deactivated 04.01.2021
                      reshID=reshID, enhetsUtvalg=as.numeric(input$enhetsUtvalg), outfile='',
                      #minald=as.numeric(input$alder[1]), deactivated 04.01.2021
                      #maxald=as.numeric(input$alder[2]), deactivated 04.01.2021
                      # minbmistart=as.numeric(input$bmistart[1]), deactivated 04.01.2021
                      # maxbmistart=as.numeric(input$bmistart[2]), deactivated 04.01.2021
                      regType=as.numeric(input$regType)#,
                      # enhetstypeDogn=as.numeric(input$enhetstypeDogn),deactivated 04.01.2021
                      # enhetstypeRegional=as.numeric(input$enhetstypeRegional)deactivated 04.01.2021
    )
  })

  output$fordelingerMed <- renderPlot({
    NorSpisFigAndeler(RegData=RegData,
                      reshID=reshID,
                      datoFra=input$datovalgMed[1], valgtVar=input$valgtVarMed, datoTil=input$datovalgMed[2],
                      #erMann=as.numeric(input$erMann), deactivated 04.01.2021
                      enhetsUtvalg=as.numeric(input$enhetsUtvalgMed), outfile='',
                      #minald=as.numeric(input$alder[1]), deactivated 04.01.2021
                      #maxald=as.numeric(input$alder[2]), deactivated 04.01.2021
                      # minbmistart=as.numeric(input$bmistart[1]), deactivated 04.01.2021
                      # maxbmistart=as.numeric(input$bmistart[2]), deactivated 04.01.2021
                      regType=as.numeric(input$regTypeMed)#,
                      # enhetstypeDogn=as.numeric(input$enhetstypeDogn),deactivated 04.01.2021
                      # enhetstypeRegional=as.numeric(input$enhetstypeRegional)deactivated 04.01.2021
    )
  })

  output$fordelingerInd <- renderPlot({
    norspis::NorSpisFigAndeler(
      RegData=RegData,
      reshID=reshID,
      datoFra=input$datovalgInd[1],
      valgtVar=input$valgtVarInd,
      datoTil=input$datovalgInd[2],
      #erMann=as.numeric(input$erMann), deactivated 04.01.2021
      enhetsUtvalg=as.numeric(input$enhetsUtvalgInd), outfile='',
      #minald=as.numeric(input$alder[1]), deactivated 04.01.2021
      #maxald=as.numeric(input$alder[2]), deactivated 04.01.2021
      # minbmistart=as.numeric(input$bmistart[1]), deactivated 04.01.2021
      # maxbmistart=as.numeric(input$bmistart[2]), deactivated 04.01.2021
      regType=as.numeric(input$regTypeInd)#,
      # enhetstypeDogn=as.numeric(input$enhetstypeDogn),deactivated 04.01.2021
      # enhetstypeRegional=as.numeric(input$enhetstypeRegional)deactivated 04.01.2021
    )
  })

  output$fordelingerPas <- renderPlot({
    NorSpisFigAndeler(RegData=RegData,
                      reshID=reshID,
                      datoFra=input$datovalgPas[1], valgtVar=input$valgtVarPas, datoTil=input$datovalgPas[2],
                      #erMann=as.numeric(input$erMann), deactivated 04.01.2021
                      enhetsUtvalg=as.numeric(input$enhetsUtvalgPas), outfile='',
                      #minald=as.numeric(input$alder[1]), deactivated 04.01.2021
                      #maxald=as.numeric(input$alder[2]), deactivated 04.01.2021
                      # minbmistart=as.numeric(input$bmistart[1]), deactivated 04.01.2021
                      # maxbmistart=as.numeric(input$bmistart[2]), deactivated 04.01.2021
                      regType=as.numeric(input$regTypePas)#,
                      # enhetstypeDogn=as.numeric(input$enhetstypeDogn),deactivated 04.01.2021
                      # enhetstypeRegional=as.numeric(input$enhetstypeRegional)deactivated 04.01.2021
    )
  })

  output$sykehusSammenlign <- renderPlot({
    norspis2::make_figFig_unitCompar(norspis2::make_figTable_unitCompar(DL$RegDataNatVal2, rlang::quo(PROP_PO10Pasientsikkerhet)))

  })


  output$andelerGrVar <- renderPlot({
    NorSpisFigAndelerGrVar(RegData=RegData, datoFra=input$datovalgAndelGrVar[1], valgtVar=input$valgtVarAndelGrVar, datoTil=input$datovalgAndelGrVar[2], #erMann=erMann,
                           grVar='SykehusAvdNavn', outfile='',
                           minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2]), reshID=reshID)#, enhetsUtvalg=enhetsUtvalg,
  })

  output$PrePost <- renderPlot({
    NorSpisFigPrePost(RegData=RegData, valgtVar=input$valgtVarPrePost,
                      datoFra='2012-01-01',datoTil='2050-12-31',
                      datoFraSluttreg = input$datovalgPrePost[1], datoTilSluttreg = input$datovalgPrePost[2], valgtMaal='Gjsn',
                      minald=as.numeric(input$alderPrePost[1]), maxald=as.numeric(input$alderPrePost[2]),
                      minbmistart = as.numeric(input$bmistartPrePost[1]), maxbmistart = as.numeric(input$bmistartPrePost[2]),
                      erMann=as.numeric(input$erMannPrePost), outfile='',
                      hentData=0, preprosess=1, regType='',enhetsUtvalg=as.numeric(input$enhetsUtvalgPrePost), reshID=reshID, diagnose = as.character(input$diagnosePrePost))
  })

  ##PDF-rapporter (FIKS: fungerer ikke - se https://rdrr.io/github/Rapporteket/nordicscir/src/inst/shinyApps/nordicscir/app.R og https://community.rstudio.com/t/commands-to-create-a-pdf-file/6264 )
  #funksjon for å kjøre Rmd-filer (render file funksjon)
  contentFile <- function(file, srcFil, tmpFile) {
    src <- normalizePath(system.file(srcFil, package="norspis"))

    #gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, tmpFile, overwrite = TRUE)

    #texfil <- rmarkdown::render(srcFil, encoding = 'UTF-8')
    #tools::texi2pdf(texfil, clean = TRUE)

    rmarkdown::render("NorSpisMndRapp.Rmd",
                      output_file = file,
                      envir = new.env(parent = globalenv()))
    gc()
    file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)

  }

  #PDF-rapport-knappen
  output$NorSpis_rapport.pdf <- downloadHandler(
    filename = function(){
      paste0('NorSpis_rapport_', format(Sys.Date(),"%Y-%m-%d"), ".pdf" )
    },

    content = function(file){
      contentFile(file, srcFil="NorSpisMndRapp.Rmd", tmpFile="tmpNorSpisMndRapp.Rmd")
    }
  )
  # content = function(file){
  #       # #gå til tempdir. Har på server ikke skriverettigheter i arbeidskatalog
  #       tempReport <- file.path(tempdir(), "NorSpisMndRapp.Rmd")
  #       file.copy("NorSpisMndRapp.Rmd", tempReport, overwrite =TRUE)
  #
  #       #setwd("C:/Users/spa-ressp-2/Documents/norspis/inst")
  #       rmarkdown::render("NorSpisMndRapp.Rmd",
  #                         output_file = file,
  #                         envir = new.env(parent = globalenv())
  #       )
  #
  #contentFile(file, srcFil="NorSpisMndRapp.Rmd", tmpFile="tmpNorSpisMndRapp.Rmd")
  # }
  # )
  #


  ##Output med kommentar fra vartilrettelegg (beskrivelse av figuren o.l)).
  # output$beskrivelse <- renderText({
  #       'Legg inn "kommentar" fra varTilrettelegg her'
  # }
  # )
  output$tableOvers <- DT::renderDataTable({
    NorSpisTabRegStatus(RegData = RegData,
                        userRole = userRole,
                        reshID = reshID,
                        datoFra=input$datovalgRegOvers[1],
                        datoTil=input$datovalgRegOvers[2])

  })

  output$tableOversUtv <- DT::renderDataTable({
    NorSpisTabRegStatusUtvidet(RegData = RegData,
                               userRole = userRole,
                               reshID = reshID,
                               datoFra=input$datovalgRegOversUtv[1],
                               datoTil=input$datovalgRegOversUtv[2])

  })

  ## Samlerapporter
  output$samlerapport <- renderUI({
    htmlRenderRmd(srcFile = input$srcFile,
                  params = list(title=paste("Rapport fra NorSpis for", reshID,
                                            "enhet i NorSpis frem til oktober 2019"),
                                author=paste0(reshID, "/Rapporteket"),
                                reshID=reshID,
                                tableFormat="html")
    )
  })

  output$downloadSamlerapport <- downloadHandler(
    filename = function() {
      tempfile(pattern = input$srcFile,
               fileext = paste0(".", input$formatSamlerapport))
    },
    content = function(file) {
      contentFile2(file, input$srcFile, input$formatSamlerapport,
                   title = paste("Rapport fra NorSpis for", reshID,
                                 "enhet i NorSpis frem til oktober 2019"))
    }
  )


})