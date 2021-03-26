library(shiny)

server <- function(input, output, session) {

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
    registryName <- "norspis"
    reshID <- rapbase::getUserReshId(session)
    userFullName <- rapbase::getUserFullName(session)
    userRole <- rapbase::getUserRole(session)
    author <- paste0(userFullName, "/", "Rapporteket")



    # Get data
    alle_scorer <-
      norspis2::query_alle_scorer(registryName, reshID, session = session)


    enkelt_ledd_num <-
      norspis2::query_enkelt_ledd_num(registryName, reshID, session = session)
    # strange type of ID in db...
    enkelt_ledd_num$ForlopsID <- as.integer(enkelt_ledd_num$ForlopsID)

    forlops_oversikt <-
      norspis2::query_forlops_oversikt(registryName, reshID, session = session)
    # strange type of ID in db...
    forlops_oversikt$ForlopsID <- as.integer(forlops_oversikt$ForlopsID)
    # for now, just remove unused OppflgSekNr containing binary values
    forlops_oversikt <- forlops_oversikt %>%
      dplyr::select(-OppflgSekNr)

    behandling_num <-
      norspis2::query_behandling_num(registryName, reshID, session = session)



    # The following mirrors we do loacally
    # - First merge three of the datasets,
    # - then change it plus the RegDataBeh dataset into tibbles and finally
    # - run the two datasets in the fun2_dataList() function.
    #
    # (changing to tibbles may not neccessary, but convinient when working
    # locally)

    #Merge data
    ForlAlleSc <- merge(forlops_oversikt, alle_scorer, suffixes = c('','y'),
                        by = "ForlopsID", all = FALSE)
    RegData <- merge(ForlAlleSc, enkelt_ledd_num, suffixes = c('','X'),
                     by = "ForlopsID", all = FALSE)

    RegData <- tibble::as_tibble(RegData)
    RegDataBeh <- tibble::as_tibble(behandling_num)

    DL <- norspis2::fun2_dataList(myInData1 = RegData, myInData2 = RegDataBeh)

  } else {
    print("Make sure that all necessary data are loaded locally - the script to
          import data locally is located locally at NLSH (on NorSpis' disk)")

  }

  # Hide/show tabs (visually)
  #hideTab(inputId = "tabs", target = "Sykehussammenligninger")
  #hideTab(inputId = "tabs", target = "Datakvalitet")
  if(!userRole %in% c('CC','SC')){
    hideTab(inputId = "tabs", target = "Administrasjon")
    hideTab(inputId = "tabsets", target = "Sykehussammenligninger (slutt)")
    hideTab(inputId = "tabsets",
            target = "Sykehussammenligninger (start/slutt)")
    hideTab(inputId = "tabsets2", target = "Oppsummeringstabeller")
  }

  # ----The different outputs----
  # Administrasjons/Nøkkeltall
  output$antallPas <- renderText({
    norspis2::NorSpis1Nokkeltall(
      RegData,
      enhetsUtvalgEgenNasjonal=input$valgtEnhetNokkeltall,
      reshID = reshID)
    #enhetsUtvalg = userRole
  })

  output$plotAntallRegTid <- renderPlot({
  if (dim(RegData)[1] > 0) {
    norspis2::NorSpis1NokkeltallTid(
      RegData,
      enhetsUtvalgEgenNasjonal=input$valgtEnhetNokkeltall,
      reshID)
  } else {
    NULL
  }

  })

  #End tab Administrasjon/Nøkkeltall

  output$fordelinger <- renderPlot({
    norspis2::NorSpis1FigAndeler(
      reshID=reshID,
      RegData=RegData,
      valgtVar= input$valgtVar,
      datoFra=input$datovalg[1],
      datoTil=input$datovalg[2],
      enhetsUtvalg=as.numeric(input$enhetsUtvalg),
      regType= c(1,2,3,4),#as.numeric(input$regType).Hard coded, only start.
      outfile=''
    )
  })

  #Introduce reactivity - update some inputs (reg. type) dependend on
  #other inputs (valgtVarMed), using "observe" (this is utilized in
  #the tab showing fordelingerMed (under))
  #Recipe: https://shiny.rstudio.com/reference/shiny/1.2.0/updateSelectInput.html
    observe({
      x <- input$valgtVarMed

      if(x == "SDQGlobalScore"){
        updateSelectInput(session, "regTypeChildAdultMed",
                          selected = "child"
                        )
      }else if(x == "SCL90GSI"){
        updateSelectInput(session, "regTypeChildAdultMed",
                          selected  = "adult")

      }else{
        updateSelectInput(session, "regTypeChildAdultMed",
                          selected  = "both")
        }

    })

  output$fordelingerMed <- renderPlot({
    norspis2::NorSpis1FigAndeler(
      reshID=reshID,
      RegData=RegData,
      valgtVar=input$valgtVarMed,
      datoFra=input$datovalgMed[1],
      datoTil=input$datovalgMed[2],
      enhetsUtvalg=as.numeric(input$enhetsUtvalgMed),
      regType=as.numeric(input$regTypeMed),
      regTypeStartEnd = input$regTypeStartEndMed,
      regTypeChildAdult = input$regTypeChildAdultMed,
      outfile=''
    )
  })

  output$fordelingerDiag <- renderPlot({
    norspis2::NorSpis1FigAndeler(
      reshID=reshID,
      RegData=RegData,
      valgtVar=input$valgtVarDiag,
      datoFra=input$datovalgDiag[1],
      datoTil=input$datovalgDiag[2],
      enhetsUtvalg=as.numeric(input$enhetsUtvalgDiag),
      #regType=as.numeric(input$regTypeDiag),
      #regTypeStartEnd = input$regTypeStartEndDiag,
      regTypeChildAdult = input$regTypeChildAdultDiag,
      outfile=''
    )
  })

  output$fordelingerOppsumTab <- renderUI({
        #filter
        dat2 <- norspis2::fun3_1_filter_RegData(RegData = DL$RegData2,
                                               # dateFrom =
                                               #   input$datovalgSykehusSammenlign[1],
                                               # dateTo =
                                               #   input$datovalgSykehusSammenlign[2],
                                               regType = c(1,2,3,4), #only startreg
                                               regStatus = 1)#only complete reg

    #Choose variables to present in table
    myvarString <- c(quo(PasientAlder_CAT_MISSING),
                     quo(B01Sivilstatus_MISSING_NAMES), #set all the variables you want to include. The quo is needed beacuse we use !! in the function
                     quo(B03Bosituasjon_MISSING_NAMES),
                     quo(B02EgneBarn_MISSING_NAMES),
                     quo(B04PabegyntUtd_MISSING_NAMES),
                     quo(B05FullfortUtd_MISSING_NAMES),
                     quo(B06Hovedaktivitet_MISSING_NAMES),
                     quo(B07Hovedinntekt_MISSING_NAMES))

    popCharTab <- norspis2::make_table_patChar(RegData = dat2, varsInTab = myvarString)

    #changing the names:
    popCharTab[,1] <- dplyr::recode(popCharTab[,1][[1]],
                                    "PasientAlder_CAT_MISSING" = "Alder",
                                    "B01Sivilstatus_MISSING_NAMES"="Sivilstatus",
                                    "B03Bosituasjon_MISSING_NAMES"="Bosituasjon",
                                    "B02EgneBarn_MISSING_NAMES"="Egne barn",
                                    "B04PabegyntUtd_MISSING_NAMES" = "P?begynt utdanning",
                                    "B05FullfortUtd_MISSING_NAMES" = "Fullf?rt utdanning",
                                    "B06Hovedaktivitet_MISSING_NAMES"="Hovedaktivitet",
                                    "B07Hovedinntekt_MISSING_NAMES" = "Hovedinntekt")

    #uniting first two columns
    popCharTab <- popCharTab %>%
      tidyr::unite("  ", 1:2, sep =" ")

    #Just printing a flextable of the output above
    ft <- flextable::flextable(popCharTab)
    ft <- flextable::autofit(ft)
    ft <- flextable::compose(ft, j =5, #minibar
                             value = flextable::as_paragraph(
                               flextable::minibar(value = N, max(N))
                             ),
                             part = "body")
    ft <- flextable::merge_v(ft, j = 1:2) #vertical merge of values in first and second column
    ft <- flextable::valign (ft, j = 1:2, valign = "top") #align the merged values in first and seceond column to the top

    ft
    flextable::htmltools_value(ft)

  })

  output$fordelingerInd <- renderPlot({
    norspis2::NorSpis1FigAndeler(
      reshID=reshID,
      RegData=RegData,
      valgtVar=input$valgtVarInd,
      enhetsUtvalg=as.numeric(input$enhetsUtvalgInd),
      datoFra=input$datovalgInd[1],
      datoTil=input$datovalgInd[2],
      #hard code regType to make the utilized regType (ChildAdult) work here:
      regType=c(3,4,5,6), #as.numeric(input$regTypeInd),
      #regTypeStartEnd = input$regTypeStartEndInd,
      regTypeChildAdult = input$regTypeChildAdultInd,
      outfile=''
    )
  })

  output$PrePost <- renderPlot({
    norspis2::NorSpis1FigPrePost(
      RegData=RegData,
      valgtVar=input$valgtVarPrePost,
      datoFra='2012-01-01',
      datoTil='2050-12-31',
      datoFraSluttreg = input$datovalgPrePost[1],
      datoTilSluttreg = input$datovalgPrePost[2],
      valgtMaal='Gjsn',
      minbmistart = input$bmistartPrePost1,#as.numeric(input$bmistartPrePost[1]),
      maxbmistart = input$bmistartPrePost2,#as.numeric(input$bmistartPrePost[2]),
      outfile='',
      hentData=0,
      preprosess=1,
      regType='',
      enhetsUtvalg=as.numeric(input$enhetsUtvalgPrePost),
      reshID=reshID,
      diagnose = as.character(input$diagnosePrePost),
      addCI = input$addCIPrePost)
  })

  output$fordelingerPas <- renderPlot({
    norspis2::NorSpis1FigAndeler(
      reshID=reshID,
      RegData=RegData,
      valgtVar=input$valgtVarPas,
      enhetsUtvalg=as.numeric(input$enhetsUtvalgPas),
      datoFra=input$datovalgPas[1],
      datoTil=input$datovalgPas[2],
      regType=as.numeric(input$regTypePas),
      regTypeStartEnd = "end",
      regTypeChildAdult = input$regTypeChildAdultPas,
      outfile=''
    )
  })

  output$sykehusSammenlign <- renderPlot({
        #filter
        dat <- norspis2::fun3_1_filter_RegData(RegData = DL$RegDataNatVal2,
                                               dateFrom =
                                                 input$datovalgSykehusSammenlign[1],
                                               dateTo =
                                                 input$datovalgSykehusSammenlign[2],
                                               regStatus = 1)#only complete reg
      #table to plot
      tab <- norspis2::make_figTable_unitCompar(
        myIndata_NatVal =  dat,
        myInvar01 = input$valgtVarSykehusSammenlign)#rlang::quo(PROP_PO10Pasientsikkerhet))#input$valgtVarSykehusSammenlign)#rlang::quo(PROP_PO10Pasientsikkerhet))#rlang::quo(PROP_PO10Pasientsikkerhet))#rlang::quo(!!input$valgtVarSykehusSammenlign))
    #plot
    norspis2::make_figFig_unitCompar(tab,
                                     YellowGoal = "",
                                     GreenGoal = "")

  })

  output$sykehusSammenlign2 <- renderPlot({
        #filter
        dat <- norspis2::fun3_2_filter_RegDataStartEnd(RegData = DL$RegDataStartEndNatVal2,
                                                       dateFrom.y  =
                                                         input$datovalgSykehusSammenlign2[1],
                                                       dateTo.y =
                                                         input$datovalgSykehusSammenlign2[2],
                                                       BasisRegStatus.y = 1)#only complete reg
      #table to plot
      tab <- norspis2::make_figTable_unitCompar(
        myIndata_NatVal =  dat,
        myInvar01 = input$valgtVarSykehusSammenlign2)#rlang::quo(PROP_PO10Pasientsikkerhet))#input$valgtVarSykehusSammenlign)#rlang::quo(PROP_PO10Pasientsikkerhet))#rlang::quo(PROP_PO10Pasientsikkerhet))#rlang::quo(!!input$valgtVarSykehusSammenlign))
    #plot
    norspis2::make_figFig_unitCompar(tab,
                                     YellowGoal = "",
                                     GreenGoal = "")

  })

  output$tableOvers <- DT::renderDataTable({
    norspis2::NorSpis1TabRegStatus(
      RegData = DL$RegData2,
      userRole = userRole,
      reshID = reshID,
      datoFra=input$datovalgRegOvers[1],
      datoTil=input$datovalgRegOvers[2])

  })

  output$tableOversUtv <- DT::renderDataTable({
    norspis2::NorSpis1TabRegStatusUtvidet(
      RegData = RegData,
      userRole = userRole,
      reshID = reshID,
      datoFra=input$datovalgRegOversUtv[1],
      datoTil=input$datovalgRegOversUtv[2])

  })



  ###-----PDF-report-----
  #(FIKS: fungerer ikke -
  #se https://rdrr.io/github/Rapporteket/nordicscir/src/inst/shinyApps/nordicscir/app.R
  #og https://community.rstudio.com/t/commands-to-create-a-pdf-file/6264 )
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
      contentFile(file,
                  srcFil="NorSpisMndRapp.Rmd",
                  tmpFile="tmpNorSpisMndRapp.Rmd")
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

  ## Samlerapporter
  output$samlerapport <- renderUI({
    htmlRenderRmd(
      srcFile = input$srcFile,
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
  ###-----PDF-report (END)

}
