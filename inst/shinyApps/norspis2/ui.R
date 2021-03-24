library(shiny)
library(shinyBS) # Additional Bootstrap Controls
library(norspis2)
library(shiny)
library(shinyalert)
library(shinydashboard)
library(shinyWidgets)
library(knitr)
library(lubridate)
library(rapbase)
library(dplyr)
library(tidyr)
library(tibble)
library(DT)

shiny::addResourcePath('rap', system.file('www', package='rapbase'))

shinyUI(

ui <- tagList(
  navbarPage(

    # Title, logo and such
    title = div(
      a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
      "Rapporteket for NorSpis"),
    windowTitle = "Norsk kvalitetsregister
                     for behandling av spiseforstyrrelser",
    theme = "rap/bootstrap.css",
    id = "tabs",

    # All the various tabs

    tabPanel(
      "HJEM",
      mainPanel(
        h2(" ", align='center'),#you can add a title on this line
        h3("Velkommen til Rapporteket", align='left'),
        br(),
        h4("Bruk"),
        strong("Formål"),
        h5("Rapporteket skal inneholde rapporter som sykehusavdelingene kan
             benytte i sitt kontinuerlige forbedringsarbeid."),
        br(),
        strong("Navigering"),
        h5("Bruk fanene i toppen til å navigere til ulike typer
             rapporter.", align='left'),
        br(),
        strong("Retningslinjer for bruk"),
        h5("– Bruk rapportene internt"),
        h5("– Avklar ekstern bruk med NorSpis (for eksempel ved ønske om bruk
           overfor media)"),
        br(),
        strong("Utvis forsiktighet ved tolking"),
        h5("– Det er den enkelte avdeling som best forstår sine tall"),
        h5("– Nasjonale tall er kun representative for pasientgruppen ved de enhetene som har registrert i NorSpis i den aktuelle tidsperioden"),
        h5("– Dekningsgrad, ved de enkelte enhetene og nasjonalt, vil kunne påvirke hvor representative dataene er"),
        h5("– Forløpskompletthet (andel registreringer med levert
           sluttregistrering) vil også påvirke representativiteten på rapporter
           som bruker data fra sluttregistreringene"),
        br(),
        h4("Videre utvikling av Rapportekets innhold"),
        h5("Innholdet vil måtte utvikles til å bli stadig mer relevant for
           behandlingsenhetene."),
        h5("Førsteutgaven(e) av Rapporteket gir dere som brukere et
            utgangspunkt, fra hvor dere kan være med å utforme innholdet."),
        h5("Endringsforslag og ønsker om nye rapporter, kan sendes til norspis@nlsh.no."),
        h5(""),
        br(),
        h4("Datakvalitet"),
        h5("Dette er en tidlig utgave av Rapporteket. Ved spørsmål til figurene
           eller mistanke om at figurene eller datagrunnlaget"),
        h5(" kan inneholde feil, rapporteres det tilbake
           til registeret."),
        br()
      )
    ),#tab HJEM

    tabPanel(
      "Pasientkarakteristika",
      tabsetPanel(
        type="tabs",
        id = "tabsets2",
        tabPanel(
          "Demografiske karakteristika",
          sidebarPanel(
            width = 3,
            wellPanel(
              h3(""),
              selectInput(
                inputId = "valgtVar", label="Variabel",
                choices = c('Alder' = 'Alder',
                            'Sivilstatus' = 'B01Sivilstatus' ,
                            'Kjønn' = 'erMann'
                )
              ),
              selectInput(inputId = 'enhetsUtvalg', label='Egen enhet/landet',
                          choices = c("Egen mot resten av landet"=1,
                                      "Hele landet"=0,
                                      "Egen enhet"=2), selected = 2
              ),
              dateRangeInput(inputId = 'datovalg',
                             start = "2018-01-01",
                             end = Sys.Date(),
                             label = "Tidsperiode",
                             separator="t.o.m.",
                             language="nb"
              ),
              br(),
              h5("Figurene her gjelder kun startregistreringer.")
              #Registration type. Now excluded. Instead hardcoded in server
              #to only show start registrations.
              # checkboxGroupInput(
              #   inputId = 'regType',
              #   label='Registreringstype',
              #   choices = c("Start: Kun utredning, voksne"=1,
              #               "Start: Kun utredning, barn og unge"=2,
              #               "Start: Startregistrering, voksne"=3,
              #               "Start Startregistrering, barn og unge"=4),
              #   selected = c("Start: Kun utredning, voksne"=1,
              #                "Start: Kun utredning, barn og unge"=2,
              #                "Start: Startregistrering, voksne"=3,
              #                "Start: Startregistrering, barn og unge"=4)
              # )
            ),

            h5("Bygget med",
               img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png",
                   height = "30px"),
               "by",
               img(src = "https://rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png",
                   height ="30px"),
               ".")
          ),

          mainPanel(
            plotOutput(outputId = 'fordelinger',
                       width="800px",
                       height = "800px"),
            # placed here but could be inside any panel
            useShinyalert(),
            appNavbarUserWidget(user = uiOutput("appUserName"),
                                organization = uiOutput("appOrgName"),
                                addUserInfo = TRUE),
            tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico"))

          )
        ),
        tabPanel(
          "Medisinske karakteristika",
          sidebarPanel(
            width = 3,
            wellPanel(
              #h3("Hovedvalg"),
              selectInput(
                inputId = "valgtVarMed", label="Variabel",
                choices = c('BMI' = 'MedBMI',
                            'CIA' = 'CIA30GlobalScore',
                            'EDE-Q' = 'EDEQ60GlobalScore',
                            'SCL-90-R'='SCL90GSI',
                            'SDQ (global skår)' = 'SDQGlobalScore'
                )
              ),
              selectInput(inputId = 'enhetsUtvalgMed',
                          label='Egen enhet/landet',
                          choices = c("Egen mot resten av landet"=1,
                                      "Hele landet"=0,
                                      "Egen enhet"=2),
                          selected = 0
              ),
              dateRangeInput(inputId = 'datovalgMed',
                             start = "2018-01-01",
                             end = "2021-12-31", #Sys.Date()
                             label = "Tidsperiode",
                             separator="t.o.m.",
                             language="nb"
              ),

              # h5("Viktig om registreringstype under: Velger du ingen kan
              #     figurene vise en blanding av start- og slutt-registreringer,
              #     og bli misvisende."),
              # checkboxGroupInput(
              #   inputId = 'regTypeMed',
              #   label ='Registreringstype',
              #   choices = c(
              #     "Start: Kun utredning, voksne"=1,
              #     "Start: Kun utredning, barn og unge"=2,
              #     "Start: Startregistrering, voksne"=3,
              #     "Start Startregistrering, barn og unge"=4,
              #     "Slutt: Sluttregistrering, voksne"=5,
              #     "Slutt: Sluttregistrering, barn og unge"=6,
              #     "Slutt: Avbruddsregistrering, voksne"=98,
              #     "Slutt: Avbruddsregistrering, barn og unge"=99),
              #   selected = c(1,2,3,4)
              # ),
              selectInput(
                inputId = 'regTypeStartEndMed',
                label = "Registreringstype (start/slutt)",
                choices = c(#" " = "",
                            "Start" = "start",
                            "Slutt" = "end")
              ),
              selectizeInput(
                inputId = 'regTypeChildAdultMed',
                label = "Registreringstype (barn/voksne)",
                choices = c("Barn/unge og voksne" = "both",
                            "Barn/unge" = "child",
                            "Voksen" = "adult")
              )
            )
          ),
          mainPanel(
            plotOutput(outputId = 'fordelingerMed',
                       width="800px",
                       height = "800px")
          )
        ),
        tabPanel(
          "Diagnoser",
          sidebarPanel(
            width = 3,
            wellPanel(
              #h3("Hovedvalg"),
              selectInput(
                inputId = "valgtVarDiag", label="Variabel",
                choices = c('Diagnoser (ICD-10)' = 'DiagVSF',
                            'Diagnoser (DSM-5)' = 'DiagVDSM5Hoved'
                )
              ),
              selectInput(inputId = 'enhetsUtvalgDiag',
                          label='Egen enhet/landet',
                          choices = c("Egen mot resten av landet"=1,
                                      "Hele landet"=0,
                                      "Egen enhet"=2),
                          selected = 0
              ),
              dateRangeInput(inputId = 'datovalgDiag',
                             start = "2018-01-01",
                             end = "2021-12-31", #Sys.Date()
                             label = "Tidsperiode",
                             separator="t.o.m.",
                             language="nb"
              ),
              # selectInput(
              #   inputId = 'regTypeStartEndDiag',
              #   label = "Registreringstype (start/slutt)",
              #   choices = c(" " = "",
              #               "Start" = "start",
              #               "Slutt" = "end")
              # ),
              selectizeInput(
                inputId = 'regTypeChildAdultDiag',
                label = "Registreringstype (barn/voksne)",
                choices = c("Barn/unge og voksne" = "both",
                            "Barn/unge" = "child",
                            "Voksen" = "adult")
              )
            )
          ),
          mainPanel(
            plotOutput(outputId = 'fordelingerDiag',
                       width="800px",
                       height = "800px")
          )
        ),

        tabPanel(
          "Oppsummeringstabeller",
          sidebarPanel(),
          mainPanel(
            uiOutput(outputId = 'fordelingerOppsumTab')
          ))
      )
    ),#tab FIGURER: Pasientkarakteristika"

    tabPanel(
      "Resultater",
      tabsetPanel(
        type="tabs",
        id = "tabsets",
        tabPanel(
          "Kvalitetsindikatorer",
          sidebarPanel(
            width = 3,
            wellPanel(
              #h3("Hovedvalg"),
              selectInput(
                inputId = "valgtVarInd",
                label="Variabel",
                choices = c(
                  'CIA endring' = 'CIA30GlobalScoreRCI',
                  'EDE-Q endring' = 'EDEQ60GlobalScoreRCI',
                  'Undervektstatus' = 'IkkeUndervektSlutt',
                  'Pasientvurdert utbytte' = 'PO09Utbytte',
                  'Pasientvurdert utfall' = 'PT03Utfallsvurd'
                )
              ),
              selectInput(
                inputId = 'enhetsUtvalgInd',
                label='Egen enhet/landet',
                choices = c(
                  "Egen mot resten av landet"=1,
                  "Hele landet"=0,
                  "Egen enhet"=2),
                selected = 0
              ),
              dateRangeInput(
                inputId = 'datovalgInd',
                start = "2018-01-01",
                end = "2020-01-01",#Sys.Date(),
                label = "Tidsperiode",
                separator="t.o.m.",
                language="nb"
              ),

              # h5("Viktig om registreringstype under: Velger du ingen kan
              #    figurene vise en blanding av start- og slutt-registreringer,
              #    og bli misvisende."),
              # checkboxGroupInput(
              #   inputId = 'regTypeInd',
              #   label='Registreringstype',
              #   choices = c(
              #     "Start: Kun utredning, voksne"=1,
              #     "Start: Kun utredning, barn og unge"=2,
              #     "Start: Startregistrering, voksne"=3,
              #     "Start Startregistrering, barn og unge"=4,
              #     "Slutt: Sluttregistrering, voksne"=5,
              #     "Slutt: Sluttregistrering, barn og unge"=6,
              #     "Slutt: Avbruddsregistrering, voksne"=98,
              #     "Slutt: Avbruddsregistrering, barn og unge"=99),
              #   selected = c(
              #     "Start: Kun utredning, voksne"=1,
              #     "Start: Kun utredning, barn og unge"=2,
              #     "Start: Startregistrering, voksne"=3,
              #     "Start Startregistrering, barn og unge"=4)
              #),
              # selectInput(
              #   inputId = 'regTypeStartEndInd',
              #   label = "Registreringstype (start/slutt)",
              #   choices = c(" " = "",
              #               "Start" = "start",
              #               "Slutt" = "end")
              # ),
              selectizeInput(
                inputId = 'regTypeChildAdultInd',
                label = "Registreringstype (barn/voksne)",
                choices = c("Barn/unge og voksne" = "both",
                            "Barn/unge" = "child",
                            "Voksen" = "adult")
              )
            )
          ),

          mainPanel(
            plotOutput(
              outputId = 'fordelingerInd',
              width="800px",
              height = "800px")
          )
        ),
        tabPanel(
          "Gjennomsnittlig endring (start/slutt)",
          sidebarPanel(
            width = 3,
            wellPanel(
              #h3("Sammenligninger fra start til slutt"),
              selectInput(
                inputId = "valgtVarPrePost",
                label="Variabel",
                choices = c(
                  'EDE-Q (global)' = 'EDEQ60GlobalScore',
                  'EDE-Q (restriksjon)' = 'EDEQ60Restriksjon',
                  'EDE-Q (kroppsform)'= 'EDEQ60Kroppsform',
                  'EDE-Q (spising)' = 'EDEQ60Spising',
                  'EDE-Q (vekt)' = 'EDEQ60Vekt',
                  'CIA (global)' = 'CIA30GlobalScore',
                  'CIA (personlig)' = 'CIA30Personlig',
                  'CIA (sosial)' = 'CIA30Sosial',
                  'CIA (kognitiv)' = 'CIA30Kognitiv',
                  'SCL-90-R (GSI)' = 'SCL90GSI',
                  'SDQ (global skår) ' = 'SDQGlobalScore',
                  'BMI' = 'MedBMI')
              ),
              selectInput(
                inputId = 'enhetsUtvalgPrePost',
                label='Egen enhet/landet',
                choices = c(#"Egen mot resten av landet"=1,
                  "Hele landet"=0,
                  "Egen enhet"=2),
                selected = 0
              ),
              dateRangeInput(
                inputId = 'datovalgPrePost',
                start = "2018-01-01",
                end = "2021-12-31",# Sys.Date(),
                label = "Tidsperiode (datoene gjelder sluttregistreringen)",
                separator="t.o.m.",
                language="nb"
              ),
              strong("BMI ved start:"),
              numericInput(
                inputId='bmistartPrePost1',
                label = "Fra:",
                min = 0,
                max = 100,
                value = 0,
                step = 0.5
              ),
              numericInput(
                inputId='bmistartPrePost2',
                label = "Til:",
                min = 0,
                max = 100,
                value = 100,
                step = 0.5
              ),
              checkboxInput(
                inputId = "addCIPrePost",
                label = "Konfidensinterval",
                value = FALSE
              )
            ),
            h5("Bygget med",
               img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png",
                   height = "30px"),
               "by",
               img(src = "https://rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png",
                   height ="30px"),
               ".")
          ),
          mainPanel(
            plotOutput(outputId = 'PrePost', width="800px", height = "800px")
          )
        ),
        tabPanel(
          "Pasientenes vurderinger",
          sidebarPanel(
            width = 3,
            wellPanel(
              #h3("Hovedvalg"),
              selectInput(
                inputId = "valgtVarPas",
                label="Variabel",
                choices = c(
                  'Pasientvurdert utbytte' = 'PO09Utbytte',
                  'Pasientvurdert utfall' = 'PT03Utfallsvurd'
                )
              ),
              selectInput(
                inputId = 'enhetsUtvalgPas',
                label='Egen enhet/landet',
                choices = c("Egen mot resten av landet"=1,
                            "Hele landet"=0,
                            "Egen enhet"=2),
                selected = 0
              ),
              dateRangeInput(
                inputId = 'datovalgPas',
                start = "2018-01-01",
                end = "2021-12-31", #start = "2012-01-01", end = Sys.Date()
                label = "Tidsperiode (datoene gjelder sluttregistreringen)",
                separator="t.o.m.",
                language="nb"
              ),

              # h5("Viktig om registreringstype under: Velger du ingen kan
              #    figurene vise en blanding av start- og slutt-registreringer,
              #    og bli misvisende."),
              # checkboxGroupInput(
              #   inputId = 'regTypePas',
              #   label='Registreringstype',
              #   choices = c("Voksen - slutt"=5,"Barn og unge - slutt"=6),
              #   selected = c(5,6)
              # ),
              selectizeInput(
                inputId = 'regTypeChildAdultPas',
                label = "Registreringstype (barn/voksne)",
                choices = c("Barn/unge og voksne" = "both",
                            "Barn/unge" = "child",
                            "Voksen" = "adult")
              )

            )

          ),

          mainPanel(
            plotOutput(outputId = 'fordelingerPas',
                       width="800px",
                       height = "800px")
          )
        ),
        tabPanel(
          "Sykehussammenligninger (slutt)", #change?Update hideTab, server
          sidebarPanel(
            width = 3,
            wellPanel(
              selectInput(
                inputId = "valgtVarSykehusSammenlign",
                label="Variabel",
                choices = c("Pasientvurdert pasientsikkerhet" = "PROP_PO10Pasientsikkerhet",
                            "KI 4: Pasientvurdert utbytte" = "PROP_PO09Utbytte",
                            "KI 5: Pasientvurdert utfall" = "PROP_PT03Utfallsvurd",
                            "Pasientvurdert tilgjengelighet" = "PasOppTilgjengelighet",
                            "Pasientvurdert tilfredshet" = "PasOppTilfredshet",
                            "Pasienterfaringer" = "PasOppErfaring")
              ),
              dateRangeInput(
                inputId = 'datovalgSykehusSammenlign',
                start = "2018-01-01",
                end = "2021-12-31",# Sys.Date(),
                label = "Tidsperiode (datoene gjelder sluttregistreringen)",
                separator="t.o.m.",
                language="nb"
              )
            )
          ),

          mainPanel(
            plotOutput(
              outputId = 'sykehusSammenlign',
              width="800px",
              height = "800px")

          )

        ),
        tabPanel(
          "Sykehussammenligninger (start/slutt)",#change? Update hideTab,server
          sidebarPanel(
            width = 3,
            wellPanel(
              selectInput(
                 inputId = "valgtVarSykehusSammenlign2",
                 label="Variabel",
                 choices = c("KI 1: Endring SF symptomer" =
                               "EDEQ60GlobalScore_CHANGE_PROP",
                             "KI 2: Endring i funksjon" =
                               "CIA30GlobalScore_RCI_01",
                             "KI 3: Bortfall undervekt" =
                               "MedBMI_start18.5_slutt18.5"
                             )
              ),
              dateRangeInput(
                 inputId = 'datovalgSykehusSammenlign2',
                 start = "2018-01-01",
                 end = "2021-12-31",# Sys.Date(),
                 label = "Tidsperiode (datoene gjelder sluttregistreringen)",
                 separator="t.o.m.",
                 language="nb"
              )
            )
          ),

          mainPanel(
             plotOutput(
               outputId = 'sykehusSammenlign2',
               width="800px",
               height = "800px")

          )
          )

      )

    ),#tab Resultater

    #TODO: This tab will be implemented later
    # tabPanel(
    #   "Datakvalitet",
    #   tabsetPanel(
    #     type="tabs",
    #     tabPanel("Variabelkompletthet",
    #              fluidPage()
    #     ),
    #     tabPanel("Forløpskompletthet",
    #              sidebarPanel(width = 3),
    #              mainPanel()
    #     ),
    #     tabPanel("Tilslutning",
    #              sidebarPanel(width = 3),
    #              mainPanel()
    #     ),
    #     tabPanel("Dekningsgrad",
    #              sidebarPanel(width = 3),
    #              mainPanel()
    #     )#,
    #   )
    # ),#tab Datakvalitet

    tabPanel(
      "Administrasjon", #change?Update hideTab, server
      tabsetPanel(
        type="tabs",
        tabPanel(
          "Nøkkeltall",
          fluidPage(
            setBackgroundColor(color = "ghostwhite"),
            useShinydashboard(),
            fluidRow(
              box(
                status = "primary",
                selectInput(
                  "valgtEnhetNokkeltall",
                  "Egen enhet/nasjonal",
                  choices = c(
                    "Nasjonal" = 'nasjonal',
                    "Egen enhet" = 'egenEnhet')
                )
              ),

              box(status = "primary",
                  selectInput(
                    "progress", " ",
                    choices = c("0%" = 0, "20%" = 20, "40%" = 40,
                                "60%" = 60, "80%" = 80, "100%" = 100)
                  )
              )
            ),

            # infoBoxes
            fluidRow(
              infoBox(
                "Antall unike pasienter",
                uiOutput("antallPas"), " ",
                icon = icon("users")
              )
              ,
              infoBox(
                " ", " ",
                icon = icon("users"),
                color = "green",
                fill = TRUE
              ),
              infoBox(
                " ",
                uiOutput("progress2"),
                icon = icon("users"),
                color = "purple"
              )
            ),
            # valueBoxes
            fluidRow(
              valueBox(
                uiOutput("orderNum"), " ",
                icon = icon("users"),
                href = "http://google.com"
              ),
              valueBox(
                tagList(" ", tags$sup(style="font-size: 20px", "%")),
                " ",
                icon = icon("line-chart"),
                color = "green"
              ),
              valueBox(
                htmlOutput("progress"), " ",
                icon = icon("users"),
                color = "purple"
              )
            ),

            # Boxes
            fluidRow(
              box(status = "primary",
                  sliderInput(
                    "orders",
                    "Tidsperiode",
                    min = 1, max = 2000,
                    value = 650),
                  selectInput(
                    "progress",
                    "Progresjon",
                    choices = c("0%" = 0, "20%" = 20, "40%" = 40, "60%" = 60,
                                "80%" = 80, "100%" = 100)
                  )
              ),
              box(title = "Antall startregistreringer over tid",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  plotOutput("plotAntallRegTid", height = 250)
              )
              ,
              box(title = " ",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(" ", height = 250)
              )
            )

          )
        ),

        tabPanel(
          "Registreringsoversikt",
          sidebarPanel(
            width = 3,
            dateRangeInput(
              inputId = 'datovalgRegOvers',
              start = "2018-01-01",
              end = "2021-12-31", #end = Sys.Date()
              label = "Velg tidsperiode",
              separator="t.o.m.",
              language="nb")
          ),
          mainPanel(
            DT::dataTableOutput("tableOvers")
          )
        ),
        tabPanel(
          "Registreringsoversikt (utvidet)",
          sidebarPanel(
            width = 3,
            dateRangeInput(
              inputId = 'datovalgRegOversUtv',
              start = "2012-01-01",
              end = "2020-12-31",
              label = 'Velg tidsperiode',
              separator="t.o.m.",
              language="nb")
          ),
          mainPanel(DT::dataTableOutput("tableOversUtv")
          )
        )#,

        # tabPanel("Samlerapporter (PDF)",
        #          sidebarPanel(width = 3,
        #            selectInput("srcFile", "Velg rapport:",
        #                        c("DUMMY_NorSpisEnhetsrapport.Rmd")),
        #            radioButtons('formatSamlerapport',
        #                         'Format for nedlasting',
        #                         c('pdf', 'html'),
        #                         inline = FALSE),
        #            downloadButton(outputId = 'downloadSamlerapport',
        #                         label='Last ned!',
        #                         class = "butt")
        #          ),
        #          mainPanel(
        #            htmlOutput("samlerapport", inline = TRUE)
        #          )
        # )
      )

    )#tab Administrasjon

  ))
)
