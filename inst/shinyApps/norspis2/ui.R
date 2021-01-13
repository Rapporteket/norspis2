library(shiny)
library(shinyBS) # Additional Bootstrap Controls
library(norspis2)
library(rapmap)
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
      "Norsk kvalitetsregister for behandling av spiseforstyrrelser"),
    windowTitle = "Norsk kvalitetsregister
                     for behandling av spiseforstyrrelser",
    theme = "rap/bootstrap.css",
    id = "tabs",

    # All the various tabs

    tabPanel(
      "HJEM",
      mainPanel(
        h2("Informasjon", align='center'),
        h3("Velkommen til Rapporteket", align='left'),
        h5("Bruk fanene  helt i toppen til å navigere til ulike typer
             rapporter.", align='left'),
        h5("Rapporteket skal inneholde rapporter som behandlingsenhetene kan
             benytte i sitt kontinuerlige forbedringsarbeid."),
        h5("Det innebærer at innholdet vil måtte utvikles til å bli stadig
             mer relevant for behandslingsenhetene."),
        h5("Tilbakemeldinger og samarbeid med behandlingsenhetene vil i så
             måte være viktig."),
        h5("Skriftlige tilbakemeldinger kan sendes til norspis@nlsh.no.")
      )
    ),#tab HJEM

    tabPanel(
      "Pasientkarakteristika",
      tabsetPanel(
        type="tabs",
        tabPanel(
          "Demografiske karakterstika",
          sidebarPanel(
            width = 3,
            wellPanel(
              #h3("Hovedvalg"),
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

              checkboxGroupInput(
                inputId = 'regType',
                label='Registreringstype',
                choices = c("Start: Kun utredning, voksne"=1,
                            "Start: Kun utredning, barn og unge"=2,
                            "Start: Startregistrering, voksne"=3,
                            "Start Startregistrering, barn og unge"=4),
                selected = c("Start: Kun utredning, voksne"=1,
                             "Start: Kun utredning, barn og unge"=2,
                             "Start: Startregistrering, voksne"=3,
                             "Start Startregistrering, barn og unge"=4)
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
                choices = c('Diagnoser (ICD-10)' = 'DiagVSF',
                            'BMI' = 'MedBMI',
                            'CIA' = 'CIA30GlobalScore',
                            'EDE-Q' = 'EDEQ60GlobalScore',
                            'Diagnoser (DSM-5)' = 'DiagVDSM5Hoved',
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

              h5("Viktig om registreringstype under: Velger du ingen kan
                  figurene vise en blanding av start- og slutt-registreringer,
                  og bli misvisende."),
              checkboxGroupInput(
                inputId = 'regTypeMed',
                label='Registreringstype',
                choices = c(
                  "Start: Kun utredning, voksne"=1,
                  "Start: Kun utredning, barn og unge"=2,
                  "Start: Startregistrering, voksne"=3,
                  "Start Startregistrering, barn og unge"=4,
                  "Slutt: Sluttregistrering, voksne"=5,
                  "Slutt: Sluttregistrering, barn og unge"=6,
                  "Slutt: Avbruddsregistrering, voksne"=98,
                  "Slutt: Avbruddsregistrering, barn og unge"=99),
                selected = c(1,2,3,4)
              )
            )
          ),
          mainPanel(
            plotOutput(outputId = 'fordelingerMed',
                       width="800px",
                       height = "800px")
          )
        )#,
      )
    ),#tab FIGURER: Pasientkarakteristika"

    tabPanel(
      "Resultater",
      tabsetPanel(
        type="tabs",
        tabPanel(
          "Indikatorer",
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

              h5("Viktig om registreringstype under: Velger du ingen kan
                 figurene vise en blanding av start- og slutt-registreringer,
                 og bli misvisende."),
              checkboxGroupInput(
                inputId = 'regTypeInd',
                label='Registreringstype',
                choices = c(
                  "Start: Kun utredning, voksne"=1,
                  "Start: Kun utredning, barn og unge"=2,
                  "Start: Startregistrering, voksne"=3,
                  "Start Startregistrering, barn og unge"=4,
                  "Slutt: Sluttregistrering, voksne"=5,
                  "Slutt: Sluttregistrering, barn og unge"=6,
                  "Slutt: Avbruddsregistrering, voksne"=98,
                  "Slutt: Avbruddsregistrering, barn og unge"=99),
                selected = c(
                  "Start: Kun utredning, voksne"=1,
                  "Start: Kun utredning, barn og unge"=2,
                  "Start: Startregistrering, voksne"=3,
                  "Start Startregistrering, barn og unge"=4)
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
          "Endring (start-slutt)",
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
              sliderInput(
                inputId='bmistartPrePost',
                label = "BMI ved start",
                min = 0,
                max = 100,
                value = c(0, 100),
                step = 0.5
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
                label = "Tidsperiode",
                separator="t.o.m.",
                language="nb"
              ),

              h5("Viktig om registreringstype under: Velger du ingen kan
                 figurene vise en blanding av start- og slutt-registreringer,
                 og bli misvisende."),
              checkboxGroupInput(
                inputId = 'regTypePas',
                label='Registreringstype',
                choices = c("Voksen - slutt"=5,"Barn og unge - slutt"=6),
                selected = c(5,6)
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
          "Sykehussammenligninger",
          sidebarPanel(
            width = 3,
            wellPanel(

            )
          ),

          mainPanel(
            plotOutput(
              outputId = 'sykehusSammenlign',
              width="800px",
              height = "800px")

          )
        )#,

      )

    ),#tab Resultater

    tabPanel(
      "Datakvalitet",
      tabsetPanel(
        type="tabs",
        tabPanel("Variabelkompletthet",
                 fluidPage()
        ),
        tabPanel("Forløpskompletthet",
                 sidebarPanel(width = 3),
                 mainPanel()
        ),
        tabPanel("Tilslutning",
                 sidebarPanel(width = 3),
                 mainPanel()
        ),
        tabPanel("Dekningsgrad",
                 sidebarPanel(width = 3),
                 mainPanel()
        )#,
      )
    ),#tab Datakvalitet

    tabPanel(
      "Administrasjon",
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
