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
library(ggplot2)

shiny::addResourcePath('rap', system.file('www', package='rapbase'))


##PREPARATIONS FOR MAP TAB:
#---
#1. Userfriendly names of variable choices in app:
#1.1. By first selecting/making it possible to choose all attribute variables (i.e. beg. w. "egensk"),
#and that are okeyed (ending w. OK):
#for different ways to use and call internal data, see:
#https://stackoverflow.com/questions/9521009/how-do-you-handle-r-data-internal-to-a-package
names <- intersect(
  grep("(^egensk)", names(norspis2:::attributedata_geo), value=TRUE),
  grep("(.OK)", names(norspis2:::attributedata_geo), value=TRUE))
names_map <- names[!(names %in% c("egensk_inkludere_OK", "egensk_datakval_kompl_dg2017_hfavd_OK",
                                  "egensk_datakval_kompl_dg2018start_hfavd_OK","egensk_datakval_kompl_dg2018slutt_hfavd_OK",
                                  "egensk_sengerSF_OK", "egensk_prioritert2020_OK", "egensk_kontaktperson_OK" ))]#variables that are (ARE NOT) OK in map
names_w_null<-c('null', names)

#1.1 And then making a dataframe with variable names and display names:
choices_df = data.frame(
  names =  names_map,
  displaynames = c('Tilslutning 2017', 'Tilslutning 2018', 'Tilslutning 2019','Tilslutning 2020')
)
# choices_df = data.frame(
#    names =  names,
#    displaynames = c('Inkludering', 'Tilslutning 2017', 'Tilslutning 2018', 'Tilslutning 2019', 'Dekningsgrad 2017')
# )

choices_df_w_null = data.frame(
  names =  names_w_null,
  displaynames = c('Ingen', 'Inkludering', 'Tilslutning 2017','Tilslutning 2018', 'Tilslutning 2019', 'Tilslutning 2020',
                   'Dekningsgrad 2017', 'Dekningsgrad 2018 (start)', 'Dekningsgrad 2018 (slutt)',
                   'Senger SF (N)', 'Prioritert 2020','Kontaktperson')
)
##PREPARATIONS FOR MAP TAB (END)


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
        h4("Navigering"),
        h5("Bruk fanene i toppen til å navigere til ulike typer
             rapporter.", align='left'),
        br(),
        h4("Retningslinjer for bruk"),
        strong("Hvem, hva, hvor?"),
        h5("Formålet med Rapporteket er å gi sykehusavdelingene statistikk og
        resultater som kan benyttes i det kontinuerlige forbedringsarbeidet.
        Rapportene er ment for intern bruk ved de registrerende
        behandlingsenhetene."),
        h5("Rapportene inneholder anonymiserte data.
        I tilfeller der man foretar utvalg som gir rapporter med få pasienter og
        hvor man kombinerer flere ulike utvalg og rapporter kan det ikke
        utelukkes med absolutt sikkerhet at rapporter kan tilbakeføres til
        enkeltpersoner."),
        h5("Husk:"),
        h5("– Bruk rapportene internt"),
        h5("– Avklar ekstern bruk med NorSpis (for eksempel ved ønske om bruk
           overfor media)"),
        br(),
        strong("Tolkning"),
        h5("Det er den enkelte avdeling som best forstår sine resultater.
        Samtidig anmoder vi om at rapportene i Rapporteket brukes
        kritisk. Utvis forsiktighet ved tolking. Ufullstendige datasett gjør at
        man må vurdere representativiteten. Med representativitet forstår
        vi hvor godt resultatene speiler pasientgruppens virkelige
        tilstand eller resultater. Lav dekningsgrad, samt at ikke alle data er
        ferdigstilt for aktuell tidsperiode, vil kunne bidra til lav
        representativitet når de man ikke har svar fra skiller seg fra de man
        har svar fra, noe som ofte er tilfellet. På lignende måte vil
        forløpskompletthet (andel registreringer med levert sluttregistrering)
        påvirke representativiteten på rapporter som bruker data fra
        sluttregistreringene. Dette er systematiske feilkilder.
        I nasjonale medisinske kvalitetsregistre er det et mål at
        dekningsgraden ved avdelingene og nasjonalt, er minst over 60 prosent,
           og helst over 80 prosent."),
        h5("Husk:"),
        h5("– Dekningsgrad, aktualitet og forløpskompletthet
           ved de enkelte enhetene og nasjonalt,
           vil kunne påvirke hvor representative dataene er"),
        h5("– Nasjonale tall vil vanskelig kunne være representative for
           andre enn pasientgruppen ved de enhetene som faktisk har
           registrert i NorSpis i den aktuelle tidsperioden"),
        br(),
        h4("Videre utvikling av Rapportekets innhold"),
        h5("Innholdet vil måtte utvikles til å bli stadig mer relevant for
           behandlingsenhetene."),
        h5("Førsteutgaven(e) av Rapporteket gir dere som brukere et
            utgangspunkt, fra hvor dere kan være med å utforme innholdet."),
        h5("Endringsforslag og ønsker eller tips om nye rapporter, kan sendes
           inn til NorSpis på norspis@nlsh.no."),
        h5(""),
        br(),
        h4("Datakvalitet"),
        h5("Dette er en tidlig utgave av Rapporteket. Ved spørsmål til figurene
           eller mistanke om at figurene eller datagrunnlaget kan inneholde feil
           , rapporteres det tilbake til registeret."),
        br()
        #Opprinnelig tekst i førsteutgaven:
        #Vi anmoder om at rapportene i Rapporteket brukes kritisk.
        #En viktig feilkilde vil trolig være ufullstendige datasett, som gjør
        #at man særlig må vurdere representativiteten kritisk. Lav dekningsgrad,
        #samt at ikke alle data er ferdgistilt for aktuell tidsperiode, vil
        #bidra til dårlig representativitet. Rapportene er ment for intern bruk
        #ved de registrerende behandlingsenhetene. Rapportene inneholder
        #anonymiserte data. I tilfeller der man foretar utvalg som gir rapporter
       #med få pasienter og hvor man kombinerer flere ulike utvalg og rapporter.
        #kan det ikke utelukkes med absolutt sikkerhet at rapporter data kan
        #tilbakeføres til enkeltpersoner. Av de ovennevnte grunnene ber NorSpis
        #om at rapportene dere henter ut fra Rapporteket ikke blir brukt i
        #offentlige sammenhenger eller offentliggjort på noe vis.
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
            wellPanel(h4("Søyler"),
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
                start = "2012-01-01",
                end = "2019-12-31",# Sys.Date(),
                label = "Tidsperiode (datoene gjelder sluttregistreringen)",
                separator="t.o.m.",
                language="nb"
              ),
              checkboxInput(
                inputId = "IDshowErrorBar",
                label = "Vis 95% konfidensintervall",
                value = FALSE
              )
            ),
            wellPanel(h4("Sammenligningsperiode (punkt)"),
                      selectInput(
                        inputId = "showComparisonPeriod",
                        label = "Legg til sammenligningsperiode",
                        choices = c("Nei" = FALSE,
                                    "Ja" = TRUE),
                        selected = FALSE
                      ),
                      conditionalPanel(
                        condition = "input.showComparisonPeriod == 'TRUE'",

                        dateRangeInput(
                          inputId = 'datovalgSykehusSammenlign_sammenlign',
                          start = "2020-01-01",
                          end = "2020-12-31",# Sys.Date(),
                          label = "Tidsperiode (datoene gjelder sluttregistreringen)",
                          separator="t.o.m.",
                          language="nb"
                        ),
                        checkboxInput(
                          inputId = "IDshowErrorBar2",
                          label = "Vis 95% konfidensintervall",
                          value = FALSE
                        )
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
            wellPanel(h4("Søyler"),
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
                 start = "2012-01-01",
                 end = "2019-12-31",# Sys.Date(),
                 label = "Tidsperiode (datoene gjelder sluttregistreringen)",
                 separator="t.o.m.",
                 language="nb"
              ),
              checkboxInput(
                inputId = "IDshowErrorBar_2",
                label = "Vis 95% konfidensintervall",
                value = FALSE
              )
            ),
            wellPanel(h4("Sammenligningsperiode (punkter)"),
                      selectInput(
                        inputId = "showComparisonPeriod_2",
                        label = "Legg til sammenligningsperiode",
                        choices = c("Nei" = FALSE,
                                    "Ja" = TRUE),
                        selected = FALSE
                      ),
                      conditionalPanel(
                        condition = "input.showComparisonPeriod_2 == 'TRUE'",

                        dateRangeInput(
                          inputId = 'datovalgSykehusSammenlign2_sammenlign',
                          start = "2020-01-01",
                          end = "2020-12-31",# Sys.Date(),
                          label = "Tidsperiode (datoene gjelder sluttregistreringen)",
                          separator="t.o.m.",
                          language="nb"
                        ),
                        checkboxInput(
                          inputId = "IDshowErrorBar2_2",
                          label = "Vis 95% konfidensintervall",
                          value = FALSE
                        )
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
    tabPanel(
      "Datakvalitet",
      tabsetPanel(
        type="tabs",
        tabPanel("Variabelkompletthet",
                 fluidPage()
        )
        # ,
        # tabPanel("Forløpskompletthet",
        #          sidebarPanel(width = 3),
        #          mainPanel()
        # ),
        # tabPanel("Tilslutning",
        #          sidebarPanel(width = 3),
        #          mainPanel()
        # ),
        # tabPanel("Dekningsgrad",
        #          sidebarPanel(width = 3),
        #          mainPanel()
        # )#,
      )
    ),#tab Datakvalitet

    tabPanel(
      "Administrasjon - registrar", #change?Update hideTab, server
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
              )


            ),

            # infoBoxes
            fluidRow(
              infoBox(
                "Antall unike pasienter",
                uiOutput("antallPas"), " ",
                icon = icon("users")
              )

            ),

            # Boxes
            fluidRow(
              box(title = "Antall startregistreringer over tid",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  plotOutput("plotAntallRegTid", height = 250)
              )
            )

          )
        ),
        tabPanel(
          "Rapportnedlastning",
          sidebarPanel(
            "Last ned en autogenerert rapport for din enhet her:",
            dateInput(inputId = "dateFromReportHospitalUnit", label = "Dato fra:"),
            dateInput(inputId = "dateToReportHospitalUnit", label = "Dato til:"),
            radioButtons('formatReportHospitalUnit', 'Dokoumentformat', c('PDF', 'HTML', 'Word'),
                         inline = TRUE),
            downloadButton(outputId = 'downloadReportHospitalUnit',
                           label='Last ned!',
                           class = "butt")
          )
        )


        )


    ),#tab Administrasjon registrar
    tabPanel(
      "Administrasjon - NorSpis", #change?Update hideTab, server
      tabsetPanel(
        type="tabs",
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
        ),
        tabPanel(
          "Datakvalitet (forløpskompletthet)",
          sidebarPanel(
            width = 3,
            dateRangeInput(
              inputId = 'datovalgDatakvalForlop',
              start = "2012-01-01",
              end = "2020-12-31",
              label = 'Velg tidsperiode',
              separator="t.o.m.",
              language="nb")
          ),
          mainPanel(tableOutput("tableDQcompletness_endreg")
          )
        ),
        tabPanel(
          "Rapport: Fagrådet",
          sidebarPanel(
            "Generer rapport til fagrådsmøte:",
            selectInput(inputId = "fileReportFagraad",
                        label = "Velg rapport:",
                        choices =
                          c("Nasjonal resultatrapport"="norspis-report-to-fagraad",
                            "Nasjonal oversiktsrapport"="norspis-oversiktsrapport")),
            dateInput(inputId = "dateFrom", label = "Dato fra:"),
            dateInput(inputId = "dateTo", label = "Dato til:"),
            radioButtons('formatReportFagraad', 'Dokoumentformat', c('PDF', 'HTML', 'Word'),
                          inline = TRUE),
            downloadButton(outputId = 'downloadReportFagraad',
                           label='Last ned!',
                           class = "butt")
            )
          #,
          # mainPanel(
          #   htmlOutput("samlerapport", inline = TRUE)
          #   )
          ),
        tabPanel(
          "Rapport: Enhetsoversikter (kart og tabell)",
          sidebarLayout(
            sidebarPanel(width = 2,
                         selectInput("chosen_hf", "Helseforetak:",
                                     choices = c('Alle',
                                                 'Helse Nord',
                                                 'Helse Midt',
                                                 'Helse Vest',
                                                 'Helse Sør-Øst',
                                                 'Helse Møre og Romsdal', 'Helse Nord-Trøndelag', 'St. Olavs Hospital',
                                                 'Finnmarkssykehuset','Helgelandssykehuset', 'Nordlandssykehuset',
                                                 'Universitetssykehuset Nord-Norge','Private behandlingssteder',
                                                 'Akershus universitetssykehus', 'Oslo universitetssykehus',
                                                 'Sykehuset i Vestfold', 'Sykehuset Innlandet', 'Sykehuset Telemark',
                                                 'Sykehuset Østfold','Sørlandet Sykehus', 'Vestre Viken', 'Helse Bergen',
                                                 'Helse Fonna', 'Helse Førde', 'Helse Stavanger'),#choices, see: unique(attributedata_geo$enhet_hf)
                                     selected = 'Alle'
                         ),

                         selectInput("chosen_color_var", "Variabel (kart):",
                                     choices = setNames(as.character(choices_df$names), choices_df$displaynames), #why as.character: https://stackoverflow.com/questions/30022732/shiny-all-sub-lists-in-choices-must-be-named
                                     selected = 'egensk_tilslutning2019_hfavd_OK'
                         ),
                         checkboxInput("includeMap", "Show map:", value=TRUE),

                         # c('Tilslutning 2019'='egensk_tilslutning2019_hfavd',
                         #          'Tilslutning 2018'='egensk_tilslutning2018_hfavd',
                         #          'Tilslutning 2017'='egensk_tilslutning2017_hfavd')),
                         # selectInput("chosen_map_area", "Område:",
                         #             choices = c('Norge'='norway', 'Nord'='north', 'Midt'='middle', 'Vest' = 'west', 'Sør-Øst'='southeast')),
                         checkboxGroupInput('chosen_unit_type',
                                            "Behandlingsenheter:",
                                            choices = c('Regional enhet'='3',
                                                        'Regional enhet (BU)'='11',
                                                        'Regional enhet (V)'='12',
                                                        'Spesialpoliklinikk' = '10',
                                                        'BU: Spisset tilbud SF'='4',
                                                        'V: spisset tilbud SF'='5',
                                                        'BU: Spisset tilbud SF med døgnbeh.' = '6',
                                                        'V: Spisset tilbud SF med døgnbeh.' = '7',
                                                        'BU: Ordinær BUP'='1',
                                                        'V: Ordinær DPS'='2',
                                                        'Annen/ukjent'='9'),
                                            selected = '3'),
                         radioButtons('visualize_points', "Vis enheter i kart:",
                                      choices = c('Ja'='yes',
                                                  'Nei'='no'),
                                      selected ='yes'),
                         radioButtons('visualize_text_unitname', "Vis enhetsnavn i kart:",
                                      choices = c('Ja'='yes',
                                                  'Nei'='no'),
                                      selected ='no'),
                         radioButtons('visualize_colour_polygons', "Fargelegg område med variabel 1:",
                                      choices = c('Ja'='yes',
                                                  'Nei'='no'),
                                      selected ='no'),
                         checkboxInput("includeTable", "Show table:", value=TRUE),
                         selectInput("chosen_color_var2", "Variabel 2 (tabell):",
                                     choices = setNames(as.character(choices_df_w_null$names), choices_df_w_null$displaynames)
                         ),
                         selectInput("chosen_color_var3", "Variabel 3 (tabell):",
                                     choices = setNames(as.character(choices_df_w_null$names), choices_df_w_null$displaynames)
                         ),
                         selectInput("chosen_string_var", "Variabel 4 (tabell)",
                                     choices = setNames(as.character(choices_df_w_null$names), choices_df_w_null$displaynames)),
                         downloadButton("report", "Generer rapport")
                         #FAILED: Tried to make a download buttont that downloaded report for all HF/RHF in one go
                         #(inspired by this post: https://stackoverflow.com/questions/40314582/how-to-download-multiple-reports-created-using-r-markdown-and-r-shiny-in-a-zip-f)
                         #downloadButton("reportS", "Rapporter for alle HF/RHF")

            ),
            # Show a plot of the generated distribution
            mainPanel(
              plotOutput("mapPlot", height="800px"),
              tableOutput("tableView")
            )
          )
        ),

        tabPanel(
          "Rapport: Sykehusavdelingene",
          sidebarPanel(
            "Lag mulighet for å generere PDF-rapporter for alle
            sykehusavdelingene her. Se fila norspis-create-reports.Rmd i inst"
          )
        ),

        tabPanel(
          "Test-rapport for Rmd til HTML/PDF",
            sidebarPanel(
              sliderInput("n", "Number of points", 1, 100, 50),

              radioButtons('format', 'Dokumentformat', c('PDF', 'HTML', 'Word'),
                           inline = TRUE),
              downloadButton("downloadReport", "Generate report")
            )
          )
        )
      )
    )
  )
)
