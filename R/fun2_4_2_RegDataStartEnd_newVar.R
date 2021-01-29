#' Title
#'
#'
#' Make new variables in start-end data
#' Quality indicators
#' * EDEQ60GlobalScore_CHANGE_PROP
#' *...
#'
#' Data quality variables or the quality indicators
#' *...
#' * MedBMI_missEnd_whenU185Start
#'
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make27_dataStartEnd_NewVar
#'
#' @param myInData RegDataStartEnd
#'
#' @return RegDataStartEnd (invisible)
#' @export
#'
#' @examples
fun2_4_2_RegDataStartEnd_newVar <- function(myInData){

  RegDataStartEnd <- myInData
  #EDEQ60GlobalScore_CHANGE (KI1/QI1)
  #EDEQ60GlobalScoreRCI (0-1)       ### basert verdier gitt av ?.R? i e-post 06.05.2019
  #as.numeric (#have to use sub, becuase values contain commas (,) not period (.))
  RegDataStartEnd$EDEQ60GlobalScore.x_NUM <- as.numeric(RegDataStartEnd$EDEQ60GlobalScore.x)
  RegDataStartEnd$EDEQ60GlobalScore.y_NUM <- as.numeric(RegDataStartEnd$EDEQ60GlobalScore.y)

  RegDataStartEnd$EDEQ60GlobalScore_CHANGE <- RegDataStartEnd$EDEQ60GlobalScore.y_NUM -RegDataStartEnd$EDEQ60GlobalScore.x_NUM
  RegDataStartEnd$EDEQ60GlobalScore_CHANGE[RegDataStartEnd$EDEQ60GlobalScore_CHANGE >0.9]       <- 1 #Verre
  RegDataStartEnd$EDEQ60GlobalScore_CHANGE[RegDataStartEnd$EDEQ60GlobalScore_CHANGE <=0.9 &
                                             RegDataStartEnd$EDEQ60GlobalScore_CHANGE >=(-0.9)]   <- 2 #Uendret
  RegDataStartEnd$EDEQ60GlobalScore_CHANGE[RegDataStartEnd$EDEQ60GlobalScore_CHANGE <(-0.9) &
                                             RegDataStartEnd$EDEQ60GlobalScore.y >=2.5]           <- 3 #Bedring
  RegDataStartEnd$EDEQ60GlobalScore_CHANGE[RegDataStartEnd$EDEQ60GlobalScore_CHANGE <(-0.9) &
                                             RegDataStartEnd$EDEQ60GlobalScore.y < 2.5]           <- 4 #Symptomfri


  RegDataStartEnd$EDEQ60GlobalScore_CHANGE_PROP <-
    dplyr::recode(RegDataStartEnd$EDEQ60GlobalScore_CHANGE,
                  "1"="0",#verre
                  "2"="0",#uendret
                  "3"="1",#bedring
                  "4"="1") #symptomfri
  RegDataStartEnd$EDEQ60GlobalScore_CHANGE_PROP <- as.numeric(RegDataStartEnd$EDEQ60GlobalScore_CHANGE_PROP)

  RegDataStartEnd$EDEQ60GlobalScore_CHANGE_NAMES <-
    dplyr::recode(RegDataStartEnd$EDEQ60GlobalScore_CHANGE,
                  "1"="Verre",
                  "2"="Uendret",
                  "3"="Bedring",
                  "4"="Symptomfri")


  RegDataStartEnd$EDEQ60GlobalScore_kommentar <- 'Figuren viser andelen pasienter som kan kategoriseres som friske ved behandlingslutt basert p? endring i EDE-Q-sk?re (n?rmere definsjon
  hvor stor endring som regnes som relevant for ? angi bedring, finnes i ?rsrapporten).
  Noen svakheter ved figuren er:
  1. Noen pasienter kan i hht. instrumenentet ha v?rt friske ved behandlingsstart , og hvis de i hht. instrumentet ogs? var friske ved behandlingsslutt telles de likevel ikke med som friske.
  2. Generelt: Resultatene kan ikke regnes som representative. Det er lav dekningssgrad for sluttregistreringene i registeret.
  3. Slutt er ikke alltid slutt for hele forl?pet, men ved det enkelte behanlingssted f?r overf?ring til nytt behandlingssted.
  F.eks. kan en pasient ha noe bedring ved Follo D?gn, og overf?res til Follo DPS og ha noe bedring ved der, og selv om de totalt sett har blit bedre
  og friske, s? kategoriseres de som uendret begge gangene.
  KImaal <- 90 # andel p? 90%' #Beregeningen baserer seg p? normadata fra ... og tar hensyn til m?leusikkerheten...


  #CIA30GlobalScore_CHANGE:
  RegDataStartEnd$CIA30GlobalScore_CHANGE <- RegDataStartEnd$CIA30GlobalScore.y - RegDataStartEnd$CIA30GlobalScore.x

  # CIA30GlobalScore_RCI
  # basert verdier gitt av ?.R? i e-post 06.05.2019
  RegDataStartEnd<- RegDataStartEnd %>%
    mutate(CIA30GlobalScore_RCI =
             case_when(CIA30GlobalScore_CHANGE > 7 ~ 1, #Verre
                       CIA30GlobalScore_CHANGE >=(-7) & CIA30GlobalScore_CHANGE <= 7  ~ 2, #Uendret
                       CIA30GlobalScore_CHANGE <(-7) & CIA30GlobalScore.y >= 16 ~ 3, #Bedring
                       CIA30GlobalScore_CHANGE <(-7) & CIA30GlobalScore.y < 16 ~ 4)) #Stor forbedring
  # CIA30GlobalScore_RCI_01
  RegDataStartEnd<- RegDataStartEnd %>%
    mutate(CIA30GlobalScore_RCI_01 =
             case_when(CIA30GlobalScore_RCI %in% c(1,2) ~ 0,
                       CIA30GlobalScore_RCI %in% c(3,4) ~ 1))

  # MedBMI_start18.5_slutt18.5
  # MedBMI below 18.5 at start, but above at end (1), below 18.5 at start and still below at end (0)
  # We use the variable MedBMI_withIsoBMIBGSvalues  that we made earlier, so that those with IsoBMIBGS values use these value
  RegDataStartEnd <- RegDataStartEnd %>%
    mutate(MedBMI_start18.5_slutt18.5 = case_when(
      MedBMI_withIsoBMIBGSvalues.x < 18.5 & MedBMI_withIsoBMIBGSvalues.y >= 18.5 ~ 1,#underweight at start, no more underweight at end
      MedBMI_withIsoBMIBGSvalues.x < 18.5 & MedBMI_withIsoBMIBGSvalues.y < 18.5 ~ 0))#underweight at start, still underweight at end

  RegDataStartEnd$MedBMI_start18.5_slutt18.5 <- as.numeric(RegDataStartEnd$MedBMI_start18.5_slutt18.5)


  # comment <- '
  # - Figuren viser andelen paseinter som var undervektig ved behandlingsstart, men som ikke lenger var undervektig ved behandlingsslutt (bortfall av undervekt).
  # - Kun pasienter som ved start som var undervektig er inkludert, mens de som hadde BMI over 18.5 allerede ved start ikke er med
  # - Frav?r av undervekt er for voksne definert som en BMI p? 18.5 eller h?yere.
  #   NB! "Ved behandlingsslutt" inneb?rer i denne figuren kun registreringer av typen "slutt" og ikke registreringer av typen "avbrudd",
  #   ettersom det ikke registreres vekt i registreringer av typen "avbrudd".
  # - For pasienter med beregnet Iso_BMI (de som er yngre enn 19 ?r ved start), er iso-BMI-verdier benyttet.
  # - Mulig feilkilde, vurdere hvordan sl?r inn: En pasient kan v?re under 19 ?r ved start og ha Iso-BMI-verdi.
  #   Ved slutt kan pasient ha blitt over 19 ?r og dermed benytte vanlig ISO-BMI-verdi.
  #   Det kan medf?re at BMI-verdiene ikke n?dvendigvis er sammenlignbare (den kan f.eks. tilsynelatende g? ned, selv om BMI-verdien har v?rt uforandret)'


  # CompleteEnd
  # Create a variable which for each started treatment or examiniation (start or "kun utredning"), give info. on wheter end registration is complete (komplett, Slutt mangler, ikke aktuelt). "Kun utredning" is always complete as it should not have a end reg.
  RegDataStartEnd <- RegDataStartEnd %>%
    mutate(CompleteEnd = case_when(!is.na(RegRegtype.y) |RegRegtype.x %in% c(1,2) ~ "Komplett", #complete cases are thosw with valid end.reg. OR those with start.reg of the type "kun utredning" (1,2)
                                   is.na(RegRegtype.y) & RegRegtype.x %in% c(3,4) ~ "Slutt mangler")) #incomplete cases are those with invalid end reg. AND at the same time start. reg of the type start (3,4)




  ##new 0-1 var
  #TODO: separate 0-1 varables in own function as for the regular data (not start-end data)???


  #EDEQ60GlobalScore_missStartEnd (missing for KI1/QI1)
  RegDataStartEnd <- RegDataStartEnd %>%
    mutate(EDEQ60GlobalScore_missStartEnd = case_when(
      #RegRegtype.x %in% c(3,4) & is.na(EDEQ60GlobalScore.x) & !is.na(EDEQ60GlobalScore.y) ~ NA_character_,
      #RegRegtype.x %in% c(3,4) & !is.na(EDEQ60GlobalScore.x) & is.na(EDEQ60GlobalScore.y) ~ NA_character_,
      !is.na(EDEQ60GlobalScore.x) & !is.na(EDEQ60GlobalScore.y) ~ "valid",#valid
      RegRegtype.x %in% c(3,4) & !is.na(EDEQ60GlobalScore.x) & is.na(EDEQ60GlobalScore.y) ~ NA_character_,
      RegRegtype.x %in% c(3,4) & is.na(EDEQ60GlobalScore.x) & !is.na(EDEQ60GlobalScore.y)~ NA_character_,
      RegRegtype.x %in% c(3,4) & is.na(EDEQ60GlobalScore.x) & is.na(EDEQ60GlobalScore.y)~ NA_character_,
      #RegRegtype.x %in% c(3,4) & is.na(EDEQ60GlobalScore.x) & is.na(EDEQ60GlobalScore.x) ~ NA_character_,
      #RegRegtype.x %in% c(3,4) & RegRegtype.y %in% c(5,6)~ "null", #missing end
      #RegRegtype.x %in% c(1,2,5,6,98,99) ~ "null",
      TRUE ~ "null"))

  #CIA30GlobalScore_missStartEnd (missing for KI1/QI1)
  RegDataStartEnd <- RegDataStartEnd %>%
    mutate(CIA30GlobalScore_missStartEnd = case_when(
      #RegRegtype.x %in% c(3,4) & is.na(CIA30GlobalScore.x) & !is.na(CIA30GlobalScore.y) ~ NA_character_,
      #RegRegtype.x %in% c(3,4) & !is.na(CIA30GlobalScore.x) & is.na(CIA30GlobalScore.y) ~ NA_character_,
      !is.na(CIA30GlobalScore.x) & !is.na(CIA30GlobalScore.y) ~ "valid",#valid
      RegRegtype.x %in% c(3,4) & !is.na(CIA30GlobalScore.x) & is.na(CIA30GlobalScore.y) ~ NA_character_,
      RegRegtype.x %in% c(3,4) & is.na(CIA30GlobalScore.x) & !is.na(CIA30GlobalScore.y)~ NA_character_,
      RegRegtype.x %in% c(3,4) & is.na(CIA30GlobalScore.x) & is.na(CIA30GlobalScore.y)~ NA_character_,
      #RegRegtype.x %in% c(3,4) & is.na(CIA30GlobalScore.x) & is.na(CIA30GlobalScore.x) ~ NA_character_,
      #RegRegtype.x %in% c(3,4) & RegRegtype.y %in% c(5,6)~ "null", #missing end
      #RegRegtype.x %in% c(1,2,5,6,98,99) ~ "null",
      TRUE ~ "null"))


  #MedBMI_start18.5_slutt18.5_miss (missing for KI3/QI3)
  RegDataStartEnd <- RegDataStartEnd %>%
    mutate(MedBMI_start18.5_slutt18.5_miss = case_when(
      !is.na(MedBMI_start18.5_slutt18.5)  ~ "valid",#valid
      is.na(MedBMI_withIsoBMIBGSvalues.y) & MedBMI_withIsoBMIBGSvalues.x < 18.5  ~ NA_character_,
      TRUE ~ "null"))
  # ))
  # MedBMI_ < 18.5 & !is.na(MedBMI.y) & MedBMI.y >=18.5 ~ "teller",
  # MedBMI.x < 18.5 & !is.na(MedBMI.y) ~ "nevner", #code to "valid", valid BMI values at start for those with BMI under 18.5
  # MedBMI.x < 18.5 & is.na(MedBMI.y) ~ "null",
  # MedBMI.x >= 18.5 ~ "null",
  # is.na(MedBMI.x) ~  NA_character_,
  # TRUE ~ "null"))


  RegDataStartEnd
}
