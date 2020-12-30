#' Make dichotomous variables (dich)
#'
#' Output is data, with new variables that ar dichotomous.
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make24_data_newVar01
#'
#' @param myInData RegData
#'
#' @return RegData (invisible)
#' @export
#'
#' @examples

fun2_1_4_RegData_newVarDich <- function(myInData=RegData){
  ###Dicotomous (0-1) variables

  #Variables indicating "missing" (REAL missing values are then coded as NA, and those that should not be considered as "null")
  #EDEQ60GlobalScore_miss
  #EDEQ60GlobalScore_missStart
  #EDEQ60GlobalScore_missEnd
  #EDEQ60GlobalScore_missStartEnd
  #CIA30GlobalScore__miss
  #CIA30GlobalScore_missStart
  #CIA30GlobalScore_missEnd
  #CIA30GlobalScore_missStartEnd
  #TODO: Diagnser ICD-10 ...404?
  #MedBMI_miss
  #MedBMI_missStart
  #MedBMI_missEnd
  #MedBMI_missEnd_whenU185Start
  #MedBMI_missStartEnd
  #PT03Utfallsvurd_miss
  #PO09Utbytte_miss
  #PasOppTilfredshet_miss
  #PasOppErfaring_miss
  #PasOppTilgjengelighet_miss
  #PasOppSikkerhet_miss
  #PT01OnsketInvolv_miss
  #PT02BleInvol_miss
  #PT04KontaktBrukerorg
  #PT05OrientertBrukerorg_miss
  #BehTiltak_miss                   (not made yet)


  #Other dicotomous variables ()
  #PROP_PO10Pasientsikkerhet

  #Quality indicators:
  #PROP_PO10Pasientsikkerhet



  #PROP here means that can be used to make proportions(TODO: rename to either Dic for Dicotomous or 01
  #(01 may be misleasing as in can indicate "one" or "first"))

  ##Variables indicating "missing"
  #EDEQ60GlobalScore_miss
  RegData <- RegData %>% mutate(EDEQ60GlobalScore_miss = case_when(
    !is.na(EDEQ60GlobalScore) & RegRegtype %in% c(1,2,3,4,5,6) ~ as.character(EDEQ60GlobalScore), #just keeping numeric values as is
    is.na(EDEQ60GlobalScore) & RegRegtype %in% c(1,2,3,4,5,6) ~ NA_character_,
    TRUE ~ "null")) #this means that RegRegType 98 and 99 are coded as "null" (and exluded from missing calc)

  #EDEQ60GlobalScore_missStart
  RegData <- RegData %>% mutate(EDEQ60GlobalScore_missStart = case_when(
    !is.na(EDEQ60GlobalScore) & RegRegtype %in% c(1,2,3,4) ~ as.character(EDEQ60GlobalScore), #just keeping numeric values as is
    is.na(EDEQ60GlobalScore) & RegRegtype %in% c(1,2,3,4) ~ NA_character_,
    TRUE ~ "null"))

  #EDEQ60GlobalScore_missEnd
  RegData <- RegData %>% mutate(EDEQ60GlobalScore_missEnd = case_when(
    !is.na(EDEQ60GlobalScore) & RegRegtype %in% c(5,6) ~ as.character(EDEQ60GlobalScore), #just keeping numeric values as is
    is.na(EDEQ60GlobalScore) & RegRegtype %in% c(5,6) ~ NA_character_,
    TRUE ~ "null"))


  #CIA30GlobalScore_miss
  RegData <- RegData %>% mutate(CIA30GlobalScore_miss = case_when(
    !is.na(CIA30GlobalScore) & RegRegtype %in% c(1,2,3,4,5,6) ~ as.character(CIA30GlobalScore), #just keeping numeric values as is
    is.na(CIA30GlobalScore) & RegRegtype %in% c(1,2,3,4,5,6) ~ NA_character_,
    TRUE ~ "null")) #this means that RegRegType 98 and 99 are coded as "null" (and exluded from missing calc)

  #CIA30GlobalScore_missStart
  RegData <- RegData %>% mutate(CIA30GlobalScore_missStart = case_when(
    !is.na(CIA30GlobalScore) & RegRegtype %in% c(1,2,3,4) ~ as.character(CIA30GlobalScore), #just keeping numeric values as is
    is.na(CIA30GlobalScore) & RegRegtype %in% c(1,2,3,4) ~ NA_character_,
    TRUE ~ "null"))

  #CIA30GlobalScore_missEnd
  RegData <- RegData %>% mutate(CIA30GlobalScore_missEnd = case_when(
    !is.na(CIA30GlobalScore) & RegRegtype %in% c(5,6) ~ as.character(CIA30GlobalScore), #just keeping numeric values as is
    is.na(CIA30GlobalScore) & RegRegtype %in% c(5,6) ~ NA_character_,
    TRUE ~ "null"))
  #MedBMI_miss
  RegData <- RegData %>% mutate(MedBMI_miss = case_when(
    !is.na(MedBMI) & RegRegtype %in% c(1,2,3,4,5,6) ~ as.character(MedBMI), #just keeping numeric values as is
    is.na(MedBMI) & RegRegtype %in% c(1,2,3,4,5,6) ~ NA_character_,
    TRUE ~ "null")) #this means that RegRegType 98 and 99 are coded as "null" (and exluded from missing calc)

  #MedBMI_missStart
  RegData <- RegData %>% mutate(MedBMI_missStart = case_when(
    !is.na(MedBMI) & RegRegtype %in% c(1,2,3,4) ~ as.character(MedBMI), #just keeping numeric values as is
    is.na(MedBMI) & RegRegtype %in% c(1,2,3,4) ~ NA_character_,
    TRUE ~ "null"))

  #MedBMI_missEnd
  RegData <- RegData %>% mutate(MedBMI_missEnd = case_when(
    !is.na(MedBMI) & RegRegtype %in% c(5,6) ~ as.character(MedBMI), #just keeping numeric values as is
    is.na(MedBMI) & RegRegtype %in% c(5,6) ~ NA_character_,
    TRUE ~ "null"))

  #PT03Utfallsvurd_miss
  RegData <- RegData %>% mutate(PT03Utfallsvurd_miss = case_when(
    PT03Utfallsvurd %in% c(1,2,3,4,5) ~ PT03Utfallsvurd, #just keeping numeric values as is
    PasientAlder >= 16 & PT03Utfallsvurd %in% c(9,"null") & RegRegtype %in% c(5,6)  ~ NA_character_, # 9 missing is coded as missing
    TRUE ~ "null"))
  #PO09Utbytte_miss
  RegData <- RegData %>% mutate(PO09Utbytte_miss = case_when(
    PO09Utbytte %in% c(0,1,2,3,4) ~ PO09Utbytte, #just keeping numeric values as is
    PO09Utbytte %in% c(9,99, "null") & RegRegtype %in% c(5,6)  ~ NA_character_, #9 and 99 which is "ikke aktuelt" and "ikke besvart" is also coded as missing
    TRUE ~ "null"))
  #PasOppTilfredshet_miss
  RegData <- RegData %>% mutate(PasOppTilfredshet_miss = case_when(
    PasOppTilfredshet != "null" ~ PasOppTilfredshet, #just keeping numeric values as is
    PasOppTilfredshet == "null" & RegRegtype %in% c(5,6)  ~ NA_character_,
    TRUE ~ "null"))
  #PasOppErfaring_miss
  RegData <- RegData %>% mutate(PasOppErfaring_miss = case_when(
    PasOppErfaring != "null" ~ PasOppErfaring, #just keeping numeric values as is
    PasOppErfaring == "null" & RegRegtype %in% c(5,6)  ~ NA_character_,
    TRUE ~ "null"))
  #PasOppTilgjengelighet_miss
  RegData <- RegData %>% mutate(PasOppTilgjengelighet_miss = case_when(
    PasOppTilgjengelighet != "null" ~ PasOppTilgjengelighet, #just keeping numeric values as is
    PasOppTilgjengelighet == "null" & RegRegtype %in% c(5,6)  ~ NA_character_,
    TRUE ~ "null"))
  #PasOppSikkerhet_miss
  RegData <- RegData %>% mutate(PasOppSikkerhet_miss = case_when(
    PasOppSikkerhet != "null" ~ PasOppSikkerhet, #just keeping numeric values as is
    PasOppSikkerhet == "null" & RegRegtype %in% c(5,6)  ~ NA_character_,
    TRUE ~ "null"))

  #PT01OnsketInvolv_miss
  RegData <- RegData %>% mutate(PT01OnsketInvolv_miss = case_when(
    PT01OnsketInvolv %in% c(0,1) ~ PT01OnsketInvolv, #just keeping numeric values
    PasientAlder >= 16 & PT01OnsketInvolv %in% c(9,"null") & RegRegtype %in% c(5,6)  ~ NA_character_,
    TRUE ~ "null"))
  #PT02BleIvolv_miss
  RegData <- RegData %>% mutate(PT02BleInvolv_miss = case_when(
    PT02BleInvolv %in% c(0,1) ~ PT02BleInvolv, #just keeping numeric values
    PasientAlder  >= 16 & PT02BleInvolv %in% c(9,"null") & RegRegtype %in% c(5,6)  ~ NA_character_,
    TRUE ~ "null"))
  #PT04KontaktBrukerorg_miss
  RegData <- RegData %>% mutate(PT04KontaktBrukerorg_miss = case_when(
    PT04KontaktBrukerorg %in% c(0,1) ~ PT04KontaktBrukerorg, #just keeping numeric values
    PasientAlder  >= 16 & PT04KontaktBrukerorg %in% c(9,"null") & RegRegtype %in% c(5,6)  ~ NA_character_,
    TRUE ~ "null"))
  #PT05OrientertBrukerorg_miss
  RegData <- RegData %>% mutate(PT05OrientertBrukerorg_miss = case_when(
    PT05OrientertBrukerorg %in% c(0,1) ~ PT05OrientertBrukerorg, #just keeping numeric values
    PasientAlder  >= 16 & PT05OrientertBrukerorg %in% c(9,"null") & RegRegtype %in% c(5,6)  ~ NA_character_,
    TRUE ~ "null"))

  ##Other dicotomous variables
  #PO10Pasientsikkerhet...Values in raw data: (0,1,2,3,4,9,99,'null)
  #1,2,3)
  RegData$PROP_PO10Pasientsikkerhet <- RegData$PO10Pasientsikkerhet
  #1)
  RegData$PROP_PO10Pasientsikkerhet <- dplyr::recode(RegData$PO10Pasientsikkerhet,
                                                     "0"="1",
                                                     "1"="0",
                                                     "2"="0",
                                                     "3"="0",
                                                     "4"="0")#recode function is "questioning" (see ?recode)
  RegData$PROP_PO10Pasientsikkerhet <- replace(RegData$PROP_PO10Pasientsikkerhet,
                                               RegData$PROP_PO10Pasientsikkerhet %in%
                                                 c("9",
                                                   "99",
                                                   "null"),
                                               NA)

  #as.numeric to enable calculations (this also removes "null" values (where there are any left) and introduces NAs)
  RegData$PROP_PO10Pasientsikkerhet <- as.numeric(RegData$PROP_PO10Pasientsikkerhet)

  #2)
  #Making missing variable ("ikke besvart" and  every observation with end registration where patient is above 15 years)
  #RegData[which(RegData$MISSING_PO10Pasientsikkerhet%in% c("0","1","2","3","4","9")), ]$MISSING_PO10Pasientsikkerhet <- "0"
  #RegData[which(RegData$MISSING_PO10Pasientsikkerhet %in% c("99")), ]$MISSING_PO10Pasientsikkerhet <- "1"
  #3) Making a "ikke aktuelt" variable:
  #RegData[which(as.numeric(RegData$UAKTUELL_PO10Pasientsikkerhet)%in%c("0","1","2","3","4","99")),]$UAKTUELL_PO10Pasientsikkerhet <-"0"
  #RegData[which(as.numeric(RegData$UAKTUELL_PO10Pasientsikkerhet) %in% c("9")), ]$UAKTUELL_PO10Pasientsikkerhet <- "1"

  #RegData$MISSING_PO10Pasientsikkerhet <- as.numeric(RegData$MISSING_PO10Pasientsikkerhet)
  #RegData$UAKTUELL_PO10Pasientsikkerhet <- as.numeric(RegData$UAKTUELL_PO10Pasientsikkerhet)

  ##Quality indicators (more precisely MISSING for QIs)
  #QI1: Already OK
  #TODO to get missing: table(is.na(RegData$EDEQ60GlobalScore))[1]/ (table(is.na(RegData$EDEQ60GlobalScore))[2] + table(is.na(RegData$EDEQ60GlobalScore))[1])
  #TODO OR USE: sum(is.na(RegData$EDEQ60GlobalScore))

  #QI5: P09 utbytte (0-1)
  RegData$PROP_PO09Utbytte <- dplyr::recode(RegData$PO09Utbytte,
                                            "0"="0",
                                            "1"="0",
                                            "2"="1",
                                            "3"="1",
                                            "4"="1")#regarding recode function, be aware that it is "questioning" (see ?recode)
  RegData$PROP_PO09Utbytte <- replace(RegData$PROP_PO09Utbytte,
                                      RegData$PROP_PO09Utbytte %in%
                                        c("9",
                                          "99",
                                          "null"),
                                      NA)
  RegData$PROP_PO09Utbytte <- as.numeric(RegData$PROP_PO09Utbytte)

  #QI4: PT03 utfall (0-1)
  RegData$PROP_PT03Utfallsvurd <- dplyr::recode(RegData$PT03Utfallsvurd,
                                                "1"="1",#Ikke noe problem lenger
                                                "2"="1",#Klar bedring
                                                "3"="1",#Noe bedring
                                                "4"="0",#Uendret
                                                "5"="0") #Forverret
  RegData$PROP_PT03Utfallsvurd <- replace(RegData$PROP_PT03Utfallsvurd,
                                          RegData$PROP_PT03Utfallsvurd %in%
                                            c("9",
                                              "null"),
                                          NA)
  RegData$PROP_PT03Utfallsvurd <- as.numeric(RegData$PROP_PT03Utfallsvurd) #need to be numeric, also introduces NAs where "null"

  #PT03 utfall MISSING (0-1)
  RegData$MISSING_PT03Utfallsvurd <- dplyr::recode(RegData$PT03Utfallsvurd,
                                                   "1"="0",#Ikke noe problem lenger
                                                   "2"="0",#Klar bedring
                                                   "3"="0",#Noe bedring
                                                   "4"="0",#Uendret
                                                   "5"="0") #Forverret

  RegData$MISSING_PT03Utfallsvurd <- replace(RegData$MISSING_PT03Utfallsvurd,
                                             RegData$MISSING_PT03Utfallsvurd == "9",
                                             1)

  RegData$MISSING_PT03Utfallsvurd <- as.numeric(RegData$MISSING_PT03Utfallsvurd) #need to be numeric, also introduces NAs where "null"

  #PT03 utfall UAKTUELL (0-1) (EMPTY - there is no such category on this variable)

  ### END: Quality indicators ###


  output <- RegData
  return(invisible(output))

}
