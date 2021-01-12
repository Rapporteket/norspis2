#' Make variables where missing is coded as NA (MAsNA)
#'
#' Output is data, with new variables where missing is coded as NA. Some are categoricial, and som have values that are labels.
#' Accordingly the new variables are named with suffixes _MISSING, CAT, and _NAMES (TODO: rename to for instance  MaSNA, NumCat and LabCat)
#'
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make23_data_newVar
#'
#' @param myInData RegData
#'
#' @return myInData (invisble)
#' @export
#'
#' @examples

fun2_1_3_RegData_newVarMAsNA  <- function(myInData){
  #Made/formatted:
  #Year
  #PasientAlder_CAT_MISSING

  #B01Sivilstatus_MISSING
  #B01Sivilstatus_MISSING_NAMES
  #B03Bosituasjon_MISSING
  #B03Bosituasjon_MISSING_NAMES
  #B02EgneBarn_MISSING
  #B04PabegyntUtd_MISSING
  #B05FullfortUtd_MISSING
  #B06Hovedaktivitet_MISSING
  #B07Hovedinntekt_MISSING
  #B02EgneBarn_MISSING_NAMES
  #B04PabegyntUtd_MISSING_NAMES
  #B05FullfortUtd_MISSING_NAMES
  #B06Hovedaktivitet_MISSING_NAMES
  #B07Hovedinntekt_MISSING_NAMES

  #MedBMI_CAT_MISSING

  #DiagVSF_MISSING_NAMES
  #DiagBUAkse1_MISSING_NAMES
  #DiagSF_V_BU_MISSING_NAMES
  #DiagDSM5Hoved_V_BU_MISSING_NAMES
  #DiagVSomatiske_CAT_MISSING

  #MedPsykofarmaka_CAT_MISSING
  #MedAntidepressiva_CAT_MISSING
  #MedNevroleptika_CAT_MISSING
  #MedBenzodiazepiner_CAT_MISSING
  #MedAnnenMedBeh_CAT_MISSING
  #MedBlodprove_CAT_MISSING
  #MedBeintetthMaling_CAT_MISSING

  #B17FysMishandl_CAT_MISSING
  #B18PsykMishandl_CAT_MISSING
  #B19Overgrep_CAT_MISSING
  #B20Mobbing_CAT_MISSING
  #B21SelvskadTidl
  #B22SelvskadSisteAr
  #B23SelvmordFTidl
  #B24SelvmordFSisteAr
  #B25Avhengighet
  #PT01OnsketInvolv
  #pT02BleInvolv
  #PT04KontaktBrukerorg
  #PT05OrientertBrukerorg
  #PO05Involvert_CAT_MISSING

  #MedBMI_withIsoBMIBGSvalues

  #MISSING(TODO:replace with "MAsNA??) means that we have specifed which values should be counted as missing if not present, and these values are given the value NA
  #CAT means(TODO:replace with "NumCat"??)...a numeric variable that have been categorized.
  #NAMES (TODO:replace with "LabCat" ??) means...it is with labeled categories

  #"Year" variable
  myInData$Year <- lubridate::year(myInData$HovedDato_FRMT)

  #PasientAlder_CAT_MISSING (categorized and where missing is set to NA)
  #this variable will by design have 100 % completeness
  #(no "null" values and NAs must thus mean that registration is still in the making (BasisRegStatus is not equl to 1))
  myInData$PasientAlder_CAT_MISSING <- cut(myInData$PasientAlder,
                                          breaks=c(0,10,15,20,25,30,40,50, Inf),
                                          labels=c("(0,10]","(10,15]","(15,20]","(20,25]","(25,30]","(30,40]","(40,50]","50+"),
                                          ordered_result = T)
  #B01 _MISSING
  #new var where we decide and code which cases with value "null" should be interpreted as missing and thus coded into NA:
  myInData$B01Sivilstatus_MISSING <- myInData$B01Sivilstatus#make new var similar to old
  #myInData$B01Sivilstatus.xMISSING[myInData$B01Sivilstatus.x == 'null']<- NA #first code "null" to NA
  myInData$B01Sivilstatus_MISSING[myInData$RegRegtype %in% c(1,2,3,4) & #then choose which that we should code as "missing"
                                   #myInData$PasientAlder.x >= 15 &
                                   myInData$B01Sivilstatus == 'null'] <- NA

  #B01 _MISSING_NAMES
  #coded the missing var into av var with text values
  myInData$B01Sivilstatus_MISSING_NAMES <- dplyr::recode_factor(myInData$B01Sivilstatus_MISSING, #make it an ordered factor to prevent strange sorting
                                                               "1"="Enslig",
                                                               "2"="Samboer",
                                                               "3"="Gift",
                                                               "4"="Skilt",
                                                               "5"="Enke/enkemann",
                                                               "9"="Annen",
                                                               "null"="null",
                                                               .ordered = TRUE)

  #B03
  myInData$B03Bosituasjon_MISSING <- myInData$B03Bosituasjon

  myInData$B03Bosituasjon_MISSING[myInData$RegRegtype %in% c(1,2,3,4) & #then choose which that we should code as "missing"
                                   #myInData$PasientAlder.x >= 15 &
                                   myInData$B01Sivilstatus == 'null'] <- NA

  #B03 _MISSING_NAMES
  myInData$B03Bosituasjon_MISSING_NAMES <- dplyr::recode_factor(myInData$B03Bosituasjon_MISSING,
                                                               "1"="Hos en av foreldrene",
                                                               "2"="Hos begge foreldrene",
                                                               "3"="Bor alene",
                                                               "4"="Med partner",
                                                               "5"="Med partner og barn",
                                                               "6"="Uten partner med barn",
                                                               "9"="Annen",
                                                               "null"="null",
                                                               .ordered = TRUE)

  # B02,B04, B05, B06 and B07..._MISSING
  varToRecode <- c("B02EgneBarn","B04PabegyntUtd","B05FullfortUtd","B06Hovedaktivitet", "B07Hovedinntekt")

  for(varToRecode in varToRecode){

    myInData[,paste0(varToRecode, "_MISSING")]<- myInData[, paste0(varToRecode)]#make new var similar to old
    #myInData$B01Sivilstatus.xMISSING[myInData$B01Sivilstatus.x == 'null']<- NA #first code "null" to NA

    myInData[,paste0(varToRecode, "_MISSING")][myInData$RegRegtype %in% c(1,2,3,4) & #CONDITIONS TO CODE AS MISSING (startreg and age>15, and where value is "null")
                                                myInData$PasientAlder >= 15 &
                                                myInData[,varToRecode] == 'null'] <- NA
  }#OUTPUT: This gives out the variables with _MISSING suffix.

  # B02 _MISSING_NAMES (NEW CATEGORIES)
  myInData$B02EgneBarn_MISSING_NAMES <- replace(myInData$B02EgneBarn_MISSING,
                                               myInData$B02EgneBarn_MISSING >= "1" &
                                                 !(myInData$B02EgneBarn_MISSING =="null") #values above 1 and not "null" categorized as "1+"
                                               ,
                                               "1+")
  #make it an ordered factor as other _MISSING_NAMES variables:
  myInData$B02EgneBarn_MISSING_NAMES <- factor(myInData$B02EgneBarn_MISSING_NAMES,
                                              levels = c("0","1+", "null"),
                                              ordered = TRUE)

  # B04 _MISSING_NAMES
  myInData$B04PabegyntUtd_MISSING_NAMES <- dplyr::recode_factor(myInData$B04PabegyntUtd_MISSING,
                                                               "1"="Grunnskole",
                                                               "2"="Videreg?ende skole (1-3 ?r)",
                                                               "3"="H?gskole eller universitet, mindre enn 4 ?r",
                                                               "4"="H?gskole eller universitet, 4 ?r eller mer",
                                                               "9"="Ukjent",
                                                               "null"="null",
                                                               .ordered = TRUE)

  # B05 _MISSING_NAMES
  myInData$B05FullfortUtd_MISSING_NAMES <- dplyr::recode_factor(myInData$B05FullfortUtd_MISSING,
                                                               "1"="Ikke fullf?rt grunnskole",
                                                               "2"="Grunnskole",
                                                               "3"="Videreg?ende skole (1-3 ?r)",
                                                               "4"="H?gskole eller universitet, mindre enn 4 ?r",
                                                               "5"="H?gskole eller universitet, 4 ?r eller mer",
                                                               "9"="Ukjent",
                                                               "null"="null",
                                                               .ordered = TRUE)

  # B06 _MISSING_NAMES
  myInData$B06Hovedaktivitet_MISSING_NAMES <- dplyr::recode_factor(myInData$B06Hovedaktivitet_MISSING,
                                                                  "1"="Heltidsarbeid",
                                                                  "2"="Deltidsarbeid",
                                                                  "3"="P? arbeidsmarkedstiltak",
                                                                  "4"="Vernepliktig",
                                                                  "5"="Skoleelev/l?rling",
                                                                  "6"="Student",
                                                                  "7"="Sykemeldt",
                                                                  "8"="Uf?r",
                                                                  "9"="Annen",
                                                                  "null"="null",
                                                                  .ordered = TRUE)

  # B07 _MISSING_NAMES
  myInData$B07Hovedinntekt_MISSING_NAMES <- dplyr::recode_factor(myInData$B07Hovedinntekt_MISSING,
                                                                "1"="Arbeidsinntekt",
                                                                "2"="Sykepenger/trygd/pensjon",
                                                                "3"="Blir fors?rget",
                                                                "4"="Sosialhjelp",
                                                                "5"="Stipend/l?n",
                                                                "6"="Kursst?nad/l?nn i arbeidsmarkedstiltak",
                                                                "9"="Andre inntekter",
                                                                "null"="null",
                                                                .ordered = TRUE)

  # MEDICAL CHARACTERISTICS

  #MedBMI_CAT_MISSING (categorized and where missing is set to NA)
  myInData$MedBMI_CAT_MISSING <- cut(myInData$MedBMI,  #this variable will be design have 100 % completeness (no "null" values and NAs must be interpreted as e.g. not yet completely registered registration)
                                    breaks=c(0,10,12.5,15,17.5,20,25,30, Inf),
                                    labels=c("(0,10]","(10,12.5]","(12.5,15]","(15,17.5]","(17.5,20]","(20,25]","(25,30]","30+"),
                                    ordered_result = T)


  # DiagVSF_MISSING_NAMES (with missing coded as NA and categories as NAMES)
  myInData <- myInData %>% dplyr::mutate(DiagVSF_MISSING_NAMES = dplyr::case_when(
    DiagVSF == "F500" ~ "F50.0",
    DiagVSF == "F501" ~ "F50.1",
    DiagVSF == "F502" ~ "F50.2",
    DiagVSF == "F503" ~ "F50.3",
    DiagVSF == "F504" ~ "F50.4",
    DiagVSF == "F505" ~ "F50.5",
    DiagVSF == "F508" ~ "F50.8",
    DiagVSF == "F509" ~ "F50.9",
    DiagVSF == "F982" ~ "F98.2",
    DiagVSF == "F983" ~ "F98.3",
    RegRegtype %in% c(1,5,98) & DiagVSF == "" ~ NA_character_,
    DiagVSF == "" ~ "null",
    TRUE ~ "Annen"))#this line code all other diagnoses as "annen"

  myInData$DiagVSF_MISSING_NAMES <- factor(myInData$DiagVSF_MISSING_NAMES, #make it an ordered factor
                                          levels = c("F50.0",
                                                     "F50.1",
                                                     "F50.2",
                                                     "F50.3",
                                                     "F50.4",
                                                     "F50.5",
                                                     "F50.8",
                                                     "F50.9",
                                                     "F98.2",
                                                     "F98.3",
                                                     "Annen",
                                                     "null"),
                                          ordered = TRUE)


  #DiagBUAkse1_MISSING_NAMES
  #Code values containing the different diagnoses
  myInData <- myInData %>% dplyr::mutate(DiagBUAkse1_MISSING_NAMES = dplyr::case_when(
    grepl("F500", DiagBUAkse1, fixed = T) == T ~ "F50.0",
    grepl("F501", DiagBUAkse1, fixed = T) == T ~ "F50.1",
    grepl("F502", DiagBUAkse1, fixed = T) == T ~ "F50.2",
    grepl("F503", DiagBUAkse1, fixed = T) == T ~ "F50.3",
    grepl("F504", DiagBUAkse1, fixed = T) == T ~ "F50.4",
    grepl("F505", DiagBUAkse1, fixed = T) == T ~ "F50.5",
    grepl("F508", DiagBUAkse1, fixed = T) == T ~ "F50.8",
    grepl("F509", DiagBUAkse1, fixed = T) == T ~ "F50.9",
    grepl("F982", DiagBUAkse1, fixed = T) == T ~ "F98.2",
    grepl("F983", DiagBUAkse1, fixed = T) == T ~ "F98.3",
    (DiagBUAkse1 == "" | DiagBUAkse1 == 1999) & RegRegtype %in% c(2,6,99) ~ NA_character_,
    DiagBUAkse1 == "" ~ "null",
    TRUE ~"Annen"))

  #Gj?r om tll factor:
  myInData$DiagBUAkse1_MISSING_NAMES <- factor(myInData$DiagBUAkse1_MISSING_NAMES, #make it an ordered factor
                                              levels = c("F50.0",
                                                         "F50.1",
                                                         "F50.2",
                                                         "F50.3",
                                                         "F50.4",
                                                         "F50.5",
                                                         "F50.8",
                                                         "F50.9",
                                                         "F98.2",
                                                         "F98.3",
                                                         "Annen",
                                                         "null"),
                                              ordered = TRUE)


  # DiagSF_V_BU_MISSING_NAMES (with missing coded as NA and categories as NAMES) (DiagVSF and DiagBUAkse1 combined)
  myInData <- myInData %>% dplyr::mutate(DiagSF_V_BU_MISSING_NAMES = dplyr::case_when(
    DiagVSF_MISSING_NAMES == "F50.0"| DiagBUAkse1_MISSING_NAMES == "F50.0" ~ "F50.0" ,
    DiagVSF_MISSING_NAMES == "F50.1"| DiagBUAkse1_MISSING_NAMES == "F50.1" ~ "F50.1",
    DiagVSF_MISSING_NAMES == "F50.2"| DiagBUAkse1_MISSING_NAMES == "F50.2" ~ "F50.2",
    DiagVSF_MISSING_NAMES == "F50.3"| DiagBUAkse1_MISSING_NAMES == "F50.3" ~ "F50.3",
    DiagVSF_MISSING_NAMES == "F50.4"| DiagBUAkse1_MISSING_NAMES == "F50.4" ~ "F50.4",
    DiagVSF_MISSING_NAMES == "F50.5"| DiagBUAkse1_MISSING_NAMES == "F50.5" ~ "F50.5",
    DiagVSF_MISSING_NAMES == "F50.8"| DiagBUAkse1_MISSING_NAMES == "F50.8" ~ "F50.8",
    DiagVSF_MISSING_NAMES == "F50.9"| DiagBUAkse1_MISSING_NAMES == "F50.9" ~ "F50.9",
    DiagVSF_MISSING_NAMES == "F98.2"| DiagBUAkse1_MISSING_NAMES == "F98.2" ~ "F98.2",
    DiagVSF_MISSING_NAMES == "F98.3"| DiagBUAkse1_MISSING_NAMES == "F98.3" ~ "F98.3",

    RegRegtype %in% c(1,2,5,6,98,99) & DiagVSF == "" & (DiagBUAkse1 == "" | DiagBUAkse1 == 1999) ~ NA_character_,
    DiagVSF == "" & (DiagBUAkse1 == "" | DiagBUAkse1 == 1999)  ~ "null",
    TRUE ~ "Annen"))#this line code all other diagnoses as "annen"

  myInData$DiagSF_V_BU_MISSING_NAMES <- factor(myInData$DiagSF_V_BU_MISSING_NAMES, #make it an ordered factor
                                              levels = c("F50.0",
                                                         "F50.1",
                                                         "F50.2",
                                                         "F50.3",
                                                         "F50.4",
                                                         "F50.5",
                                                         "F50.8",
                                                         "F50.9",
                                                         "F98.2",
                                                         "F98.3",
                                                         "Annen",
                                                         "null"),
                                              ordered = TRUE)



  #DiagDSM5Hoved_V_BU_MISSING_NAMES (DiagVDSM5Hoved and DiagBUDSM5Hoved combined )
  myInData <- myInData %>% dplyr::mutate(DiagDSM5Hoved_V_BU_MISSING_NAMES = dplyr::case_when(
    DiagVDSM5Hoved == "0" | DiagBUDSM5Hoved == "0" ~ "307.52 Pica (ICD-10: F50.8)", #"307.52 \n Pica \n (ICD-10: F50.8)", #"307.52 \n Pica \n (ICD-10: F50.8)",
    DiagVDSM5Hoved == "1" | DiagBUDSM5Hoved == "1" ~ "307.53 Dr?vtygging (ICD-10: F98.21)", #"307.53 \n Dr?vtygging \n (ICD-10: F98.21)", #"307.53 \n Dr?vtygging \n (ICD-10: F98.21)",
    DiagVDSM5Hoved == "2" | DiagBUDSM5Hoved == "2" ~ "307.59 Unnv./restr. (ICD-10: F50.8)",#"307.59 \n Unnv./restr. \n (ICD-10: F50.8)",#"307.59 \n Unnv./restr. \n (ICD-10: F50.8)",
    DiagVDSM5Hoved == "3" | DiagBUDSM5Hoved == "3" ~ "307.1 Anorexia N.  (ICD-10: F50.0)", #"307.1 \n Anorexia N. \n (ICD-10: F50.0)", #"307.1 \n Anorexia N. \n (ICD-10: F50.0)",
    DiagVDSM5Hoved == "4" | DiagBUDSM5Hoved == "4" ~ "307.51 Bulimia N. (ICD-10: F50.2)", #"307.51 \n Bulimia N. \n (ICD-10: F50.2)", #"307.51 \n Bulimia N. \n (ICD-10: F50.2)",
    DiagVDSM5Hoved == "5" | DiagBUDSM5Hoved == "5" ~ "307.51 Overspisingsl. (ICD-10: F50.8)",#"307.51 \n Overspisingsl. \n (ICD-10: F50.8)",#"307.51 \n Overspisingsl. \n (ICD-10: F50.8)",#klokebok sier "overspisningsforstyrrelse", nasjonale retn.linjer *lidelse, derfor forkortet til "*l."
    DiagVDSM5Hoved == "6" | DiagBUDSM5Hoved == "6" ~ "307.59 Annen spesifisert (ICD-10: F50.8)", #"307.59 \n Annen spesifisert \n (ICD-10: F50.8)", #"307.59 \n Annen spesifisert \n (ICD-10: F50.8)",
    DiagVDSM5Hoved == "7" | DiagBUDSM5Hoved == "7" ~ "307.50 Uspesifisert (ICD-10: F50.9)",#"307.50 \n Uspesifisert \n (ICD-10: F50.9)",#"307.50 \n Uspesifisert \n (ICD-10: F50.9)",
    RegRegtype %in% c(1,2,5,6,98,99) & DiagVDSM5Hoved == "null" & DiagBUDSM5Hoved == "null" ~ NA_character_, #NA valueas are give those observations that should have had values
    DiagVDSM5Hoved == "null" & DiagBUDSM5Hoved == "null"  ~ "null",
    TRUE ~ "Annen"))#this line code all other diagnoses as "annen"

  myInData$DiagDSM5Hoved_V_BU_MISSING_NAMES <- factor(myInData$DiagDSM5Hoved_V_BU_MISSING_NAMES, #make it an ordered factor
                                                     levels = c("307.52 Pica (ICD-10: F50.8)", #"307.52 \n Pica \n (ICD-10: F50.8)",
                                                                "307.53 Dr?vtygging (ICD-10: F98.21)", #"307.53 \n Dr?vtygging \n (ICD-10: F98.21)",
                                                                "307.59 Unnv./restr. (ICD-10: F50.8)",#"307.59 \n Unnv./restr. \n (ICD-10: F50.8)",
                                                                "307.1 Anorexia N.  (ICD-10: F50.0)", #"307.1 \n Anorexia N. \n (ICD-10: F50.0)",
                                                                "307.51 Bulimia N. (ICD-10: F50.2)", #"307.51 \n Bulimia N. \n (ICD-10: F50.2)",
                                                                "307.51 Overspisingsl. (ICD-10: F50.8)",#"307.51 \n Overspisingsl. \n (ICD-10: F50.8)",
                                                                "307.59 Annen spesifisert (ICD-10: F50.8)", #"307.59 \n Annen spesifisert \n (ICD-10: F50.8)",
                                                                "307.50 Uspesifisert (ICD-10: F50.9)",#"307.50 \n Uspesifisert \n (ICD-10: F50.9)",
                                                                "Annen", #"Annen",
                                                                "null"),#"null"),
                                                     ordered = TRUE)



  #DiagVSomatiske_CAT_MISSING
  myInData <- myInData %>% dplyr::mutate(DiagVSomatiske_CAT_MISSING = dplyr::case_when(
    DiagVSomatiske == "0"  ~ "Ingen relevant somatiske",
    DiagVSomatiske == "1" & DiagVMalabsorpsjon == "1" ~ "Malabsorpsj.tilstander",
    DiagVSomatiske == "1" & DiagVDiabetes == "1" ~ "Diabetes",
    DiagVSomatiske == "1" & DiagVDiabetes == "0" & DiagVMalabsorpsjon == "0" ~ "Ja, men ikke spesifisert",
    DiagVSomatiske == "null" ~ "null",
    RegRegtype %in% c(1,5,98) & DiagVSomatiske == "null" ~ NA_character_)
  )
  #Gj?r om tll factor:
  myInData$DiagVSomatiske_CAT_MISSING <- factor(myInData$DiagVSomatiske_CAT_MISSING, #make it an ordered factor
                                               levels = c("Diabetes",
                                                          "Malabsorpsj.tilstander",
                                                          "Ja, men ikke spesifisert",
                                                          "Ingen relevant somatiske",
                                                          "null"="null"),
                                               ordered = TRUE)

  #MedPsykofarmaka_CAT_MISSING Psykofarmakologisk behandling
  myInData <- myInData %>% dplyr::mutate(MedPsykofarmaka_CAT_MISSING = dplyr::case_when(
    RegRegtype %in% c(1,2,3,4,5,6) & MedPsykofarmaka == "null" ~ NA_character_,
    MedPsykofarmaka == "0" ~ "Nei",
    MedPsykofarmaka == "1" ~ "Ja",
    MedPsykofarmaka == "null" ~ "null"))

  #MedAntidepressiva_CAT_MISSING
  myInData <- myInData %>% dplyr::mutate(MedAntidepressiva_CAT_MISSING = dplyr::case_when(
    RegRegtype %in% c(1,2,3,4,5,6) & MedPsykofarmaka == "null" ~ NA_character_,
    MedPsykofarmaka == "0" & MedAntidepressiva  == "0" ~ "Nei",
    MedPsykofarmaka == "1" & MedAntidepressiva  == "0" ~ "Nei",
    MedPsykofarmaka == "1" & MedAntidepressiva  == "1" ~ "Ja",
    MedAntidepressiva  == "null" ~ "null"))

  #MedNevroleptika_CAT_MISSING
  myInData <- myInData %>% dplyr::mutate(MedNevroleptika_CAT_MISSING = dplyr::case_when(
    RegRegtype %in% c(1,2,3,4,5,6) & MedPsykofarmaka == "null" ~ NA_character_,
    MedPsykofarmaka == "0" & MedNevroleptika  == "0" ~ "Nei",
    MedPsykofarmaka == "1" & MedNevroleptika  == "0" ~ "Nei",
    MedPsykofarmaka == "1" & MedNevroleptika  == "1" ~ "Ja",
    MedNevroleptika  == "null" ~ "null"))

  #MedBenzodiazepiner_CAT_MISSING
  myInData <- myInData %>% dplyr::mutate(MedBenzodiazepiner_CAT_MISSING = dplyr::case_when(
    RegRegtype %in% c(1,2,3,4,5,6) & MedPsykofarmaka == "null" ~ NA_character_,
    MedPsykofarmaka == "0" & MedBenzodiazepiner  == "0" ~ "Nei",
    MedPsykofarmaka == "1" & MedBenzodiazepiner  == "0" ~ "Nei",
    MedPsykofarmaka == "1" & MedBenzodiazepiner  == "1" ~ "Ja",
    MedBenzodiazepiner  == "null" ~ "null"))

  #MedAnnenMedBeh_CAT_MISSING
  myInData <- myInData %>% dplyr::mutate(MedAnnenMedBeh_CAT_MISSING = dplyr::case_when(
    RegRegtype %in% c(1,2,3,4,5,6) & MedPsykofarmaka == "null" ~ NA_character_,
    MedPsykofarmaka == "0" & MedAnnenMedBeh  == "0" ~ "Nei",
    MedPsykofarmaka == "1" & MedAnnenMedBeh  == "0" ~ "Nei",
    MedPsykofarmaka == "1" & MedAnnenMedBeh  == "1" ~ "Ja",
    MedAnnenMedBeh  == "null" ~ "null"))

  #MedBlodprove_CAT_MISSING
  myInData <- myInData %>% dplyr::mutate(MedBlodprove_CAT_MISSING = dplyr::case_when(
    RegRegtype %in% c(1,2,3,4,5,6) & MedBlodprove == "null" ~ NA_character_,
    MedBlodprove == "0" ~ "Nei",
    MedBlodprove == "1" ~ "Ja",
    MedBlodprove == "9" ~ "Ikke aktuelt",
    MedBlodprove  == "null" ~ "null"))
  #ordered factor
  myInData$MedBlodprove_CAT_MISSING <- factor(myInData$MedBlodprove_CAT_MISSING, #make it an ordered factor
                                             levels = c("Nei",
                                                        "Ja",
                                                        "Ikke aktuelt",
                                                        "null"),
                                             ordered = TRUE)

  #MedBeintetthMaling_CAT_MISSING
  myInData <- myInData %>% dplyr::mutate(MedBeintetthMaling_CAT_MISSING = dplyr::case_when(
    RegRegtype %in% c(1,2,3,4,5,6) & MedBeintetthMaling == "null" ~ NA_character_,
    MedBeintetthMaling == "0" ~ "Nei",
    MedBeintetthMaling == "1" ~ "Ja",
    MedBeintetthMaling == "9" ~ "Vet ikke",
    MedBeintetthMaling == "null" ~ "null"))
  #ordered factor
  myInData$MedBeintetthMaling_CAT_MISSING <- factor(myInData$MedBeintetthMaling_CAT_MISSING, #make it an ordered factor
                                                   levels = c("Nei",
                                                              "Ja",
                                                              "Vet ikke",
                                                              "null"),
                                                   ordered = TRUE)

  #B17FysMishandl
  myInData <- myInData %>% dplyr::mutate(B17FysMishandl_CAT_MISSING = dplyr::case_when(
    #RegRegtype %in% c(1,2,3,4) & B17FysMishandl == "null" ~ NA_character_, #should not be necessary as 9 is missing value for this question
    B17FysMishandl == "0" ~ "Nei",
    B17FysMishandl == "1" ~ "Ja",
    B17FysMishandl == "9" ~ NA_character_, #9 is NA because it is the value "registrar" choose when patient has not answered
    B17FysMishandl == "null" & RegRegtype %in% c(1,2,3,4) ~ NA_character_,
    B17FysMishandl == "null" ~"null"))
  #B18PsykMishandl
  myInData <- myInData %>% dplyr::mutate(B18PsykMishandl_CAT_MISSING = dplyr::case_when(
    #RegRegtype %in% c(1,2,3,4) & B17FysMishandl == "null" ~ NA_character_, #should not be necessary as 9 is missing value for this question
    B18PsykMishandl == "0" ~ "Nei",
    B18PsykMishandl == "1" ~ "Ja",
    B18PsykMishandl == "9" ~ NA_character_, #9 is NA because it is the value "registrar" choose when patient has not answered
    B18PsykMishandl == "null" & RegRegtype %in% c(1,2,3,4) ~ NA_character_,
    B18PsykMishandl == "null" ~"null"))

  #B19Overgrep
  myInData <- myInData %>% dplyr::mutate(B19Overgrep_CAT_MISSING = dplyr::case_when(
    #RegRegtype %in% c(1,2,3,4) & B17FysMishandl == "null" ~ NA_character_, #should not be necessary as 9 is missing value for this question
    B19Overgrep == "0" ~ "Nei",
    B19Overgrep == "1" ~ "Ja",
    B19Overgrep == "9" ~ NA_character_, #9 is NA because it is the value "registrar" choose when patient has not answered
    B19Overgrep == "null" & RegRegtype %in% c(1,2,3,4) ~ NA_character_,
    B19Overgrep == "null" ~"null"))


  #B20Mobbing
  myInData <- myInData %>% dplyr::mutate(B20Mobbing_CAT_MISSING = dplyr::case_when(
    #RegRegtype %in% c(1,2,3,4) & B17FysMishandl == "null" ~ NA_character_, #should not be necessary as 9 is missing value for this question
    B20Mobbing == "0" ~ "Nei",
    B20Mobbing == "1" ~ "Ja",
    B20Mobbing == "9" ~ NA_character_, #9 is NA because it is the value "registrar" choose when patient has not answered
    B20Mobbing == "null" & RegRegtype %in% c(1,2,3,4) ~ NA_character_,
    B20Mobbing == "null" ~ "null"))


  #B21SelvskadTidl,B22SelvskadSisteAr, B23SelvmordFTidl
  ##doing as above, but more effectively, using a for loop
  ##FIX/IMPROVE: currently the suffix _CAT_MISSING is not added, so original variable is overwritten (we want to keep original var.)

  myVarQuo <- c(rlang::quo(B21SelvskadTidl),
                rlang::quo(B22SelvskadSisteAr),
                rlang::quo(B23SelvmordFTidl),
                rlang::quo(B24SelvmordFSisteAr),
                rlang::quo(B25Avhengighet))

  for(i in 1:length(myVarQuo)){

    myInData <- myInData %>% dplyr::mutate(!!myVarQuo[[i]] := dplyr::case_when(#paste0(!!myVarQuo[[i]],"_CAT_MISSING") := case_when
      #RegRegtype %in% c(1,2,3,4) & B17FysMishandl == "null" ~ NA_character_, #should not be necessary as 9 is missing value for this question
      !!myVarQuo[[i]] == "0" ~ "Nei",
      !!myVarQuo[[i]] == "1" ~ "Ja",
      !!myVarQuo[[i]] == "9" ~ NA_character_, #9 is NA because it is the value "registrar" choose when patient has not answered
      !!myVarQuo[[i]] == "null" & RegRegtype %in% c(1,2,3,4) ~ NA_character_,
      !!myVarQuo[[i]] == "null" ~ "null"))

  }

  #PT01OnsketInvolv,
  #pT02BleInvolv,
  #PT04KontaktBrukerorg,
  #PT05OrientertBrukerorg,
  ##FIX/IMPROVE: currently the suffix _CAT_MISSING is not added, so original variable is overwritten (we want to keep original var.)
  ##...so that it becomes:
  #PT01OnsketInvolv_01_MISSING,
  #PT02BleInvolv_01_MISSING,
  #PT04KontaktBrukerorg_01_MISSING,
  #PT05OrientertBrukerorg_01_MISSING,
  myVarQuo2 <- c(rlang::quo(PT01OnsketInvolv),
                 rlang::quo(PT02BleInvolv),
                 rlang::quo(PT04KontaktBrukerorg),
                 rlang::quo(PT05OrientertBrukerorg))

  for(i in 1:length(myVarQuo2)){

    myInData <- myInData %>% dplyr::mutate(!!myVarQuo2[[i]] := dplyr::case_when(
      RegRegtype %in% c(5,6) & PasientAlder >= 16 & !!myVarQuo2[[i]] == "null" ~ NA_character_,
      !!myVarQuo2[[i]] == "0" ~ "0",
      !!myVarQuo2[[i]] == "1" ~ "1",
      !!myVarQuo2[[i]] == "9" ~ NA_character_, #9 is NA because it is the value "registrar" choose when patient has not answered
      !!myVarQuo2[[i]] == "null" ~ "null"))
  }


  ##Pasienterfaringer (PasOpp)
  #PO05Involvert_CAT_MISSING
  myInData <- myInData %>% dplyr::mutate(PO05Involvert_CAT_MISSING = dplyr::case_when(
    #RegRegtype %in% c(1,2,3,4) & B17FysMishandl == "null" ~ NA_character_, #should not be necessary as 9 is missing value for this question
    PO05Involvert == "0" ~ "Ikke i det hele tatt",
    PO05Involvert == "1" ~ "I liten grad",
    PO05Involvert == "2" ~ "I noen grad",
    PO05Involvert == "3" ~ "I stor grad",
    PO05Involvert == "4" ~ "I sv?rt stor grad",
    PO05Involvert == "9" ~ "Ikke aktuelt",
    PO05Involvert == "null" & RegRegtype %in% c(5,6) ~ NA_character_,
    PO05Involvert == "null" ~ "null"))
  #ordered factor
  myInData$PO05Involvert_CAT_MISSING <- factor(myInData$PO05Involvert_CAT_MISSING, #make it an ordered factor
                                              levels = c("Ikke i det hele tatt",
                                                         "I liten grad",
                                                         "I noen grad",
                                                         "I stor grad",
                                                         "I sv?rt stor grad"),
                                              #"Ikke aktuelt"),
                                              ordered = TRUE)

  #MedBMI_withIsoBMIBGSvalues (MedBMI but with MedISOBMIBGS values for those that have ISOBMIBGS values)
  myInData <- myInData %>% dplyr::mutate(MedBMI_withIsoBMIBGSvalues = dplyr::case_when(
    MedIsoBMIBGS !="null" ~ MedIsoBMIBGS, #those with ISOBMIBGS values get ISoBMIBGS values
    TRUE ~ as.character(MedBMI))) #rest gets ordinary BMI values


  output <- myInData
  return(invisible(output))
}
