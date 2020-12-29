#' Import data locally (and convert it to a tibble for convinience - easier to work with in the console than a data.frame)
#'
#' FEA refers to "forlopsoversikt", "enkeltledd" and "alle scorer"
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: import11_data)
#'
#' Use this function like this:
#' RegData <- fun1_1_import_data_FEA()
#'
#'
#' @param path_data path of you data (F,E, and A - named NorSpis_ForlopsOversikt_datadump_DATE, NorSpis_EnkeltLeddNum_datadump_DATE and NorSpis_AlleScorer_datadump_DATE
#' @param date_data date on the format "2020-12-31" (the names of your data files will end in with date current download set up from Rapporteket)
#'
#' @return a tibble with the data (invisible)
#' @export
#'
#' @examples

fun1_1_import_data_FEA <- function(path_data="F:/2020-28-12_data-til-utvikling/", #disk and folder of data to import
                                 date_data="2020-12-28"){ #date of data
  #Load data
  NorSpisForlop <- utils::read.table(paste0(path_data, "NorSpis_ForlopsOversikt_datadump_", date_data,".csv"),
                                     sep=';', dec=',', header=T, encoding = 'UTF-8', stringsAsFactors = FALSE, fill = TRUE)
  colnames(NorSpisForlop)[1] <- 'AvdRESH'
  NorSpisForlop[is.na(NorSpisForlop)] <- 'null'

  NorSpisEnkeltledd <- utils::read.table(paste0(path_data, "NorSpis_EnkeltLeddNum_datadump_", date_data,".csv"),
                                         sep=';',dec=',', header=T, encoding = 'UTF-8', stringsAsFactors = FALSE, fill = TRUE)
  colnames(NorSpisEnkeltledd)[1] <- 'PasientID'
  NorSpisEnkeltledd[is.na(NorSpisEnkeltledd)] <- 'null'

  NorSpisAlleScorer <- utils::read.table(paste0(path_data, "NorSpis_AlleScorer_datadump_", date_data,".csv"),
                                         sep=';', dec=',', header=T, encoding = 'UTF-8',stringsAsFactors = FALSE, fill = TRUE)
  colnames(NorSpisAlleScorer)[1] <- 'ForlopsID'
  NorSpisAlleScorer[is.na(NorSpisAlleScorer)] <- 'null'

  #Merge data
  ForlAlleSc <- merge(NorSpisForlop, NorSpisAlleScorer, suffixes = c('','y'), by = "ForlopsID", all = FALSE)
  NorSpisData <- merge(ForlAlleSc, NorSpisEnkeltledd, suffixes = c('','X'), by = "ForlopsID", all = FALSE)
  #NorSpisData <- merge(NorSpisForlop, suffixes = c('','X'), by = c("ForlopsID" ), NorSpisEnkeltledd,
  #                     all = FALSE)  #by.x = "ForlopsID", by.y = "ForlopsID",#"SykehusNavn", "AvdRESH"
  #Change data name
  RegData <- NorSpisData

  RegData <- tibble::as_tibble(RegData)

  output <- RegData
  return(invisible(output))
}
