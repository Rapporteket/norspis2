#' Import data with information about treatmeant "tiltak" (this also is available in _num format, but we import the non-"num" .csv)
#'
#' B refers to "behandling"
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: import12_dataBeh )
#'
#' Use this function like this:
#' RegData <- fun2_import_data_B()
#'
#'
#' @param path_data path of you data (B - named NorSpis_BehandlingNum_datadump_DATE)
#' @param date_data date on the format "2020-12-31" (the names of your data files will end in with date in current download set up from Rapporteket)
#'
#'
#' @return a tibble with the data (invisible)
#' @export
#'
#' @examples

fun1_2_import_data_B <- function(path_data="F:/2020-28-12_data-til-utvikling/", #disk and folder of data to import
                              date_data="2020-12-28"){
  NorSpisBehandling <- utils::read.table(paste0(path_data, "NorSpis_BehandlingNum_datadump_", date_data,".csv"),
                                         sep=';',dec=',', header=T, encoding = 'UTF-8', stringsAsFactors = FALSE, fill = TRUE)
  colnames(NorSpisBehandling)[1] <- 'BehandlingID' #because first column is importet with strange prefix we must rename it
  #NorSpisBehandling[is.na(NorSpisBehandling)] <- 'null'

  #Change data name
  RegDataBeh <- NorSpisBehandling

  RegDataBeh <- tibble::as_tibble(RegDataBeh)

  output <- RegDataBeh
  return(invisible(output))

}
