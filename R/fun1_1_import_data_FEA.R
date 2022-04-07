#' Import data locally (and convert it to a tibble for convinience - easier to work with in the console than a data.frame)
#'
#' FEA refers to "forlopsoversikt", "enkeltledd" and "alle scorer"
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: import11_data)
#'
#' Use this function like this:
#' RegData <- fun1_1_import_data_FEA()
#'
#' @return a tibble with the data (invisible)
#' @export
#'
#' @examples

fun1_1_import_data_FEA <- function(){

  NorSpisForlop <- norspis::queryForlopsOversikt("norspis") %>%
    dplyr::select(-c("Fodselsdato", "KryptertFnr", "AvdodDato")) %>%
    mutate(PasientAlder = round(PasientAlder, digits = 1))

  NorSpisEnkeltledd <- norspis::queryEnkeltLeddNum("norspis")
  NorSpisAlleScorer <- norspis::queryAlleScorer("norspis")
  #Merge data
  output <- merge(NorSpisForlop, NorSpisAlleScorer, suffixes = c('','y'),
                  by = "ForlopsID", all = FALSE) %>%
    merge(NorSpisEnkeltledd, suffixes = c('','X'), by = "ForlopsID", all = FALSE) %>%
    tibble::as_tibble() %>% dplyr::mutate(ForlopsID = as.numeric(ForlopsID))

  return(invisible(output))
}
