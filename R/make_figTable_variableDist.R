#' Function to make data table to go into simple plots of distributions of variables
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make_proptable
#'
#' @param myvar e.g. expr(PO10Pasientsikkerhet)
#' @param mycat e.g. expr(c(0,1,2,3,4))
#' @param mylab e.g. vars("0"="Ikke i det hele tatt", 1"="I liten grad", "2"="I noen grad", "3"="I stor grad","4"="I svært stor grad")
#'
#' @param myInData myInData <- RegDataStartEnd
#'
#' @return summary table with propoprtions of answers withing each variable category, to go into make_figFig_variableDist
#' @export
#'
#' @examples

make_figTable_variableDist <- function(myvar,mycat,mylab,myInData){
  # We use dplyr for this (group_by and summarize combined - see H.W. book)
  myInData_summarized <-
    myInData  %>%
    filter(!!myvar %in% !!mycat)%>% #the only values we want to include to calculate proportions
    group_by(!!myvar)%>%
    summarize(n = sum(!is.na(!!myvar))) %>%
    mutate(perc = prop.table(n))%>%
    #mutate(PO10Pasientsikkerhet.x = c("fasdfas","fasdf","afsdf","afdf","asfdfa"))
    mutate(!!myvar := dplyr::recode(!!myvar , !!!mylab))%>%
    dplyr::rename(cat=!!myvar)

  # prevent sorting by telling ggplot that you have an ordered factor already
  # RegDataStartEnd_summarized$PO10Pasientsikkerhet.x <- factor(RegDataStartEnd_summarized$PO10Pasientsikkerhet.x,
  #                                                                     levels = RegDataStartEnd_summarized$PO10Pasientsikkerhet.x)
  #dynamic:
  myInData_summarized[,1][[1]] <- factor(myInData_summarized[,1][[1]],
                                         levels = myInData_summarized[,1][[1]])

  return(myInData_summarized)#return(invisible(RegDataStartEnd_summarized))

}
