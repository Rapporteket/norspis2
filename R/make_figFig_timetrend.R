#' Title
#'
#' @param variable01
#' @param add_fill Add fill under line
#'
#' @return
#' @export
#'
#' @examples

make_figFig_timetrend <- function(variable01,
                                  #= c(quo(EDEQ60GlobalScore_CHANGE_PROP))
                                  add_fill = F
                                  ){


  variable01 <- sym({{noquote(variable01)}})
  variable01 <- quo({{variable01}})

  #filter data for e.g. 2012-2019
  RegDataStartEndNatValFiltered <- norspis2::fun3_2_filter_RegDataStartEnd(
    RegDataStartEnd = DL$RegDataStartEndNatVal2,
    BasisRegStatus.x = c(1),#c(0,1,-1),
    BasisRegStatus.y = c(1),#c(0,1,-1,NA)
    RegRegType.x = c(0,1,2,3,4,5,6,98,99),
    RegRegType.y = c(5,6,98,99,NA),
    dateFrom.x = "2012-01-01",
    dateTo.x = "2100-12-31",
    dateFrom.y = "2012-01-01",
    dateTo.y = "2020-12-31",
    ageFrom.x = 0,
    ageTo.x = 200,
    ageFrom.y = 0,
    ageTo.y = 200)

  # tableToFig <- make_figTable_unitCompar(myIndata_NatVal = RegDataStartEndNatValFiltered,
  #                                        myInvar01 = "EDEQ60GlobalScore_CHANGE_PROP")

  RegDataStartEndNatValFiltered <- RegDataStartEndNatValFiltered%>%
    dplyr::mutate(AvdKategori.x = dplyr::case_when(AvdNavn == "Nasjonal" ~ "nasjonal",
                                                   !(AvdKategori.x %in% c("regional","nasjonal")) ~ "annen",
                                                   TRUE ~ AvdKategori.x)) %>%
    dplyr::mutate(AvdNavn = dplyr::case_when(AvdKategori.x == "annen" ~ "Andre",
                                             TRUE ~ AvdNavn))

  library(directlabels)

  RegDataStartEndNatValFiltered_summarized <- RegDataStartEndNatValFiltered %>%
    #group_by together with summarize (see Wickham, R for Data Science)
    dplyr::group_by(Year.y,AvdNavn,AvdKategori.x)%>%
    dplyr::summarize(perc = mean(!!variable01, na.rm = T)*100,#mean() of 0-1 gives
                     #proportion
                     n = sum(!is.na(!!variable01)))%>%
    #removes hospitals w n<20 (choose 0.00000123 as easy identifiable value
    dplyr::mutate(perc = ifelse(n<5, NA, perc)) #%>%

  #as above, but for all years (per department)
  RegDataStartEndNatValFiltered_summarized2 <- RegDataStartEndNatValFiltered %>%
    #group_by together with summarize (see Wickham, R for Data Science)
    dplyr::group_by(AvdNavn,AvdKategori.x)%>%
    dplyr::summarize(perc = mean(!!variable01, na.rm = T)*100,#mean() of 0-1 gives
                     #proportion
                     n = sum(!is.na(!!variable01)))
  # %>%
  #   mutate(Year.y= 2021)



  #as above but only for all years in total:
  RegDataStartEndNatValFiltered_summarized3 <- RegDataStartEndNatValFiltered %>%
    #group_by together with summarize (see Wickham, R for Data Science)
    dplyr::summarize(perc = mean(!!variable01, na.rm = T)*100,#mean() of 0-1 gives
                     #proportion
                     n = sum(!is.na(!!variable01)))
  # %>%
  #     mutate(Year.y= 2021,#proxy, not real value, just because need a number here
  #            AvdNavn = "Alle avdelinger, alle år",
  #            AvdKategori.x = "Alle")


  RegDataStartEndNatValFiltered_summarized4 <-
    bind_rows(RegDataStartEndNatValFiltered_summarized,
              RegDataStartEndNatValFiltered_summarized2)

  # %>%
  #
  #     #When perc is between 0-1 we multiply by 100, else we keep the perc value
  #     #(which will then be a mean ordinary mean value):
  #     dplyr::mutate(perc = dplyr::case_when(perc >= 0 &
  #                                             perc <= 1 ~ perc*100,
  #                                           TRUE ~perc)) %>%
  #     #makes hospital names include "n":
  #     dplyr::mutate(AvdNavn = paste0(AvdNavn, ' (', n,')' )) %>%
  #     #removes hospitals w n<20 (choose 0.00000123 as easy identifiable value
  #     dplyr::mutate(perc = ifelse(n<5, 0.00000123, perc)) %>%
  #     dplyr::mutate(!!myInvar01 := 0)%>%
  #     #create new variable with name same as myinVar name, and set the values
  #     #equal to name of 4. columns whic is the new myInvar01 column:
  #     dplyr::mutate(!!myInvar01 := (colnames(.[,4])))%>%
  #     #recode nan - choose 0.00000123 (as above for units with N<5):
  #     dplyr::mutate(perc = dplyr::case_when(is.nan(perc) ~ 0.00000123,
  #                             TRUE ~ perc))
  #
  #   #SORT (from smallest to largest proportions)
  #   ## forcats needed to reorder the levels (as a means to SORT the variable):
  #   myInData_NatVal_summarized$AvdNavn <-
  #     myInData_NatVal_summarized$AvdNavn %>%
  #     forcats::fct_reorder(myInData_NatVal_summarized$n,
  #                          .desc=F) %>%
  #     forcats::fct_reorder(myInData_NatVal_summarized$perc,
  #                          .desc=F)#T for the other order
  #
  #   ## Then we first sort by N and then by percentage
  #   myInData_NatVal_summarized <- myInData_NatVal_summarized %>%
  #     dplyr::arrange(n) %>%
  #     dplyr::arrange(perc)

  plot1 <-
    ggplot2::ggplot(data = RegDataStartEndNatValFiltered_summarized4,
                    mapping = aes(x = Year.y, y = perc, color = AvdNavn, fill=AvdNavn)
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, .25))) +
    scale_y_continuous(limits = c(0,100))+
    {if(add_fill ==T)
      geom_area(aes(fill = AvdNavn, group = AvdNavn),
              alpha = 0.25, position = 'identity',
              show.legend = F)}+
    geom_line(show.legend = F)+
    geom_point(show.legend = F)+
    geom_dl(aes(label = AvdNavn), method = list("last.bumpup", cex = 0.7))+
    theme_bw()+
    ylab("Andel (prosent)")+
    xlab("År")+
    geom_hline(yintercept = RegDataStartEndNatValFiltered_summarized3$perc, color = "grey", linetype= "dashed", size=1)

  #a "spread" table of the data we use in the plot
  perc <- RegDataStartEndNatValFiltered_summarized4 %>%
    select(-n)%>%
    tidyr::spread(key=Year.y, value = perc)%>%
    rename('Alle år' =  `<NA>` )%>%
    select(-'AvdKategori.x')%>%
    mutate_if(is.numeric, round, 1)




  n <- RegDataStartEndNatValFiltered_summarized4 %>%
    select(-perc)%>%
    tidyr::spread(key=Year.y, value = n)%>%
    rename('Alle år' =  `<NA>` )%>%
    select(-'AvdKategori.x')




  table1 <- flextable::flextable(perc)
  table2 <- flextable::flextable(n)


  RegDataStartEndNatValFiltered_summarized5 <- RegDataStartEndNatValFiltered_summarized4 %>%
    filter(is.na(Year.y))

  plot2<-
    ggplot2::ggplot(RegDataStartEndNatValFiltered_summarized5) +
    geom_hline(#yintercept = RegDataStartEndNatValFiltered_summarized5$perc,
      mapping = aes(yintercept = perc, color = AvdNavn),
      show.legend = F)+
    scale_y_continuous(limits = c(0,100))+
    geom_dl(aes(y= perc, x= "", label = AvdNavn), method = list("last.bumpup", cex = 0.7))+
    theme_bw()+
    ylab("Andel (prosent)")+
    xlab(" ")+
    geom_hline(yintercept = RegDataStartEndNatValFiltered_summarized3$perc, color = "grey", linetype= "dashed", size=1)

  return(list(table1,
              table2,
              plot1,
              plot2))

}
