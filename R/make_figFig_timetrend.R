#' Plot time trends of quality indicators
#'
#' This function outputs figure and table (flextable) of content in figure.
#' You need the input variable must be a 0-1 variable. If variable is not 0-1,
#' but ranges from 0 to 100, use the argument variable_type_0_100 = TRUE
#'
#'
#' @param add_fill Add fill under line
#' @param my_data
#' @param do_facet_wrap
#' @param data_type Choices: "start-end" and "regular"
#' @param variable_type_0_100
#' @param variable01
#'
#' @return
#' @export
#'
#' @examples

make_figFig_timetrend <- function(my_data,
                                  data_type = "start_end",
                                  variable01,
                                  variable_type_0_100 = FALSE,
                                  #= c(quo(EDEQ60GlobalScore_CHANGE_PROP))
                                  add_fill = F,
                                  do_facet_wrap = TRUE,
                                  show_legend = TRUE,
                                  show_national_mean = FALSE
                                  ){


  variable01 <- sym({{noquote(variable01)}})
  variable01 <- quo({{variable01}})

  #filter data for e.g. 2012-2019
  if(data_type == "start-end"){
  RegDataStartEndNatValFiltered <- norspis2::fun3_2_filter_RegDataStartEnd(
    RegDataStartEnd = my_data,
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
  }

  #quick fix: if data is regular data frame we just use the
  #filter function for such data and call it the same as above
  if(data_type == "regular"){
  RegDataStartEndNatValFiltered <- norspis2::fun3_1_filter_RegData(
    RegData = my_data,
    regStatus = c(1),#c(0,1,-1),
    regType = c(5,6,98,99,NA),
    dateFrom = "2012-01-01",
    dateTo = "2020-12-31",
    ageFrom = 0,
    ageTo = 200)%>%
    #quick fix - just to make this work for now:
    rename("AvdKategori.x" = "AvdKategori",
           "Year.y" = "Year")
  }

  #quick fix, when variable in is 0-100 instead of 0-1 (we divide by 100)
  if(variable_type_0_100 == TRUE){
    RegDataStartEndNatValFiltered <- RegDataStartEndNatValFiltered %>%
      mutate(!!variable01 := (!!variable01/100))
  }

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

  #exclude national from this data
  RegDataStartEndNatValFiltered_summarized6 <-
    RegDataStartEndNatValFiltered_summarized4 %>%
    filter(AvdNavn != "Nasjonal")


  #Addiditonal data frame that will be used to plot national values over time
  #in each facet when using facet wrap:
       names_defines_facets <-
          unique(RegDataStartEndNatValFiltered_summarized4$AvdNavn)[
            unique(RegDataStartEndNatValFiltered_summarized4$AvdNavn) != "Nasjonal"]

      #empty tibble to add values to in loop
      RegDataStartEndNatValFiltered_summarized5 <- tibble()

    for(i in 1:length(names_defines_facets)){
    new <-  RegDataStartEndNatValFiltered_summarized4[
      RegDataStartEndNatValFiltered_summarized4$AvdKategori.x=="nasjonal",] %>%
      mutate(names_defines_facets = names_defines_facets[i])

    RegDataStartEndNatValFiltered_summarized5 <- bind_rows(RegDataStartEndNatValFiltered_summarized5,
                                                           new)
    }

  RegDataStartEndNatValFiltered_summarized5 <-
    RegDataStartEndNatValFiltered_summarized5  %>%
    dplyr::select(-c("AvdNavn"))%>%
    dplyr::rename(AvdNavn = names_defines_facets)

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
    ggplot2::ggplot(data = RegDataStartEndNatValFiltered_summarized6,
                    mapping = aes(x = Year.y,
                                  y = perc,
                                  color = AvdNavn,
                                  fill = AvdNavn)
                    #,
                    #show.legend = T
                    )+
    #Do this first, so that national line get behind other lines:
    {if(do_facet_wrap == TRUE)
    facet_wrap(~AvdNavn, ncol = 3)}+
    #national variation over time for each facet (own data frame made earlier)
    {if(do_facet_wrap == TRUE)
    geom_line(data=RegDataStartEndNatValFiltered_summarized5,
              aes(x = Year.y,
                  y = perc
              ),
              size = 1,
              show.legend = F,
              color = "#bbc2c6"#, #"grey")
              #linetype="dashed"
              )
    }+
    scale_x_continuous(breaks =
                         RegDataStartEndNatValFiltered_summarized6$Year.y,
                       #expand = expansion(mult = c(0, .25))
    ) +
    scale_y_continuous(limits = c(0,100),
                       breaks = seq(0,100,10),
                       minor_breaks = seq(0,100,2)
    )+
    scale_color_manual(values = c(Nasjonal="#003283", #blue",
                                  "#81A9E1",#ligth blue,
                                  "#dd052b",#red,
                                  "#b68069" ,#brown
                                  Andre = "#fbba00",#"gold",
                                  "#4da32f", #green
                                  "black"))+
    {if(add_fill ==T)
      geom_area(aes(fill = AvdNavn, group = AvdNavn),
              alpha = 0.25, position = 'identity',
              show.legend = show_legend)}+
    geom_line(aes(#linetype = AvdNavn
                  ),
              size = 1,
              show.legend = show_legend)+
    #geom_point(show.legend = T)+
    #geom_dl(aes(label = AvdNavn), method = list("last.bumpup", cex = 0.7))+
    theme_minimal()+
    #add grids to theme above:
    theme(panel.grid.minor.y = element_blank(),#element_line() ,
          panel.grid.major.y = element_line(size = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line())+
    ylab("Andel (prosent)")+
    xlab("År")+

    {if(show_national_mean == T)
    #this line is included in all facets:
    geom_hline(yintercept = RegDataStartEndNatValFiltered_summarized3$perc,
               color = "#bbc2c6",#"grey"
               linetype= "dashed",
               size=1)}+
    #https://stackoverflow.com/questions/41225294/avoid-overlapping-x-axis-labels-in-ggplot-facet-grid/53030221:
    theme(panel.spacing.x = unit(8, "mm"))




  #a "spread" table of the data we use in the plot
  perc <- RegDataStartEndNatValFiltered_summarized4 %>%
    select(-n)%>%
    tidyr::spread(key=Year.y, value = perc)%>%
    rename('Alle år' =  `<NA>` )%>%
    select(-'AvdKategori.x')%>%
    mutate_if(is.numeric, round, 1)

  #modified
  perc_mod <-
    perc %>%
    mutate_all(as.character)%>%
    mutate_all(coalesce,"-")#this replaces all NAs with "-"




  n <- RegDataStartEndNatValFiltered_summarized4 %>%
    select(-perc)%>%
    tidyr::spread(key=Year.y, value = n)%>%
    rename('Alle år' =  `<NA>` )%>%
    select(-'AvdKategori.x')

  #modified
  n_mod <-
    n %>%
    #rowwise()%>%
    #mutate(">=2017" = sum(`2017`, na.rm=TRUE))%>%#sum three first y.
    #relocate(">=2017",.after="AvdNavn")%>%
    #select(-c(`2017`))%>%#remove columns
    mutate_all(as.character)%>%
    mutate_all(coalesce,"-")%>% #this replaces all NAs with "-"
    #add brackets:
    mutate(across(1:(ncol(n)-1), ~ paste0("(",.x ,")")))


  #merge perc_mod and n_mod
  merged <- perc_mod %>%
    full_join(n_mod, by = intersect(colnames(n_mod), colnames(perc_mod))) %>%
    #relocate(colnames(end_mod))%>% #correct order of columns
    group_by(AvdNavn) %>%
    mutate_all(coalesce,"-")%>% #this replaces all NAs with "-"
    #summarize_all(na.omit)
    dplyr::group_by(AvdNavn) %>%
    #dplyr::mutate(paste(across(), collapse = " "))
    dplyr::summarise_each(funs(paste(., collapse = " ")))%>%
    #some - had not been changed to (-) becuase they were created during merging:
    mutate(across(.fns = ~replace(., . == "-", "(-) -")))%>%
    mutate(across(.fns = ~replace(., . == "- -", "(-) -")))%>%
    #To get total last(arrange by an and ordered factor):
    arrange(factor(AvdNavn, levels = c(unlist(.[.$AvdNavn !="Total",1], use.names=FALSE) , "Total")))#, desc(AvdNavn))
    #<-gotten to by combining the two following answers:
    #https://stackoverflow.com/questions/49225596/merge-multiple-rows-into-one-using-r
    #https://stackoverflow.com/questions/32936226/aggregating-rows-for-multiple-columns-in-r

  #Place national value at bottom...
    #Without (wo) national values
    merged_wo_national <- merged %>%
      subset(AvdNavn !="Nasjonal")
    #Only national values
    merged_national <- merged %>%
      subset(., AvdNavn =="Nasjonal")

  merged_sorted <- rbind(merged_wo_national,merged_national)%>%
    rename(" " = "AvdNavn")


  table1 <- flextable::flextable(perc)
  table2 <- flextable::flextable(n)
  table3 <- flextable::flextable(merged_sorted)

  #fix format on table 3
  table3 <- flextable::fontsize(table3, size = 8, part ="all")
  table3 <- flextable::width(table3, j=c(1),width= 2)
  table3 <- flextable::height_all(table3, height = 0.2, part = "all")
  table3 <- flextable::hrule(table3, rule = "exact")
  border_type <- officer::fp_border(color = "black", style = "solid", width = 1.25)
  table3 <- flextable::hline(table3, i = (nrow(merged_sorted)-1), border = border_type, part = "body")


  RegDataStartEndNatValFiltered_summarized5 <- RegDataStartEndNatValFiltered_summarized4 %>%
    filter(is.na(Year.y))

  #this is just a plot for support during development, showing a straight
  #line witht the level for each department
  plot2<-
    ggplot2::ggplot(RegDataStartEndNatValFiltered_summarized5) +
    geom_hline(#yintercept = RegDataStartEndNatValFiltered_summarized5$perc,
      mapping = aes(yintercept = perc, color = AvdNavn),
      show.legend = T)+
    scale_y_continuous(limits = c(0,100))+
    #adds names directly in graph, at end of each line
    geom_dl(aes(y= perc, x= "", label = AvdNavn),
            method = list("last.bumpup", cex = 0.7))+
    theme_bw()+
    ylab("Andel (prosent)")+
    xlab(" ")+
    {if(show_national_mean ==T)
    geom_hline(yintercept = RegDataStartEndNatValFiltered_summarized3$perc,
               color = "#bbc2c6", #"grey",
               linetype= "dashed",
               size=1)}

  return(list(table1,
              table2,
              table3,
              plot1,
              plot2))

}
