#' Function to make simple figure with distribution of a variable's values (nationally)
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make_propfig
#'
#' @param my_proptable table from make_figTable_variableCompar()
#' @param mytitle title of plot
#'
#' @return figure
#' @export
#'
#' @examples

make_figFig_variableDist <- function(my_proptable,
                                     mytitle,
                                     #with comparison
                                     comparison = FALSE,
                                     my_proptable_compare
                                     ){

  #Theme:
  if(!exists("skriftstorleik")) # Skriftstorleik b?r vera definert i kvar ?rsrapportfil
    skriftstorleik = 13
  tema = ggplot2::theme_light(base_size=skriftstorleik)
  tema$panel.grid.minor$colour="white"
  tema$strip.background$fill="#f3f1ee"
  tema$strip.background$colour="#e4e0da"
  tema$strip.text.x = ggplot2::element_text(colour="black")
  tema$panel.spacing=ggplot2::unit("13" ,"pt")
  tema$panel.border$colour=tema$strip.background$colour
  tema$panel.grid.major$colour=tema$strip.background$colour
  tema$panel.grid.minor$colour=tema$strip.background$fill
  tema$axis.title.y$angle=0
  tema$axis.title.y$margin=ggplot2::margin(r=5)
  tema$axis.title.x$margin=ggplot2::margin(t=5)
  tema$plot.title = ggplot2::element_text(hjust=0.5)

  # Fjern vannrette rutenett
  fjern_x = ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                  panel.grid.minor.x = ggplot2::element_blank())
  fjern_y = ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                  panel.grid.minor.y = ggplot2::element_blank())

  my_color = "#084594"


#figure without comparison group
if(comparison == FALSE){

  fig <-  ggplot2::ggplot(my_proptable) +
    #The bars:
    ggplot2::geom_col(mapping=ggplot2::aes(x = cat, y=perc*100),#,width=1/3
             fill=my_color) +
    ggplot2::geom_text(
      ggplot2::aes(x = cat, y= perc*100,
          label=scales::percent(perc), #to map percentace label at top of bar use "y= perc"
          vjust=-0.5),
      alpha=0.5) +
    ggplot2::geom_text(#to map percentace label at bar
      ggplot2::aes(x = cat, y= 0, # ta map on TOP of bar - use
                   label=paste0("(N=",n, ")")),
      alpha=0.5,
      position = ggplot2::position_dodge(0.9), #to place N text at each bar (not each bar pair)
      vjust=-0.5)+
    ggplot2::labs(title = mytitle,  #labels: title, subtitle and caption
         subtitle = paste0("N = ",sum(my_proptable$n), ""))+
    #caption = "NorSpis (2020)")+

    ggplot2::scale_y_continuous(
      limits = c(0,max(my_proptable$perc*100)),
      expand = ggplot2::expansion(mult=c(0,0.1)),breaks = seq(0,100,10))+
    ggplot2::labs(y="Andel pasienter(%)")+
    ggplot2::xlab(NULL)+
    tema +
    fjern_x +
    ggplot2::theme(axis.title.y = ggplot2::element_text(angle=90))+
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank())+#+
    ggplot2::theme(plot.title = ggplot2::element_text(face= "bold", vjust=1))+
    #plot.subtitle = ggplot2::element_text(face ="bold")+
    #plot.caption = ggplot2::element_text(color=my_color, face = "italic")) # caption color
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust=0.5, vjust=-2))
  #coord_flip()
}

if(comparison == TRUE){


  #(only keeping rows from 1 by left_join):
  my_proptable_compare_merged <- left_join(my_proptable,
                                           my_proptable_compare,
                                           by="cat",
                                           #add sufix only to merged data
                                           suffix = c("",".compare"))

  #TABLE FOR FIGTYPE 2:
  #make a longer table which where perc and perc.compare is gathered in one
  #variable, SO THAT WE CAN USE THIS TO MAKE A GROUPED BARPLOT with two bars for
  #each hospital: https://stackoverflow.com/questions/42820677/ggplot-bar-plot-side-by-side-using-two-variables
  my_proptable_compare_merged_longer <-
    tidyr::pivot_longer( my_proptable_compare_merged,#my_proptable_hospitals,
                        cols=c('perc.compare','perc'),
                        names_to=c('variable'),
                        values_to="value")%>%
    #some mutates to remove duplicate values/values not belonging in variable:
    dplyr::mutate(n = dplyr::case_when(variable == "perc.compare" ~ NA_integer_,
                                       TRUE ~ n))%>%
    dplyr::mutate(n.compare = dplyr::case_when(variable == "perc" ~ NA_integer_,
                                               TRUE ~ n.compare)) %>%
    dplyr::mutate(n =
                    dplyr::case_when(variable == "perc.compare" ~ n.compare,
                                     variable == "perc" ~ n,
                                     TRUE ~ NA_integer_))%>%
    dplyr::select(-n.compare)

    #%>%
    # #create only one pari of CI variables :
    # dplyr::mutate(CILower_percANDperc.compare =
    #                 dplyr::case_when(variable == "perc.compare" ~ CILower2,
    #                                  variable == "perc" ~ CILower,
    #                                  TRUE ~ NA_real_))%>%
    # dplyr::mutate(CIUpper_percANDperc.compare =
    #                 dplyr::case_when(variable == "perc.compare" ~ CIUpper2,
    #                                  variable == "perc" ~ CIUpper,
    #                                  TRUE ~ NA_real_))
    # #%>%
    #remove old/not used CI variables:
    #select(-c("CILower","CILower2","CIUpper","CIUpper2"))



  fig <-  ggplot2::ggplot(my_proptable_compare_merged_longer,
                          mapping=ggplot2::aes(x = cat,
                                               y = value*100,
                                               fill= variable)) +
    #The bars:
    ggplot2::geom_bar(position = "dodge",
                      stat = "identity"
                      #mapping=ggplot2::aes(x = cat, y=perc*100),#,width=1/3
                      #fill=my_color
                      ) +
    ggplot2::scale_fill_manual(values = c(perc="#e4e0da",
                                          perc.compare = "lightblue"),
                               labels = c("T.o.m. 2019", "2020"))+ #"#f3f1ee"
    ggplot2::geom_text(
      ggplot2::aes(y= value*100,
                   label=scales::percent(value), #to map percentace label at top of bar use "y= perc"
                   vjust=-0.5),
      alpha=0.5,
      position = ggplot2::position_dodge(0.9) #to place N text at each bar (not each bar pair)
    ) +
    ggplot2::geom_text(#to map percentace label at bar
      ggplot2::aes( y= 0, # ta map on TOP of bar - use
                   label=paste0("(N=",n, ")")),
      alpha=0.5,
      position = ggplot2::position_dodge(0.9), #to place N text at each bar (not each bar pair)
      vjust=-0.5) +
    ggplot2::labs(title = mytitle,  #labels: title, subtitle and caption
                  subtitle = paste0("(N = ",
                                    sum(
                                      as.numeric(
                                        case_when(
                                          my_proptable_compare_merged_longer$variable == "perc" ~ my_proptable_compare_merged_longer$n)), na.rm = T),
                                    ") ",
                                    "N = ",
                                    sum(
                                      as.numeric(
                                        case_when(
                                          my_proptable_compare_merged_longer$variable == "perc.compare" ~ my_proptable_compare_merged_longer$n)), na.rm = T),
                                    ""))+
    #caption = "NorSpis (2020)")+

    ggplot2::scale_y_continuous(
      limits = c(0,max(my_proptable_compare_merged_longer$value*100)),
      expand = ggplot2::expansion(mult=c(0,0.1)),breaks = seq(0,100,10))+
    ggplot2::labs(y="Andel pasienter(%)")+
    ggplot2::xlab(NULL)+
    tema +
    fjern_x +
    ggplot2::theme(axis.title.y = ggplot2::element_text(angle=90))+
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank())+#+
    ggplot2::theme(plot.title = ggplot2::element_text(face= "bold", vjust=1))+
    #plot.subtitle = ggplot2::element_text(face ="bold")+
    #plot.caption = ggplot2::element_text(color=my_color, face = "italic")) # caption color
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust=0.5, vjust=-2))
  #coord_flip()

}

return(fig)

}
