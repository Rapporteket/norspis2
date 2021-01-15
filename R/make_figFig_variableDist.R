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

make_figFig_variableDist <- function(my_proptable, mytitle){

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



  fig <-  ggplot2::ggplot(my_proptable) +
    #The bars:
    ggplot2::geom_col(mapping=ggplot2::aes(x = cat, y=perc*100),#,width=1/3
             fill="#2171b5") +
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
    #plot.caption = ggplot2::element_text(color="#2171b5", face = "italic")) # caption color
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust=0.5, vjust=-2))
  #coord_flip()

  return(fig)
}

