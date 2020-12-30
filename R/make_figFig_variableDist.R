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
  tema = theme_light(base_size=skriftstorleik)
  tema$panel.grid.minor$colour="white"
  tema$strip.background$fill="#f3f1ee"
  tema$strip.background$colour="#e4e0da"
  tema$strip.text.x = element_text(colour="black")
  tema$panel.spacing=unit("13" ,"pt")
  tema$panel.border$colour=tema$strip.background$colour
  tema$panel.grid.major$colour=tema$strip.background$colour
  tema$panel.grid.minor$colour=tema$strip.background$fill
  tema$axis.title.y$angle=0
  tema$axis.title.y$margin=margin(r=5)
  tema$axis.title.x$margin=margin(t=5)
  tema$plot.title = element_text(hjust=0.5)

  # Fjern vannrette rutenett
  fjern_x = theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank())
  fjern_y = theme(panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank())



  fig <-  ggplot(my_proptable) +
    #The bars:
    geom_col(mapping=aes(x = cat, y=perc*100),#,width=1/3
             fill="#2171b5") +
    geom_text(
      aes(x = cat, y= perc*100,
          label=scales::percent(perc), #to map percentace label at top of bar use "y= perc"
          vjust=-0.5),
      alpha=0.5) +
    labs(title = mytitle,  #labels: title, subtitle and caption
         subtitle = paste0("N = ",sum(my_proptable$n), ""))+
    #caption = "NorSpis (2020)")+

    scale_y_continuous(limits = c(0,max(my_proptable$perc*100)), expand = expansion(mult=c(0,0.1)),breaks = seq(0,100,10))+
    labs(y="Andel pasienter(%)")+
    xlab(NULL)+
    tema +
    fjern_x +
    theme(axis.title.y = element_text(angle=90))+
    theme(axis.ticks.y = element_blank())+#+
    theme(plot.title = element_text(face= "bold", vjust=1))+
    #plot.subtitle = element_text(face ="bold")+
    #plot.caption = element_text(color="#2171b5", face = "italic")) # caption color
    theme(plot.subtitle = element_text(hjust=0.5, vjust=-2))
  #coord_flip()

  return(fig)
}

