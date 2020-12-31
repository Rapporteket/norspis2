#' Figure with comparisons of hospital units
#'
#' Note: This function's name when writing aarsrapport 2019 (in 2020) was: make_compfig_hospitals
#'
#'
#' @param my_proptable_hospitals my_proptable_hospitals <- RegData_NatVal_summarized #with NATIONA vulues!
#' @param my_y_lab "Andel", unless you change it
#' @param my_title '' - empty, unless you change it
#' @param YellowGoal can be give the values 1)'' 2)'mean' or 3) A character value which will be converted to na , for instance '60' if the goal value is 60 percent
#' @param GreenGoal can be give the values 1)'' 2)'mean' or 3) A character value which will be converted to na , for instance '60' if the goal value is 60 percent
#'
#' @return figure
#' @export
#'
#' @examples

make_figFig_unitCompar <- function(my_proptable_hospitals,
                                   my_y_lab = "Andel (%)", #default "Andel (%), unless you change it
                                   my_title = '',#default empty (""), unless you change it
                                   YellowGoal = 'mean',  #can be give the values 1)'' 2)'mean' or 3) A character value which will be converted to na , for instance '60' if the goal value is 60 %
                                   GreenGoal = 'mean'){
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
  tema$axis.title.y$margin = ggplot2::margin(r=5)
  tema$axis.title.x$margin = ggplot2::margin(t=5)
  tema$plot.title = ggplot2::element_text(hjust=0.5)
  tema$plot.title = ggplot2::element_text(vjust=3)

  # Fjern vannrette rutenett
  fjern_x = ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                  panel.grid.minor.x = ggplot2::element_blank())
  fjern_y = ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                  panel.grid.minor.y = ggplot2::element_blank())

  fig <-
    ggplot2::ggplot(my_proptable_hospitals)+
    #The bars:
    ggplot2::geom_col(mapping=ggplot2::aes(x = AvdNavn, y=perc),#,width=1/3
             fill=factor(ifelse(stringr::str_detect(my_proptable_hospitals$AvdNavn, "Nasjonal"),"grey", "#2171b5"))) +

    ggplot2::labs(y=my_y_lab, #my_proptable_hospitals$my_y_lab,#
         title = my_title)+#colnames(my_proptable_hospitals[,4]))+ #my_figText$title)+
    ggplot2::scale_y_continuous(limits = c(0,max(my_proptable_hospitals$perc)), #c(0,105)
                       expand = ggplot2::expansion(mult=c(0,0.1)),breaks = seq(0,100,10))+ #in "expansion use "add" instead of "mult" to make expansion abolute rather than relative(mult for multiplication)
    ggplot2::xlab(NULL)+
    # geom_errorbar(aes(x=my_proptable_hospitals$AvdNavn, ymin=my_proptable_hospitals$perc-sd(my_proptable_hospitals$perc),
    #                                                 ymax=my_proptable_hospitals$perc+sd(my_proptable_hospitals$perc))
    #               , colour= 'orange',alpha=0.5, size=1, width=0.2)+
    ggplot2::geom_text(
      ggplot2::aes(x = AvdNavn, y= 1,
          label= factor(ifelse(perc == 0.00000123,'',paste0(round(perc,1)))), #ifelse statement to remove 0% values (if you want with "%", use: paste0(round(perc,1),'%') #to map percentace label at top of bar use "y= perc"
          hjust='left'),
      alpha=0.5)+

    ggplot2::geom_text(
      ggplot2::aes(x = AvdNavn, y= 1,
          label= factor(ifelse(n < 5,"N<5",'')), #ifelse statement to add text saying n is lower than 5
          hjust='left'),
      alpha=0.5)+

    # geom_text(
    #   aes(x = AvdNavn, y= 100,
    #       label=n_missing, #to map percentace label at top of bar use "y= perc"
    #       hjust='left'),
    #   alpha=0.5)+

    # geom_text(
    #   aes(x = AvdNavn, y= 105,
    #       label=n_uaktuell, #to map percentace label at top of bar use "y= perc"
    #       hjust='left'),
  #   alpha=0.5)+

  ##make N appear to the left/below bars:
  #geom_text(
  #  aes(x = AvdNavn, y= -0.15, label=paste0('N=(',n,')')),
  #  hjust='left')+
  #scale_y_continuous(limits = c(-0.15,1))+ # - 0,15 to increase margin below/to the left of bar to make space for N
  tema +
    fjern_y +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank())+
    ggplot2::coord_flip()

  # green goal line:
  if (GreenGoal!=''){
    if(GreenGoal=='mean'){
      fig <-   fig +
        ggplot2::geom_hline(yintercept = my_proptable_hospitals[stringr::str_detect(my_proptable_hospitals$AvdNavn, "Nasjonal"),]$perc, col='forestgreen', size=1, linetype=1) #to set goal value at national value: "my_proptable_hospitals[stringr::str_detect(my_proptable_hospitals$AvdNavn, "Nasjonal"),]$perc"

    } else {
      fig <-   fig +
        ggplot2::geom_hline(yintercept = as.numeric(GreenGoal), col='forestgreen', size=1, linetype=1)
    }
  }
  # yellow goal line:
  if (YellowGoal!=''){
    if(YellowGoal=='mean'){
      fig <-   fig +
        ggplot2::geom_hline(yintercept = my_proptable_hospitals[stringr::str_detect(my_proptable_hospitals$AvdNavn, "Nasjonal"),]$perc, col='gold', size=1, linetype=1) #to set goal value at national value: "my_proptable_hospitals[stringr::str_detect(my_proptable_hospitals$AvdNavn, "Nasjonal"),]$perc"

    } else {
    fig <-   fig +
      ggplot2::geom_hline(yintercept = as.numeric(YellowGoal), col='gold', size=1, linetype=1) #
    }

  }



  return(fig)
}
