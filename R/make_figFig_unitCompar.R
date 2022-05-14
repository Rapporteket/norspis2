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
#' @param showErrorBar 95% CI around each bar (my_proptable_hospitals will then need additional perc and n columns)
#' @param showComparisonPoints visualise point for comparison period (my_proptable_hospitals will then need additional perc and n columns)
#'
#' @return figure
#' @export
#'
#' @examples

make_figFig_unitCompar <- function(

  my_proptable_hospitals,
  showErrorBar = FALSE,
  showErrorBar2 = FALSE,
  showComparisonPoints = FALSE,
  #my_proptable_hospitals_year_x,
  my_y_lab = "Andel (%)", #default "Andel (%), unless you change it
  my_title = '',#default empty (""), unless you change it
  YellowGoal = 'mean',  #1)'' 2)'mean' or 3) for instance '60'
  GreenGoal = 'mean',
  exclude_unit = "Tønsberg (BUP, s.team)",
  sort_on_comparison_group_instead = TRUE,
  use_name_without_n = TRUE,
  letter_size = 12,

  fig_type_comparison = 'two_bars'#'two_bars', 'bar_point'
  )

  {
  #Theme:
  if(!exists("skriftstorleik")) # Skriftstørrelse bør være definert
                                #i kvarrtalsrapportfil
    skriftstorleik = letter_size
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

  my_color = "lightblue"

  if(use_name_without_n == TRUE){
    #(in make_figTable_.. , we made variables both with and without n)
    my_proptable_hospitals$AvdNavn <- my_proptable_hospitals$AvdNavnNavn
  }

  # SORTING... if want change which group is comparison (and sorted on):
  if(sort_on_comparison_group_instead == TRUE){
    #SORT:
    #remember that plot will sort by AvdNavn ,so we must reorder this factor
    my_proptable_hospitals$AvdNavn <-
      my_proptable_hospitals$AvdNavn %>%
      forcats::fct_reorder(my_proptable_hospitals$n.compare,
                           .desc=F) %>%
      forcats::fct_reorder(my_proptable_hospitals$perc.compare,
                           .desc=F)%>%#T for the other order
      forcats::fct_reorder(!is.na(my_proptable_hospitals$n),
                           .desc=T)%>%
      forcats::fct_reorder(!is.na(my_proptable_hospitals$n.compare),
                           .desc=F)


    #also sort table (but I think this is just for visual inspection, since
    #plot will sort on factor level order of AvdNavn either way)
    my_proptable_hospitals <- my_proptable_hospitals%>%
      dplyr::arrange(n.compare) %>%
      dplyr::arrange(perc.compare)

    # my_proptable_hospitals <- my_proptable_hospitals %>%
    #    arrange(!is.na(perc.compare), perc.compare, n.compare)
    #
   }

  #Add confidence intervals to table (formula for proportions used)
  my_proptable_hospitals <- my_proptable_hospitals %>%
    mutate(CILower = ((my_proptable_hospitals$perc/100)- 1.96*sqrt(( (my_proptable_hospitals$perc/100) * (1-(my_proptable_hospitals$perc/100)))/n)) *100,
           CIUpper = ((my_proptable_hospitals$perc/100)+ 1.96*sqrt(( (my_proptable_hospitals$perc/100) * (1-(my_proptable_hospitals$perc/100)))/n))*100)%>%
    #mutate so that if upper CI is above 100 make it 100 (else will be outside
    #plot and errorbar will not show):
    mutate(CIUpper = case_when(CIUpper>100 ~ 100,
                               TRUE ~ CIUpper),
           CILower = case_when(CILower<0 ~0,
                               TRUE ~ CILower))%>%
    mutate(CIUpper = case_when(n<5 ~ NaN,
                               TRUE ~ CIUpper),
           CILower = case_when(n<5 ~ NaN,
                               TRUE ~ CILower))

  #Add confidence intervals for comparison to table (proportions formula used)
  my_proptable_hospitals <- my_proptable_hospitals %>%
    mutate(CILower2 = ((my_proptable_hospitals$perc.compare/100)- 1.96*sqrt(( (my_proptable_hospitals$perc.compare/100) * (1-(my_proptable_hospitals$perc.compare/100)))/n.compare)) *100,
           CIUpper2 = ((my_proptable_hospitals$perc.compare/100)+ 1.96*sqrt(( (my_proptable_hospitals$perc.compare/100) * (1-(my_proptable_hospitals$perc.compare/100)))/n.compare))*100)%>%
    #mutate so that if upper CI is above 100 make it 100 (else will be outside
    #plot and errorbar will not show):
    mutate(CIUpper2 = case_when(CIUpper2>100 ~ 100,
                               TRUE ~ CIUpper2),
           CILower2 = case_when(CILower2<0 ~0,
                               TRUE ~ CILower2))%>%
    mutate(CIUpper2 = case_when(n.compare<5 ~ NaN,
                               TRUE ~ CIUpper2),
           CILower2 = case_when(n.compare<5 ~ NaN,
                               TRUE ~ CILower2))

  #make longer proptable to visualize bars side by side:
  # my_proptable_hospitals_longer <-
  #   tidyr::pivot_longer(my_proptable_hospitals,
  #                       cols=c('perc', 'perc.compare','n', 'n.compare'),
  #                       names_to='variable_perc',
  #                       values_to="value_perc") %>%

  # my_proptable_hospitals_longe <-
  #   tidyr::pivot_longer(tab3,
  #                       cols=c('perc', 'perc.compare','n', 'n.compare'),
  #                       names_to=c('variable'),
  #                       values_to="value")



  if(fig_type_comparison == "two_bars"){

  #TABLE FOR FIGTYPE 2:
  #make a longer table which where perc and perc.compare is gathered in one
  #variable, SO THAT WE CAN USE THIS TO MAKE A GROUPED BARPLOT with two bars for
  #each hospital: https://stackoverflow.com/questions/42820677/ggplot-bar-plot-side-by-side-using-two-variables
  my_proptable_hospitals_longer <-
    tidyr::pivot_longer(my_proptable_hospitals,#my_proptable_hospitals,
                        cols=c('perc.compare','perc'),
                        names_to=c('variable'),
                        values_to="value")%>%
    #some mutates to remove duplicate values/values not belonging in variable:
    dplyr::mutate(n = dplyr::case_when(variable == "perc.compare" ~ NA_integer_,
                                       TRUE ~ n))%>%
    dplyr::mutate(n.compare = dplyr::case_when(variable == "perc" ~ NA_integer_,
                                               TRUE ~ n.compare))%>%
    #create only one pari of CI variables :
    dplyr::mutate(CILower_percANDperc.compare =
                    dplyr::case_when(variable == "perc.compare" ~ CILower2,
                                     variable == "perc" ~ CILower,
                                     TRUE ~ NA_real_))%>%
    dplyr::mutate(CIUpper_percANDperc.compare =
                    dplyr::case_when(variable == "perc.compare" ~ CIUpper2,
                                     variable == "perc" ~ CIUpper,
                                     TRUE ~ NA_real_))%>%
    #remove old/not used CI variables:
    select(-c("CILower","CILower2","CIUpper","CIUpper2"))


  #exclude some units from visualization:
  my_proptable_hospitals_longer <-
    my_proptable_hospitals_longer[my_proptable_hospitals_longer$AvdNavnNavn!=
                                  exclude_unit,]
  #FIG FOR FIGTYPE 2
  fig <-
    ggplot2::ggplot(my_proptable_hospitals_longer,
                    mapping=ggplot2::aes(x = AvdNavn,
                                         y=value,
                                         fill= variable))+
    #Green goal
    {if(GreenGoal != '')
    ggplot2::annotation_raster(alpha('#4da32f',.2),
      xmin=0,xmax=Inf,
      ymin=as.numeric(GreenGoal), ymax=100)
    }+
    #The bars:
    ggplot2::geom_bar(
                                             #factor(ifelse(stringr::str_detect(
                                             #my_proptable_hospitals_longer$AvdNavn,
                                             #"Nasjonal"),
                                             #c(0,1),#"lightgrey",
                                             #variable))),
                                             #variable),#,width=1/3
                       position = "dodge",
                       stat =  "identity")+
    ggplot2::scale_fill_manual(values = c(perc="#e4e0da",
                                          perc.compare = "lightblue"),
                               labels = c("T.o.m. 2020",
                                          "2021"))+ #"#f3f1ee"
#Mads sin funksjon:
    ggplot2::labs(y=my_y_lab, #my_proptable_hospitals$my_y_lab,#
                  title = my_title, #)+#colnames(my_proptable_hospitals[,4]))+
                  fill=" ")+
    #my_figText$title)+ #denne var hashtagget ut
    # ggplot2::scale_y_continuous(
      # limits = c(min(my_proptable_hospitals_longer$CILower_percANDperc.compare),max(my_proptable_hospitals_longer$CIUpper_percANDperc.compare)), #c(0,105)
      # expand = ggplot2::expansion(mult=c(0,0.1)),breaks = seq(0,100, by=10)
      #,labels = function(x) format(x, big.mark = ",", scientific = FALSE) #denne var hashtagget ut
      # )+
    #test carina:
    ggplot2::scale_y_continuous(limits = c(0,100),
                                breaks = seq(0,100, by=10))+
    ggplot2::scale_x_continuous(limits = c(0,100),
                                breaks = seq(0,100, by=10))+
    #above, in "expansion use "add" instead of "mult" to make expansion
    #absolute rather than relative(mult for multiplication)
    ggplot2::xlab(NULL)+

    {if(showErrorBar == TRUE)
      ggplot2::geom_errorbar(
        ggplot2::aes(#x=AvdNavn,
                     ymin=CILower_percANDperc.compare,  #sd(my_proptable_hospitals$perc),
                     ymax=CIUpper_percANDperc.compare)
        #sd(my_proptable_hospitals$perc))
        ,colour= 'black',alpha=0.6, size=0.3, width=0.2,
        position = ggplot2::position_dodge(0.9))
      }+
    ggplot2::geom_text(
      ggplot2::aes(#x = AvdNavn,
                   y= 1,
                   label= factor(ifelse(value == 0.00000123,'',paste0(round(value,1), " (n=", ifelse(!is.na(n),n,n.compare), ")"))),
                   #ifelse statement to remove 0% values (if you want with "%", use:
                   #paste0(round(perc,1),'%')
                   #to map percentace label at top of bar use "y= perc"
                   hjust='left'),
      alpha=0.5,
      position = ggplot2::position_dodge(0.9))+
    # ggplot2::geom_text(
    #   ggplot2::aes(#x = AvdNavn,
    #                y= 1,
    #                label= factor(ifelse(value == 0.00000123,'',n)),
    #                #ifelse statement to remove 0% values (if you want with "%", use:
    #                #paste0(round(perc,1),'%')
    #                #to map percentace label at top of bar use "y= perc"
    #                hjust='left'),
    #   alpha=0.5,
    #   position = ggplot2::position_dodge(0.9))+
    ggplot2::geom_text(
      ggplot2::aes(#x = AvdNavn,
                   y= 1,
                   label= factor(ifelse(n < 5 |n.compare <5,"n<5",'')), #ifelse statement to add
                   #text saying n is lower than 5
                   hjust='left'),
      alpha=0.5,
      position = ggplot2::position_dodge(0.9))+

    tema +
    fjern_y +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                   legend.position = "top")+
    #make national bold (https://stackoverflow.com/questions/39694490/highlighting-individual-axis-labels-in-bold-using-ggplot2):
    ggplot2::scale_x_discrete(labels=c("Nasjonal"=expression(bold(Nasjonal)),
                              parse=TRUE))+
    ggplot2::coord_flip()



  }

  if(fig_type_comparison == "bar_point"){ #if figtype is comparison of bar with point:
  fig <-
    ggplot2::ggplot(my_proptable_hospitals)+
    #The bars:
    ggplot2::geom_col(mapping=ggplot2::aes(x = AvdNavn, y=perc),#,width=1/3
             fill=factor(ifelse(stringr::str_detect(
               my_proptable_hospitals$AvdNavn,
               "Nasjonal"),
               "lightgrey",
               my_color))) +
    #
    # ggplot2::geom_col(mapping=ggplot2::aes(x = AvdNavn, y=perc.compare),#,width=1/3
    #                   fill=factor(ifelse(stringr::str_detect(
    #                     my_proptable_hospitals$AvdNavn,
    #                     "Nasjonal"),
    #                     "lightgrey",
    #                     my_color)))+

    ggplot2::labs(y=my_y_lab, #my_proptable_hospitals$my_y_lab,#
         title = my_title)+#colnames(my_proptable_hospitals[,4]))+
                           #my_figText$title)+
    #ggplot2::scale_y_continuous(
     # limits = c(min(my_proptable_hospitals$CILower),max(my_proptable_hospitals$CIUpper)), #c(0,105)
     # expand = ggplot2::expansion(mult=c(0,0.1)),breaks = seq(0,100,by=10)
      #,labels = function(x) format(x, big.mark = ",", scientific = FALSE)
      #)+
    #test carina:
    ggplot2::scale_y_continuous(limits = c(0,100),
                                breaks = seq(0,100, by=10))+
    ggplot2::scale_x_continuous(limits = c(0,100),
                                breaks = seq(0,100, by=10))+
      #above, in "expansion use "add" instead of "mult" to make expansion
      #absolute rather than relative(mult for multiplication)
    ggplot2::xlab(NULL)+

    {if(showErrorBar == TRUE)
    ggplot2::geom_errorbar(
       ggplot2::aes(x=AvdNavn,
           ymin=CILower,  #sd(my_proptable_hospitals$perc),
           ymax=CIUpper)
           #sd(my_proptable_hospitals$perc))
       ,colour= 'black',alpha=0.6, size=0.3, width=0.2)}+
    {if(showErrorBar2 == TRUE)
      ggplot2::geom_errorbar(
        ggplot2::aes(x=AvdNavn,
                     ymin=CILower2,  #sd(my_proptable_hospitals$perc),
                     ymax=CIUpper2)
        #sd(my_proptable_hospitals$perc))
        ,colour= 'black',alpha=0.6, size=0.3, width=0.2)}+

    ggplot2::geom_text(
      ggplot2::aes(x = AvdNavn, y= 1,
          label= factor(ifelse(perc == 0.00000123,'',paste0(round(perc,1)))),
          #ifelse statement to remove 0% values (if you want with "%", use:
          #paste0(round(perc,1),'%')
          #to map percentace label at top of bar use "y= perc"
          hjust='left'),
      alpha=0.5)+

    ggplot2::geom_text(
      ggplot2::aes(x = AvdNavn, y= 1,
          label= factor(ifelse(n < 5,"n<5",'')), #ifelse statement to add
                                                #text saying n is lower than 5
          hjust='left'),
      alpha=0.5)+

    {if(showComparisonPoints == TRUE)
    #show n for comparison period:
    ggplot2::geom_text(
      ggplot2::aes(x = AvdNavn, y= 110,
                   label= paste0( "(",
                                  (ifelse(is.na(n.compare), 0,n.compare))
                                  , ")"),
                   hjust='right'),
      alpha=0.5)}+

    {if(showComparisonPoints == TRUE)
    #points for chosen comparison year:
    ggplot2::geom_point(
      ggplot2::aes(x = AvdNavn,
                   y=ifelse(n.compare < 5,
                            NA,
                            perc.compare) #ifelse to remove
                                                          #point when n<5
                   ),
      size = 5,
      color= "fig.height = 7")}+

    # geom_text(
    #   aes(x = AvdNavn, y= 100,
    #     label=n_missing, #to map percentace label at top of bar use "y= perc"
    #       hjust='left'),
    #   alpha=0.5)+

    # geom_text(
    #   aes(x = AvdNavn, y= 105,
    #   label=n_uaktuell, #to map percentace label at top of bar use "y= perc"
    #       hjust='left'),
  #   alpha=0.5)+

  ##make N appear to the left/below bars:
  #geom_text(
  #  aes(x = AvdNavn, y= -0.15, label=paste0('N=(',n,')')),
  #  hjust='left')+
  #scale_y_continuous(limits = c(-0.15,1))+ # - 0,15 to increase margin
  #below/to the left of bar to make space for N
  tema +
    fjern_y +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank())+
    ggplot2::coord_flip()




  }
  # yellow goal line:
  if (YellowGoal!=''){
    if(YellowGoal=='mean'){
      fig <-   fig +
        ggplot2::geom_hline(
          yintercept = my_proptable_hospitals[stringr::str_detect(
            my_proptable_hospitals$AvdNavn, "Nasjonal"),]$perc,
          col='gold',
          size=1,
          linetype=1) #to set goal value at national value:
      #"my_proptable_hospitals[stringr::str_detect(
      #my_proptable_hospitals$AvdNavn, "Nasjonal"),]$perc"

    } else {
      fig <-   fig +
        ggplot2::geom_hline(
          yintercept = as.numeric(YellowGoal),
          col='gold',
          size=1,
          linetype=1) #
    }

  }

  # # green goal line:
  # if (GreenGoal!=''){
  #   if(GreenGoal=='mean'){
  #     fig <-   fig +
  #       ggplot2::geom_hline(
  #         yintercept = my_proptable_hospitals[stringr::str_detect(
  #           my_proptable_hospitals$AvdNavn, "Nasjonal"),]$perc,
  #         col='#4da32f',#forestgreen',
  #         size=1,
  #         linetype=1) #to set goal value at national value, use:
  #     #"my_proptable_hospitals[stringr::str_detect(
  #     #  my_proptable_hospitals$AvdNavn, "Nasjonal"),]$perc"
  #
  #   } else {
  #     fig <-   fig +
  #       ggplot2::geom_rect(
  #         aes(xmin=0,xmax=Inf, ymin=as.numeric(GreenGoal), ymax=100),
  #         col='#4da32f',
  #         fill='#4da32f'
  #       )
  #
  #       # ggplot2::geom_hline(
  #       #   yintercept = as.numeric(GreenGoal),
  #       #   col='#4da32f',#'forestgreen',
  #       #   size=1,
  #       #   linetype=1)
  #   }
  # }

  return(fig)
}
