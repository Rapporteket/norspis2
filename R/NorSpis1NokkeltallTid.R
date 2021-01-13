#' Plot of key figures over time
#'
#' @param RegData Registry data
#' @param enhetsUtvalgEgenNasjonal Whether key figures should show for own unit or be a national value
#' @param reshID
#'
#' @return ggplot2 figure og key figures over time:
#' - Antall startregistreringer (ferdigstilte)
#'
#' @export
#'
#' @examples

#UNIKE PASIENTER

NorSpis1NokkeltallTid <- function(RegData,
                                 enhetsUtvalgEgenNasjonal='nasjonal',
                                 reshID)
{

  if(enhetsUtvalgEgenNasjonal == 'nasjonal') {

    PasienterOverTid <- RegData %>%
      dplyr::filter(HovedDato>='2012-01-01' &
               HovedDato <= '2030-12-31',
             RegRegtype %in% c(1,2,3,4),
             BasisRegStatus==1) %>%#kun startreg. og ferdigstilte
      dplyr::mutate(HovedDato=as.Date(HovedDato),
             Maaned=cut(HovedDato,
                        breaks = "month")) %>%#Ordner datofromat, og lager månedsvariabel
      dplyr::mutate(Maaned = as.Date(Maaned))%>%
      dplyr::count(Maaned)
  }

  if(enhetsUtvalgEgenNasjonal == 'egenEnhet') {
    PasienterOverTid <- RegData %>%
      dplyr::filter(HovedDato>='2012-01-01' & HovedDato <= '2030-12-31', RegRegtype %in% c(1,2,3,4), BasisRegStatus==1,
             AvdRESH == reshID) %>%#kun startreg. og ferdigstilte
      dplyr::mutate(HovedDato=as.Date(HovedDato), Maaned=cut(HovedDato, breaks = "month")) %>%#Ordner datofromat, og lager månedsvariabel
      dplyr::mutate(Maaned = as.Date(Maaned))%>%
      dplyr::count(Maaned)
  }

  #plotter akkumulerte, ferdigstilte startregisteringer over tid.
  ggplot2::ggplot(data=PasienterOverTid)+
    ggplot2::geom_line(mapping= ggplot2::aes(Maaned,cumsum(n)))+
    ggplot2::scale_x_date(breaks = "1 month")+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust=1))

}
