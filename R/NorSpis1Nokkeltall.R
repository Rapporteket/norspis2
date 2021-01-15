#' Different key figures to display in dashboard
#'
#' @param RegData Registry data
#' @param enhetsUtvalgEgenNasjonal String user role
#' @param reshID String org id
#'
#' @return Ulike nøkkeltall:
#' - Antall startregistreringer
#' - Antall unike pasienter (ferdigstilte)
#'
#' @export
#'
#' @examples


NorSpis1Nokkeltall <- function(RegData, enhetsUtvalgEgenNasjonal='nasjonal', reshID)

{

  #antall unike pasienter
  if(enhetsUtvalgEgenNasjonal == 'nasjonal') {

    RegDataStart <- RegData %>% filter(HovedDato>='2012-01-01' & HovedDato <= '2030-12-31',
                                       RegRegtype %in% c(1,2,3,4), BasisRegStatus==1)
    # #ANTALL STARTREG:
    # length(RegDataStart$PasientID)

    #ANTALL UNIKE PASIENTER (BASERT PÅ STARTREG) (ferdigstilte):
    #length(unique(RegDataStart$PasientID))
  } else {
    RegDataStart <- RegData %>% filter(HovedDato>='2012-01-01' & HovedDato <= '2030-12-31',
                                       RegRegtype %in% c(1,2,3,4), BasisRegStatus==1,
                                       AvdRESH == reshID)
    # #ANTALL STARTREG:
    # length(RegDataStart$PasientID)

    #ANTALL UNIKE PASIENTER (BASERT PÅ STARTREG) (ferdigstilte):
    #length(unique(RegDataStart$PasientID))

  }


  length(unique(RegDataStart$PasientID))


}
