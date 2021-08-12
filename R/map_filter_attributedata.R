#' Function that filter the data (using dplyr syntax)
#' An own function because we want to use the returned data several places later
#' (in both plot and table).
#'
#' @param chosen_map_area norway, north, middle, west, southeast
#' @param chosen_unit_type 1,2,4,5,6,7,8,9,3,10
#'
#' @return attributedata_geo_chosenarea Prepared dataset with attributes and geometry column *for the chosen area*
#'
#' @export

#Step 1:  Load and prepare data on treatment units and map data.
#         Do it int he feateure_dataANDgadm.R script in data-raw folder.
#Step 2:
#filter data , depending on chosen region
map_filter_attributedata <- function(chosen_unit_type=c(1,2,4,5,6,7,8,9,3,10,11,12),
                                 chosen_hf = 'Alle'#c('Alle',
                                 # 'Helse Nord',
                                 # 'Helse Midt',
                                 # 'Helse Vest',
                                 # 'Helse Sør-Øst',
                                 # 'Helse Møre og Romsdal', 'Helse Nord-Trøndelag', 'St. Olavs Hospital',
                                 # 'Finnmarkssykehuset','Helgelandssykehuset', 'Nordlandssykehuset',
                                 # 'Universitetssykehuset Nord-Norge','Private behandlingssteder',
                                 # 'Akershus universitetssykehus', 'Oslo universitetssykehus',
                                 # 'Sykehuset i Vestfold', 'Sykehuset Innlandet', 'Sykehuset Telemark',
                                 # 'Sykehuset Østfold','Sørlandet Sykehus', 'Vestre Viken', 'Helse Bergen',
                                 # 'Helse Fonna', 'Helse Førde', 'Helse Stavanger')
)
#choices chosen_hf, see: unique(attributedata_geo$enhet_hf)

#TODO: Fix waring:
#     Warning in if (chosen_hf == "Helse Nord" | chosen_hf %in% c("Finnmarkssykehuset",  :
#       the condition has length > 1 and only the first element will be used
#see:
#https://stackoverflow.com/questions/47034933/what-does-the-error-the-condition-has-length-1-and-only-the-first-element-wil

{
  #call the attributedata that is built into the package


  library(dplyr)

  if (chosen_hf == 'Alle'){
    chosen_hf <- c('Helse Møre og Romsdal', 'Helse Nord-Trøndelag', 'St. Olavs Hospital',
                   'Finnmarkssykehuset','Helgelandssykehuset', 'Nordlandssykehuset',
                   'Universitetssykehuset Nord-Norge',
                   'Akershus universitetssykehus', 'Oslo universitetssykehus',
                   'Sykehuset i Vestfold', 'Sykehuset Innlandet', 'Sykehuset Telemark',
                   'Sykehuset Østfold','Sørlandet Sykehus', 'Vestre Viken', 'Helse Bergen',
                   'Helse Fonna', 'Helse Førde', 'Helse Stavanger',
                   "Private behandlingssteder")
  }

  if (chosen_hf == 'Helse Nord'){
    #1.changing name of "Private behandlingssteder" to use in north filter (i.e. changes only if "Helse Nord"is chosen)
    attributedata_geo$enhet_hf <- as.character(attributedata_geo$enhet_hf) #as.character() (from factor)
    attributedata_geo$enhet_hf[attributedata_geo$enhet_hf == "Private behandlingssteder" &
                                 attributedata_geo$enhet_rhf == 'Helse Nord']                <- 'Private (nord)'
    attributedata_geo$enhet_hf <- as.factor(attributedata_geo$enhet_hf) ##back to factor (not sure if rally needed)
    #2.chosen_hf
    chosen_hf <- c('Finnmarkssykehuset','Helgelandssykehuset', 'Nordlandssykehuset',
                   'Universitetssykehuset Nord-Norge',
                   'Private (nord)')
  }else if(chosen_hf == 'Helse Midt'){
    #1.name change priv
    attributedata_geo$enhet_hf <- as.character(attributedata_geo$enhet_hf)
    attributedata_geo$enhet_hf[attributedata_geo$enhet_hf == "Private behandlingssteder" &
                                 attributedata_geo$enhet_rhf == 'Helse Midt']                <- 'Private (midt)'
    attributedata_geo$enhet_hf <- as.factor(attributedata_geo$enhet_hf)
    #2.chosen_hf
    chosen_hf <- c('Helse Møre og Romsdal', 'Helse Nord-Trøndelag', 'St. Olavs Hospital',
                   'Private (midt)')
  }else if(chosen_hf == 'Helse Vest'){
    #1.name change priv
    attributedata_geo$enhet_hf <- as.character(attributedata_geo$enhet_hf)
    attributedata_geo$enhet_hf[attributedata_geo$enhet_hf == "Private behandlingssteder" &
                                 attributedata_geo$enhet_rhf == 'Helse Vest']                <- 'Private (vest)'
    attributedata_geo$enhet_hf <- as.factor(attributedata_geo$enhet_hf)
    #2.chosen_hf
    chosen_hf <- c('Helse Bergen',
                   'Helse Fonna', 'Helse Førde', 'Helse Stavanger',
                   'Private (vest)')
  }else if(chosen_hf == 'Helse Sør-Øst'){
    #1.name change priv
    attributedata_geo$enhet_hf <- as.character(attributedata_geo$enhet_hf)
    attributedata_geo$enhet_hf[attributedata_geo$enhet_hf == "Private behandlingssteder" &
                                 attributedata_geo$enhet_rhf == 'Helse Sør-Øst']             <- 'Private (sør-øst)'
    attributedata_geo$enhet_hf <- as.factor(attributedata_geo$enhet_hf)
    #2.chosen_hf
    chosen_hf <- c('Akershus universitetssykehus', 'Oslo universitetssykehus',
                   'Sykehuset i Vestfold', 'Sykehuset Innlandet', 'Sykehuset Telemark',
                   'Sykehuset Østfold','Sørlandet Sykehus', 'Vestre Viken',
                   'Private (sør-øst)')
  }#else, chosen_hf will be the ONE chosen HF
  else if(chosen_hf %in% c('Helse Møre og Romsdal', 'Helse Nord-Trøndelag', 'St. Olavs Hospital',
                           'Finnmarkssykehuset','Helgelandssykehuset', 'Nordlandssykehuset',
                           'Universitetssykehuset Nord-Norge',
                           'Akershus universitetssykehus', 'Oslo universitetssykehus',
                           'Sykehuset i Vestfold', 'Sykehuset Innlandet', 'Sykehuset Telemark',
                           'Sykehuset Østfold','Sørlandet Sykehus', 'Vestre Viken', 'Helse Bergen',
                           'Helse Fonna', 'Helse Førde', 'Helse Stavanger',
                           'Private behandlingssteder')){
    chosen_hf <- chosen_hf
  }
  #data file filtered for correct area/treatment units
  attributedata_geo_chosenarea <- attributedata_geo %>%
    filter(enhet_hf %in% chosen_hf) %>%
    filter(enhet_kat_sf_num %in% chosen_unit_type)

  #return data so that we can use them in both map and table
  return(attributedata_geo_chosenarea)
}

