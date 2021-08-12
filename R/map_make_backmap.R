#' Function that makes the background map.
#'
#' @param chosen_map_area norway, north, middle, west, southeast
#'
#' @return backgroundmap
#'
#' @export

#Step 3:
##We prepare the map in several LAYERS to use with ggplot syntax (+) to visualize in the end

#Map, part 1:
#theme and background map, depending on region
map_make_backmap <- function(#chosen_map_area='norway'
  chosen_hf = c('Alle',
                'Helse Nord',
                'Helse Midt',
                'Helse Vest',
                'Helse Sør-Øst',
                'Helse Møre og Romsdal', 'Helse Nord-Trøndelag', 'St. Olavs Hospital',
                'Finnmarkssykehuset','Helgelandssykehuset', 'Nordlandssykehuset',
                'Universitetssykehuset Nord-Norge','Private behandlingssteder',
                'Akershus universitetssykehus', 'Oslo universitetssykehus',
                'Sykehuset i Vestfold', 'Sykehuset Innlandet', 'Sykehuset Telemark',
                'Sykehuset Østfold','Sørlandet Sykehus', 'Vestre Viken', 'Helse Bergen',
                'Helse Fonna', 'Helse Førde', 'Helse Stavanger')){
  library(ggplot2)
  #Layer 1: make blank/minimal background theme
  theme_blankaround <-  theme(panel.background = element_rect(fill = "white"),
                              axis.line = element_blank(),
                              axis.ticks = element_blank(),
                              axis.text = element_blank(),
                              axis.title = element_blank(),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.border = element_blank())

  #Layer 2, background map (pluss layer 1 (theme))
  #NORWAY
  if (chosen_hf == 'Alle' | chosen_hf == 'Private behandlingssteder') {
    backgroundmap <- ggplot(data=gadm36_NOR_1_sf_simp) +
      geom_sf(fill = "#DADADA", color = "gray100")+
      theme_blankaround
  }
  #NORTH:
  if (chosen_hf == 'Helse Nord' |
      chosen_hf %in% c('Finnmarkssykehuset','Helgelandssykehuset', 'Nordlandssykehuset',
                       'Universitetssykehuset Nord-Norge')) {

    gadm36_NOR_1_sf_north <- gadm36_NOR_1_sf_simp %>%
      filter(NAME_1 %in% c('Nordland', 'Troms', 'Finnmark'))

    backgroundmap <- ggplot(data=gadm36_NOR_1_sf_north) +
      geom_sf(fill = "#DADADA", color = "gray100")+
      theme_blankaround
  }
  #MIDDLE
  if (chosen_hf == 'Helse Midt' |
      chosen_hf %in% c('Helse Møre og Romsdal', 'Helse Nord-Trøndelag', 'St. Olavs Hospital')) {
    gadm36_NOR_1_sf_middle <- gadm36_NOR_1_sf_simp %>%
      filter(NAME_1 %in% c('Nord-Trøndelag', 'Sør-Trøndelag', 'Møre og Romsdal'))

    backgroundmap <- ggplot(data=gadm36_NOR_1_sf_middle) +
      geom_sf(fill = "#DADADA", color = "gray100")+
      theme_blankaround
  }
  #WEST:
  if (chosen_hf == 'Helse Vest' |
      chosen_hf %in% c('Helse Bergen','Helse Fonna', 'Helse Førde', 'Helse Stavanger')) {
    gadm36_NOR_1_sf_vest <- gadm36_NOR_1_sf_simp %>%
      filter(NAME_1 %in% c('Hordaland', 'Sogn og Fjordane', 'Rogaland'))

    backgroundmap <- ggplot(data=gadm36_NOR_1_sf_vest) +
      geom_sf(fill = "#DADADA", color = "gray100")+
      theme_blankaround
  }
  #SOUT-EAST
  if (chosen_hf == 'Helse Sør-Øst' |
      chosen_hf %in% c('Akershus universitetssykehus', 'Oslo universitetssykehus','Sykehuset i Vestfold',
                       'Sykehuset Innlandet', 'Sykehuset Telemark','Sykehuset Østfold','Sørlandet Sykehus', 'Vestre Viken')) {
    gadm36_NOR_1_sf_southeast <- gadm36_NOR_1_sf_simp %>%
      filter(NAME_1 %in% c('Hedmark','Oppland', 'Buskerud', 'Vestfold', 'Telemark', 'Vest-Agder', 'Aust-Agder', 'Ãstfold', 'Oslo', 'Akershus'))

    backgroundmap <- ggplot(data=gadm36_NOR_1_sf_southeast) +
      geom_sf(fill = "#DADADA", color = "gray100")+
      theme_blankaround
  }

  return(backgroundmap)
}
