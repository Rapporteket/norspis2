#' Function that makes the the final map.(map_make_map uses the function map_make_backmap)
#'
#' @param chosen_unit_type c(1,2,4,5,6,7,8,9,3,10,11,12)
#' @param chosen_map_area norway, north, middle, west, southeast
#' @param chosen_color_var 'egensk_tilslutning2019_hfavd_OK'
#' @param chosen_size_var 'egensk_n_pop'
#' @param chosen_shape_var 'enhet_kat_sf_num'
#' @param visualize_colour_polygons 'yes'
#' @param visualize_size_points  'no'
#' @param visualize_text_unitname 'yes'
#'
#' @return Plots the map.
#'
#' @export

#Step 4.
#THE map with geometric points
map_make_map <- function(chosen_unit_type= c(1,2,4,5,6,7,8,9,3,10,11,12),
                        #chosen_map_area='norway',
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
                                      'Helse Fonna', 'Helse Førde', 'Helse Stavanger'),
                        chosen_color_var='egensk_tilslutning2019_hfavd_OK',
                        chosen_size_var='egensk_n_pop',
                        chosen_shape_var='enhet_kat_sf_num',
                        visualize_points='yes',
                        visualize_text_unitname='no',
                        visualize_colour_polygons='no',
                        visualize_size_points = 'no' #not in use p.t.?
)
{
  library(ggplot2)
  library(sf)
  library(ggrepel)

  #First fetch the  prepared data,from map_filter_attributedata() function, since we need them down below:
  attributedata_geo_chosenarea <- norspis2::map_filter_attributedata(#chosen_map_area=chosen_map_area,
    chosen_hf = chosen_hf,
    chosen_unit_type = chosen_unit_type
  )
  #And get the prepared background map from the function map_make_backmap, since we need them down below:
  backgroundmap <- norspis2::map_make_backmap(chosen_hf = chosen_hf)#chosen_map_area = chosen_map_area)

  if (visualize_size_points =='yes'){
    points <- geom_point(data = attributedata_geo_chosenarea,#attributedata[which(attributedata$enhet_kat_sf_num %in% enheter), ],
                         aes(x = geo_long, y = geo_lat,
                             color = as.factor(attributedata_geo_chosenarea[[chosen_color_var]]),
                             #size=2.5,
                             size = as.numeric((attributedata_geo_chosenarea$egensk_n_pop_ekte)),
                             shape = as.character(attributedata_geo_chosenarea[[chosen_shape_var]])),
                         position = position_jitter(h=0.1,w=0.1),
                         alpha =0.5)
  }else{
    points <- geom_point(data = attributedata_geo_chosenarea,#attributedata[which(attributedata$enhet_kat_sf_num %in% enheter), ],
                         aes(x = geo_long, y = geo_lat,
                             color = as.factor(attributedata_geo_chosenarea[[chosen_color_var]]),
                             # size = as.numeric((attributedata_geo_chosenarea$egensk_n_pop_ekte)),
                             shape = as.character(attributedata_geo_chosenarea[[chosen_shape_var]])), #needed as.character instead of as. factor because if not the order of the values became wrong
                         position = position_jitter(h=0.1,w=0.1, seed=123), #seed arg makes the jitter reproducible (below)
                         size = 2.5,
                         alpha =0.5)
    #make black points similar as above, to plot behind points above - just to make points a little darker on coloured background
    points_border <- geom_point(data = attributedata_geo_chosenarea,#attributedata[which(attributedata$enhet_kat_sf_num %in% enheter), ],
                                aes(x = geo_long, y = geo_lat,
                                    # size = as.numeric((attributedata_geo_chosenarea$egensk_n_pop_ekte)),
                                    shape = as.character(attributedata_geo_chosenarea[[chosen_shape_var]])), #needed as.character instead of as. factor because if not the order of the values became wrong
                                position = position_jitter(h=0.1,w=0.1, seed=123), #seed arg makes the jitter reproducible (above)
                                fill = 'black',
                                size = 3,
                                alpha =0.5)

  }

  #Map, part3: Coloured polygons

  #TODO: Try geom_polygon instead of geom_sf, to manually set colour along with scale_fill_manual
  polygons <- geom_sf(data = attributedata_geo_chosenarea,
                      aes(fill = as.factor(attributedata_geo_chosenarea[[chosen_color_var]]),#attributedata_geo_chosenarea[[chosen_color_var]], #colour of fill
                          color = NA), #colour of border of polygon
                      alpha = 0.35)
  #+
  #scale_fill_manual(values=c("1"="limegreen","2"="orange","3"="red"))

  ##Map, part 4: Brush up he guide/legends
  ##Colour and guide for colour of points:
  #variable name of variables beginning with "egensk" (for norwegian "egenskaper")
  #If chosen variable is about "tilslutning"(beginning with "egensk_tilslutning"),
  #then write so in guide
  #TODO: make other if statements for other types of "egenskaper")
  if(chosen_color_var %in% grep("^egensk_tilslutning",colnames(attributedata_geo_chosenarea), value = TRUE)){
    guide_pointcolour <- scale_color_manual("Tilslutning",
                                            values=c("1"="limegreen","2"="orange","3"="red"),
                                            labels = c("1"="Leverer data", "2"="Planlagt oppstart", "3"="Ingen aktivitet"))
  }

  #Shape and guide for shape and points:
  guide_pointshape <- scale_shape_manual("Enhetstype",
                                         values=c("1"=15,"2"=16, "3"=17, "4"= 18, "5"=19,"6"=20,"7"=0,"9"=1,"10"=5,
                                                  "11"=24,"12"=2),#unique(attributedata_geo_chosenareaInUnder$enhet_kat_sf_num),
                                         labels=c("1"='Ordinær BUP',"2"='Ordinær DPS',"3"='Regional enhet',"4"='BU: Spisset SF',
                                                  "5"='V: Spisset SF', "6"="BU: Spisset SF med døgnb.", "7" ="V: Spisset SF med døgnb.",
                                                  "9"='Annen/ukjent',"10"='Spesialpoliklinikk',
                                                  "11"='Regional enhet (BU)', "12"='Regional enhet (V)')) #"unique(attributedata_geo_chosenareaInUnder$enhet_kat_sf))

  # #Guide for size of point:
  # guide_pointsize <- guides(size=guide_legend("Antall pasienter (n)"))#, override.aes = list(shape = 15)))

  guide_polygoncolour <- scale_fill_manual("Tilslutning, \n opptaksområde",
                                           values=c("1"="limegreen","2"="orange","3"="red"),
                                           labels = c("1"="Leverer data", "2"="Planlagt oppstart", "3"="Ingen aktivitet"))

  ## Labels
  label_text <- paste0(unique(attributedata_geo_chosenarea$enhet_kat_sf), collapse=", ")

  map_labels <- labs(caption=expression(""))#labs(caption=expression(bold("Figur: Enhetsoversikt"))
  # caption= #expression(paste(bold("ENHETER INKLUDERT I FIGUR "))) #to make bold (see: https://stackoverflow.com/questions/32555531/how-to-italicize-part-one-or-two-words-of-an-axis-title)
  #          paste(
  #             strwrap(paste0("ENHETER INKLUDERT I FIGUR: ", label_text),
  #                     #whitespace_only = TRUE,
  #                     width = 50),
  #             collapse = "\n"
  #)
  #subtitle= label_text
  #)#+
  ## Left align caption (will hower only work in next version of ggplot2/
  ## - currently only in development version (see: https://stackoverflow.com/questions/41105759/aligning-title-subtitle-and-caption-for-horizontal-ggplot-barchart)):
  #+
  #theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
  #      plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
  #      plot.caption.position =  "plot") #left adjusts the caption text below plot
  ## Add logo
  #recipe from: https://www.markhw.com/blog/logos
  # l <- grid::rasterGrob(png::readPNG("PATH...norspis2/inst/shinyApps/Norspis.png"), interpolate = TRUE)
  # logo <-     annotation_custom(l, xmin = 25, xmax = 30, ymin = 58, ymax = 59)
  #             #+
  #             #coord_sf(clip = "off") +
  #             #theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))


  ##Labels to the geometric points:
  #https://stackoverflow.com/questions/50044835/can-geom-label-draw-points-to-a-position-on-a-map/50045289
  #https://stackoverflow.com/questions/15624656/label-points-in-geom-point
  #https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html#polar-coordinates
  #TODO: check if can remove the sourounding if statement, and only include code inside instead.
  if(visualize_text_unitname == 'yes'){
    point_labels <- ggrepel::geom_text_repel(data = attributedata_geo_chosenarea,#geom_TEXT_repel/geom_LABEL_repel for boxes around text.
                                             aes(x = geo_long, y = geo_lat,label = attributedata_geo_chosenarea$enhet_hfavd),
                                             force = 5,
                                             color = 'black',
                                             size  = 3,
                                             box.padding = 0.7, point.padding = 0.5)
  }

  #THE PLOT

  #Two following lines will only be active if visalize_size_point== 'yes':
  #guide_pointsize+
  #  scale_size_continuous(range = c(3, 20))+ # this line increases the size of the point (see: https://stackoverflow.com/questions/20251119/increase-the-size-of-variable-size-points-in-ggplot2-scatter-plot)

  if(visualize_text_unitname == 'yes' & visualize_colour_polygons == 'yes' & visualize_points =='no'){
    backgroundmap +
      #points+
      #guide_pointcolour+
      #guide_pointshape+
      point_labels+
      polygons+
      guide_pointcolour+ #TODO: Fix - line only included to avoid error: "Error: Must request at least one colour from a hue palette."
      guide_polygoncolour+
      map_labels
  }else if(visualize_text_unitname == 'no' & visualize_colour_polygons == 'yes'  & visualize_points =='no'){
    backgroundmap +
      #points+
      #guide_pointcolour+
      #guide_pointshape+
      polygons+
      guide_pointcolour+ #TODO: Fix - line only included to avoid error: "Error: Must request at least one colour from a hue palette."
      guide_polygoncolour+
      map_labels
  }else if(visualize_text_unitname == 'yes' & visualize_colour_polygons == 'no'  & visualize_points =='no'){
    backgroundmap +
      # points+
      # guide_pointcolour+
      # guide_pointshape+
      # guide_polygoncolour+
      point_labels+
      map_labels
  }else if(visualize_text_unitname == 'no' & visualize_colour_polygons == 'no' & visualize_points =='no'){
    backgroundmap+
      map_labels
    # +
    # points+
    # guide_pointcolour+
    # guide_pointshape+
    # guide_polygoncolour+
    # point_labels+
    # polygons
  }else if(visualize_text_unitname == 'yes' & visualize_colour_polygons == 'yes' & visualize_points =='yes'){
    backgroundmap +
      points_border+
      points+
      guide_pointcolour+
      guide_pointshape+
      polygons+
      guide_polygoncolour+
      point_labels+
      map_labels
  }else if(visualize_text_unitname == 'yes' & visualize_colour_polygons == 'no' & visualize_points =='yes'){
    backgroundmap +
      points_border+
      points+
      guide_pointcolour+
      guide_pointshape+
      #polygons+
      #guide_polygoncolour+
      point_labels+
      map_labels
  }else if(visualize_text_unitname == 'no' & visualize_colour_polygons == 'yes' & visualize_points =='yes'){
    backgroundmap +
      points_border+
      points+
      guide_pointcolour+
      guide_pointshape+
      polygons+
      guide_polygoncolour+
      map_labels#+
    #point_labels
  }else{#only the condition for showing points is 'yes (no,no,yes)
    backgroundmap +
      points_border+
      points+
      guide_pointcolour+
      guide_pointshape+
      map_labels#+
    #   polygons+
    #   guide_polygoncolour+
    # point_labels
  }

}#map_make_map function end
