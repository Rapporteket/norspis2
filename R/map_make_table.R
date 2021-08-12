#' Function that makes the table with map information pluss other variables of choice
#'
#' @param chosen_unit_type write description of variable here
#' @param chosen_map_area write description of variable here
#' @param chosen_color_var write description of variable here
#' @param chosen_color_var2 write description of variable here
#' @param chosen_color_var3 write description of variable here
#' @param chosen_string_var write description of variable here
#'
#' @return Shows the table.
#'
#' @export

#Step 5:
#Table with same information as visible in map - to place underneath map.
map_make_table <- function(chosen_unit_type= c(1,2,4,5,6,7,8,9,3,10,11,12),#c(1,2,4,5,6,7,8,9,3,10),
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
                          chosen_string_var='egensk_datakval_kompl_dg2017_hfavd_OK',
                          chosen_color_var='egensk_tilslutning2019_hfavd_OK',
                          chosen_color_var2='egensk_tilslutning2018_hfavd_OK',
                          chosen_color_var3='null',
                          chosen_table_type='kable'){ #not implemented as a choice in app - must be changed manually


  library(kableExtra)
  library(flextable)
  library(pagedown)
  library(dplyr)
  #devtools::install_github("davidgohel/officedown")
  library(officedown) #needed in som cases to knit word documents in rmarkdown, see: comment here: https://davidgohel.github.io/flextable/articles/overview.html
  # Notes about rmarkdown::word_document
  # This note is only relevant when using flextable in an R Markdown document with Word output.
  # In some context, officedown::rdocx_document() will have to be used instead of rmarkdown::word_document(). This is because Word don’t store hyperlinks or images in the main document but store references. This is not managed by rmarkdown unless officedown::rdocx_document() is used.
  # If you don’t add images or hyperlinks in a flextable rendered by officedown::rdocx_document() then you can forget about that.


  ###---START OF CODE COMMON FOR BOTH TABLE TYPES:
  ##
  #
  #First fetch the  prepared data,from map_filter_attributedata() function, since we need them down below:
  attributedata_geo_chosenarea <- norspis2::map_filter_attributedata(#chosen_map_area=chosen_map_area,
    chosen_unit_type = chosen_unit_type,
    chosen_hf = chosen_hf)

  sf::st_geometry(attributedata_geo_chosenarea) <- NULL # remove geometry, coerce to data.frame (see: https://rdrr.io/cran/sf/man/st_geometry.html
  #sf::st_set_geometry(attributedata_geo_chosenarea, NULL) # remove geometry, coerce to data.frame (see: https://rdrr.io/cran/sf/man/st_geometry.html

  ##Preparing columns
  #fixed
  fixedcolumns <- c('enhet_rhf','enhet_hf', 'enhet_hfavd', 'enhet_kat_sf')
  #all possible choices, i.e. variables beginning with egensk, and are OKeyed (logic also used in app):
  possiblechosencolumns <- intersect(
    grep("(^egensk)", names(norspis2:::attributedata_geo), value=TRUE),
    grep("(.OK)", names(norspis2:::attributedata_geo), value=TRUE))
  #adding fixed and all possible choices (giving all columns)
  fixedplusspossiblechosen <- c(fixedcolumns,possiblechosencolumns)
  #Variable names (by making a dataframe with variable names and display names):
  choices_df = data.frame(
    fixedplusspossiblechosen  =  fixedplusspossiblechosen ,
    displaynames = c('RHF', 'HF','HF-avdeling','Enhetskategori','Inkludering',
                     'Tilslutning 2017','Tilslutning 2018', 'Tilslutning 2019','Tilslutning 2020','Dekningsgrad 2017',
                     'Dekningsgrad 2018 (start)', 'Dekningsgrad 2018 (slutt)', 'Senger SF (N)', 'Prioritert 2020','Kontaktperson'))
  #TODO:must be updated when
  #new possible variables are added
  #i.e. new egensk_ variables
  #that are OK-eyed in Excel.
  #All CHOSEN columns (including the fixed columns)
  chosencolumns <- c(chosen_color_var,chosen_color_var2,chosen_color_var3, chosen_string_var)
  chosencolumns <- chosencolumns[!(chosencolumns=='null')] #remove the variables that is not chosen  (value == 'null)
  columns <- c(fixedcolumns,chosencolumns)
  #Displayname for CHOSEN columns (seleceted from total dataframe above (called choices_df))
  columnnames <- lapply(columns, function(x) choices_df$displaynames[match(x, choices_df$fixedplusspossiblechosen)])
  columnnames <-as.character(unlist(columnnames))
  #did not work,because values chosen twice or more appeared only once:
  #columnnames <- as.character(choices_df$displaynames[fixedplusspossiblechosen %in% columns])
  #
  ##
  ###---END OF CODE COMMON FOR BOTH TABLE TYPES

  ###TYPE 1 table:
  ##Kable
  #
  if(chosen_table_type == 'kable'){
    #Just adjusting som options for the knitr table (not sure if/why this is really needed)
    options(knitr.table.format ="html")

    #variables/columns
    #TODO: see if not possible to remove "%in% chosen_unit_type" underneath. Isn't it abundant?
    kt<-attributedata_geo_chosenarea[which(attributedata_geo_chosenarea[['enhet_kat_sf_num']] %in% chosen_unit_type),           #rows
                                     columns                                                                                  #columns
                                     #c('enhet_rhf','enhet_hf', 'enhet_hfavd', 'enhet_kat_sf',paste0(chosenvariables))      #columns
                                     #paste0(chosen_color_var3), paste0(chosen_color_var2), paste0(chosen_color_var)        #columns
                                     #paste0(chosen_string_var)                                                             #columns
    ] %>%
      #mutate numeric variables (i.e. p.t. the ones with values 1,2,3 that need color change):
      #MUTATE_IF requires the use of a function, see: https://stackoverflow.com/questions/42052078/correct-syntax-for-mutate-if
      #,and/or https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html
      mutate_if(names(.) %in% c("egensk_tilslutning2017_hfavd_OK",
                                "egensk_tilslutning2018_hfavd_OK",
                                "egensk_tilslutning2019_hfavd_OK"), #TODO: Update here the variables you want to colorcode r,y,green.
                function(x){kableExtra::cell_spec( "..", "html",
                                                   color = factor(x, c(1,2,3),
                                                                  c("limegreen !important", "#CCCC00 !important", "red !important")),
                                                   background = factor(x, c(1,2,3),
                                                                       c("limegreen !important", "#CCCC00 !important", "red !important")))
                }
      )%>%
      mutate_if(names(.) %in% c("egensk_prioritert2020_OK"), #TODO: Update here the variables you want to colorcode r,y,green.
                function(x){kableExtra::cell_spec( "..", "html",
                                                   color = factor(x, c(0,1),
                                                                  c("blank !important","limegreen !important")),
                                                   background = factor(x, c(0,1),
                                                                       c("blank !important","limegreen !important")))
                }
      )%>%
      knitr::kable(format ="html", escape = F,
                   caption = paste0("Tabell: Oversikt ","for enhetstype:",
                                    paste0(unique(attributedata_geo_chosenarea$enhet_kat_sf), collapse=", ")),
                   col.names = columnnames) %>% #column names
      kable_styling("striped", full_width =F)# latex_options = "scale_down")

    kt
  }
  #
  ##
  ### END: kable

  ### START: TYPE 2 table:
  ## Flextable
  #
  else if(chosen_table_type == 'flextable'){
    #Making FLEXTABLE
    #flextable data and preparations
    ft_data <- attributedata_geo_chosenarea[which(attributedata_geo_chosenarea[['enhet_kat_sf_num']] %in% chosen_unit_type),           #rows
                                            columns                                                                                  #columns
                                            #c('enhet_rhf','enhet_hf', 'enhet_hfavd', 'enhet_kat_sf',paste0(chosenvariables))      #columns
                                            #paste0(chosen_color_var3), paste0(chosen_color_var2), paste0(chosen_color_var)        #columns
                                            #paste0(chosen_string_var)                                                             #columns
    ]

    #PREP 1.COLUMN NAMES (can also be done IN flextable with set_header_labels,
    #but was difficult to achieve with dynumic code/chosen columns)
    colnames(ft_data) <- columnnames

    #PREP 2. COLORS: GIVE CATEGORICAL VALUES OF "TILSLUTNING"-VARIABLES COLORS OF CHOICE (TO DISPLAY IN FLEXT.)
    columnindex <- grep("Tilslutning|tilslutning", colnames(ft_data))
    ft_data[,columnindex][ft_data[,columnindex] == 3] <- 'red'
    ft_data[,columnindex][ft_data[,columnindex] == 2] <- 'orange'
    ft_data[,columnindex][ft_data[,columnindex] == 1] <- 'limegreen'

    #PREP 3:  N tilsl.-vars. For use in for-loop in flextable
    nTilslutningsVars <- length(grep("Tilslutning|tilslutning", colnames(ft_data)))

    #PREP 4: index of the tilslutnings-vars. For use in for-loop in flextable
    indexTilslutningvars <- grep("Tilslutning|tilslutning", colnames(ft_data))

    #THE flextable
    ft <- flextable(ft_data) %>%
      #THEME:
      theme_zebra() %>%
      #UP TO THREE TILSLUTNINGSVARS AND THEIR COLORS:
      ##1 - COLOR -first tilslutnings-var:
      #read about this special syntax here:   https://stackoverflow.com/questions/30604107/r-conditional-evaluation-when-using-the-pipe-operator
      {if(nTilslutningsVars>=1)
        compose(.,j = indexTilslutningvars[1], #the 1. tilslutning-var
                #i = ft_data[,grep("Tilslutning|tilslutning", colnames(ft_data))[1]] == 2,#only values equal to
                value = as_paragraph(
                  #set length of bar constant in value argument
                  minibar(barcol=ft_data[,indexTilslutningvars[1]], bg='transparent', value=rep(1,                                                             nrow(ft_data)), max=4)
                ),
                part = "body")
        else .
      } %>%
      ##2 - COLOR - second tilslutnings-var:
      {if(nTilslutningsVars>=2)
        compose(.,j = indexTilslutningvars[2],
                value = as_paragraph(
                  minibar(barcol=ft_data[,indexTilslutningvars[2]], bg='transparent', value=rep(1,                                                             nrow(ft_data)), max=4)
                ),
                part = "body")
        else .
      } %>%
      ##3 - COLOR - third tilslutnings-var:
      {if(nTilslutningsVars>=3)
        compose(.,j = indexTilslutningvars[3],
                value = as_paragraph(
                  minibar(barcol=ft_data[,indexTilslutningvars[3]], bg='transparent', value=rep(1,                                                             nrow(ft_data)), max=4)
                ),
                part = "body")
        else .
      } %>%

      #LINERANGE for deknignsgrad
      #TODO need to remove % first, and also find a way to handle intervals, e.g. DG=32.9-42.5%
      # compose(j = 1,
      #         value = as_paragraph(
      #           linerange(value = ft_data[,7], max = 100)
      #         ),
      #         part = "body") %>%

      #ALIGNMENT AND FIT:
      valign(valign = "top", part = "all") %>%
      autofit() %>%
      width(j = 1:2,1) %>%
      width(j = 3,2) %>%
      width(j = 4,2) %>%
      width(j = 5:ncol(ft_data), 1.2) %>%
      align(align = "l",part = "all")

    ft

  }
  #
  ##
  ### END Flextable:

  #END FUNCTION
}
