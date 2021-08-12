#attributedata_geo - this script reads, edits and save data on treatment units in package
#as well as gadm map data (gadm36_NOR_0_sf , _0_sp, _1_sf, _1_sp, _2_sf, and _2_sp)


library(xlsx)
library(sf)



#I - treatment unit data
  #1
  #read gadm data
  #Done further down...

  #1.2
  #read Kartverket 2020 data
  munic2020 <- st_read("PATH.../Downloads/Basisdata_0000_Norge_25833_NorskeFylkerKommunerIllustrasjonsdata2020_GeoJSON/Kommuner.geojson")
  counties2020 <- st_read("PATH.../Downloads/Basisdata_0000_Norge_25833_NorskeFylkerKommunerIllustrasjonsdata2020_GeoJSON/Fylker.geojson")
  #TODO: update paths above when running script


  #transform - according to geocompr book we need to tranform polygon before simplifying:
  munic2020_proj <-st_transform(munic2020, 32631 )#find codes here: https://epsg.io/?q=Norway%20kind:PROJCRS&page=8
  counties2020_proj <-st_transform(counties2020, 32631 )
  #simplyfy the data (Kartverket data is too detailed and takes a lot of time to load):
  munic2020_simp <- st_simplify(munic2020_proj, dTolerance = 1000)
  counties2020_simp <- st_simplify(counties2020_proj, dTolerance = 1000)
    #object.size(kommuner2020_simp)
  #transform back:
  munic2020_simp <- st_transform(munic2020_simp, "+proj=longlat +datum=WGS84 +no_defs")
  counties2020_simp <- st_transform(counties2020_simp, "+proj=longlat +datum=WGS84 +no_defs")

  #Give Våler and Herøy names that correspond with name in attributedata(below)/Excel-sheet
    munic2020_simp$navn <- as.character(munic2020_simp$navn) #convert to character
  munic2020_simp$navn[munic2020$kommunenummer == 1818] <- 'Herøy (Nordl.)'
  munic2020_simp$navn[munic2020$kommunenummer == 1515] <- 'Herøy (M. og R.)'
  munic2020_simp$navn[munic2020$kommunenummer == 3419] <- 'Våler (Hedm.)'
  munic2020_simp$navn[munic2020$kommunenummer == 3018] <- 'Våler (Østf.)'
    munic2020_simp$navn <- as.factor(munic2020_simp$navn) #convert back to factor

  #2
  #read data on treatment units
  attributedata <-read.xlsx("PATH.../norspis2/inst/extdata/norspis_units_keyfigures_minuscontacts.xlsx",
                         sheetName="Ark1", encoding = 'UTF-8')
  #TODO: update path above when running script


  #3
  ##Make the attributedata_geo file into a sf datafile.
  #Create empty geometries (see https://r-spatial.github.io/sf/reference/sf.html):
  #make it the same length as the number of treatment units:
  nrows <- nrow(attributedata)
  geometry = st_sfc(lapply(1:nrows, function(x) st_geometrycollection()))
  #add empty geometries to attributedata data, and in the same turn convert these data into sf  by using st_sf:
  attributedata_geo <- st_sf(attributedata, geometry = geometry)
  ## If needed, convert the whole dataset to sf data:
  #attributedata_geo <- st_as_sf(attributedata_geo)

  #4
  #Make geometry value
    #To make coloured polygons for each treatment unit, we need to make we make a geometry value
    # (list value with polygons) for each treatment unit.
    # The value represents all the areas/municipalities linked with the treatment unit:
  treatment_units <- attributedata_geo$enhet_hfavd

  output <- list(1)

  for(i in 1:length(treatment_units)) {
    #Pull out municipality value (vector with all municipalities linked with a treatment center):
    output[[i]] <- as.character(attributedata_geo$kommuner_dps[i])
    #Fix the municipality strings/"vectors" for each cell (into real R vectors):
    output[i] <- strsplit(output[[i]], ";")
  }


  output2 <- list(1)

  #(KOMMUNER)
  for(i in 1:length(treatment_units)) {
    #Then, make index variable pointing at the location of the values to be transferred:
    index_municgeos <- which(munic2020_simp$navn %in% output[i][[1]]) #| gadm36_NOR_2_sf$NAME_1 %in% output[i][[1]])
    #Pull out those geometries:
    new_geometry <- munic2020_simp$geometry[index_municgeos]
    #Then merge these multipolygons into one geometry value:
    new_geometry_merged <- st_union(new_geometry[1:length(new_geometry)])

    #and transfer values to output list of this for loop:
    output2[[i]] <- new_geometry_merged
    #and then transfer these vaules to attributedata_geo dataset:
    attributedata_geo$geometry[i] <- output2[i][[1]]
    #TODO: the code works, but some, for instance the last couple of lines may be in excess due to uncomplete understanding of for loop and      outputs of for loops.
  }

  #TODO: Code below does not work becuase it overwrites the above values...fix? Or just plot munic vales in Excel sheet instead.
  # #(FYLKER/regional units)
  # #Do similiar as above for regional units using old county (fylker) data (NAME_1 in gadm 2) since regionale enheter is associated with fylker and not kommuner
  # for(i in 1:length(treatment_units)) {
  #   #Then, make index variable pointing at the location of the values to be transferred:
  #   index_municgeos <- which(gadm36_NOR_1_sf$NAME_1 %in% output[i][[1]]) #| gadm36_NOR_2_sf$NAME_1 %in% output[i][[1]])
  #   #Pull out those geometries:
  #   new_geometry <- gadm36_NOR_1_sf$geometry[index_municgeos]
  #   #Then merge these multipolygons into one geometry value:
  #   new_geometry_merged <- st_union(new_geometry[1:length(new_geometry)])
  #
  #   #and transfer values to output list of this for loop:
  #   output2[[i]] <- new_geometry_merged
  #   #and then transfer these vaules to attributedata_geo dataset:
  #   attributedata_geo$geometry[i] 	<-  output2[i][[1]]
  #   #TODO: the code works, but some, for instance the last couple of lines may be in excess due to uncomplete understanding of for loop and      outputs of for loops.
  # }

  #5
  #set CRS:
  st_crs(attributedata_geo) <- "+proj=longlat +datum=WGS84 +no_defs"

  # #CODE TO DO THE ABOVE FOR A SINGLE TREATMENT UNIT, IF NEEDED:
  # #Pull out the polygons/geometry objects from gadm that should be linked with each treatmens center:
  # #First, the index for the unit at interest
  # index_unit <- which(treateat_geo$enhet_hfavd %in% c('Salten DPS, Fauske'))
  # #Then pull out municipality value (vector with all municipalities linked with a treatment center):
  # municipalities_related <- treateat_geo$kommuner_dps[index_unit]
  # #Fix the municipality vector (into a real R vector):
  # municipalities_related <- as.character(municipalities_related)
  # municipalities_related <- strsplit(municipalities_related, ";")  #Last line with strsplit, see: https://stackoverflow.com/questions/29222019/converting-a-vector-of-sentences-to-vector-of-words-using-apply-functions
  # municipalities_related <- municipalities_related[[1]]
  # #Then, make index variable pointing at the location of the values to be transferred:
  # index_municgeos <- which(gadm36_NOR_2_sf$NAME_2 %in% municipalities_related)
  # #Pull out those geometries:
  # new_geometry <- gadm36_NOR_2_sf$geometry[index_municgeos]
  # #Then merge these multipolygons into one geometry value:
  # new_geometry_merged <- st_union(new_geometry[1:length(new_geometry)])
  # ##plot to inspect
  # #plot(new_geometry)
  # #plot(new_geometry_merged)
  # #Finally, transfer values to treateat_geo dataset, for the specific unit indexed bt index_unit:
  # treateat_geo$geometry[index_unit] <- new_geometry_merged
  # #And remember to set CRS:
  # st_crs(treateat_geo) <- "+proj=longlat +datum=WGS84 +no_defs"

#II - map data
  #read gadm which has been downloaded and saved locally:
  # gadm36_NOR_0_sp <- readRDS("PATH.../gadm36_NOR/gadm36_NOR_0_sp.rds")
  # gadm36_NOR_1_sp <- readRDS("PATH.../gadm36_NOR/gadm36_NOR_1_sp.rds")
  # gadm36_NOR_2_sp <- readRDS("PATH.../gadm36_NOR/gadm36_NOR_2_sp.rds")
  #
  # gadm36_NOR_0_sf <- readRDS("PATH.../gadm36_NOR/gadm36_NOR_0_sf.rds")
    gadm36_NOR_1_sf <- readRDS("LOCAL PATH TO DATA.../gadm36_NOR/gadm36_NOR_1_sf.rds")
    #TODO: update path above when running script


    #transform - according to geocompr book we need to tranform polygon before simplifying:
    gadm36_NOR_1_sf_proj <-st_transform(gadm36_NOR_1_sf, 32631 )#find codes here: https://epsg.io/?q=Norway%20kind:PROJCRS&page=8
    #simplify:
    gadm36_NOR_1_sf_simp <- st_simplify(gadm36_NOR_1_sf_proj, dTolerance = 1000)
    # gadm36_NOR_2_sf <- readRDS("PATH.../gadm36_NOR/gadm36_NOR_2_sf.rds")
    #transform back:
    gadm36_NOR_1_sf_simp <- st_transform( gadm36_NOR_1_sf_simp, "+proj=longlat +datum=WGS84 +no_defs")

#III
#finally save both treatment unit data and gadm data internal to the package
usethis::use_data(attributedata_geo,
                  counties2020_simp,
                  munic2020_simp,
                  #gadm36_NOR_0_sp,gadm36_NOR_1_sp,gadm36_NOR_2_sp,
                  #gadm36_NOR_0_sf,               gadm36_NOR_2_sf,
                  gadm36_NOR_1_sf,
                  gadm36_NOR_1_sf_simp,
                  internal = TRUE, overwrite = TRUE)
