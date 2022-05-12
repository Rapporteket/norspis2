#' make_table_popchar
#'
#' Function for table with overview of patient characteristics.
#'
#' The name of this function when maing aarsrapport 2019, in 2020,  was make_popCharTab().
#'
#' @param RegData
#' @param varsInTab
#' @param varsNames
#' @param comparison
#' @param dont_visualize_nei
#' @param dont_visualize_mangler
#' @param mergevalues_2nd_column
#' @param row_height_ft
#' @param font_size_ft
#'
#' @return
#' @export
#'
#' @examples

make_table_popchar <- function(RegData = myFilteredData,
                               varsInTab = myvarString,
                               varsNames = myvarNames,
                               #comparison:
                               comparison = FALSE,
                               #exclude:
                               dont_visualize_nei = FALSE,
                               dont_visualize_mangler = FALSE,
                               mergevalues_2nd_column = TRUE,
                               #format ft
                               row_height_ft= 0.25,
                               font_size_ft=8,
                               lollipop_max_ft = 10,
                               lollipop_min_ft = -10




                               ) {

  output_tibble <- tibble() #just making an empty tibble where output of the "for loop" can go

  #Prepare three data sets/tables that go into final table:
  #1. Full data (e.g. 2017-2021)
  #2. Comparison1 (e.g. 2017-2020)
  #3. Comparison2 (e.g. 2021)

  #1. Full data (e.g. 2017-2021)
  for(myvar in varsInTab) {

  #This makes a small summary table with counts for each category of the variable
  summarytable_variable <- RegData %>%
    group_by(!!myvar)%>% #we group by this variable (i.e. the variable's categories) to get counts within each category(below, by summarize())
    summarize(counts = sum(!is.na(PasientID))) %>%#summarizes with counts for each category. Here we use Pasient.ID to count/represent each patient
    mutate(counts2 = case_when(!is.na(!!myvar) ~ counts))%>% #sum N, except NAs
    mutate(perc = counts/(sum(counts))*100) %>%
    mutate(perc2 = counts2/(sum(counts2, na.rm = T))*100) %>% #perc2 is the percantages where missing are not brought into calculations
    #mutate(perc2 := counts/(sum(counts)-.[is.na(!!myvar) , "counts"][[1]]) * 100) %>% #"[is.na(!!myvar) , "counts"][[1]]" extracts the cell value
    #mutate(perc2 := case_when(.[is.na(!!myvar),][[1]] ~ .[is.na(!!myvar) , "perc"][[1]],
    #                          TRUE ~ perc2))%>%
    mutate(perc2 = case_when(is.na(perc2) ~ perc,
                             TRUE ~ perc2)) %>%
    mutate(countsum = paste0("(N = ", sum(counts2, na.rm = T), ")")) %>% #sums the valid observations (i.e. does not include NAs)
    #Rounding
    mutate(perc =round(perc,1),
           perc2 =round(perc2,1))%>% #just rounding perc to one decimal
    #Add variable name as new  column or row
    mutate(myvarname := colnames(.[,1])) %>% #variable name added as a column
    #add_row(!!myvar:=colnames(.[,1]), .before = 1) %>% #adds a new row with variable name as value in first cell, and second cell empty, this is to prepare for graphical/table presentation later
    ## If you add it as a row you must bake NA values blank (togehter with converting them to characters)
    #mutate(counts = case_when(is.na(counts) ~ as.character(" "), #make NAs blank in counts column
    #                           TRUE ~ as.character(counts))) %>%
    # mutate(perc2 = case_when(is.na(perc2) ~ as.character(" "), #make NAs blank in perc2 column
    #                           TRUE ~ as.character(perc2))) %>%

    ##Changing names:
    mutate(!!myvar := forcats::fct_explicit_na(!!myvar, "Mangler"))%>%
    #make column with variable categories character (from ordered factor) to be able to merge with other variables, else error will appear
    mutate(!!myvar :=as.character(!!myvar))%>%#make column with variable categories character (from ordered factor) to be able to merge with other variables, else error will appear
    #mutate(!!myvar := case_when(is.na(!!myvar) ~ "Mangler", #This worked when myvar var character, but failed for factor
    #                            TRUE ~ !!myvar)) %>% #in the first column, named "!!myvar", we give NA the value "Mangler"
    #First column...
    rename(" " := !!myvar) %>% #renames the first column to " " empty(before I used the name"categories")
    rename(N = counts) %>%  #rename counts2 column
    rename("%" = perc2) %>%
    select(., -counts2) %>%  #remove the counts2 abnd the perc column, don't need them anymore
    select(., -perc) %>%
    relocate(myvarname, countsum, .before=1) %>%#moving myvarname and countsum column first %>%
    rename("  " = myvarname) %>% #rename column to empty (NOTE: double spaces used here because empty with one space is already used)
    mutate("   " = "") %>%  #make new ampty columnt that we can use for, for instance, a  minibar (flextable::minibar())
    rename("    " = countsum)
  #Fix format

  #Here we just merge the summary tables made above (one table/variable is added for each new running of the loop)
  output_tibble <- dplyr::bind_rows(output_tibble,
                                    summarytable_variable)



  }



  if(comparison == TRUE){

    #2. Comparison1 (e.g. 2017-2020)


    #first filter the data that will be comparison
    RegDataComparison1 <-
      RegData %>%
      filter(Year>= 2000 & Year <=2020)


    output_tibble_comparison1 <- tibble() #just making an empty tibble where output of the "for loop" can go

    for(myvar in varsInTab) {

      #This makes a small summary table with counts for each category of the variable
      summarytable_variable_comparison <- RegDataComparison1 %>%
        group_by(!!myvar)%>% #we group by this variable (i.e. the variable's categories) to get counts within each category(below, by summarize())
        summarize(counts = sum(!is.na(PasientID))) %>%#summarizes with counts for each category. Here we use Pasient.ID to count/represent each patient
        mutate(counts2 = case_when(!is.na(!!myvar) ~ counts))%>% #sum N, except NAs
        mutate(perc = counts/(sum(counts))*100) %>%
        mutate(perc2 = counts2/(sum(counts2, na.rm = T))*100) %>% #perc2 is the percantages where missing are not brought into calculations
        #mutate(perc2 := counts/(sum(counts)-.[is.na(!!myvar) , "counts"][[1]]) * 100) %>% #"[is.na(!!myvar) , "counts"][[1]]" extracts the cell value
        #mutate(perc2 := case_when(.[is.na(!!myvar),][[1]] ~ .[is.na(!!myvar) , "perc"][[1]],
        #                          TRUE ~ perc2))%>%
        mutate(perc2 = case_when(is.na(perc2) ~ perc,
                                 TRUE ~ perc2)) %>%
        mutate(countsum = paste0("(N = ", sum(counts2, na.rm = T), ")")) %>% #sums the valid observations (i.e. does not include NAs)
        #Rounding
        mutate(perc =round(perc,1),
               perc2 =round(perc2,1))%>% #just rounding perc to one decimal
        #Add variable name as new  column or row
        mutate(myvarname := colnames(.[,1])) %>% #variable name added as a column
        #add_row(!!myvar:=colnames(.[,1]), .before = 1) %>% #adds a new row with variable name as value in first cell, and second cell empty, this is to prepare for graphical/table presentation later
        ## If you add it as a row you must bake NA values blank (togehter with converting them to characters)
        #mutate(counts = case_when(is.na(counts) ~ as.character(" "), #make NAs blank in counts column
        #                           TRUE ~ as.character(counts))) %>%
        # mutate(perc2 = case_when(is.na(perc2) ~ as.character(" "), #make NAs blank in perc2 column
        #                           TRUE ~ as.character(perc2))) %>%

        ##Changing names:
        mutate(!!myvar := forcats::fct_explicit_na(!!myvar, "Mangler"))%>%
        #make column with variable categories character (from ordered factor) to be able to merge with other variables, else error will appear
        mutate(!!myvar :=as.character(!!myvar))%>%#make column with variable categories character (from ordered factor) to be able to merge with other variables, else error will appear
        #mutate(!!myvar := case_when(is.na(!!myvar) ~ "Mangler", #This worked when myvar var character, but failed for factor
        #                            TRUE ~ !!myvar)) %>% #in the first column, named "!!myvar", we give NA the value "Mangler"
        #First column...
        rename(" " := !!myvar) %>% #renames the first column to " " empty(before I used the name"categories")
        rename(N = counts) %>%  #rename counts2 column
        rename("%" = perc2) %>%
        select(., -counts2) %>%  #remove the counts2 abnd the perc column, don't need them anymore
        select(., -perc) %>%
        relocate(myvarname, countsum, .before=1) %>%#moving myvarname and countsum column first %>%
        rename("  " = myvarname) %>% #rename column to empty (NOTE: double spaces used here because empty with one space is already used)
        mutate("   " = "") %>%  #make new ampty columnt that we can use for, for instance, a  minibar (flextable::minibar())
        rename("    " = countsum)
      #Fix format

      #Here we just merge the summary tables made above (one table/variable is added for each new running of the loop)
      output_tibble_comparison1 <- dplyr::bind_rows(output_tibble_comparison1,
                                                   summarytable_variable_comparison)
    }
    #merge the resulting table of loop with the  first table
    output_tibble_merged_comparison1 <-
      # output_tibble_comparison %>% full_join(output_tibble,
      #                                        by = intersect(colnames(output_tibble),
      #                                                       colnames(output_tibble_comparison)))
      full_join(output_tibble,
                output_tibble_comparison1,
                by=c("  ", " "),
                #add suffix only to merged data
                suffix = c("",".compare1"))%>%
      #compute differences:
      # mutate(
      #   #N.diff = N.compare-N,
      #   `%.diff` = `%.compare1`-`%` )%>%
      select(-c(7,10))





    #3. Comparison2 (2021)

    #first filter the data that will be comparison
    RegDataComparison2 <-
      RegData %>%
      filter(Year>= 2021 & Year <=2021)


    output_tibble_comparison2 <- tibble() #just making an empty tibble where output of the "for loop" can go

    for(myvar in varsInTab) {

    #This makes a small summary table with counts for each category of the variable
    summarytable_variable_comparison <- RegDataComparison2 %>%
      group_by(!!myvar)%>% #we group by this variable (i.e. the variable's categories) to get counts within each category(below, by summarize())
      summarize(counts = sum(!is.na(PasientID))) %>%#summarizes with counts for each category. Here we use Pasient.ID to count/represent each patient
      mutate(counts2 = case_when(!is.na(!!myvar) ~ counts))%>% #sum N, except NAs
      mutate(perc = counts/(sum(counts))*100) %>%
      mutate(perc2 = counts2/(sum(counts2, na.rm = T))*100) %>% #perc2 is the percantages where missing are not brought into calculations
      #mutate(perc2 := counts/(sum(counts)-.[is.na(!!myvar) , "counts"][[1]]) * 100) %>% #"[is.na(!!myvar) , "counts"][[1]]" extracts the cell value
      #mutate(perc2 := case_when(.[is.na(!!myvar),][[1]] ~ .[is.na(!!myvar) , "perc"][[1]],
      #                          TRUE ~ perc2))%>%
      mutate(perc2 = case_when(is.na(perc2) ~ perc,
                               TRUE ~ perc2)) %>%
      mutate(countsum = paste0("(N = ", sum(counts2, na.rm = T), ")")) %>% #sums the valid observations (i.e. does not include NAs)
      #Rounding
      mutate(perc =round(perc,1),
             perc2 =round(perc2,1))%>% #just rounding perc to one decimal
      #Add variable name as new  column or row
      mutate(myvarname := colnames(.[,1])) %>% #variable name added as a column
      #add_row(!!myvar:=colnames(.[,1]), .before = 1) %>% #adds a new row with variable name as value in first cell, and second cell empty, this is to prepare for graphical/table presentation later
      ## If you add it as a row you must bake NA values blank (togehter with converting them to characters)
      #mutate(counts = case_when(is.na(counts) ~ as.character(" "), #make NAs blank in counts column
      #                           TRUE ~ as.character(counts))) %>%
      # mutate(perc2 = case_when(is.na(perc2) ~ as.character(" "), #make NAs blank in perc2 column
      #                           TRUE ~ as.character(perc2))) %>%

      ##Changing names:
      mutate(!!myvar := forcats::fct_explicit_na(!!myvar, "Mangler"))%>%
      #make column with variable categories character (from ordered factor) to be able to merge with other variables, else error will appear
      mutate(!!myvar :=as.character(!!myvar))%>%#make column with variable categories character (from ordered factor) to be able to merge with other variables, else error will appear
      #mutate(!!myvar := case_when(is.na(!!myvar) ~ "Mangler", #This worked when myvar var character, but failed for factor
      #                            TRUE ~ !!myvar)) %>% #in the first column, named "!!myvar", we give NA the value "Mangler"
      #First column...
      rename(" " := !!myvar) %>% #renames the first column to " " empty(before I used the name"categories")
      rename(N = counts) %>%  #rename counts2 column
      rename("%" = perc2) %>%
      select(., -counts2) %>%  #remove the counts2 abnd the perc column, don't need them anymore
      select(., -perc) %>%
      relocate(myvarname, countsum, .before=1) %>%#moving myvarname and countsum column first %>%
      rename("  " = myvarname) %>% #rename column to empty (NOTE: double spaces used here because empty with one space is already used)
      mutate("   " = "") %>%  #make new ampty columnt that we can use for, for instance, a  minibar (flextable::minibar())
      rename("    " = countsum)
    #Fix format

    #Here we just merge the summary tables made above (one table/variable is added for each new running of the loop)
    output_tibble_comparison2 <-
      dplyr::bind_rows(output_tibble_comparison2,
                       summarytable_variable_comparison)
    }
    #merge the resulting table of loop with the  first table
    output_tibble_merged_comparison2 <-
      # output_tibble_comparison %>% full_join(output_tibble,
      #                                        by = intersect(colnames(output_tibble),
      #                                                       colnames(output_tibble_comparison)))
      full_join(output_tibble_merged_comparison1,
                output_tibble_comparison2,
                by=c("  ", " "),
                #add suffix only to merged data
                suffix = c("",".compare2"))%>%
      #compute differences:
      mutate(
              #N.diff = N.compare-N,
             `%.diff` = `%.compare2`-`%.compare1` )%>%
      select(-c(9,12))

  }

  if(comparison == FALSE){
    #just give the non-comparison same name as in "return()" at end of function:
    output_tibble_merged_comparison2 <- output_tibble_comparison1

  }



  #SOME EXTRA OPTIONAL EDITS TO OUTPUT:

  if(dont_visualize_nei == TRUE){
    #just remove "nei" answers from the correct column:
    output_tibble_merged_comparison2 <-
      output_tibble_merged_comparison2 %>%
      filter(` `!= "Nei")
  }

  if(dont_visualize_mangler == TRUE){
    #just remove "Mangler" from the correct column:
    output_tibble_merged_comparison2 <-
      output_tibble_merged_comparison2 %>%
      filter(` `!= "Mangler")
  }



  #CHANGE NAMES OF VARS

  output_tibble_merged_comparison2[,1] <- dplyr::recode(output_tibble_merged_comparison2[,1][[1]],
                                  !!!varsNames)#"unpacks" vector




  #FORMATTING AS FLEXTABLE

  #uniting first two columns
  to_ft <- output_tibble_merged_comparison2 %>%
    tidyr::unite("  ", 1:2, sep =" ") %>%
    #add empty column for lollipop
    mutate("    "= "")

  #Just printing a flextable of the output above
  ft <- flextable::flextable(to_ft)
  ft <- flextable::autofit(ft)
  # ft <- flextable::compose(ft, j =5, #minibar
  #                                  value = flextable::as_paragraph(
  #                                    flextable::minibar(value = N, max = max(N, na.rm = T),barcol = "grey")
  #                                  ),
  #                                  part = "body")
  ft <- flextable::compose(ft, j =5, #minibar
                           value = flextable::as_paragraph(
                             flextable::minibar(value = N, max = max(N, na.rm = T),barcol = "grey")#"lightblue")
                           ),
                           part = "body")
  ft <- flextable::compose(ft, j = 10, #lollipop
                           value = flextable::as_paragraph(
                             flextable::lollipop(value = `%.diff`,
                                                 max = lollipop_max_ft,
                                                 min = lollipop_min_ft)
                             #barcol = "lightblue")
                           ),
                           part = "body")


  ft <- flextable::merge_v(ft, j = 1) #vertical merge of values in first column

  if(mergevalues_2nd_column == TRUE){
  ft <- flextable::merge_v(ft, j = 2) #vertical merge of values in second column
  }

  ft <- flextable::valign (ft, j = 1:2, valign = "top") #align the merged values in first and seceond column to the top

  ft <- flextable::width(ft, j=c(3:4, 6:9), width= 0.5)
  ft <- flextable::width(ft, j=c(1), width= 0.75)
  ft <- flextable::width(ft, j=c(2), width= 1)
  ft <- flextable::width(ft, j=c(5), width= 1.1) #bar
  ft <- flextable::width(ft, j=c(10), width= 1.25)
  #ft <- flextable::width(ft, j=c(11), width= 2.5)
  ft <- flextable::height_all(ft, height = row_height_ft, part = "all")
  ft <- flextable::hrule(ft, rule = "exact")
  ft <- flextable::fontsize(ft, size = 8)

  # Borders and lines
  # add extra table header row on top:
  ft <- flextable::add_header(ft,
                              N = "Alle år",
                              `%` = "Alle år",
                              N.compare1 = "T.o.m. 2020",
                              `%.compare1` = "T.o.m. 2020",
                              N.compare2 = "2021",
                              `%.compare2` = "2021",
                              top = TRUE )
  # merge new headers rows horizontally when there are identical values
  ft <- flextable::merge_h(ft, part = "header")
  #lines/borders
  border_type <- officer::fp_border(color = "black", style = "solid", width = 1.25)

  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft, j = NULL,
                             border = border_type,
                             part = "header")
  ft <- flextable::hline(ft, i = 2,
                         border = border_type,
                         part = "header")
  ft <- flextable::hline_bottom(ft,
                                border = border_type,
                                part = "body")

  ft




  #RETURN:

  return(ft)
}
