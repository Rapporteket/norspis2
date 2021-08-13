#' Title
#'
#' Make data quality table with completeness of registrations (proportion of
#' start registrations that has delivered end registration("forlopskompletthet)).
#' The measure is based on expected number of delivered end registrations,
#' Expected number of delivered end registrations is calculated by use of
#' historical treatment times given by end registrations that actually have been
#' delivered.
#'
#' The method used is outlines in NorSpis' annual report for 2019 and 2020 at
#' published at www.kvalitetsregistre.no.
#'
#' Since we use "start.end" data, we identify each individual start
#' registration that has an end registration, when we make the counts.
#'
#' All registrations since start of NorSpis is included in the calculation of
#' treatment time and empirical cumulative distribution function.
#'
#' @param RegData
#' @param dateFrom
#' @param dateTo
#' @param timeseries
#'
#' @return
#' @export
#'
#' @examples

make_table_DQ_completeness_endreg <- function(RegDataStartEnd =
                                                DL$RegDataStartEnd2,
                                              userRole,
                                              reshID,
                                              dateFrom = "2012-01-01",
                                              dateTo="2021-12-31",
                                              timeseries=F






){





# library(lubridate)
# library(tibbletime)

#Treatment length, days:
times <- as.Date(RegDataStartEnd$HovedDato.y)-as.Date(RegDataStartEnd$HovedDato.x)

#Summary statistics:
#summary(as.numeric(times))
#
# density(x=as.numeric(as.Date(DL$RegDataStartEnd2$HovedDato.y)-as.Date(DL$RegDataStartEnd2$HovedDato.x)), na.rm = T)
#
# plot(density(x=as.numeric(as.Date(DL$RegDataStartEnd2$HovedDato.y)-as.Date(DL$RegDataStartEnd2$HovedDato.x)), na.rm = T))
#
# hist(x=as.numeric(as.Date(DL$RegDataStartEnd2$HovedDato.y)-as.Date(DL$RegDataStartEnd2$HovedDato.x)),
#      na.rm = T,
#      breaks = max(as.numeric(as.Date(DL$RegDataStartEnd2$HovedDato.y)-as.Date(DL$RegDataStartEnd2$HovedDato.x)), na.rm = T))
#
# hist(x=as.numeric(as.Date(DL$RegDataStartEnd2$HovedDato.y)-as.Date(DL$RegDataStartEnd2$HovedDato.x)),
#      na.rm = T)
#


# We use the ecdf function (tip on it here: https://stats.stackexchange.com/questions/209369/estimate-the-probability-of-the-distribution-of-a-sample)
# plot(ecdf(times))
time_cdf <- ecdf(times) #this becomes a function
##then we can use the time_cdf function to calculate probabilities:
##test for 365 days:
#time_cdf(365)

#use survival analysis
#summary(survival::survfit(survival::Surv()) )

#DELETE:
#filter data you want to calculate completness for:
# filtered_data <-
#   DL$RegDataStartEnd2 %>%
#   filter(HovedDato.x=)


#make a table with all information we need to make calculation
base_data <-
  RegDataStartEnd %>%
  filter(RegRegtype.x %in% c(3,4), #start reg.(end comes along as VARNAME.y)
         BasisRegStatus.x == 1) %>%
  select(HovedDato.x,
         HovedDato.y,
         BasisRegStatus.x,
         BasisRegStatus.y,
         AvdRESH.x,
         AvdRESH.y,
         AvdNavn.x,
         AvdNavn.y
         ) %>%
  #treatment lengths:
  mutate(treatment_lenght =
           as.numeric((as.Date(HovedDato.y)-as.Date(HovedDato.x))))%>%
  ##median treatment length(NOT USED)
    #mutate(treatment_lenght_median = (median(treatment_lenght, na.rm = T))) %>%
  #days since start
  mutate(days_since_start =
           as.numeric(as.Date(dateTo) - as.Date(HovedDato.x))) %>% #Note: We use
                                                                   #datoTo input
  #Probability of end should have been delivered:
  #Note: We calculate probability before we filter on date,
  #so that the treatment lengths used is for all data ever delivered to NorSpis.
  mutate(probability_should_be_delivered =
           time_cdf(days_since_start)) %>% #Note: Use the time_cdf function made
  #Then filter on period you want to calculate prob. of should been delivered:
  filter(HovedDato.x >= as.Date(dateFrom),
         HovedDato.y <= as.Date(dateTo) | is.na(HovedDato.y))

#make into a time tibble, which will make it easier to make time series:
base_data2 <-
  as_tbl_time(base_data, HovedDato.x) %>%
  mutate(HovedDato.x = as.Date(as.character(HovedDato.x))) %>% #format for time tibble manipulation
  arrange(HovedDato.x) %>%  #must be sorted to enable timetibble functionality
  #Make new date variable for year:
  #(see: https://cran.r-project.org/web/packages/tibbletime/vignettes/TT-04-use-with-dplyr.html)
  mutate(date_index_year = collapse_index(HovedDato.x,"yearly")) %>%
  relocate(date_index_year)
  #        %>%
  # group_by(HovedDato.x)


estimated_completeness <- base_data2 %>%
  #estimated completeness per treatment unit:
    #about if statement, see John Paul's comment here:
    #https://stackoverflow.com/questions/30604107/r-conditional-evaluation-when-using-the-pipe-operator
  {if(timeseries==T) group_by(., AvdNavn.x, AvdRESH.x,date_index_year)
    else group_by(., AvdNavn.x, AvdRESH.x)} %>%
  summarise(#Actual n. of start registrations, just to show this as well and use
            #to calculate "raw" completeness that do not take into account the
            #treatment length aspect:
            actual_n_start_reg = sum(BasisRegStatus.x, na.rm = T),
            #actaual n end reg:
            actual_n_end_reg = sum(BasisRegStatus.y, na.rm = T),
            #actual n end reg not delivered:
            actual_n_end_reg_notdelivered =
              actual_n_start_reg - actual_n_end_reg,
            expected_n_end_reg = sum(probability_should_be_delivered),
            expected_n_end_reg_notdelivered =
              expected_n_end_reg - actual_n_end_reg
            ) %>%
  mutate(completeness_end_reg_estimated = actual_n_end_reg/expected_n_end_reg,
         completeness_raw = actual_n_end_reg/actual_n_start_reg,
         completeness_diff_estimated_vs_raw =
           completeness_end_reg_estimated - completeness_raw) %>%
  relocate(completeness_raw, .after = actual_n_end_reg_notdelivered)



# %>%
#   rename("Startdato" = HovedDato.x,
#          "sluttdato" = HovedDato.y)


#2) Also make a separate table with FID of start registrations missing
#end registrations

estimated_completeness
#DT::datatable(estimated_completeness, options = list(pageLength = 50))

}
