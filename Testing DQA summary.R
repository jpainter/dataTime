country = 'Burkina Faso'
indicator = 'ANC'


## Libraries ####
library(pacman)
p_load( officer , rvg ,tidyverse, scales, 
        tidyfast, tidytable, progress ,
        lubridate , 
        readxl, patchwork, 
        tsibble, fable, fabletools, feasts, 
        slider, anomalize, brolgar ,
        tsbox , CausalImpact , tibbletime , dygraphs , 
        forecast, fpp3  , fabletools, 
        furrr, tictoc, magrittr, hrbrthemes, data.tree, igraph ,
        sf, mapview, GGally , plotly , sugrrants ,
        data.tree, igraph, progressr
        )

harmonic_mean = function( x ){
    1 / mean( 1/x, na.rm = TRUE )
}

# https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

# Sources and directories ----

source( 'Deviation_Expected_Functions.R')
source( 'formula_files.R')

dir = file.dir( country )

dir.files = list.files( dir )

# data file ----
 data_files = formula_data_files( dir = dir )
 f.indicator = grepl( indicator, data_files )
 data_file = paste0( dir, data_files[ f.indicator ] )[1]

 
# dTs.clean -----
dTs.clean = readRDS( paste0( dir , indicator , '_dTs_clean.rds' )  )

glimpse( dTs.clean )

dTs.clean %>% as_tibble() %>% 
  # filter( period > yearmonth( "Jan 1 2019" ) ) %>%
  group_by( vars ) %>%
  count( missing, extremeOut, seasonalOut  ) %>%
  mutate( n_percent = percent( n / sum(n)  , accuracy = .1 ) ) 

dTs.clean %>% as_tibble() %>% 
  # filter( period > yearmonth( "Jan 1 2019" ) ) %>%
  group_by( vars ) %>%
  count( missing, Interp  ) %>%
  mutate( n_percent = percent( n / sum(n)  , accuracy = .1 ) ) 

# Map ----

geo.files = dir.files[ grepl( 'geo' , dir.files ) & grepl( 'rds' , dir.files )] 
geo.file = geo.files[ rev(order( geo.files )) ][1]

sf = readRDS( paste0( dir, geo.file ))

# Table A. Reporting ----

reportingForms  = 
  read_xlsx( data_file  , 'Formula Elements' ) %>% 
  select( dataSet,  dataSet.id ) %>% 
  mutate_all( ~stringi::stri_split_fixed(.x , ";\r\r\n" ) ) %>%
  unnest( c(dataSet, dataSet.id) )

files.reporting = dir.files[ grepl( 'Nation' , dir.files) & grepl( 'Reporting' , dir.files) ]
file.reporting = files.reporting[ rev(order( files.reporting)) ][1]

reporting = readRDS( paste0( dir, file.reporting ) ) %>%
  filter( dataSet.id %in% reportingForms$dataSet.id ) %>%
  inner_join( reportingForms ,  by = c("dataSet.id", "dataSet") )

r.n = reporting %>%
  select( dataSet, period, reporting, value ) %>%
  unique() %>% # for some reasoan, one dataset is duplicated
  mutate( value = as.integer( value )) %>%
  pivot_wider( id_cols = c( dataSet , period) ,  names_from = reporting, 
               values_from = value ) 

r.n.annual.rates = r.n %>% 
  mutate(
    Year = year( yearmonth( period )) ) %>%
  select(-period) %>%
  group_by( dataSet, Year ) %>% 
  summarise_all(  sum ) %>%
  mutate(
    formReporting = ACTUAL_REPORTS / EXPECTED_REPORTS ) 
  
biggest_dataset = r.n.annual.rates %>% 
  arrange( EXPECTED_REPORTS ) %>% tail(1) %>% pull(dataSet)

r.n.annual.rates.combined = r.n %>% 
  mutate(
    Year = year( yearmonth( period )) ) %>%
  select(-period, -dataSet ) %>%
  group_by( Year ) %>%
  summarise_all(  ~sum(.x, na.rm = TRUE ) ) %>%
  mutate(
    formReporting = ACTUAL_REPORTS / EXPECTED_REPORTS ) 

form_reporting = 
  r.n.annual.rates.combined %>% 
  rename( formActual = ACTUAL_REPORTS ,
          formExpected = EXPECTED_REPORTS ) %>%
  select( Year, formActual, formExpected , formReporting )


dataReporting  = 
  read_xlsx( data_file  , 'summaryData' ) %>%
  filter( leaf ) %>%
  mutate( Year = year( yearmonth( period )) ) %>%
  group_by( orgUnit, Year) %>%
  summarize(
    allData = sum( Count.Complete ) ,
    someData = sum( Count.Any ) ,
    months = n()
    ) %>%
  group_by( Year ) %>%
  summarise(
       dataFacilities = n_distinct( orgUnit ) ,
       totalAllData = sum( allData ) ,
       totalSomeData = sum( someData ) ,
        n_months = max( months ) ,
        # meanAllData = sum( allData )   , 
        # meanSomeData = sum( someData )   , 
        # 
        # dataFacilities = sum( someData > 0 ) ,
        dataAllMonths= sum( someData == max( months ) ) ,
        dataNoMonths = sum( someData == 0 ) ,
        dataMeanMonths = mean( someData ) 
    ) %>%
  mutate( dataReporting = totalSomeData / (n_months *  dataFacilities ) )


reporting = inner_join( dataReporting , form_reporting , by = "Year") 

library( gt )
reporting_tab_rtf <- 
  reporting %>%
  gt(  ) %>%
  tab_spanner(label = "Forms", columns = matches("form")) %>%
  tab_spanner(label = "Data", columns = matches("data")) %>%
  cols_move_to_start( columns = vars(Year, formActual, formExpected, formReporting )) %>%

  fmt_number(columns = starts_with('form'), decimals = 0 ) %>%
  fmt_number(columns = starts_with('data') , decimals = 0 ) %>%
  fmt_number(columns = matches('mean') , decimals = 1 ) %>%
  fmt_percent(columns = ends_with( 'reporting' ), decimals = 1) %>%
  
  tab_header(
    title = md(indicator),
    subtitle = "Reporting rates for submitting form and for reporting data in the form"
  ) 
  
  gtsave( reporting_tab_rtf, 
          paste0( dir, "_" , indicator , "_reporting.png" ) , 
          vwidth = 8*200 )
  
# Table B. Error ----

Annual_Error_Rates  = fms_summary %>% # View
    mutate( MhAPE = ifelse( is.nan(MhAPE) | MhAPE %in% Inf , NA, MhAPE ) ) %>% 
    as_tibble() %>%
    group_by( Year ) %>%
    summarise( 
        nData = sum(n) ,
        total_raw = sum( total_raw ) , 
        nOutlier = sum( total_outlier != 0 ) ,
        total_outlier = sum( total_outlier ) , 
        nInterp = sum( total_interp!= 0  ) ,
        total_interp = sum( total_interp ) , 
        # mpe = mean( MhAPE , na.rm = TRUE ) ,
        # gmpe = gm_mean( MhAPE )  ,
        Percent_Error = gm_mean( MgAPE ) ,
        # Percent_Error = mean( MhAPE , na.rm = T )
    ) %>%
    mutate( 
        `percOutlier` = total_outlier / total_raw  ,
        `percInterp.` = total_interp / total_raw ,
        `Estimated` = total_raw - total_outlier + total_interp ,
        `Est./Reported` = Estimated / total_raw
    ) %>%
  rename( Reported = total_raw )

tab_rtf <- 
  Annual_Error_Rates %>%
  gt(  ) %>%
  tab_spanner(label = "Reported", columns = matches("report")) %>%
  tab_spanner(label = "Consistency", columns = matches("error")) %>%
  tab_spanner(label = "Missingness", columns = matches("inter")) %>%
  tab_spanner(label = "Outliers", columns = matches("outlier")) %>%
  tab_spanner(label = "Estimated Total", columns = vars(
    Estimated,`Est./Reported` )) %>%

  fmt_number(columns = matches(paste(c('total','Reported', 'Estimated','n') ,
                                     collapse="|") ) ,
             decimals = 0 ) %>%
  fmt_number(columns = contains('Est.'), decimals = 2 ) %>%
  fmt_number(columns = c(ends_with( 'e' ), starts_with('avg')) , decimals = 1 ) %>%
  fmt_percent(columns = starts_with( 'perc' ), decimals = 1) %>%
  tab_header(
    title = md(formula),
    subtitle = "Outliers, Missingness, and Annual Error Rates"
  ) %>%
  tab_source_note(md( "Results are averaged across all facilities reporting.")) 
  
  
  gtsave( tab_rtf, "table.png", vwidth = 8*200 )
