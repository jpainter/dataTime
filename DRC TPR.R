# DQA STEPs

## Libraries ####
library(pacman)
p_load( officer , rvg ,tidyverse, scales, 
        tidyfast, tidytable, progress ,
        lubridate , 
        readxl, patchwork, 
        tsibble, fable, fabletools, feasts, 
        slider, anomalize, brolgar ,
        tsbox , CausalImpact , tibbletime , dygraphs , 
        fpp3  , fabletools, 
        furrr, tictoc, magrittr, hrbrthemes, data.tree, igraph ,
        sf, mapview, GGally , plotly , sugrrants ,
        data.tree, igraph
        )

## Functions ####
source( "ingest_formula_data.r")
source( "formula_files.r" )
source( "clean_ts.r" )
source( "outlier.r" )

## directory ####
dir = '../dataDictionary/dhis2_dictionary/Formulas/DRC/'

## Formulas ####
formulas = formula_files( dir = dir )
formulas

## TPR ####
f = paste0( dir ,  formulas[1] )

tic()
data = read_excel( f , 'formulaData' , guess_max = 21474836 ) 
toc()

d = d %>%
  mutate(  period = zoo::as.yearmon( period, "%Y%m") %>%
             yearmonth( . ) 
  ) %>%  
  unite( 'de' , dataElement, Categories , remove = TRUE ) %>%
  select( - dataElement.id , - categoryOptionCombo.ids , -COUNT ) %>%
  mutate( SUM = as.numeric( SUM ) )


tpr = ingest_formula_data( filename = f , 
                           leaf = FALSE ,
                           summary = FALSE , # Include summary data
                           total.name = 'Total' , 
                           pivotWider = TRUE )


tpr = data %>% 
    complete( de, period , 
              nesting( leaf, level, levelName, orgUnitName, orgUnit ) , 
              fill = list( SUM = 0 , COUNT = 0 ) ) %>%
    arrange( level , levelName, orgUnitName, orgUnit, de , desc( period )   )

  
glimpse( tpr )

## TS ####

skipCols = c('level' , 'Count.Any' , 'Count.Complete' )

dataCols = setdiff( names( tpr )[ map_lgl( tpr, is.numeric ) ]  ,  skipCols ) 

data_to_time_series = function( d , 
                                dataCols , 
                                skipCols  ){
    dts = d %>% 
    
        select( 'orgUnit' , 'period' , all_of(dataCols) ) %>%
        
        
        as_tsibble( index =  period , 
                            key = orgUnit
                            ) 
        
    if ( sum( has_gaps( dts )$.gaps ) > 0 )  dts = fill_gaps( dts )
        
    dts = dts %>% 
        pivot_longer( {{ dataCols }} ,
                      values_to = 'original' ,
                      names_to = 'vars' )
 return( dts )
}
    
tpr.dts = data_to_time_series( d = tpr , dataCols , skipCols )

glimpse( tpr.dts )

maxInterval = tpr.dts %>% as_tibble %>% group_by(orgUnit, vars) %>% 
    summarise( n = n()) %>% pull(n) %>% max

## Visualize facility-months ####
dTs %>% 
    as_tibble %>% 
    group_by( orgUnit , vars ) %>%
    summarise( Months_with_Data = sum(!is.na( original )) ) %>% 
    group_by( orgUnit ) %>%
    summarise( Months_with_Data = max( Months_with_Data )  ) %>% 
    ggplot() + 
    geom_histogram( aes( x = Months_with_Data ) , binwidth = 1 )

dTs %>% 
    group_by( vars ) %>%
    summarise( orgUnnits_with_Data = sum(!is.na( original )) ) %>% 
    summarise( orgUnnits_with_Data = max( orgUnnits_with_Data )  ) %>% 
    ggplot() + 
    geom_line( aes( y = orgUnnits_with_Data , x = period  )  )

dTs %>% 
    group_by( vars ) %>%
    summarise( orgUnnits_with_Data = sum(!is.na( original )) ) %>% 
    ggplot() + 
    geom_line( aes( y = orgUnnits_with_Data , x = period  , color = vars )  ) 


