
# Libraries ####
library(pacman)

p_load( officer , rvg ,tidyverse, scales, tidyfast, tidytable, 
        lubridate , anytime, progressr , assertthat ,
        readxl, patchwork, cowplot, kableExtra ,
        tsibble, fable, fabletools, feasts, 
        fable.prophet ,  fable.bsts , 
        slider, anomalize, brolgar ,
        tsbox , CausalImpact , tibbletime , dygraphs , 
        fpp3  , fabletools, 
        furrr, tictoc, magrittr, hrbrthemes, data.tree, igraph ,
        sf, mapview, GGally , plotly , sugrrants
        )
# Functions ####

source( '../dataDictionary/TS_Modeling_Functions.r')
# source("Model_TS.R") # new version of model.ts()
source("Summary_TS.R") # new version of summary_ts()
source('TS_model_outlier_impute.R') # model_ts()
source( 'Deviation_Expected_Functions.R')
source( 'theme_ppt.R')
source( 'clean_ts.r' )
source( 'model_ts2.r' )

Month_Year = function( x ){ yearmonth( zoo::as.yearmon( x , "%Y%m") ) }

options(dplyr.summarise.inform = FALSE)

output.folder =  paste0( params$country, "/" , params$evaluation )

dir.create( file.path( getwd(), 
                      output.folder
                       ) , recursive = TRUE
)

files = function(  search = 'All' , type = 'xlsx' , other = "" , ... ){
                        
                      dir = file.dir( ... )
                      dir.files = list.files( dir )
                      search_and_type =  
                          str_detect( dir.files, fixed( search , ignore_case=TRUE )  )  &
                           grepl( paste0( type , '$' ) , dir.files, 
                                  ignore.case =  TRUE  ) &
                          grepl( other , dir.files , ignore.case = TRUE ) 
                      files = dir.files[  search_and_type   ]
                      return( files[ rev( order( files )) ]  )
                      }

file.dir = function( country = params$country ,
                     dir.base = '../dataDictionary/dhis2_dictionary/Formulas/' ){
  paste0( dir.base , country , "/")
}

string_wrap = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}

string_wrap = Vectorize(string_wrap)


stderr <- function(x, na.rm=TRUE ) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

# Harmonic and geometric mean functions 

  harmonic_mean = function( x ){
    x = ifelse( is.nan(x) | x %in% Inf , NA, x )
    1 / mean( 1/x, na.rm = TRUE )
  }
 
  sdhm <- function(x) sqrt((mean(1/x, na.rm = TRUE ))^(-4)*var(1/x, na.rm = TRUE)/length(x) )
 
# https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
  
gm_sd = function(x, na.rm = FALSE, ...)
  { exp(sd(log(x, ...), na.rm = na.rm, ...)) }

most_recent_file = function( file_list_with_date ){
  rdsFileSplit = str_split( file_list_with_date, "_")
  download_date = map( rdsFileSplit ,
                       ~str_split( .x[ length(.x)] , "\\.")[[1]]
  ) %>% map_chr(1) 
  
  dates  = map_chr( download_date , ~ anydate(.x)  )
  
  if ( identical( dates , character(0) ) ) return( NA )
  
  # most recent file 
  file = file_list_with_date[ which( dates == max(dates, na.rm = T ) ) ]
  
  return ( file )
}



extremely_mad = function( x , 
                          deviation = 15, 
                          smallThreshold = 50 , 
                          maximum_allowed = NULL , 
                          logical = FALSE ,
                          .pb = NULL
                           ){
  if (!is.null( .pb ) ){
      if ( 'progressor' %in% class(.pb) ){ .pb() } else { .pb$tick() }
  } 
  
  if ( (all( x <= smallThreshold | is.na( x ) ) ) ) {
      if ( logical ) return( ifelse( !is.na( x ), TRUE , NA ) ) 
      return( x )    
  }
  
  # Remove any value above maximum_allowed 
  if ( !is.null( maximum_allowed )){
    over_max = which( x > maximum_allowed )
    x[ over_max ] = NA 
  }
  
  
  
      # pre clean extreme values
      medianVal = median( x , na.rm = TRUE )
      medianAbsDeviation = mad( x , na.rm = TRUE )
      # se <- function(x) sqrt( var(x , na.rm = TRUE )/ length( !is.na(x) ))
      # sdVal = sd( x , na.rm = TRUE )
      
      extreme = x > ( medianVal + deviation * medianAbsDeviation ) |
          x < ( medianVal - deviation * medianAbsDeviation )
      if ( logical ) return( !extreme ) # FALSE = outlier
      x[ extreme ] = NA 
      return( x )
}

unseasonal = function( x ,  
                       smallThreshold = 100 , 
                       deviation = 3 ,
                       logical = FALSE ,
                       interpolate = FALSE , # only useful when logical is FALSE
                       .lambda = 1 , 
                       .pb = NULL ){
  if (!is.null( .pb ) ){
      if ( 'progressor' %in% class(.pb) ){ .pb() } else { .pb$tick() }
  } 

    value = ifelse( !is.na(x), TRUE , NA ) 
    
    if (( sum( !is.na( x ) ) <= 1  ) | (all( x <= smallThreshold | is.na( x ) ) )){
      if ( logical ) return( value ) 
      return( x )
    } 
                 
      x.ts = x %>% as.ts( . , frequency = 12 ) 
        
        x.forecast  = 
                
            forecast::tsclean( x.ts ,
                               replace.missing = TRUE ,
                               lambda = .lambda ) %>%
                as.integer()
            
        MAD = mad( x , na.rm = TRUE )
        # standard_dev = sd( x, na.rm = TRUE )
            
        outlier = abs( ( x.forecast - x.ts ) / MAD ) >= deviation
            
        x.ts[ which( outlier ) ] =  NA
        value[ outlier ] =  FALSE 
      
      if ( logical ) return( value ) # FALSE = outlier
      return( x.ts )
}

# Data files ####
country = 'Guinea'

formula.dir = '../dataDictionary/dhis2_dictionary/Formulas/' 
dir = paste0( formula.dir , country , "/" )
dir.files = list.files( dir )
all.levels.data.files = dir.files[ grepl( 'All levels' , dir.files) & !grepl( 'rda|rds' , dir.files) ] # lookup all xls files

data.list  = tibble( 
  file = all.levels.data.files , 
  cat = map_chr( all.levels.data.files,  ~ str_split( .x , "All levels")[[1]][1] ) ,
  date.part = map_chr( all.levels.data.files,  ~ str_split( .x , "All levels")[[1]][2] ) )


data.files = data.list %>%
  group_by( cat ) %>%
  summarise( date.part = most_recent_file( date.part ) ) %>%
  inner_join( data.list , by = c("cat", "date.part") ) %>%
  pull( file , cat )


# Parallel modeling  ####
plan( multisession )

# MAD outliers ####
for ( i in seq_along( data.files ) ){ #1:length( data.files ) ){ #:length( data.files ) 
  
  file = data.files[ i ]
  cat( paste( '\n\n\nopening' , i , ":" , file, '\n') )
  
  # if ( file.exists( saved.file.name ) ){
  #   cat( 'data previously prepared' )
  #   next
  # }
  
  # load dataset  (df)
  tic()
  df = read_excel( paste0( dir , file ) , 
                   sheet = 'formulaData' , 
                   guess_max = 21474836 ) 
  toc() 
  cat( paste( 'finished reading data from excel file' ) )


  # prepare dataset as time-series ( df.ts )
  tic()
  df.ts = df_pre_ts( df %>% distinct  ) # use distinct because downloads create duplicates
  if ( 'raw' %in% colnames(df.ts) ) df.ts = rename( df.ts, original = raw ) 
  if ( !'Month' %in% colnames(df.ts) ) df.ts = mutate( df.ts , Month = Month_Year( period ))
  if ( ! 'original' %in% colnames(df.ts) ) df.ts = rename( df.ts , original = SUM ) 
  df.ts = mutate( df.ts , value = !is.na( original ))
  toc() ; cat( 'df.ts completed '  )  
  
  # glimpse( df.ts )
  # prepare data nested by orgUnit (ts.nest)
  orgUnit.details = df.ts %>%
    dplyr::select( -data, -dataElement.id, -categoryOptionCombo.ids ,
            -Month, -period , 
            -COUNT, -original ,-value, ) %>%
    distinct()
  
  keyCols = c( 'orgUnit' , 'data' )
  dataCols = unique( df.ts$data )
  
  tic()
  dTs = df.ts %>% 
    as_tsibble( index =  Month, 
                        key = {{ keyCols }} 
                        )  
    # index_by( year_month = yearmonth( month ) ) %>%

   if ( sum( has_gaps( dTs )$.gaps ) > 0 )  dTs = fill_gaps( dTs)
   cat( 'df.ts converted to tsibble ' ) ; toc()   
   

  .total = length( key_size( dTs ) )

  pb <- progress_bar$new( 
    format = ":current :percent  [:bar] :elapsedfull",
    total = .total, clear = FALSE, width= 50 )

  .threshold = 50
  # for stockout days: max = 31 
  
  
  dTs = dTs %>% 
    # filter( orgUnit %in% unique( dTs$orgUnit )[] ,
    #         data %in% unique( dTs$data )[] ) %>%
    
    # for stockout days: max = 31 
    mutate( 
      .max = ifelse( 
        grepl("jour|day", .$data ) &
        grepl("out|rupture", .$data )   &
        effectiveLeaf
        , 31, NA  )  
      )  %>%
    group_by( orgUnit, data ) %>%
    mutate( 

      mad15 = extremely_mad( original , 
                             deviation = 15 , 
                             smallThreshold = .threshold ,
                             maximum_allowed = .max , 
                             logical = TRUE, .pb = pb ) 
      , mad10 = extremely_mad( ifelse( mad15, original , NA ), 
                               deviation = 10 , 
                               smallThreshold = .threshold ,
                               maximum_allowed = .max , 
                               logical = TRUE ) 
      , mad5 = extremely_mad( ifelse( mad10, original , NA ), 
                              deviation = 5 , 
                              smallThreshold = .threshold * 2 ,
                              maximum_allowed = .max , 
                              logical = TRUE ) 
  )


  saved.file.name = paste0( dir, str_replace( file , ".xlsx" , "") , ".dTs.rds" ) 

  saveRDS( dTs , saved.file.name )

}

# Seasonal outliers ####
for ( i in seq_along( data.files ) ){ #1:length( data.files ) ){ #:length( data.files ) 
  
  file = data.files[ i ]
  saved.file.name = paste0( dir, str_replace( file , ".xlsx" , "") , ".dTs.rds" ) 
  cat( paste( '\n\n\nopening' , i , ":" , saved.file.name, '\n') )
  dTs = readRDS( saved.file.name )

  .total = length( key_size( dTs ) )

  cat( 'Seasonal 5')
  pb <- progress_bar$new( 
    format = ":current :percent  [:bar] :elapsedfull",
    total = .total, clear = FALSE, width= 50 )

  dTs = dTs %>% 
    # filter( orgUnit %in% unique( dTs$orgUnit )[] ,
    #         data %in% unique( dTs$data )[] ) %>%
    group_by( orgUnit, data ) %>%
    mutate(
      
      seasonal5 = unseasonal(  ifelse( mad10, original , NA) , 
                              smallThreshold = .threshold * 2  , 
                              deviation = 5 ,
                              logical = TRUE , .pb = pb ) 
  )

  cat( 'Seasonal 3')
  pb <- progress_bar$new( 
    format = ":current :percent  [:bar] :elapsedfull",
    total = .total, clear = FALSE, width= 50 )


  dTs = dTs %>% 
    # filter( orgUnit %in% unique( dTs$orgUnit )[] ,
    #         data %in% unique( dTs$data )[] ) %>%
    group_by( orgUnit, data ) %>%
    mutate(

      seasonal3 = unseasonal(  ifelse( seasonal5, original , NA) , 
                              smallThreshold = .threshold * 2 , 
                              deviation = 3 ,
                              logical = TRUE , .pb = pb )
  )
  #  dTs %>% as_tibble %>% count( value, mad15, mad10, mad5, seasonal )
  
  saved.file.name =  paste0( dir , 
                             "Seasonal_" , 
                             str_replace( file , ".xlsx" , "") , 
                             ".dTs.rds" ) 
  
  saveRDS( dTs , saved.file.name )
  
}

# Review counts

for ( i in seq_along( data.files ) ){ #1:length( data.files ) ){ #:length( data.files ) 
  
  file = data.files[ i ]
  # saved.file.name = paste0( dir, 
  #                           # "Seasonal_MAD10." , 
  #                           str_replace( file , ".xlsx" , "") , ".dTs.rds" ) 
  # cat( paste( '\n\n\nopening' , i , ":" , saved.file.name, '\n') )
  # dTs = readRDS( saved.file.name )
  # 
  # dTs %>% as_tibble %>% count( value, mad15, mad10, mad5, seasonal3, seasonal5 ) %>% print
  # 
  saved.file.name = paste0( dir, 
                            "Seasonal_" , 
                            str_replace( file , ".xlsx" , "") , ".dTs.rds" ) 
  cat( paste( '\n\n\nopening' , i , ":" , saved.file.name, '\n') )
  dTs = readRDS( saved.file.name )
  
  dTs %>% as_tibble %>% count( mad10, mad5, seasonal5 , seasonal3 ) %>% 
    print
  
}

## 7.b  Visualize cleaning  ####
flag = unique( as_tibble(dTs) %>% filter( mad5, !seasonal ) %>% 
                 select( orgUnit, data ) )

y=dTs %>% as_tibble() %>%
  semi_join( flag[450,] , by = c("orgUnit", "data") ) 
x= ifelse( y$mad10, y$original, NA )

y %>%
  ggplot( aes( x = Month, y = original,  group = data ) ) +
  geom_line( alpha = .25 , aes( linetype = data ) ) +
  geom_point( aes( color = mad5 , shape = seasonal )) +
  labs( title = paste( unique(y$orgUnitName), collapse = ","))
  
# examples. Burkina Faso  
# i=17.  filter( !mad5, seasonal )
# flag 10 - is mad, not seasonal (correct) 
# flag 100 - is mad (correct?), not seasonal
# filter( mad5, !seasonal )
# flag 100, one that is mad5 and seasonal, one only seasonal
# flag 200 and 300, seasonal false but all ok by MAD 5 
# flag 100, one that is mad5 and seasonal, one only seasonal



# Consistency ####
  
for ( i in seq_along( data.files ) ){ #1:length( data.files ) ){ #:length( data.files ) 
  
  file = data.files[ i ]
  saved.file.name =  paste0( dir , 
                             "Seasonal_" , 
                             str_replace( file , ".xlsx" , "") , 
                             ".dTs.rds" ) 
  cat( paste( '\n\n\nopening' , i , ":" , saved.file.name, '\n') )
  dTs = readRDS( saved.file.name )

  .total = length( key_size( dTs ) )


  
  # select chunks of ous so that each chunk has an ous with some data.  
  # if error with 10, try 20
  # chunks <- function(x,n) split(x, ceiling(seq_along(x)/n))
  # chunk_ous = chunks( 1:length(ous), 60 )
  tic()
  
  cat( 'Consistency...')
  pb <- progress_bar$new( 
    format = ":current :percent  [:bar] :elapsedfull",
    total = .total, clear = FALSE, width= 50 )
  
  dTs = dTs %>% 
    filter( orgUnit %in% unique( dTs$orgUnit )[1] ,
            data %in% unique( dTs$data )[1] ) %>%
    group_by( orgUnit, data ) %>%
    mutate(

      cleaned = ifelse( seasonal3, original , NA) ,
      
      arima = clean_ts( . , 
                                   tsibble_var = cleaned , 
                                   .lambda = .5 , 
                                   .clean = 'arima' , #MAD, arima
                                   interpolate = TRUE ,
                                   predict = FALSE , 
                                   pred_data = 6 , 
                                   arima.list = TRUE , # if arima, returns fit, resid, and interp
                                   MAD = 5 , 
                                   ignoreSmall = TRUE , 
                                   smallThreshold = 50 ,
                                   .pb = NULL ,
                                   timing = FALSE )
    )
  
  # fm = future_map( chunk_ous ,
  #                    ~model_ts( ts =  ts.nest  , 
  #                               ou = ous[.x] , 
  #                               pb = NULL ) , 
  #     .progress = TRUE ) 
  #   fms = reduce( fm , bind_rows )
  toc() 
  # glimpse(fms)

# add summary ( fms_summary )

  tic()
  fm_s = future_map( chunk_ous , 
                     ~summary_ts( dts = fms[.x , ] ) , 
                     .progress = TRUE )  
  fms_summary = fm_s %>% reduce( ., bind_rows )
  toc()
  cat( paste( 'fms_summary completed ' ) )

  # save( hosp.nest , fms , fms_summary , file = saved.file.name )
  
  saved.file.name =  paste0( dir , 
                             "Consistency_" , 
                             str_replace( file , ".xlsx" , "") , 
                             ".dTs.rds" ) 
  
  saveRDS( dTs , saved.file.name )
    
}

# View(fms)
# ggplot( fms_summary %>% filter( SDhAPE > 0 )) + 
#   geom_histogram( aes( MAPE ), binwidth = .1) + 
#   scale_x_continuous(limits = c(0,2)) +
#   facet_grid( ~data )
# 
# ggplot( fms_summary %>% filter( SDhAPE > 0 ) ) + 
#   geom_histogram( aes( MgAPE ), binwidth = .1) + 
#   # scale_x_continuous(limits = c(0,2)) +
#   facet_grid( Year~data , scales = 'free')
# 
# ggplot( fms_summary %>% filter( SDhAPE > 0 ) ) + 
#   geom_point( aes( x = MgAPE , y = total_value )) + 
#   # scale_x_continuous(limits = c(0,2)) +
#   facet_grid( Year~data , scales = 'free')
