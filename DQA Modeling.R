# Script to clean data downloaded from 'Formula Data Downloads.R'

# Setup ====

source('DHIS2details.txt') # Copies in dhis2 login details.  
## Replace the text values for country and data.directory.  
## This is the same file used for Formula Data Downloads.R


# Libraries ####
pacman::p_load( officer , rvg ,tidyverse, scales, 
         data.table, tidyfast, tidytable, 
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

source( 'TS_Modeling_Functions.r')
# source("Model_TS.R") # new version of model.ts()
source( "Summary_TS.R") # new version of summary_ts()
source( 'TS_model_outlier_impute.R') # model_ts()
source( 'Deviation_Expected_Functions.R')
source( 'theme_ppt.R')
source( 'clean_ts.r' )
source( 'model_ts2.r' )
source( 'api_data.r' )


Month_Year = function( x ){ yearmonth( zoo::as.yearmon( x , "%Y%m") ) }

options(dplyr.summarise.inform = FALSE)

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




extremely_mad = function( x , 
                          deviation = 15, 
                          smallThreshold = 50 , 
                          key_entry_error = NA , 
                          maximum_allowed = NA , 
                          logical = FALSE ,
                          .pb = NULL
                           ){
  if (!is.null( .pb ) ){
      if ( 'progressor' %in% class(.pb) ){ .pb() } else { .pb$tick() }
  } 
  
  # Remove any value above maximum_allowed or key error
  if ( !is.na( maximum_allowed ) ){
    over_max = x > maximum_allowed 
  } else { over_max = !x>=0 }
  
  # Remove key entry errors
  if ( !all(is.na( key_entry_error )) ) {
    key_error =  x>=0 & x %in% key_entry_error$original 
  } else { key_error = !x>=0 }
  
  y = x
  y[ over_max | key_error ] = NA 
   
  if ( (all( y <= smallThreshold | is.na( y ) ) ) ) {
      if ( logical ) return( ! ( over_max | key_error ) )
      return( y )    
  }
  
      # pre clean extreme values
      medianVal = median( y , na.rm = TRUE )
      medianAbsDeviation = mad( y , na.rm = TRUE )
      # se <- function(x) sqrt( var(x , na.rm = TRUE )/ length( !is.na(x) ))
      # sdVal = sd( x , na.rm = TRUE )
      
      extreme = x > ( medianVal + deviation * medianAbsDeviation ) |
          x < ( medianVal - deviation * medianAbsDeviation )
      if ( logical ) return( !extreme & !( over_max | key_error ) ) # FALSE = outlier
      y[ extreme ] = NA 
      return( y )
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

# Formulas  ####

if ( is_empty( data.dir) | nchar( data.dir ) < 1 ) data.dir = dir( country )

# make sure directory has terminal slash
has.slash.at.end = str_locate_all( data.dir , "/") %>% 
  unlist %in% nchar( data.dir) %>% any 
if ( !has.slash.at.end  ){ data.dir = paste0( data.dir , "/" ) }


formula.file = files( 'Formula' , dir = data.dir , country = country ) %>% 
  most_recent_file()

formulas =  read_excel( paste0( data.dir, formula.file ) , sheet = 'Formula') %>% 
  filter( !is.na( Formula.Name ) ) 

formula.names = formulas$Formula.Name 


# Data files ####

data.dir.files = list.files( data.dir )
all.levels.data.files = data.dir.files[ grepl( 'All levels' , data.dir.files) & 
                                     grepl( 'rda|rds' , data.dir.files) &
                                     !grepl( 'df.ts.rds', data.dir.files ) &
                                     grepl( 'formulaData' , data.dir.files) 
                                     ] # lookup all xls files

# data.list  = tibble( 
#   file = all.levels.data.files , 
#   cat = map_chr( all.levels.data.files,  ~ str_split( .x , "All levels")[[1]][1] ) ,
#   date.part = map_chr( all.levels.data.files,  ~ str_split( .x , "All levels")[[1]][2] ) )

data.list = map_df( formula.names , 
                 ~ tibble(
                   formula.name = .x , 
                   file = files( search = .x , other = "formulaData.rds" , 
                         dir = data.dir  , type = "rds")  %>%
                   most_recent_file()
                 )
)

data.files = data.list$file
# 
# data.files = data.list %>% 
#   group_by( cat ) %>%
#   summarise( date.part = most_recent_file( date.part ) ) %>%
#   inner_join( data.list , by = c("cat", "date.part") ) %>%
#   select( file , cat ) %>%
#   arrange( cat ) %>% View


# Parallel modeling  ####
plan( multisession )

# MAD outliers ####
re_run = FALSE 
for ( i in seq_along( data.files ) ){ #1:length( data.files ) ){ #:length( data.files ) 
  
  file = data.files[ i ]
  cat( paste( '\n\n\nopening' , i , ":" , file, '\n') )
  
  if ( grepl( fixed(".dTs.rds"), data.files[i] )  & !re_run ){
    cat( 'data previously prepared' )
    next
  }
  
  saved.file.name = paste0( data.dir, str_replace( file , ".rds" , "") , ".df.ts.rds" ) 

  
  # load dataset  (df)
  tic()
  # df = read_excel( paste0( data.dir , file ) , 
  #                  sheet = 'formulaData' , 
  #                  guess_max = 21474836 ) 
  df = readRDS( paste0( data.dir , file ) )
  cat( paste( 'finished reading data' ) );   toc() 


  # remove duplicate rows because downloads may create duplicates
  tic()
  nrow(df)
  df = df %>% as.data.table() %>% unique
  toc(); nrow(df)
  
  # re-format and convert to Tsibble

  plan(multisession)
  # prevent size error (https://stackoverflow.com/questions/40536067/how-to-adjust-future-global-maxsize-in-r) 
  options(future.globals.maxSize= 891289600)
  options(future.rng.onMisuse  = "ignore" )
  handlers(list(
  handler_progress(
    format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
    width    = 60,
    complete = "+"
  )) )
  
  ## pre-condition data before converting to tsibble
  ## Convert SUM and COUNT to numeric, 
  ## period from character to Month or Week
  tic()
  # split into 1000 chunks and observe progress
  n_splits = 1000
  row_splits = split( 1:nrow(df), cut_number( 1:nrow(df), n_splits ))
  
  with_progress({
    p <- progressor(steps = n_splits )
    
    df.pre.ts =  
      future_map_dfr( 1:n_splits , ~{
      p()
      df_pre_ts( df[ row_splits[[.x]], ] ) 
    } ) 
  })
  toc()
  
  glimpse( df.pre.ts )
  
 # Convert to tsibble
  weekly = any( grepl( "W", df.pre.ts$period[1:10]) )
  period = ifelse( weekly, "Week", "Month" )

  print( 'converting to tsibble' )
  tic()
  df.ts = df_ts( df.pre.ts , period = period ) 
  toc()
  
  glimpse( df.ts )
  
  # Label SUM as original, to clearly denote original value
  
  if ( 'raw' %in% colnames(df.ts) ) df.ts = rename( df.ts, original = raw ) 
  # if ( !'Month' %in% colnames(df.ts) ) df.ts = mutate( df.ts , Month = Month_Year( period ))
  if ( ! 'original' %in% colnames(df.ts) ) df.ts = rename( df.ts , original = SUM ) 
  
  # fill explicit gaps
  tic()
  n_pre.fill = nrow( df.ts )
  df.ts = df.ts %>% fill_gaps( original = NA, 
                               COUNT = 0 , .full = TRUE )
  cat( 'filled implicit gaps with time'  ); toc()
  cat(' increased from' , comma(n_pre.fill) , 'to' , 
      comma(nrow(df.ts)) , 'rows' )
  
  # Create value column to denote months that had data originally,
  tic()
  df.ts = mutate( df.ts , value = !is.na( original ))
  cat( 'added value column'  ) ; toc()

  cat( 'df.ts completed '  )
 
# 
#   tic()
# 
#   orgUnit.details = df.ts %>%
#     dplyr::select( -data, -dataElement.id, -categoryOptionCombo.ids ,
#             -Month, -period , 
#             -COUNT, -original ,-value, ) %>%
#     distinct()
#   
#   keyCols = c( 'orgUnit' , 'data', 'name' )
#   dataCols = unique( df.ts$data )
#   
#   df.ts = df.ts %>% 
#     as_tsibble( index =  Month, 
#                         key = {{ keyCols }} 
#                         )  
#     # index_by( year_month = yearmonth( month ) ) %>%
#    cat( 'df.ts converted to tsibble ' ) 
#    toc()
   
   # print( 'filling in gaps'); tic()
   # if ( sum( has_gaps( df.ts )$.gaps ) > 0 )  df.ts = fill_gaps( df.ts)
   # toc()
   
   print( 'scan for key_entry_errors')
   key_entry_errors =
     count(as_tibble(df.ts %>% 
                     filter( nchar(original)>3 , 
                            effectiveLeaf ) ) , 
           original ) %>% 
     arrange(-n) 
   
   # Default: values where the number happens at least 3 > than 
   # medianof the top 10 rows 
   key_entry_errors = key_entry_errors %>% 
     filter(  n > 3 * median( 
       key_entry_errors %>% filter( row_number()<11 )  %>%
         pull( n ) )
       ) 
   
   print( "key_entry_errors:" ) 
   print( key_entry_errors )
   
  print( 'scanning for MAD outliers ')
  .total = length( key_size( df.ts ) )

  pb <- progress_bar$new( 
    format = ":current :percent  [:bar] :elapsedfull :eta ",
    total = .total, clear = FALSE, width= 50 )

  .threshold = 50
  # for stockout days: max = 31 
  
  df.ts = df.ts %>% 
    # filter( orgUnit %in% unique( df.ts$orgUnit )[] ,
    #         data %in% unique( df.ts$data )[] ) %>%
    
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
                             key_entry_error = key_entry_errors ,
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

  saveRDS( df.ts , saved.file.name )

}

# Seasonal outliers ####
for ( i in seq_along( data.files ) ){ #1:length( data.files ) ){ #:length( data.files ) 
  
  file = data.files[ i ]
  saved.file.name = paste0( data.dir, str_replace( file , ".rds" , "") , ".df.ts.rds" ) 
  df.ts.file.name = saved.file.name
  saved.file.name =  str_replace(  df.ts.file.name , ".rds" ,"_Seasonal.rds" ) 
    
  if ( file.exists( saved.file.name ) & !re_run  ){
    cat( 'data previously prepared' )
    next
  }
  
  cat( paste( '\n\n\nopening' , i , ":" , df.ts.file.name, '\n') )
  df.ts = readRDS( df.ts.file.name  )

  .total = length( key_size( df.ts ) )

  print( 'Seasonal 5')
  pb <- progress_bar$new( 
    format = ":current :percent  [:bar] :elapsedfull :eta ",
    total = .total, clear = FALSE, width= 50 )

  df.ts = df.ts %>% 
    # filter( orgUnit %in% unique( df.ts$orgUnit )[] ,
    #         data %in% unique( df.ts$data )[] ) %>%
    group_by( orgUnit, data ) %>%
    mutate(
      
      seasonal5 = unseasonal(  ifelse( mad10, original , NA) , 
                              smallThreshold = .threshold * 2  , 
                              deviation = 5 ,
                              logical = TRUE , 
                              .pb = pb ) 
  )

  print( 'Seasonal 3')
  pb <- progress_bar$new( 
    format = ":current :percent  [:bar] :elapsedfull :eta ",
    total = .total, clear = FALSE, width= 50 )

  df.ts = df.ts %>% 
    # filter( orgUnit %in% unique( df.ts$orgUnit )[] ,
    #         data %in% unique( df.ts$data )[] ) %>%
    group_by( orgUnit, data ) %>%
    mutate(

      seasonal3 = unseasonal(  ifelse( seasonal5, original , NA) , 
                              smallThreshold = .threshold * 2 , 
                              deviation = 3 ,
                              logical = TRUE , .pb = pb )
  )
  #  df.ts %>% as_tibble %>% count( value, mad15, mad10, mad5, seasonal )
  
  saveRDS( df.ts , saved.file.name )
  
}

# Review counts

for ( i in seq_along( data.files ) ){ #1:length( data.files ) ){ #:length( data.files ) 
  
  file = data.files[ i ]
  
  cat( paste( '\n\n\nopening' , i , ":" , saved.file.name, '\n') )
  df.ts = readRDS( saved.file.name )
  
  df.ts %>% as_tibble %>% count( mad10, mad5, seasonal5 , seasonal3 ) %>% 
    print
  
}

# UNDER DEVELOPMENT  =====
  ## Visualize cleaning  ####
  flag = unique( as_tibble(df.ts) %>% filter( mad5, !seasonal ) %>% 
                   select( orgUnit, data ) )
  
  y=df.ts %>% as_tibble() %>%
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
  
  
  
  ## Consistency UNDER DVELOPMENT: DO NOT RUN ####
    
  for ( i in seq_along( data.files ) ){ #1:length( data.files ) ){ #:length( data.files ) 
    
    file = data.files[ i ]
    saved.file.name =  paste0( data.dir , 
                               "Seasonal_" , 
                               str_replace( file , ".xlsx" , "") , 
                               ".df.ts.rds" ) 
    cat( paste( '\n\n\nopening' , i , ":" , saved.file.name, '\n') )
    df.ts = readRDS( saved.file.name )
  
    .total = length( key_size( df.ts ) )
  
  
    
    # select chunks of ous so that each chunk has an ous with some data.  
    # if error with 10, try 20
    # chunks <- function(x,n) split(x, ceiling(seq_along(x)/n))
    # chunk_ous = chunks( 1:length(ous), 60 )
    tic()
    
    cat( 'Consistency...')
    pb <- progress_bar$new( 
      format = ":current :percent  [:bar] :elapsedfull",
      total = .total, clear = FALSE, width= 50 )
    
    df.ts = df.ts %>% 
      filter( orgUnit %in% unique( df.ts$orgUnit )[1] ,
              data %in% unique( df.ts$data )[1] ) %>%
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
                       ~summary_ts( df.ts = fms[.x , ] ) , 
                       .progress = TRUE )  
    fms_summary = fm_s %>% reduce( ., bind_rows )
    toc()
    cat( paste( 'fms_summary completed ' ) )
  
    # save( hosp.nest , fms , fms_summary , file = saved.file.name )
    
    saved.file.name =  paste0( data.dir , 
                               "Consistency_" , 
                               str_replace( file , ".xlsx" , "") , 
                               ".df.ts.rds" ) 
    
    saveRDS( df.ts , saved.file.name )
      
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
  
# END ====