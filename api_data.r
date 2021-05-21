# api_data with option for parallel processing


library(pacman)
p_load( knitr, scales, tidyverse, readxl, patchwork, 
        tsibble, fable, fabletools, feasts, slider, anomalize, lubridate ,
        furrr, tictoc, ggrepel , sf , ggspatial, mdthemes , extrafont ,
        hrbrthemes ,tidyfast , progressr, gt , rlist )

# dplyr option to set summarise grouping behavior (as 'keep')
options( dplyr.summarise.inform = FALSE )

# Functions ####
code.dir =  "../dataDictionary/dhis2_dictionary/"
source(  paste0( code.dir, 'dhis2_functions.r') )
source( paste0( code.dir, 'API.r') )

api_formula_elements = function( formulaName , ...  ){
  
  formula.elements = read_excel( 
    paste0( dir , files('formula' , country = country )[1]  ), 
    sheet = 'Formula Elements') 
  
  de = filter( formula.elements , 
               Formula.Name %in% formulaName ) %>%
    select( dataElement.id , categoryOptionCombo.ids ) %>%
    unite( "de" , sep = "." ) %>% pull(de ) %>% paste( collapse =  ";" )
  
  return( de )
}


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

file.dir = function( country = country ,
                     dir.base = '../dataDictionary/dhis2_dictionary/Formulas/' ){
  paste0( dir.base , country , "/")
}

most_recent_file = function( file_list_with_date ){
  file_list_with_date = file_list_with_date %>% unlist
  rdsFileSplit = str_split( file_list_with_date , "_")
  download_date = map( rdsFileSplit ,
                       ~str_split( .x[ length(.x)] , "\\.")[[1]]
  ) %>% map_chr(1) 
  
  dates  = map_chr( download_date , ~ anydate(.x)  )
  
  if ( identical( dates , character(0) ) ) return( NA )
  
  # most recent file 
  file = file_list_with_date[ which( dates == max(dates, na.rm = T ) ) ]
  
  return ( file )
}

dir = function( country ){ file.dir( country ) }

metadata.file = function( dir = NULL, country = country ){
  if ( is.null( dir ) ) dir  = dir( country )
  paste0( dir, 
          files( 'meta' , country = country  ) )  %>%
    most_recent_file 
}
                        
OrgUnitLevels = function( metadata.file = NULL ){
  if ( is.null( metadata.file) ) metadata.file = metadata.file()
  read_excel( metadata.file  , 
                       sheet = 'OrgUnitLevels')
}
OrgUnits =  function( metadata.file = NULL ){
  if ( is.null( metadata.file) ) metadata.file = metadata.file()
  read_excel( metadata.file , 
              sheet = 'OrgUnits')
}
DataElements =  function( metadata.file = NULL ){
  if ( is.null( metadata.file) ) metadata.file = metadata.file()
  read_excel( metadata.file  , 
              sheet = 'DataElements')
}
DataSets =  function( metadata.file = NULL ){
  if ( is.null( metadata.file) ) metadata.file = metadata.file()
  read_excel( metadata.file , sheet = 'DataSets')
}
Indicators =  function( metadata.file = NULL ){
  if ( is.null( metadata.file) ) metadata.file = metadata.file()
  read_excel( metadata.file , sheet = 'Indicators')
}

# update fetch for non shiny environ
fetch <- function( baseurl. , de. , periods. , orgUnits. , aggregationType. , .print = FALSE ){
    
    if (.print ) print( paste( 'period is ', periods. ) )
    periods_vector = str_split( periods. , ";" ) %>% unlist
    
    n_periods = length( periods_vector )
    if (.print )  print( paste( 'n_periods' , n_periods ))
    
    # translate level name to LEVEL-1, etc
    # level = ousLevels() %>% filter( levelName %in% orgUnits.) %>% pull( level )
    # ouLevel = paste0( "LEVEL-", level )
    # print( paste( level, ouLevel) )
    
    # Version before furr and progressor
    # data = list( n_periods )
    # 
    #   for ( i in 1:n_periods ){
    #     
    #     data[[i]] = fetch_get( baseurl. , de. , periods_vector[i] , orgUnits. , aggregationType.,
    #                            get.print = .print )
    #   }
    
    handlers(list(
      handler_progress(
        format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
        width    = 50,
        complete = "+"
      )
    ))
    
 
    with_progress({
      p <- progressor( steps = n_periods )
      
      data = 
        future_map( 
          1:n_periods , 
          ~{
            if ( !is.null( p ) ) p()
            
            fetch_get( baseurl. , de. , periods_vector[.x] , orgUnits. , aggregationType.,
                       get.print = .print )
            
          } )
    })
    
    return( bind_rows( data ) )
  }
  
fetch_get <- function( baseurl. , de. , periods. , orgUnits. , aggregationType. ,
                       get.print = FALSE ){
    
    url = api_url( baseurl. , de. , periods. , orgUnits. , aggregationType. )
    
    fetch = retry( get( url , .print = get.print )[[1]] ) # if time-out or other error, will retry 
    # fetch = get( url , .print = get.print )
    
    # print( paste( 'fetch class' , class( fetch ) ) )
    # print( paste( 'fetch class[[1]]' , class( fetch[[1]] ) ) ) 
    
    # if returns a data frame of values (e.g. not 'server error'), then keep
    # print( paste( 'did fetch return data frame?' , is.data.frame( fetch )))
    
    if ( is.data.frame( fetch ) ){ 
      
    # remove unneeded cols
      
      cols = colnames( fetch ) 
      
      unneeded.cols = which( cols %in% c( 'storedBy', 'created', 'lastUpdated', 'comment' ))
    
      # print( glimpse( fetch )  )
      
      # print( paste( 'unneeded cols' , 
      #               paste( unneeded.cols , collapse = "," ))
      # )
      
      data.return = fetch %>% select( -unneeded.cols ) %>% as_tibble()
      
      if (get.print) print( paste( 'col names data', 
                    paste( colnames( data.return ) , collapse = "," ) 
                    )
      )
      
      } else {
      
      if ( get.print ) print( paste( nrow( fetch ) , 'records\n' ) )
      
      de.cat = str_split( de. , fixed(".")) %>% unlist  
      
      data.return  = tibble( 
        dataElement = de.cat[1] , 
        categoryOptionCombo = de.cat[2] , 
        period =  periods. ,
        orgUnit =  orgUnits. ,
        aggregationType = aggregationType. ,
        value = NA
      )
      
      if ( get.print ) print( "no records" )
    }
    
    return( data.return )
  }
  
translate_fetch = function( df , formulaElements = NULL , ous = NULL ){
  
  if ( is.null( formulaElements )) formulaElements = DataElements()
  if ( is.null( ous )) ous = ous()
    
  # prepare formula elements as one row per de and coc
  if ( count( formulaElements , dataElement ) %>%
    filter( n > 1 ) %>% nrow == 0 ){
    
      de.coc = formulaElements  %>% 
      separate_rows( Categories , categoryOptionCombo.ids, sep = ";" ) %>%
      mutate( Categories = Categories %>% str_trim  ,
              categoryOptionCombo.ids = categoryOptionCombo.ids %>% str_trim )
      
  } else { de.coc = formulaElements }
  

      df %>%
      
      rename( dataElement.id = dataElement , 
              categoryOptionCombo.ids = categoryOptionCombo 
      ) %>%
      
      left_join( de.coc %>% 
                   select( dataElement, dataElement.id , 
                           Categories, categoryOptionCombo.ids ) %>% 
                   mutate( dataElement.id = dataElement.id %>% str_trim ,
                           categoryOptionCombo.ids = categoryOptionCombo.ids %>% str_trim )  ,
                 by = c( "dataElement.id" , "categoryOptionCombo.ids" )
      ) %>%
      
      left_join( ous %>% 
                   select( id, name, level, levelName )  %>% 
                   rename( orgUnit = id , orgUnitName = name ) ,
                 by = 'orgUnit' 
      )  
  }
 
api_data = function(      periods = "LAST_YEAR" ,
                          level = 'All levels' ,
                          elements = NA ,
                          baseurl = NA , 
                          formula = NA , 
                          parallel = FALSE ,
                          print = TRUE ,
                          update = FALSE ,
                          check_previous_years = 2 , 
                          previous_dataset_file = NULL ,
                          ...
){
  tic()
  ##### cycle through each period, each data element...

  # Periods ####
  if ( periods %in% 'months_last_year' ) periods = date_code( YrsPrevious = 1 )
  if ( periods %in% 'months_last_2_years' ) periods = date_code( YrsPrevious = 2 )
  if ( periods %in% 'months_last_3_years' ) periods = date_code( YrsPrevious = 3 )
  if ( periods %in% 'months_last_4_years' ) periods = date_code( YrsPrevious = 4 )
  if ( periods %in% 'months_last_5_years' ) periods = date_code( YrsPrevious = 5 )
  
  if ( all( is.na( periods )  ) ) periods = date_code( YrsPrevious = 4 )
    
  period_vectors = strsplit( periods , ";" , fixed = TRUE )[[1]]
    
  ## UPDATE data options ####
    if ( update ){
      if (!file.exists( previous_dataset_file )){
        cat('previous data file missing') ; next()
      } 
      
      # check for last 2 years only 
      periods = date_code( YrsPrevious = check_previous_years )
      period_vectors = strsplit( periods , ";" , fixed = TRUE )[[1]]
      
      # excel version
      # prev.data = read_excel(previous_dataset_file , 
      #                          sheet = 'formulaData') %>%
      #           filter( !is.na( dataElement  ) )
      
      prev.data = readRDS( previous_dataset_file ) %>% select( - starts_with('aggr'))
      
      if ( nrow( prev.data ) == 0 ){
        cat('previous data file empty'); next()
      }  
      
      first.month = min(prev.data$period  )
      last.month =  max( prev.data$period )
      
      # excel version
      # des = prev.data %>% select( dataElement, dataElement.id , Categories ,
      #                      categoryOptionCombo.ids ) %>%
      #   unique 
      
      des = prev.data %>% 
        filter( !is.na( COUNT ) ) %>%
        select( dataElement,  categoryOptionCombo ) %>%
        unique 
      
      # excel version
      # elements = paste( des$dataElement.id , des$categoryOptionCombo.ids , sep = ".") %>%
      #   paste( collapse = ";")
      
      # rds version 
      prev.elements = paste( des$dataElement , des$categoryOptionCombo , sep = ".") %>%
        paste( collapse = ";") 
      
      # limit to those in the request (elements)
      elements.vector = str_split( elements, ";") %>% unlist 
      prev.elements.vector = str_split( prev.elements, ";") %>% unlist 
      prev.elements = prev.elements.vector[ prev.elements.vector %in% elements.vector ] %>%
        paste0( ., collapse = ';')
      
      # Lookup national counts for last month, then get current counts ####
     
      ## for excel
      # prev = prev.data %>% filter( level == 1 , period %in% period_vectors ) 
      
      ## for rds, need orgunit for level 1 
      # prev = prev.data %>% filter( orgUnit == 'LEVEL-1' , period %in% period_vectors )
      level1 = ous %>% filter( level == 1 ) %>% pull( id )
      prev = prev.data %>% filter( orgUnit == level1  , period %in% period_vectors ) 
      
      prev.periods = prev %>% pull( period ) %>% unique %>%  paste(. , collapse = ';')
      
      ## FETCH CURRENT VALUES of previous data 
      print( 'checking previous counts\n' )
      current.count = fetch(  baseurl , elements , prev.periods , orgUnits. = "LEVEL-1" , "COUNT" )  %>%
        mutate(current.count = as.integer( value ) ) %>%
        select( - value )
      
      print( 'checking previous values\n' )
      current.value = fetch(  baseurl , elements , prev.periods , orgUnits. = "LEVEL-1" , "SUM" )  %>%
        mutate(current.value = as.integer( value ) ) %>%
        select( - value )
      
      update_compare = inner_join( current.count, current.value , 
                                   by = c("dataElement", "period", "orgUnit", "categoryOptionCombo")
                                   ) %>%
        left_join( prev %>% 
                      mutate( prev.count = as.integer( COUNT ) ,
                              prev.value = as.integer( SUM ) ) %>%
                     select( - COUNT, -SUM ), 
                  # Excel version  
                  # by = c( 'dataElement' = 'dataElement.id' , 
                  #         'categoryOptionCombo' = 'categoryOptionCombo.ids' ,
                  #         'period' = 'period' , 
                  #         'orgUnit' = 'orgUnit' )
                  # rds version  
                  by = c( 'dataElement' , 
                          'categoryOptionCombo',
                          'period' , 
                          'orgUnit' )
                  
                  ) %>%
        mutate( same = ( current.count %in% prev.count ) & 
                  ( current.value %in% prev.value ) 
        )
      
        
        saveRDS( update_compare ,  paste0( dir, 'update_compare_', formula,"_", Sys.Date() , ".rds"))
      
        prev.periods.same.data = update_compare %>% 
          group_by( period ) %>%
          filter( all( same ) ) %>%
          pull( period ) %>% unique 
        
        if ( print ) print( paste( 'Previous data had the same data for ' , 
                                   paste( prev.periods.same.data , collapse = ';') )
        )
        
      
        update.periods = setdiff( period_vectors, prev.periods.same.data )
        
        period_vectors = update.periods
        
    } 
  
   if ( print ) print( paste( 'Periods requested are' , 
                              paste( period_vectors , collapse = ';') )
   )
    
  
  # orgUnits ####
  orgUnits = case_when(
    
    level %in% 'All levels' ~ 
       orgUnitLevels  %>% arrange( desc( level )) %>% pull( level ) %>%
              paste0( "LEVEL-" , .  ) ,
    
    TRUE ~ level 
    
  )
  
  if ( print ) print( paste( 'Levels requested are' , 
                             paste( orgUnits , collapse = ';') )
  )
    
  ## Fetch requests ####
  
  if ( parallel ) plan( multicore ) # plan( multisession ) for windows
  if (!parallel ) plan( sequential )

    handlers(list(
      handler_progress(
        format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
        width    = 50,
        complete = "+"
      )
    ))

    v2 <- expand_grid( period_vectors , orgUnits )
    
    with_progress({
      p <- progressor( steps = nrow( v2 ) )
    
      d = 
        future_map2( 
           v2$period_vectors, 
           v2$orgUnits 
           , 
            .f = ~{
                
                  if ( !is.null( p ) ) p()
        
            # tic()
            # if ( print ) message( paste( "" , formula, " : " , .x , .y , 
            #               parse_date_time( Sys.time(), '%I:%M:%S %p') )  )

              
            d.sum = fetch_get(  baseurl. = baseurl , de. = elements , 
                                periods. = .x , orgUnits. = .y , "SUM" )  
           
            d.count = fetch_get(  baseurl. = baseurl , de. = elements , 
                                  periods. = .x  , orgUnits. = .y , "COUNT" )
            
            d = d.count %>%
                  rename( COUNT = value ) %>%
                  full_join( d.sum %>% rename( SUM = value ) 
                            # ,  by = c("dataElement", "dataElement.id", "Categories" , "categoryOptionCombo.ids", "period", "orgUnit", "orgUnitName" ,  "level" , "levelName")
                            , by = c("dataElement", "period", "orgUnit", "categoryOptionCombo")
                            )
            
            d = d %>% select(-starts_with('aggreg'))
            
            if ( print ) message( paste( comma( nrow( d ) ), "records"  ) )
            
            return( d )
                
            } 
        )
    })
    
     if ( print ) message( "binding downloads" )
     d = bind_rows( d )
     if ( print ) message( toc() )
     
     # update value in most_recent_data_file
     if ( update ){

       good.prev.data = prev.data %>% filter( !period %in% unique( d$period ))
       
       updated.data  = bind_rows( good.prev.data , d ) %>% 
         filter( !is.na( COUNT ) ) %>%
         arrange( period , orgUnit, dataElement , categoryOptionCombo  ) 

       return( updated.data )
     }
     
     return( d )
}

# Testing
# # Formulas  ----
# library(progressr)
# files('Formula' , country = country )[1]
# 
# f = formulas( country ) %>% 
#   filter( !is.na(Formula.Name)) %>% 
#   pull(Formula.Name )
# f= f[4]
# 
# ## login and credentials ----
#     credentials = read_excel( paste0( "../dataDictionary/dhis2_dictionary/Formulas/",
#                                      "_Instances_jp.xlsx")) %>%
#     filter( Instance == country )
#     
#     baseurl = credentials$IPaddress
#     username = credentials$UserName
#     password = credentials$Password
# 
#     loginurl <-paste0( baseurl , "api/me" )
#     GET( loginurl, authenticate( username, password ) )
# 
#     periods = 'months_last_year' # date_code( YrsPrevious  = 4 )
#     level = 'All levels'
#     elements = formula_elements( f[i] )
#     start = Sys.time()
#     y  = api_data( periods, level , elements, baseurl , formula = f[i]  
#                    # , parallel = TRUE 
#                    )
#     print( paste( 'finished downloading' , f[i] ) )
#     stop = Sys.time(); stop - start
# 
