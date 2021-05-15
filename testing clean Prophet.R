# Libraries ----
pacman::p_load( officer , rvg ,tidyverse, scales, tidyfast, tidytable, 
        lubridate , anytime, progressr , progress ,assertthat ,
        readxl, patchwork, cowplot, kableExtra ,
        tsibble, fable, fabletools, feasts, 
        fable.prophet ,  fable.bsts , 
        slider, anomalize, brolgar ,
        tsbox , CausalImpact , tibbletime , dygraphs , 
        fpp3  , fabletools, 
        furrr, tictoc, magrittr, hrbrthemes, data.tree, igraph ,
        sf, mapview, GGally , plotly , sugrrants
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

country.dir = file.dir( 'Malawi')
dir.files = list.files( country.dir )
file.type = 'rds' # input$file.type 
file.other = 'Seasonal_' 
all.levels.data.files = dir.files[ grepl( 'All levels' , dir.files) &
                                       grepl( file.type , dir.files) &
                                       grepl( file.other, dir.files, fixed = TRUE  )] %>%
    most_recent_file() %>%
    paste0( country.dir , . )

dTs =  readRDS( all.levels.data.files[3] ) %>% 
  mutate(
        cleaned = ifelse( seasonal3, original , NA)
        ) %>%
  fill_gaps()

### FUNCTION fit.prophet ----

# Mean percent absolute error for leaf orgunit, and national 
mable_mpae = function( x ){
        x %>%
        augment %>%  
        group_by( orgUnit , data ) %>%
        summarise( cleaned = sum( ifelse(!is.na(.fitted) , cleaned , NA ), na.rm = T ),
                   .fitted = sum( .fitted , na.rm = T ) 
                   ) %>%
        as_tibble() %>%
        group_by( orgUnit , data  )  %>%
        summarise( 
          wt = mean(cleaned , na.rm = T ) ,
          mpae = ifelse( wt > 0 ,
                       mean( abs( cleaned - .fitted ) , na.rm = T ) / 
                       mean( cleaned , na.rm = T ) ,
                       NA ) 
                   ) %>%
        filter( !is.na( mpae )) %>%
        group_by( orgUnit ) %>% 
        summarise(tibble(min = min(mpae), 
                         mean = mean( mpae )  ,
                         wt.mean = weighted.mean( mpae, wt ) ,
                         median=median(mpae), 
                         max = max(mpae) ,
                         n = n(),
                         sum.wt = sum( wt, na.rm = TRUE ))
                  )
}

fit.prophet = function( dataset , org , tsibble_var = 'cleaned' ,
                        min_val =5 , min_n = 24 , mpae = TRUE ,
                        .pb = NULL ){
    defaultW <- getOption("warn")
    options(warn = -1)
    
    if (!is.null( .pb ) ){
      if ( 'progressor' %in% class(.pb) ){ .pb() } else { .pb$tick() }
    } 
        
    x = dataset %>% filter( orgUnit %in% org ) 
    if ( nrow(x) == 0 ) return()
    has.data = x %>% as_tibble() %>% 
        group_by( data ) %>%
        summarise( n = sum( !!sym( tsibble_var ) > min_val, na.rm = T) ) %>%
        filter( n > min_n ) %>%
        pull( data )

    if ( length( has.data) == 0 ) return()
    
    fit  = x %>%
         filter( data %in% has.data ) %>% 
         model( 
            prophet = prophet( !!sym( tsibble_var ) ~

                                    growth( type = 'logistic',
                                            changepoint_range = 1 ,
                                            changepoint_prior_scale = 1 ,
                                            capacity = 1e5 ,
                                            floor = 0 ) +
                                    season(period=12, 
                                           order = 4 ,
                                           type='multiplicative'),
                               seed = TRUE)
        )
    
     if ( mpae ) return( mable_mpae( fit ) )
     options(warn = defaultW)
     return( fit )
    
}


# parallel
plan( 'multisession' )
handlers(global = TRUE)
handlers("progress") # use progress bar from progress package
handlers(list(
  handler_progress(
    format = ":current :percent  [:bar] :elapsedfull eta: :eta",
    width    = 60
  )
))

run_parallel_prophet <- function( x , orgUnitIndex = NULL) {
  orgUnits = unique( x$orgUnit )
  if ( !is.null( orgUnitIndex ) ) orgUnits = orgUnits[ orgUnitIndex ] 
       
  p <- progressor( along = orgUnits )
  # when requesting mape, can us map_dfr (will not work when returning mable)
  ffit <- future_map_dfr( orgUnits, function(.x) {
    fit.prophet( x, org = .x , .pb = p  )
  })
  return( ffit )
}

# Run Prophet on District Data----

dts.district = dTs %>% 
  filter( effectiveLeaf == FALSE , levelName == 'District')

tic(); pfit.district = run_parallel_prophet( x = dts.district ) ; toc()

pfit.district.filename =  str_replace( all.levels.data.files[3] , 'Seasonal_', 'Prophet_District_' )
saveRDS( pfit.district ,  pfit.district.filename )
pfit.district = readRDS( pfit.district.filename )

# Run Prophet on leaf facilities----

dts.leaf = dTs %>% filter( effectiveLeaf == TRUE )

tic(); pfit.leaf = run_parallel_prophet( x = dts.leaf ) ; toc()

pfit.leaf.filename =  str_replace( all.levels.data.files[3] , 'Seasonal_', 'Prophet_Leaf_' )
saveRDS( pfit.leaf ,  pfit.leaf.filename )
pfit.leaf = readRDS( pfit.leaf.filename )

# summarise leaf data by district, then compare with district...



leaf.orgunit = dTs %>% filter( effectiveLeaf ) %>% pull( orgUnit ) %>% unique 
national.orgunit = dTs %>% filter( level == 1 ) %>% pull( orgUnit ) %>% unique
is.leaf = map_lgl( pfit, ~ any(.x$orgUnit %in% leaf.orgunit ) )
is.national = map_lgl( pfit, ~ any(.x$orgUnit %in% national.orgunit ) )

leaf.fit.error  = pfit[ which(has.model & is.leaf ) ] %>% 
    map_df( mable_mpae )
leaf.fit.error
ggplot( leaf.fit.error ) + 
    geom_histogram( aes( median ), binwidth = .1 ) +
    scale_x_continuous( limits = c(0,2))

national.fit.error  = pfit[ which( has.model & is.national ) ] %>% 
    map_df( mable_mpae )
national.fit.error


## Plot actual vs fit (when pfit is the mable, not mpae)
# Test for non-null models
has.model = !map_lgl( pfit, is.null)
model.not.null = map( pfit , ~.x$prophet %>% is_null_model  ) %>% unlist 
# prophet.fit = ffit[ which(has.model)[1:3] ] %>% bind_rows()

# Mean percent absolute error for leaf orgunit, and national 
ffit[ which(has.model)[7] ][[1]]  %>% mable_mpae()

pfit %>% select(prophet) %>% augment %>% 
    group_by( orgUnit ) %>%
    summarise( cleaned = sum( ifelse(!is.na(.fitted) , cleaned , NA ), na.rm = T ),
               .fitted = sum( .fitted , na.rm = T ) 
               ) %>%

    # autoplot(vars( cleaned, .fitted) ) 
    ggplot( ., aes( Month )) +
    geom_line( aes( y = cleaned), color = 'black') +
    geom_line( aes( y = .fitted), color = 'red') +
    scale_y_continuous( limits = c(0, NA)) +
    facet_wrap( ~ orgUnit, scales = 'free' )

   


