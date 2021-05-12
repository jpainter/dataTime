
country.dir = file.dir( 'Malawi')
dir.files = list.files( country.dir )
file.type = 'rds' # input$file.type 
file.other = 'Seasonal_' 
all.levels.data.files = dir.files[ grepl( 'All levels' , dir.files) &
                                       grepl( file.type , dir.files) &
                                       grepl( file.other, dir.files, fixed = TRUE  )] %>%
    most_recent_file() %>%
    paste0( country.dir , . )

dTs =  readRDS( all.levels.data.files[3] ) %>% fill_gaps()

x =  dTs %>% 
    # filter( orgUnit %in% unique( dTs$orgUnit )[1] ,
    # data %in% unique( dTs$data )[1] ) %>%
    mutate(
        cleaned = ifelse( seasonal3, original , NA)
        )

m = function( x , orgUnit_index=1, tsibble_var='cleaned' ){
    orgUnits = unique( x$orgUnit )
     m = x %>%
        filter( orgUnit %in% orgUnits[orgUnit_index] ) %>%
        model( arima = ARIMA( !!sym( tsibble_var )  ~
                                  pdq( 0:2, 0:1, 0:2 ) +
                                  PDQ( 0:2, 0:1, 0:2 ,  period = 12 ) 
                              )
        )
   
    if ( all( is.na( m ) )  ) return()
        
     m.ts = m %>% augment()
     fit = m.ts$.fitted  %>% as.integer()
     resid = m.ts$.resid
     innov = m.ts$.innov
    
     return( list( fit , resid, innov ))       
}

orgUnits = unique( x$orgUnit )  
changedates = c(yearmonth('201901'),yearmonth('202001'), yearmonth('202103'))
tic()
fit = x %>%
    filter( orgUnit %in% orgUnits[1:5] ) %>%
    # group_by( orgUnit ) %>%
    # summarise( cleaned = sum( cleaned , na.rm = T )) %>%

        model( 
            # arima = ARIMA( !!sym( tsibble_var )  ~
            #                       pdq( 0:2, 0:1, 0:2 ) +
            #                       PDQ( 0:2, 0:1, 0:2 ,  period = 12 ) 
            #                   )
            #    , 
            prophet = prophet( !!sym( tsibble_var ) ~
                                   # growth( n_changepoints = 3 ) +
                                    growth( type = 'logistic',
                                            changepoint_range = 1 ,
                                            changepoint_prior_scale = 1 ,
                                            capacity = 1e5 ,
                                            floor = 0 ) +
                                    season("year", type = 'multiplicative') )
        )
toc()
fit  %>% report
fit %>% accuracy()    
fit %>% select(prophet) %>% augment %>% 
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

### FUNCITION fit.prophet
fit.prophet = function( dataset , org , tsibble_var = 'cleaned' ,
                        min_val =5 , min_n = 24 , 
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
                                    season("year", type = 'multiplicative') )
        )
     options(warn = defaultW)
     return( fit )
    
}

#test
dataset = dTs %>% mutate( cleaned = ifelse( seasonal3, original , NA) )
orgUnits = unique( dataset$orgUnit )[1:3]  
.total = length( key_size( dTs ) )
pb <- progress_bar$new( 
    format = ":current :percent  [:bar] :elapsedfull",
    total = length( orgUnits ), clear = FALSE, width= 50 )

fit = map( orgUnits , ~fit.prophet( dataset, org = .x , .pb = pb) )

plan( 'multisession' )
tic()
orgUnits = unique( dataset$orgUnit )[] 
with_progress({
 p <- progressor(steps = length(orgUnits))
 ffit = future_map( orgUnits , ~{
     p()
     fit.prophet( dataset, org = .x ) 
 })
})
toc()
has.model = !map_lgl(ffit, is.null)
prophet.fit = ffit[which(has.model[1:3])] %>% reduce( ., bind_rows )


# Mean percent absolute error 
fit %>% select(prophet) %>% augment %>%  
    group_by( orgUnit , data ) %>%
    summarise( cleaned = sum( ifelse(!is.na(.fitted) , cleaned , NA ), na.rm = T ),
               .fitted = sum( .fitted , na.rm = T ) 
               ) %>%
    as_tibble() %>%
    group_by( orgUnit , data  )  %>%
    summarise( mpae = ifelse( mean(cleaned , na.rm = T ) > 0 ,
                   mean( abs( cleaned - .fitted ) , na.rm = T ) / 
                   mean(cleaned , na.rm = T ) ,
                   NA )
               ) %>%
    filter( !is.na( mpae )) %>%
    group_by( orgUnit ) %>% 
    summarise(tibble(min = min(mpae), 
                     mean = mean(mpae), median=median(mpae), 
                     max = max(mpae))
              )

    
fit.details.a = fit %>% augment()
fit.details.p = fit %>% select(-arima) %>% augment() #fabletools::components( ) 
fit.details.p %>% autoplot()
x %>%  filter( orgUnit %in% orgUnits[5] ) %>% autoplot( cleaned ) 

tic()
fit = model %>% augment() %>% pull(.fitted) %>% as.integer()
toc()



y = x  %>%
    mutate( 
        m = m(. , tsibble_var = 'cleaned' )
        )
    


glimpse( y )

mutate(  
    arima = clean_ts( . , 
                      tsibble_var = 'cleaned' , 
                      .lambda = .5 , 
                      .clean = 'arima' , #MAD, arima
                      interpolate = FALSE ,
                      predict = FALSE , 
                      pred_data = 6 , 
                      arima.list = TRUE , # if arima, returns fit, resid, and interp
                      MAD = 5 , 
                      ignoreSmall = TRUE , 
                      smallThreshold = 50 ,
                      .pb = NULL ,
                      timing = FALSE )
)


glimpse(x$arima)
