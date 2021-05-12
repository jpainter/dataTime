

# Libraries and functions ####
library(pacman)
p_load( tidyverse, tidyfast, lubridate , readxl, patchwork, 
        tsibble, fable, fabletools, feasts, slider, anomalize, 
        furrr, tictoc, magrittr, hrbrthemes )

# Functions
source( '../dataDictionary/TS_Modeling_Functions.r')
source("Model_TS.R") # new version of model.ts()
source("Summary_TS.R") # new version of summary_ts()
source('TS_model_outlier_impute.R')

Month_Year = function( x ){ yearmonth( zoo::as.yearmon( x , "%Y%m") ) }

options(dplyr.summarise.inform = FALSE)



# Data files ####

dir = '../dataDictionary/dhis2_dictionary/Formulas/Malawi/'
dir.files = list.files( dir )
files.all = dir.files[ grepl( 'All' , dir.files) ]
files.meta = dir.files[ grepl( 'meta' , dir.files) ]

data.file = paste0( dir, files.all[1] )
data = read_excel( data.file , sheet = 'formulaData' )  %>%
  filter( ! is.na( orgUnitName ) )

meta.file = paste0( dir, files.meta[2] )
ous = read_excel( meta.file , sheet = 'OrgUnits')

leaf = data %>%
  group_by( orgUnit ) %>%
  summarise( effectiveLeaf = max( as.integer( COUNT ) ) <= 1 ) %>%
  left_join( ous , by = c( 'orgUnit' = 'id') )
 
count( leaf, effectiveLeaf )

# prepare dataset as time-series ( df.ts )
  tic()
  ts = df_pre_ts( data  ) 
  toc() 
  cat( paste( 'ts completed ' ) )  
  
  # glimpse( hosp )
  # prepare data nested by orgUnit (hosp.nest)
  tic()
  ts.nest = ts %>% 
    # as_tibble %>% 
    select( orgUnit, data, name , Month , value , raw) %>%
    # group_by( orgUnit, data, name ) %>%
    nest( ts = c(-orgUnit, -data, -name  ) )
  toc() 
  cat( paste( 'ts.nest completed ' ) )

  
# Parallel modeling  
plan("multiprocess", workers = 3 )
  
ous = unique( ts.nest$orgUnit )
n = length( ous )
p <- progress::progress_bar$new( total = n)
tic()
  initial_model = future_map( 1:n , ~ts_model( ts =  ts.nest %>%
                    filter( name %in% 'SUM' ,
                            orgUnit %in% ous[.x] ) ,
                  pb = NULL ) ,
                  .progress = TRUE
  )
toc()
initial_model = data.table::rbindlist( initial_model )
glimpse( initial_model )
# 
object.size( initial_model ) %>% as.numeric() %>% scales::comma()
# object.size( initial_model %>% select(-arima)  ) %>% as.numeric() %>% scales::comma()
# 
# tic(); initial_model = initial_model %>% select(-arima) %>% pull( ts ) %>% data.table::rbindlist() ; toc()
# object.size( b ) %>% as.numeric() %>% scales::comma()

saveRDS( initial_model , paste0( dir, "Malawi_initial_model_attendance.rds") )
initial_model = readRDS( paste0( dir, "Malawi_initial_model_attendance.rds")  )
  

## National ####

chart_outliers = function( l = 1){
  
  d = data %>% filter( level == l ) %>% 
    mutate( Month = Month_Year( period ) ,
            data = paste0( dataElement, Categories ) 
            ) %>% 
    pivot_longer( cols = c( COUNT, SUM ) , names_to = 'name' ) %>% 
    mutate( value = as.numeric( value ) )  

  d.outlier =  anomalize( d %>% filter( name =='SUM' ) , 
                          value , alpha = 0.05 )   %>%
    rename( outlier = anomaly )

  d. = d.outlier %>% 
               filter(  outlier %in% 'No' ) %>%
               mutate( name = 'Sum_Revised' ) %>% 
    # bind_rows( d ) %>% 
    as_tsibble( key = c( orgUnit, data, name  ) , index = Month ) %>%
    fill_gaps() %>%
    filter( !Month %in% max(Month) ) # remove last month

  
  m = model( d., arima = ARIMA( value  ) )

  m.data = m %>% augment() %>% 
    mutate( err = .resid / .fitted ) %>%
    anomalize( err  , alpha = 0.05  )   %>%
    as_tsibble( key = c( orgUnit, data, name  ) , index = Month ) %>%
    bind_rows( d  )   
  
  m.hasAnomaly = m.data %>% as_tibble() %>%
    mutate( name = 'Sum_Revised' ) %>%
    group_by( orgUnit, data, name  ) %>%
    summarise( has.anomaly = if ( any(anomaly %in% 'Yes') ){ TRUE }else{ FALSE }   ) %>%
    ungroup

  
  units = length(unique(m$orgUnit))
  if( units > 1 ){
    
    total2 = m.data %>% as_tibble() %>%
      filter( name %in% 'Sum_Revised' ) %>%
      mutate( value = ifelse( anomaly %in% 'Yes' , NA , value ) ,
              orgUnit = 'Total' , 
              name = 'Total_(2)' ) %>%
      group_by( orgUnit , data, name , Month ) %>% 
      summarise( value = sum( value , na.rm = TRUE ))  %>%
      ungroup %>%
      as_tsibble( key = c( orgUnit, data, name  ) , index = Month ) 
    
    # total with only the biggest outliers removed
    total1 = d. %>% as_tibble() %>%
      mutate( 
              orgUnit = 'Total' , 
              name = 'Total_(1)' ) %>%
      group_by( orgUnit, data, name , Month ) %>% 
      summarise( value = sum( value , na.rm = TRUE ))  %>%
      ungroup %>%
      as_tsibble( key = c( orgUnit, data, name  ) , index = Month ) %>%
      filter( !Month %in% max(Month) )
    
    combined.data = bind_rows( total1, total2 , m.data ) %>% 
      as_tibble() %>%
      left_join( m.hasAnomaly %>% as_tibble , 
                 by = c( 'orgUnit', 'data', 'name'  ) ) %>%
      mutate( alpha.anomaly = ifelse( has.anomaly | is.na( has.anomaly )  , 1 , .5  ) ) %>%
      as_tsibble( key = c( orgUnit, data, name  ) , index = Month ) 
  
    } else {
    
        combined.data = m.data  %>% 
          mutate( alpha.anomaly = 1L )
  }

  combined.data %>% 
    # autoplot( value ) + 
    ggplot( aes( x = Month, y = value , 
                 color = orgUnit , group = orgUnit ) ) +
    geom_line( aes( alpha = alpha.anomaly ) ) +
    scale_alpha_continuous() +
    theme(legend.position = "none" ) +
    geom_point( data = filter( m.data , anomaly == 'Yes' ), 
                aes( shape = anomaly ) ) +
    # scale_shape_manual( values = c('No' = NULL , 'Yes' = 'triangle' ) ) +
    facet_grid(name ~ data  , scales = 'free') +
    labs( title = paste( 'Level-' , l , '(No. orgUnits =' , units , ")") ,  
          subtitle = 'dots highlight outliers' ,
          x='' , y='')
    
}

chart_outliers( 1 )
chart_outliers( 2 )
chart_outliers( 3 )



## Add up first pass outliers  ####

initial_anomalies = function( index , progress = NULL ){
    if ( !is.null( progress ) ) progress$tick()
    
    io = initial_model[ index , 'ts'] %>% extract2(1) %>% extract2(1) %>%
        mutate( anomaly_value = ifelse( anomaly , raw , NA ) ) %>% 
        bind_cols( initial_model[ index  , c('orgUnit', 'data' , 'name') ] )
    
    return( io )
}

tic()
n = nrow( initial_model )
pb = progress_bar$new( total = n )
firstPassOutliers = map( 1:n , 
                         ~initial_anomalies( .x , progress = pb ) 
                         )
toc()

firstPassOutliers. = data.table::rbindlist( firstPassOutliers ) %>%
    as_tsibble( key = c( orgUnit, data, name) , index = Month ) %>%
    summarise( outlier_sum = sum( anomaly_value , na.rm = TRUE ) ,
               outlier_n = sum( anomaly  ) 
               ) 

## Flag second pass outliers ####


## Add up second pass outliers



## Impute



## Add up imputation and number of data points still missing


# Now, redo above for recent facilities...

# Current facilities reporting data ####
## looks like this was originally restricted to leaf units (need to confirm)
dts.model  = initial_model %>% 
    select( -arima ) %>%
    semi_join( leaf %>% filter( effectiveLeaf == TRUE ) , by = 'orgUnit' ) %>%
    unnest( ts ) 

months = count( dts.model , Month )  %>% arrange( desc( Month ) )
last.month = months[ 1 , 'Month']

ous.lastmonth = dts.model %>%
    filter(  Month %in% last.month ) %>%
    pull( orgUnit ) %>% unique  # n= 1711

# describe size of attendance in cohort
data.last.summary %>% 
    group_by( data ) %>%
    summarise( 
       orgUnits  = n() , 
        meanN = mean( n , na.rm = TRUE ) ,
        mean = mean( meanValue , na.rm = TRUE ) ,
        sd = sd( meanValue , na.rm = TRUE )
        )
plot( log( data.last.summary$meanValue ) ,  data.last.summary$n )
plot( log( data.last.summary$meanValue )  ,data.last.summary$MhAPE )

median_n = median( data.last.summary$n ) 
mean_n = mean( data.last.summary$n ) 
median_MhAPE = median( data.last.summary$MhAPE , na.rm = TRUE )
mean_MhAPE = mean( data.last.summary$MhAPE , na.rm = TRUE )

# Examine anomalies
ou.best = data.last.summary %>%
    filter( 
        # meanValue > median( meanValue ) ,
        n >= mean_n 
        , MhAPE <= mean_MhAPE 
        # , anomaly <= mean( data.last.summary$anomaly , na.rm = TRUE)  
        # , interpolate <= median( data.last.summary$interpolate , na.rm = TRUE ) 
        ) %>%
    distinct( orgUnit, data ) %>%
    ungroup

count( ou.best  , data )


# Model pre event period  ####
event.month = Month_Year("20203")
                          
preMarch = ts %>%
    filter( Month < event.month ) %>%
    semi_join( ou.best , by = c( 'orgUnit', 'data' ) ) 

postMarch = ts %>% 
    filter( Month >= event.month  ) %>%
    semi_join( ou.best , by = c( 'orgUnit', 'data' ) ) 

# is this same/similar to fms from modeling whole dataset?

# Describe cohort
preMarch %>% as_tibble() %>%
    group_by( orgUnit , data ) %>% 
    summarise( value = mean( value , na.rm = TRUE )) %>%
    group_by( data ) %>%
    summarise( 
        n = n() ,
        median = median( value , na.rm = TRUE ) ,
        mean = mean( value , na.rm = TRUE ) ,
        sd = sd( value , na.rm = TRUE )
        )
# All facilities (pre-event)
ts %>%
    filter( Month < event.month ) %>%
  as_tibble() %>%
    group_by( orgUnit , data ) %>% 
    summarise( value = mean( value , na.rm = TRUE )) %>%
    group_by( data ) %>%
    summarise( 
        n = n() ,
        median = median( value , na.rm = TRUE ) ,
        mean = mean( value , na.rm = TRUE ) ,
        sd = sd( value , na.rm = TRUE )
        )


ous = unique( ou.best$orgUnit )
n = length( ous )
p <- progress::progress_bar$new( total = n)
tic()

  pre.model = future_map( 1:n , ~ts_model( ts =  preMarch %>%
                    filter( name %in% 'SUM' ,
                            orgUnit %in% ous[.x] ) ,
                  pb = p ) 
  )
toc()
pre.model = data.table::rbindlist( pre.model )



# Forecast vs actual  ####

tic()
expected =  map( 1:nrow(pre.model), ~pre.model[ .x , 'arima'][[1]] %>% 
                    forecast( h = '5 months' ,
                              point_forecast = list(.median = median ) ) %>%
                    # generate(h = 30, times = 5, bootstrap = TRUE) %>%
                    extract2(1) %>%
                    mutate( orgUnit = pre.model$orgUnit[.x]  ,
                            data = pre.model$data[.x]) 
)
toc() 

expected = data.table::rbindlist( expected ) 
# saveRDS( expected , paste0( dir, 'Malawi OPD Attendance expected.rds') )
# expected = readRDS( paste0( dir, 'Malawi OPD Attendance expected.rds') )


actual_expected = expected %>%
    select( -value ) %>%
    inner_join( postMarch %>% filter( name %in% 'SUM' ) %>%  
                    rename( actual = value ) , 
                by = c("Month", "orgUnit", "data")
    ) %>%
    # right_join( preMarch %>% filter( name %in% 'SUM' )  , 
    #             by = c("Month", "orgUnit", "data")
    #             ) %>%
    mutate( pct.change = ( actual - .median  ) / .median   ) %>% 
    as_tsibble( key = c(orgUnit, data), index = Month ) 


change.weighted %>% autoplot( mean.pct.change ) + 
    # geom_ribbon( aes(ymin = resid - 2*se, ymax = resid + 2*se) , 
                 # color = 'grey', alpha = .2 ) + 
    facet_grid( ~ data , scales = 'free') +
    scale_y_percent()
  
# mean change in *pre-event* period 

pre.model.data = pre.model %>%
  mutate( 
    fit = map( arima ,  ~.x %>% extract2(1) %>% extract2(2) ) ,
    dts = map( arima ,  ~.x %>% extract2(3) %>% as_tibble ) 
  ) %>%
  select( -arima )

pre.model.fitted   = 
  map( 1:nrow( pre.model.data ) , 
          ~ if( nrow( unnest( pre.model.data[.x, ], fit ) ) > 1  ){
            bind_cols( unnest( pre.model.data[.x, ], dts ) ,
                      unnest( pre.model.data[.x, ], fit ) %>%
                        select( -data, -orgUnit, -name ) )%>% 
              select( -fit, -dts  )
          } 
) %>% data.table::rbindlist() %>%
  mutate(
     pct.change = ( value - .fitted  ) / .fitted   
  )

  
pre.model.change =   pre.model.fitted %>%
  as_tsibble( key = c(orgUnit, data) , index = Month ) %>% 
  group_by( data ) %>%
  summarise(
        mean.pct.change = weighted.mean( pct.change , .fitted , na.rm = TRUE ) ,
        sd.pct.change = weighted.var.se( pct.change , .fitted , na.rm = TRUE )^.5 
    ) 

change = bind_rows( change.weighted  %>% as.tibble() , 
                    pre.model.change %>% as.tibble() ) %>%
  as_tsibble( key = data , index = Month )


pred.interval = 1.28 #(80%)

change %>% autoplot( mean.pct.change , color = 'red') +
    facet_wrap( ~ data , scales = 'free' , ncol = 1) +
    # facet_wrap(. ~ data  ) +
    theme_ipsum() +
    scale_y_percent( 
      # limits = c(-1, 1.5),
      breaks = seq(-1,1,.1) ) +
    theme( legend.position = "none") +
    geom_hline( yintercept = 0 , color ='blue', alpha =.25 ) +
    geom_rect( data = summary.r , inherit.aes = FALSE ,
               aes(
                      ymin = pred.interval*sd ,
                      ymax = -pred.interval*sd
                      ) ,
                      xmin = min( r$Month ) %>% as.Date() ,
                      xmax= max( r$Month ) %>% as.Date() ,
               alpha = .1
    ) +
      # annotate("rect", xmin = min(r$Month), xmax = max(r$Month), 
      #        ymin = avg.pre.eval - 1*se.pre.eval , 
      #        ymax = avg.pre.eval + 1*se.pre.eval,
      #   color = 'gray' , alpha = .2) + 
    annotate("text", x =event.month %>% as.Date(),
             y = .12 , label = 'March 2020' , 
             vjust = 0 ) +

  
    theme(panel.grid.minor = element_blank() ) +
    geom_vline( xintercept  = event.month %>% as.Date(), color = 'blue') + 
    labs( title = 'Malawi OPD Attendance: Deviation from expected'
          , subtitle =  'Key: Red line is mean deviation (weighted by expected value). \nGrey rectange indicates range with 80% confidence interval of mean prior to March 2020'
          , caption = 'Malawi DHIS2'
    )



