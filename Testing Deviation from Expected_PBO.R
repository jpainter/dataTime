
# Libraries and functions ####
library(pacman)
p_load( officer , rvg ,tidyverse, tidyfast, tidytable, lubridate , readxl, patchwork, 
        tsibble, fable, fabletools, feasts, slider, anomalize, 
        furrr, tictoc, magrittr, hrbrthemes, scales , data.tree, igraph )


# Functions
source( '../dataDictionary/TS_Modeling_Functions.r')
# source("Model_TS.R") # new version of model.ts()
source("Summary_TS.R") # new version of summary_ts()
source('TS_model_outlier_impute.R') # model_ts()
source( 'Deviation_Expected_Functions.R')
source( 'theme_ppt.R')

Month_Year = function( x ){ yearmonth( zoo::as.yearmon( x , "%Y%m") ) }

options(dplyr.summarise.inform = FALSE)

# powerpoint slide (officer)
 create_pptx <- function(plot, path){
   
      if( !file.exists(path) ) {
          doc <- read_pptx()
      } else {
          doc <- read_pptx(path)
      }
      
      doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
      my_vec_graph <- plot # dml(code = barplot(1:5, col = 2:6))
      doc <- ph_with(doc, my_vec_graph , location = ph_location_fullsize() )
      print(doc, target = path )

  }
  

# SETUP ####
country = "Malawi"
subject = 'confirm'
subset = '.' # default, ".", includes everything
adjust = TRUE 

dir = file.dir( country ) 

event.month = Month_Year("201811") 
# 'pre' period is before this month
forecast_horizon = '12 months' 
intervention_assignment = TRUE


anomaly.alpha1 = .01
anomaly.alpha2 = .01

run_previous = FALSE
start = Sys.time()
warningStart = getOption("warn")

# Parallel modeling  
# plan("multiprocess", workers = 3 )

# options(warn=-1)

# Formulas ####
dir = file.dir( country )
formula.file = files( 'Formula' , country = country )[1]  
formulas = read_excel( paste0( dir , formula.file ) , sheet = 'Formula Elements' ) 
# View(formulas)

# Data files ∫####

# files( 'meta' , country = country ) 
# files( search = subject , country = country , other = "All Levels")

data.file = files( search = subject , country = country , other = "All Levels" )[1]

data = read_excel( paste0( dir , data.file ) , sheet = 'formulaData' )  %>%
  filter( ! is.na( level ) )

# OUS structure ####
meta.file = files( 'meta' , country = country  )[1]
ous = read_excel( paste0( dir,  meta.file ) , sheet = 'OrgUnits')
ous.levels = read_excel( paste0( dir,  meta.file ) , sheet = 'OrgUnitLevels')

ous.id_parent = ous %>% select( id, parent )
ous.id_parent[ is.na( ous.id_parent$parent) , 'parent'] = country
ous.tree = FromDataFrameNetwork( ous.id_parent )

ou_leaves = ous.tree$Get("isLeaf")
ous.leaves = tibble( id = attributes(ou_leaves)$name , 
                      tree.leaf = ou_leaves ) %>%
             inner_join( ous , by = 'id')

count( ous.leaves , leaf, tree.leaf )
count( ous.leaves, level , levelName , tree.leaf ) %>% 
  pivot_wider( names_from = tree.leaf , values_from = n ) %>%
  arrange( level )
  
# hierarchcal transform from data ####

ous.parent.child = data %>% select( orgUnit ) %>%
  distinct( orgUnit) %>%
  inner_join( ous %>% select( id, parent ) , by = c( 'orgUnit' = 'id') ) %>%
  filter( !is.na( parent ))

data.tree <- FromDataFrameNetwork( ous.parent.child )

# averageBranchingFactor( data.tree )

data.leaves = data.tree$Get("isLeaf")
data.leaves = tibble( id = attributes(data.leaves)$name , 
                      effectiveLeaf = data.leaves )

  saveRDS( data.leaves , paste0( dir, 'data.leaves.rds') , compress=FALSE )
  # data.leaves = readRDS( paste0( dir, 'data.leaves.rds') )

# find region (level = 2 ) for each leaf node
if ( run_previous ){ 
  paths = readRDS( paste0( dir, 'ou_paths.rds') )
} else {
  dti = as.igraph.Node( data.tree )
  
  leaf.node.ids = data.leaves %>% filter( effectiveLeaf ) %>% pull(id)
  
  # sllllooowww
  paths = map_df(  leaf.node.ids  , ~{
    p = all_simple_paths(dti, .x  , 1)[[1]]             
    as_ids( p ) %>% rev %>% t %>% 
      as_tibble() %>% 
      set_colnames( ous.levels$levelName[ 1:length(p) ] ) %>%
      mutate( id = .x ) %>%
      select( id, everything() )
    }
) 

  # update with level 2 names (fast!)
  paths[,3] = ous[ match( paths %>% pull(3) , ous$id ) ,]$name
  
  saveRDS( paths , paste0( dir, 'ou_paths.rds') , compress=FALSE )
  # paths = readRDS( paste0( dir, 'ou_paths.rds') )
}

# link data with hierarchy ####
data = data %>%
  left_join( data.leaves , by = c( 'orgUnit' = 'id') ) 
 
count( data , leaf , effectiveLeaf ) 
count( data %>% group_by( level , levelName, orgUnit ) %>% summarise( effectiveLeaf = unique( effectiveLeaf ) ) , 
       level , levelName , effectiveLeaf ) %>% 
  pivot_wider( names_from = effectiveLeaf , values_from = n ) %>%
  arrange( level )

# prepare leaf and non-leaf datasets as time-series (effectiveLeaf) ####
 
  ts.leaf = df_pre_ts( data  )  %>%
    filter( effectiveLeaf == TRUE  )

  ts.nonLeaf = df_pre_ts( data  )  %>%
    filter( effectiveLeaf == FALSE  )

  rm( ous.parent.child , data.leaves, data.tree, ous.tree  ); gc()

# Confirm  totals of ts.leaf with ts.nonLeaf level 1 ####
  ts.leaf %>% group_by( year = year(Month)  ) %>% summarise( raw = sum( raw ) )
  ts.nonLeaf %>% group_by( year = year(Month)  ) %>% filter( level == 1 ) %>% summarise( raw = sum( raw ) )
  
  
# ts.nest (deprec)  
  # tic()
  # ts.nest = ts %>% 
  #   # as_tibble %>% 
  #   select( orgUnit, data, Month , value , raw) %>%
  #   # group_by( orgUnit, data, name ) %>%
  #   nest( ts = c(-orgUnit, -data ) )
  # toc() 
  # cat( paste( 'ts.nest completed ' ) )

# Current facilities reporting data ####

  ts.leaf %>% group_by( Month , data ) %>%
    summarise( n = n() ) %>%
    ggplot( aes( x = Month , y = n , group = data ,
                 color  = data ) ) +
    geom_line() +
    facet_wrap( ~data ) %>%
    labs( title = "N: number of reports with data available" )
  
  # should be identical plot
  ts.nonLeaf %>% 
    filter( level ==1 ) %>% 
    ggplot( aes( x = Month , y = COUNT , group = data ,
                 color  = data ) ) +
    geom_line() +
    facet_wrap( ~data ) %>%
    labs( title = "COUNTS: number of reports with data available")
  
  
# Vector of  facilities reporting 'last month', ous.lasmonth ####

months = ts.leaf %>% as_tibble %>% count( Month , data )  %>% 
    pivot_wider( names_from = data , values_from = n ) %>%
    arrange( desc( Month ) )

# Inspect Months to set reasonable endpoint for time-series
last.month = months[ 1 , 'Month' ]

ous.lastmonth = ts.leaf %>%
    filter(  Month %in% last.month ) %>%
    pull( orgUnit ) %>% unique  # n= 1711

# number of unique facilities reporting
ts.leaf %>% filter(  Month %in% last.month ) %>% 
  summarise( orgUnits = n_distinct( orgUnit ) )

# Visualize ####
  
l = 1
  
d = ts.nonLeaf %>% 
    filter( level == l ) %>%
    filter( Month >= Month_Year( "201601" ) 
            , Month <= Month_Year( "202008" )
            ) %>%
    # pivot_longer( cols = c( COUNT, SUM ) , names_to = 'name' ) %>% 
    mutate( value = as.numeric( value ) )  
  

  title. = paste0( subject, "_",
                  ifelse( subset %in% "." ,  "", paste0( subset, "_" ) ), 
                  'Level-' , l 
  )
  
 v =  d %>% 
    group_by( Month, orgUnit )  %>%
    summarise( value = sum( value ) ) %>%
    # autoplot( value ) + 
    ggplot( aes( x = Month, y = value , 
                 color = orgUnit , group = orgUnit ) ) +
    geom_line( size = 1.5 ) +
    scale_alpha_continuous() +
    # scale_y_continuous( labels = unit_format(unit = "M", scale = 1e-6)) +
    theme_ppt(  grid = FALSE  ) + 

    theme(legend.position = "none" ) +
    # facet_grid(name ~ data  , scales = 'free') +
    labs( title = title. ,  
          subtitle = country ,  #'dots highlight outliers' ,
          x='' , y='' , 
          caption = "source: DHIS2")
  v
  ggsave( paste0( "plots/" , title. , ".png" ), width = 12, height =7)
  

  create_pptx( v, paste0( country , "_covid_deviation.pptx" ) )
  
## Visualize Level 1 Outliers ####

chart_outliers = function( dataset,  l = 1 ){
  
  d = dataset %>% 
    filter( level == l )   %>%
    mutate( name = 'Original')

  d.outlier =  anomalize( d  , 
                          value , alpha = anomaly.alpha1 )   %>% 
    mutate( outlier = anomaly %in% 'Yes' )

  d. = d.outlier %>% 
               filter(  !outlier  ) %>%
               mutate( name = 'Revised' ) %>% 
    # bind_rows( d ) %>% 
    as_tsibble( key = c( orgUnit, data , name ) , index = Month ) %>%
    fill_gaps() %>%
    filter( !Month %in% max(Month) ) # remove last month

  
  m = model( d., arima = ARIMA( value  ) )

  m.data = m %>% augment() %>% 
    mutate( err = .resid / .fitted ,
            name = 'Revised' ) %>%
    anomalize( err  , alpha = anomaly.alpha2  )   %>%
    as_tsibble( key = c( orgUnit, data, name ) , index = Month ) %>%
    bind_rows( d  )   
  
  m.hasAnomaly = m.data %>% as_tibble() %>%
    mutate( name = 'Revised' ) %>%
    group_by( orgUnit, data, name  ) %>%
    summarise( has.anomaly = if ( any(anomaly %in% 'Yes') ){ TRUE }else{ FALSE }   ) %>%
    ungroup

  
  units = length(unique(m$orgUnit))
  if( units > 1 ){
    
    total2 = m.data %>% as_tibble() %>%
      filter( name %in% 'Revised' ) %>%
      mutate( value = ifelse( anomaly %in% 'Yes' , NA , value ) ,
              orgUnit = 'Total' , 
              name = 'Total_(2)' ) %>%
      group_by( orgUnit , data,  Month ) %>% 
      summarise( value = sum( value , na.rm = TRUE ))  %>%
      ungroup %>%
      as_tsibble( key = c( orgUnit, data ) , index = Month ) 
    
    # total with only the biggest outliers removed
    total1 = d. %>% as_tibble() %>%
      mutate( 
              orgUnit = 'Total' , 
              name = 'Total_(1)' ) %>%
      group_by( orgUnit, data, Month ) %>% 
      summarise( value = sum( value , na.rm = TRUE ))  %>%
      ungroup %>%
      as_tsibble( key = c( orgUnit, data  ) , index = Month ) %>%
      filter( !Month %in% max(Month) )
    
    combined.data = bind_rows( total1, total2 , m.data ) %>% 
      as_tibble() %>%
      left_join( m.hasAnomaly %>% as_tibble , 
                 by = c( 'orgUnit', 'data', 'name'  ) ) %>%
      mutate( alpha.anomaly = ifelse( has.anomaly | is.na( has.anomaly )  , 1 , .5  ) ) %>%
      as_tsibble( key = c( orgUnit, data  ) , index = Month ) 
  
    } else {
    
        combined.data = m.data  %>% 
          mutate( alpha.anomaly = 1L )
  }

  combined.data %>% 
    # autoplot( value ) + 
    ggplot( aes( x = Month, y = value , 
                 color = orgUnit , group = orgUnit ) ) +
    geom_line( size = 1 ,
               # , aes( alpha = alpha.anomaly ) 
               ) +
    scale_alpha_continuous() +

    theme_ppt() +
    theme(legend.position = "none" ) +
    
    geom_point( data = filter( m.data , anomaly == 'Yes' ), 
                aes( shape = anomaly ) , color = 'black') +
    # scale_shape_manual( values = c('No' = NULL , 'Yes' = 'triangle' ) ) +
    facet_grid(name ~ data  , scales = 'free') +
    labs( title = paste( 'Level-' , l , '(No. orgUnits =' , units , ")") ,  
          subtitle = 'dots highlight outliers' ,
          x='' , y='')
    
}

chart_outliers( ts.nonLeaf , 1 )
  
create_pptx( chart_outliers( ts.nonLeaf , 1) , 
             paste0( country , "_covid_deviation.pptx" )
)
  

# 
# ggsave( paste0( "plots/" , title. , ".png" ), width = 12, height =7)

# chart_outliers( 2 )
# chart_outliers( 3 )
# Flag and remove !st pass anomalies in leaf units (ts.nest)

# Anomaly1: 1st Pass anomalies : remove outliers...  ####

  
  ts.leaf.nest = ts.leaf %>%
    as_tibble %>%
    rename( var = data ) %>%
    group_by( orgUnit, var ) %>%
    nest()


  # Seasonal decomposition (when >24 obs) and anomaly detection
  tic()
  n = nrow( ts.leaf.nest )
  p <- progress::progress_bar$new( total = n)
  ts.anomaly1 =
    future_map( 1:n , ~ suppressWarnings( 
        
      anomaly_search( ts.leaf.nest[.x,] , raw , 
                      alpha = anomaly.alpha1 , 
                      seasonal = FALSE ,  
                      min.months = 24 ,
                      hampel = FALSE , 
                      hampel.mad = 5 ,
                      hampel.halfWidth = 5 ,
                      pb = NULL ) 
      )
      , .progress = TRUE 
        ) 
  
    ts.anomaly1 = data.table::rbindlist( ts.anomaly1 ) %>%
      ungroup() %>%
      mutate(
        anomaly1_value  = ifelse( anomaly1 , observed, NA ) ,
        value  = ifelse( anomaly1 , NA , observed ) 
              ) %>%
     select( orgUnit, var, Month, observed, anomaly1,  
             # season, trend , remainder, 
             everything()  )
  toc()
  
  anomaly1.file = paste0( dir, country , "_" , subject, "_anomaly1.rds" )
  saveRDS( ts.anomaly1 , anomaly1.file , compress = FALSE )
  # ts.anomaly1 = readRDS( anomaly1.file )
  
  # View( ts.anomaly1 )
  summary( ts.anomaly1$anomaly1_value )
  count( ts.anomaly1, anomaly1  )
  summary( ts.anomaly1$value )
  

  # Test for values in same category being the same--esp if >1000
 ts.anomaly1. = ts.anomaly1 %>%
    group_by( orgUnit, Month ) %>%
    arrange( orgUnit, Month , observed ) %>% 
    mutate( anomaly.dupe = 
              !anomaly1 &&
              observed >= 100 && 
              ( observed == lag( observed , default = FALSE ) | 
                observed == lead( observed , default = FALSE  ) 
                ) 
              ) %>% 
    ungroup %>%
    mutate( 
            anomaly1 = ifelse( anomaly1 | anomaly.dupe , TRUE, FALSE ) , 
            anomaly1_value  = ifelse( anomaly1 , observed, NA ) ,
            value  = ifelse( anomaly1 , NA , observed ) 
            )
  
  count( ts.anomaly1., anomaly1 , anomaly.dupe )
  summary( ts.anomaly1.$anomaly1_value )
  # View( ts.anomaly1.. )

  # After the fact, remove the little anomalies
  ts.anomaly1.. = ts.anomaly1. %>%
    mutate( anomaly1 = ifelse( anomaly1 & abs( observed - (l1+l2)/2  )>=10, TRUE , FALSE ) ,
            anomaly1_value  = ifelse( anomaly1 , observed, NA ) ,
            value  = ifelse( anomaly1 , NA , observed ) 
              )
  
  count( ts.anomaly1.., anomaly1 , anomaly.dupe )
  summary( ts.anomaly1..$anomaly1_value )
            
# Summary of outliers (1) ####
  
ts.leaf.revised1 = ts.anomaly1.. %>%
    as_tsibble( index = Month, key = c( orgUnit, var ) ) %>%
    fill_gaps() 

  ts.leaf.revised1.file = paste0( dir, country , "_" , subject, "_revised1.rds" )
  saveRDS( ts.leaf.revised1 , ts.leaf.revised1.file , compress = FALSE )
  # ts.leaf.revised1 = readRDS(ts.leaf.revised1.file )
    
if ( sum( ts.leaf.revised1$anomaly1 , na.rm = TRUE ) > 0 ){
  
  ts.leaf.revised1  %>%
    index_by( Year = year( Month )) %>%
    # group_by( anomaly1 ) %>% 
    summarise(
          n= n() ,
          orgUnits = n_distinct( orgUnit ) ,
          an_anomaly = sum( !is.na(anomaly1_value ) ) ,
          pct_has_anomaly1 = an_anomaly / n ,
          missing = sum( is.na( value ) & is.na( anomaly1_value )) / n() ,
          pct_anomaly1 = sum(anomaly1_value, na.rm = TRUE )/sum(value, na.rm = TRUE )
  )
  
  # how many outliers in any one orgUnit?
  ts.leaf.revised1 %>%
    as_tibble() %>%
    filter( anomaly1 ) %>%
    count( orgUnit ) %>%
    arrange( desc(n) )

}
  
  

# Plot: sample facility data  ####
 sample.ous = ous.lastmonth[10:15] 
  
 o = ts.leaf.revised1 %>% 
  filter( orgUnit %in% sample.ous ) %>% 
  autoplot( observed  ) + 
  facet_wrap( ~ var , scale = 'free' ) +
  guides(color=FALSE) +
  theme_ppt() +
  scale_x_yearmonth( date_breaks = '2 years' ) +
  labs( title = 'Original data')
  
  create_pptx( o , paste0( country , "_covid_deviation.pptx" ) )
  
 r = ts.leaf.revised1 %>% 
  filter( orgUnit %in% sample.ous ) %>% 
  autoplot( value  ) + 
  facet_wrap( ~ var , scale = 'free' ) +
  guides(color=FALSE) +
  theme_ppt() +
  scale_x_yearmonth( date_breaks = '2 years' ) +
  labs( title = 'Extreme outliers removed')

   create_pptx( r , paste0( country , "_covid_deviation.pptx" ) )

# Spare RAM, delete objects ####
  # remove 'data' to clear space 
  rm( data , ts.anomaly1, ts.anomaly1., ts.anomaly1.. , ts.nonLeaf , 
      ts.leaf , ts.leaf.nest ); gc()
# Initial Model of ts.leaf.revised1 #####
  
ous = unique( ts.leaf.revised1$orgUnit ) 
n = length( ous )

# Initial_model: ts_model ####
if ( run_previous ){ 
  
  model.files = files( subject , country = country , type = ".rds")
  select_initial = str_detect( model.files, 
                               fixed("initial" , ignore_case = TRUE )  
                               ) 
  initial_model_file = paste0( dir, subject , "_Initial_model_" , ".rds")
  initial_model = readRDS( initial_model_file )
  
} else {
    p <- progress::progress_bar$new( total = n)
    tic()
      initial_model = future_map( 1:n , 
                          ~ ts_model( ts =  ts.leaf.revised1 %>%
                                        filter( orgUnit %in% ous[.x]  ) 
                                     , pb = NULL
                                     ) 
                      , .progress = TRUE
      )
    toc()
    initial_model = data.table::rbindlist( initial_model )
    glimpse( initial_model )
    # 
    # object.size( initial_model ) %>% as.numeric() %>% scales::comma()
    # object.size( initial_model %>% select(-arima)  ) %>% as.numeric() %>% scales::comma()
    # 
    # tic(); initial_model = initial_model %>% select(-arima) %>% pull( ts ) %>% data.table::rbindlist() ; toc()
    # object.size( b ) %>% as.numeric() %>% scales::comma()
    initial_model_file = paste0( dir, subject , "_Initial_model_" , ".rds")
    saveRDS( initial_model , initial_model_file ,  compress=FALSE )

}

# Initial_model.fit: Extract data and fit from initial model ####

ts_model_fit = function( x , pb = NULL ){
  
   if ( !is.null( pb ) ) pb$tick()
  
    ou = initial_model[ x, ]$orgUnit
    var = initial_model[ x, ]$var 
            
    a = initial_model[ x, ]$arima %>% extract2(1) 
            
    fit = a$fit$est 
    dts = a$data 
    # spec = a$fit$spec #pdqPDQ
            
     if( is.null( fit ) ){ fit = tibble( .fitted = NA, 
                                                .resid = NA ,
                                                .regression_resid = NA )
            }
            
            d = bind_cols( orgUnit = ou, var = var, fit, dts ) %>%
                  mutate(  
                        value = exp( `log(value + 1)` ) - 1 ,
                        .fitted = exp( .fitted ) - 1 ,
                        .fitted = ifelse( .fitted < 0, 0 , .fitted ) ,
                        pct.change =  ifelse( value == 0 ,
                                              ifelse( .fitted == 0 , 0 , .fitted ) ,
                                              .fitted / value  - 1 )
                        )
      return(d) 
} 

tic()
n= nrow( initial_model )
p <- progress::progress_bar$new( total = n)

initial_model.fit   = 
  
  map( 1:nrow( initial_model ) , 
            
          ~ ts_model_fit( .x, pb = p ) 
  ) %>% 
    data.table::rbindlist() 
toc()

# glimpse(initial_model.fit)
# View(initial_model.fit)

rm( initial_model ) ; gc()
# rm( initial_model.data) ; gc()


initial_consistency = initial_model.fit %>%
  group_by( orgUnit , var  ) %>%
  summarise(
    hMAPE = harmonic_mean( abs( pct.change ) ) ,
    gMAPE = exp( mean( log( abs( pct.change ) )  ) ) ,
    n  =  sum( !is.na( value ) ) ,
    model = !all( is.na( .fitted ) )  , 
  ) %>%
  group_by( var  ) 

summary( initial_consistency$hMAPE )
summary( initial_consistency$gMAPE )

initial_consistency.summary = initial_consistency %>%
  group_by(  var  ) %>%
  summarise(
    n_orgUnit = n_distinct( orgUnit ) ,
    n_model = sum( model ) ,
    mean_n = mean( n ) ,
    mean_hMAPE = mean( hMAPE , na.rm = TRUE ) ,
    median_n = median( n ) ,
    median_hMAPE = median( hMAPE , na.rm = TRUE ) ,
    quartile_n = quantile( n )[3] ,
    quartile_hMAPE = quantile( hMAPE , na.rm = TRUE )[2]
  )

# initial_consistency.summary %>% View

# Initial.best - most  consistent of modeled ts.leaf.revised1   ####

initial.best =  initial_consistency  %>%
    filter( model ) %>%
    inner_join( initial_consistency.summary , by = c( 'var' )) %>%
    filter( 
        # meanValue > median( meanValue ) ,
        ( n >= mean_n & hMAPE <= mean_hMAPE )  |  n_model< 5 
        # , anomaly <= mean( data.last.summary$anomaly , na.rm = TRUE)  
        # , interpolate <= median( data.last.summary$interpolate , na.rm = TRUE ) 
        ) %>%
    distinct( orgUnit, var ) %>%
    ungroup

count( initial.best  , var )


## Anomaly2: Post-model Anomalies  ####

# TODO: This calculation should consider difference between outlier and imputed value, 
# not full value of outlier 


ts.initial.best.nest = initial_model.fit %>%  
    semi_join( initial.best , by = c("orgUnit", "var") ) %>% 
    # as_tsibble( key = c( orgUnit, var ) , index = Month ) %>%
    group_by( orgUnit, var ) %>%
    nest()

n.best = nrow( ts.initial.best.nest )

tic()
pb <- progress::progress_bar$new( total = n.best )
ts.anomaly2 =
    map_df( 1:n.best, ~ suppressWarnings( 
        
      anomaly_search( ts.initial.best.nest[.x,] , .resid , 
                      alpha = anomaly.alpha2 , 
                      seasonal = TRUE ,  
                      min.months = 24 ,
                      hampel = TRUE , 
                      hampel.mad = 5 ,
                      hampel.halfWidth = 5 ,
                      pb = pb )
      ) 
        ) %>%
      rename( anomaly2 = anomaly1 ) %>%
      ungroup() %>%
      mutate(
        anomaly2_value  = ifelse( anomaly2 , observed, NA ) 
        # , value2  = ifelse( anomaly2 , NA , value ) 
              ) %>%
     select( orgUnit, var, Month, observed, anomaly2,  
             # season, trend , remainder, 
             l1, l2 , anomaly2_value )
toc()

# View( ts.anomaly2 %>% as_tibble )
sum( ts.anomaly2$anomaly2 )
sum( ts.anomaly2$anomaly2 ) / nrow( ts.anomaly2 )
summary( ts.anomaly2$anomaly2_value)

# ts.revised2: Summary of outliers (2) ####

ts.revised2 = 
  initial_model.fit %>%  
  # semi_join( initial.best , by = c("orgUnit", "var") ) %>% 
  left_join( ts.anomaly2 %>% select( orgUnit, var, Month, anomaly2  ), 
             by = c('orgUnit' , 'var', 'Month') ) %>%
  left_join( ts.leaf.revised1 %>% select( orgUnit, var, Month , observed , anomaly1) ,
             by = c('orgUnit' , 'var', 'Month') ) %>%
  mutate( 
    value  = ifelse( anomaly1 | anomaly2 , NA , observed )  
    ) %>%
  as_tsibble( index = Month, key = c( orgUnit, var )) %>%
  fill_gaps()

glimpse( ts.revised2 )  
# View( ts.best.revised2 )

saveRDS( ts.revised2 , paste0( dir, subject , '_ts.revised2.rds') )
# ts.revised2 = readRDS( paste0( dir, subject , '_ts.revised2.rds')   )


ts.revised2 %>%
  index_by( Year = year( Month )) %>%
  # group_by( var ) %>%
  summarise(
          orgUnits = n_distinct( orgUnit ) ,
          pct_has_anomaly2 = sum( anomaly2, na.rm = TRUE  )  / n() ,
          missing = sum( is.na( observed ) ) / n() ,
          pct_anomaly2 = 1- sum( value , na.rm = TRUE )/ sum( observed , na.rm = TRUE ) 
  ) 

ts.revised2_consistency = ts.revised2 %>%
  as_tibble() %>%
  group_by( orgUnit , var  ) %>%
  summarise(
    hMAPE = harmonic_mean( abs( pct.change ) ) ,
    gMAPE = exp( mean( log( abs( pct.change ) )  ) ) ,
    n  =  sum( !is.na( value ) ) ,
    model = !all( is.na( .fitted ) )  , 
  ) 

summary( ts.revised2_consistency$hMAPE )
summary( ts.revised2_consistency$gMAPE )

ts.revised2_consistency.summary = 
  ts.revised2_consistency %>%
  group_by( var ) %>%
  summarise(
    n_orgUnit = n_distinct( orgUnit ) ,
    n_model = sum( model ) ,
    mean_n = mean( n ) ,
    mean_hMAPE = mean( hMAPE , na.rm = TRUE ) ,
    median_n = median( n ) ,
    median_hMAPE = median( hMAPE , na.rm = TRUE ) ,
    quartile_n = quantile( n )[3] ,
    quartile_hMAPE = quantile( hMAPE , na.rm = TRUE )[2]
  )

ts.revised2_consistency.summary %>% View

## Impute

# median_n = median( ts.leaf.summary$n ) 
# mean_n = mean( ts.leaf.summary$n ) 
# median_hMAPE = median( ts.leaf.summary$hMAPE , na.rm = TRUE )
# mean_hMAPE = mean( ts.leaf.summary$hMAPE , na.rm = TRUE )



# ts.best.revised2: Summary of outliers (2) ####

ts.best.revised2 = 
  initial_model.fit %>%  
  semi_join( initial.best , by = c("orgUnit", "var") ) %>% 
  left_join( ts.anomaly2 %>% select( orgUnit, var, Month, anomaly2  ), 
             by = c('orgUnit' , 'var', 'Month') ) %>%
  left_join( ts.leaf.revised1 %>% select( orgUnit, var, Month , observed , anomaly1) ,
             by = c('orgUnit' , 'var', 'Month') ) %>%
  mutate( 
    value  = ifelse( anomaly1 | anomaly2 , NA , observed )  ) %>%
  as_tsibble( index = Month, key = c( orgUnit, var )) %>%
  fill_gaps() 

glimpse( ts.best.revised2 )  
# View( ts.best.revised2 )

saveRDS( ts.best.revised2 , paste0( dir, subject , '_ts.best.revised2.rds') )
# ts.best.revised2 = readRDS( paste0( dir, subject , '_ts.best.revised2.rds')   )


ts.best.revised2 %>%
  index_by( Year = year( Month )) %>%
  summarise(
          orgUnits = n_distinct( orgUnit ) ,
          pct_has_anomaly2 = sum( anomaly2, na.rm = TRUE  )  / n() ,
          missing = sum( is.na( observed ) ) / n() ,
          pct_anomaly2 = 1- sum( value , na.rm = TRUE )/ sum( observed , na.rm = TRUE ) ,
          hMAPE = harmonic_mean( abs( pct.change ) )
  )


## Impute

# median_n = median( ts.leaf.summary$n ) 
# mean_n = mean( ts.leaf.summary$n ) 
# median_hMAPE = median( ts.leaf.summary$hMAPE , na.rm = TRUE )
# mean_hMAPE = mean( ts.leaf.summary$hMAPE , na.rm = TRUE )



# Visualize sample of best  ####
  
l = 1
  

  title. = paste( "Best of" , subject, "_" , subset , "_", 'Level-' , l 
                  # , '(No. orgUnits =' , units , ")"
  )
  
 v.best =  ts.best.revised2 %>%
    group_by( var )  %>%
    summarise( value = sum( value , na.rm = TRUE ) ) %>%
    # autoplot( value ) + 
    ggplot( aes( x = Month, y = value , 
                 color = var , group = var ) ) +
    geom_line( size = 1.5 ) +
    scale_alpha_continuous() +
    # scale_y_continuous( labels = unit_format(unit = "M", scale = 1e-6)) +
    theme_ppt(  grid = FALSE  ) + 
   
    facet_wrap( ~ var , scales = 'free') +

    theme(legend.position = "none" ) +
    # facet_grid(name ~ data  , scales = 'free') +
    labs( title = title. ,  
          subtitle = paste( "Country-X" ) ,  #'dots highlight outliers' ,
          x='' , y='' , 
          caption = "source: Country-X DHIS2")
  v.best
  
  ggsave( paste0( "plots/" , title. , ".png" ), width = 12, height =7)
  

  create_pptx( v, paste0( country , "_covid_deviation.pptx" ) )
  
  # year over year plot...
  
  
  


# Visual comparison of cohorts ####
ts.cohort = 
  ts.best.revised2 %>% 
  filter(  orgUnit %in% ous.lastmonth )  
  
rd = 
ts.leaf.revised1 %>% 
    select( orgUnit, var, Month, value , observed ) %>%
     rename( revised1 = value ) %>% 
     group_by( var ) %>%
     summarise_if( is.double , sum , na.rm = TRUE ) %>%
left_join(
   ts.revised2 %>% select( orgUnit, var, Month, value ) %>%
     rename( revised2 = value ) %>%
     group_by( var ) %>%
     summarise_if( is.double , sum , na.rm = TRUE ) ,
   by = c( 'var', 'Month' ) 
) %>%
left_join(
  ts.cohort %>% select( orgUnit, var, Month, value ) %>%
     rename( cohort = value ) %>% 
     group_by( var ) %>%
     summarise_if( is.double , sum , na.rm = TRUE ),
   by = c( 'var', 'Month' ) 
) %>%
  pivot_longer( c( observed, revised1, revised2, cohort ) )

saveRDS( rd , paste0( dir, subject , "_observed_v_cohort.rds" ) )
# rd = readRDS( paste0( dir, subject , "_observed_v_cohort.rds" ) )

rd.g = ggplot( rd, aes( Month, value , color = name, group = name ) ) +
  geom_line( ) +
  # ylim(0 , 5e+06 ) +
  theme_ppt() +
  facet_wrap( ~ var , ncol = 1 , scales = 'free', strip.position="top" )

rd.g
create_pptx( rd.g , paste0( country , "_covid_deviation.pptx" ) )
 
# year over year 
rdy = rd %>% as_tibble() %>%
  mutate( year = factor(  year(Month)  ) ,
          Month = factor( month( Month ) ) ,
          name = factor( name , 
                         levels = c('observed', 'revised1', 'revised2', 'cohort') ) 
          )
  
rdy.g = 
  ggplot( rdy %>%
            filter( str_detect( var, regex( subset , ignore_case = TRUE )) ), 
          aes( Month, value , color = year, group = year ) ) +
  geom_line( ) +
  # ylim(0 , 5e+06 ) +
  theme_ppt() +
  facet_grid( name  ~ var ,  scales = 'free')

rdy.g
create_pptx( rdy.g , paste0( country , "_covid_deviation.pptx" ) )
 
  ##### EVALUATION ######

ts.best.revised2 = readRDS( paste0( dir, subject , '_ts.best.revised2.rds')   )

# Covariates ####
if ( adjust ){
  
  arc = readRDS('malawi_arc.rds') %>%
    select( orgUnit , month, avg_month ) %>%
    rename( Month = month , rainfall = avg_month )
  
  paths = readRDS( paste0( dir, 'ou_paths.rds') )
  
  facility_district = 
    paths %>% 
    inner_join( ous.leaves , by = c( 'District' = 'id') ) %>%
    rename( District.id = District , 
            District = displayName ) %>%
    inner_join( ous.leaves , by = c( 'Facility' = 'id') ) %>%
    rename( Facility.id = Facility , 
            Facility = displayName ) %>%
    select( Facility.id ,Facility , District , District.id ) 
  
  arc_facility = arc %>% inner_join( facility_district , 
                                     by = c('orgUnit' = 'District.id')
                                     )
  
  
  dir = file.dir( country )
  stock.file = files( search = 'RDT' , country = country , other = "" )[1]
  stock = read_excel( paste0( dir , stock.file ) , 
                      sheet = 'formulaData' )  %>%
    filter( ! is.na( level ) ) %>%
    filter( grepl( 'Stock out days' , dataElement ) ) %>%
    select( orgUnit, period, SUM ) %>%
    mutate( Month = Month_Year( period ) ,
            SUM = as.numeric( SUM )
            ) %>%
    select( - period ) %>%
    rename( stockout = SUM ) %>%
    filter( stockout <= 31 )
  
  ts.best.revised2 = ts.best.revised2 %>%
    left_join( arc_facility , by=c( 'orgUnit'='Facility.id', 'Month' ) ) %>%
    mutate( previous.rainfall = lag( rainfall ) ) %>%
    left_join( stock, by=c( 'orgUnit', 'Month' ) )
  
  covariates = c('previous.rainfall', 'stockout' )
} 

# Define cohort and Split data into pre and post event periods  ####
months = ts.best.revised2 %>% as_tibble %>% count( Month , var )  %>% 
    pivot_wider( names_from = var , values_from = n ) %>%
    arrange( desc( Month ) )

# Inspect Months to set reasonable endpoint for time-series
last.month = months[ 1 , 'Month' ]

# ous.lastmonth = ts.best.revised2 %>%
#     filter(  Month %in% last.month ) %>%
#     pull( orgUnit ) %>% unique  # n= 1711

ous.lastmonth = ts.best.revised2 %>%
  filter(  Month %in% Month_Year(('201911')) ) %>%
    pull( orgUnit ) %>% unique
  
ts.cohort = 
  ts.best.revised2 %>% 
  filter(  orgUnit %in% ous.lastmonth )  
# saveRDS( ts.cohort , paste0( dir, 'ts.cohort.rds'))
# ts.cohort = readRDS( paste0( dir, 'ts.cohort.rds'))

print( event.month )
                          
preEvent= ts.cohort %>%
    filter( Month < event.month ) 

# glimpse( preEvent )


postEvent = ts.cohort %>% 
    filter( Month >= event.month  ) 

unique( postEvent$Month )

# Intervention - Cohort ####

if ( intervention_assignment ){
  intervention = 
    read_excel( 'MalawiPBO/PBO Monitoring in Malawi_last updated Nov 2018.xlsx' , 
                sheet = 'Districts List') %>%
    rename( District = `Region/ District Name` , 
            net = `Type of nets/Intervention` ,
            campaign = `Dates of mass campaign`  ) %>%
    rename( Intervention = net ) %>%
    filter( ! is.na( Intervention ) ) %>%
    mutate( 
      District = paste0( District , '-DHO' ) 
      ) %>%
    mutate(
      District = 
        case_when( 
          District == "Nkhata Bay-DHO" ~ "Nkhata-Bay-DHO" ,
          District == "Mzimba South-DHO" ~ "Mzimba-South-DHO" ,
          District == "Mzimba North-DHO" ~ "Mzimba-North-DHO" ,
          TRUE ~ District 
        ))
  
  glimpse( intervention )
  
  districts = read_excel( paste0( dir,  meta.file ) , sheet = 'OrgUnits') %>%
    filter( levelName %in% "District" ) %>%
    rename( District.id = id , District = name ) %>%
    select( District.id , District )

    
  Intervention_District =
  intervention %>%   
  inner_join( districts , by = 'District' )
    
  ous.intervention = paths %>% 
    inner_join( ous.leaves , by = c( 'District' = 'id') ) %>%
    rename( District.id = District , 
            District = displayName ) %>%
    inner_join( ous.leaves , by = c( 'Facility' = 'id') ) %>%
    rename( Facility.id = Facility , 
            Facility = displayName ) %>%
    select( Facility.id ,Facility , District , District.id ) %>%
    inner_join( Intervention_District , by = 'District.id' )
                  
  glimpse( ous.intervention )
  
  postEvent = postEvent %>%
    inner_join( ous.intervention %>% select( Facility.id , Facility, Intervention ), 
                by = c('orgUnit' = 'Facility.id') )
  
  preEvent = preEvent %>%
    inner_join( ous.intervention %>% select( Facility.id , Facility, Intervention ), 
                by = c('orgUnit' = 'Facility.id') )
}

# Intervention Map ####



# describe variable (e.g. attendance) for each cohort  ####

## All facilities
ts.leaf.revised1 %>% 
    as_tibble() %>% 
    group_by( var ) %>%
    summarise( 
       orgUnits  = n_distinct( orgUnit ) , 
       reports = n() ,
        meanN = reports / orgUnits ,
        mean_value = mean( value , na.rm = TRUE ) ,
        median_value = median( value , na.rm = TRUE ) ,
        sd = sd( value , na.rm = TRUE )
        )
## Best facilities
 ts.best.revised2  %>% 
  as_tibble() %>%
  # group_by( var ) %>%
  summarise( 
         orgUnits  = n_distinct( orgUnit ) , 
         reports = n() ,
          meanN = reports / orgUnits ,
          mean_value = mean( value , na.rm = TRUE ) ,
          median_value = median( value , na.rm = TRUE ) ,
          sd = sd( value , na.rm = TRUE )
          )
 
 ## Cohort facilities (filtered to ous.lastmonth )
 
ts.cohort  %>% 
  as_tibble() %>%
  group_by( var ) %>%
  summarise( 
         orgUnits  = n_distinct( orgUnit ) , 
         reports = n() ,
          meanN = reports / orgUnits ,
          mean_value = mean( value , na.rm = TRUE ) ,
          median_value = median( value , na.rm = TRUE ) ,
          sd = sd( value , na.rm = TRUE )
          )

# model pre-event best that are also in last month  ####


  model.files = files( subject , country = country , type = ".rds")
  select_pre = str_detect( model.files, 
                               fixed( "pre" , ignore_case = TRUE )  
                               ) 
  pre_model.file = model.files[ select_pre  ][1]
  pre.model = readRDS( paste0( dir, pre_model.file )   )

  ouss = unique( preEvent$orgUnit )
  n = length( ouss )
  p <- progress::progress_bar$new( total = n)
  covariates = c('stockout', 'previous.rainfall') 

  if ( run_previous ){
    pre.model = readRDS( paste0( dir, subject , "_pre_model_", 
                                event.month , ".rds" ) )
  } else {  
  tic()
    pre.ts_model = 
      # future_map( 1:n ,
      map( 1:n ,
              ~ ts_model( ts =  preEvent  %>%
                                         filter( orgUnit %in% ouss[.x] ) ,
                      augment = TRUE , 
                      covariates = covariates ,
                      pb = p ) 
                    # , .progress = TRUE
    )
  toc()
  
  pre.model.model = data.table::rbindlist( map( pre.ts_model, 1 ) )
  pre.model = data.table::rbindlist( map( pre.ts_model, 2 ) ) %>%
    mutate(  
      .fitted = ifelse( .fitted < 0, 0 , .fitted ) ,
      pct.change = ( ( value + 1 ) / ( .fitted  + 1 ) ) -1
      ) %>%
    nest( data = c(.model, Month, value, observed , {{covariates}} , 
                   .fitted, pct.change , .resid, .innov) 
          ) %>%
    inner_join( pre.model.model , by = c("orgUnit", "var") )
  
  saveRDS( pre.model ,  paste0( dir, subject , "_pre_model_", 
                                event.month , ".rds" ) , compress=FALSE   )
  
}

# Pre.model consistency ####
    
pre_consistency = pre.model %>%
  unnest( data ) %>%
  group_by( orgUnit , var  ) %>%
  summarise(
    hMAPE = harmonic_mean( abs( pct.change ) ) ,
    medianAPE = median(abs( pct.change ) , na.rm = TRUE  ) ,
    n  = sum( !is.na( value ) ) ,
    model = !all( is.na( .fitted ) )  , 
    total.fitted = sum( .fitted, na.rm = TRUE )
  ) %>%
  group_by( var  ) 

# Pr.model cohort description
pre_consistency.summary = pre_consistency %>%
  summarise(
    n_orgUnit = n_distinct( orgUnit ) ,
    n_model = sum( model ) ,
    mean_n = mean( n ) ,
    median_n = median( n ) ,
    mean_hMAPE = weighted.mean( hMAPE , total.fitted , na.rm = TRUE ) ,
    median_hMAPE = weighted.median( hMAPE , total.fitted ,na.rm = TRUE ) ,
    median_medianAPE = weighted.median( medianAPE , total.fitted , na.rm = TRUE ) ,
    quartile_n = quantile( n )[3] ,
    quartile_hMAPE = quantile( hMAPE , na.rm = TRUE )[2]
  )

# pre_consistency.summary  %>% View

pre.model %>%
   unnest( data ) %>%
    as_tibble() %>% 
    group_by( var ) %>%
    summarise( 
       orgUnits  = n_distinct( orgUnit ) , 
       reports = n() ,
       values = sum( !is.na( value ) ) ,
        meanN = reports / orgUnits ,
        meanV = values / orgUnits ,
        mean_value = mean( value , na.rm = TRUE ) ,
        median_value = median( value , na.rm = TRUE ) ,
        sd = sd( value , na.rm = TRUE )
        )


# pre.best - most  consistent of modeled ts.leaf.revised1   ####

#### WILL KEEP ALL PRE-MODEL - NOT US?ING PRE-MODEL BEST (sep 22 2020)
count( pre_consistency  , var )

pre.best =
  pre_consistency  %>%
  filter( model ) %>%
  inner_join( pre_consistency.summary , by = c('var' )) %>%
  filter( 
        # meanValue > median( meanValue ) ,
        # (n >= mean_n & hMAPE <= mean_hMAPE )  |  n_model< 10
        (n >= median_n & hMAPE <= median_hMAPE )  |  n_model< 10
        # (n >= median_n & medianAPE <= median_medianAPE )  |  n_model< 10
        # medianAPE <= median_medianAPE
        # (n >= median_n )  
    ) %>%
    distinct( orgUnit, var ) %>%
    ungroup

count( pre.best  , var )


pre.model.best = pre.model %>% 
  semi_join( pre.best, by = c("orgUnit", "var") )

#### WILL KEEP ALL PRE-MODEL - NOT US?ING PRE-MODEL BEST (sep 22 2020)

# visualise pre.model.best data  #####

  pre.model.best  %>%
    unnest( data ) %>%
    # semi_join( pre.best , by = c('orgUnit', 'var')) %>%
    filter( str_detect( var, regex( subset , ignore_case=TRUE )  ) ) %>%
    semi_join( pre.best[, ] , by = c("orgUnit", "var")) %>%  
    as_tsibble( key = c(orgUnit, var) , index = Month ) %>%
    autoplot( value , alpha = .4 ) +
    facet_wrap( ~ var , scales = 'free' ) +
    theme_bw() + 
    theme( legend.position = "none")  +
   labs( title = 'pre.best data')
    
# Kenya apparent outlier October 2017 in orgunit ALISP5DsvzD
 # View( preMarch %>% filter( orgUnit %in% 'ALISP5DsvzD') )
    
# Expected , post-event  ####

if ( run_previous ){ 
  
  subject.file = files( subject , country = country , type = ".rds") 
  # expected.file = subject.file[ str_detect( subject.file , fixed( 'expected', ignore_case = TRUE )) ] 
  expected.file = paste0( dir, subject , '_Expected_' , 
                                event.month , ".rds" )
  expected = readRDS( expected.file  ) 

  } else {
  
  
  tic()
  n = nrow( pre.model.best ) 
  pb <- progress::progress_bar$new( total = n )
    
  expected =  map( 1:n , 
                   ~{
                     if ( !is.null( pb ) ) pb$tick()
                     
                     nd = postEvent %>% 
                                  semi_join( pre.model.best[ .x ,] ,
                                              by = c("orgUnit", "var") ) %>%
                                  select( Month, orgUnit, var, {{covariates}} ) %>%
                       drop_na() %>% 
                       as_tsibble( key = c(orgUnit, var) , index = Month ) %>%
                       fill_gaps()
                     
                     e = pre.model.best[ .x , ]$arima[[1]] %>% 
                         forecast( 
                        # h = forecast_horizon
                        point_forecast = list(.median = median ) 
                        , new_data = nd 
                       ) %>%
                      # generate(h = 30, times = 5, bootstrap = TRUE) %>%
                      # extract2(1) %>%
                      mutate( orgUnit = pre.model.best$orgUnit[.x]  ,
                              var = pre.model.best$var[.x]) 
                     return(e)
                   }
  )
  
  expected = data.table::rbindlist( expected, fill=TRUE ) 

  toc() 
  

  saveRDS( expected , paste0( dir, subject , '_Expected_' , 
                                event.month , ".rds" ) , compress=FALSE )

  }

# plot expected 
expected %>% 
  as_tsibble( key = c( orgUnit, var ) , index = Month ) %>%
  # ggplot( aes( Month, .median, group = orgUnit )) +
  # geom_line() + 
  autoplot( .median  ) +
  guides( color = FALSE ) + 
  facet_wrap( ~var , scales = 'free')

# expected %>% 
#   as_tsibble( key = c( orgUnit, var ) , index = Month ) %>%   
#   autoplot( rainfall )

# post event actual_expected ####

## TODO craft confidence interval. 
## value column in expected is a distribution

actual_expected = expected %>%
    select( -value ) %>%
    inner_join( postEvent %>% 
                    rename( actual = value ) , 
                by = c("Month", "orgUnit", "var")
    ) %>%
    mutate( 
      expected = ifelse( .median < 0, 0 , .median ) ,
      pct.change = ( ( actual + 1 ) / ( expected  + 1 ) ) -1
      ) %>% 
    as_tsibble( key = c(orgUnit, var), index = Month ) %>%
    filter( Month <= event.month + 12 )

#### Subset / Intervention here (1) ####
  post.event.change = actual_expected %>%
    filter( str_detect( var, regex( subset , ignore_case = TRUE )  ) ) 
  
# review...
  post.event.change %>% 
    filter( var %in% 'NMCP OPD Confirmed Malaria Cases Through RDT_>5Yrs') %>%
    select( Month, pct.change ) %>%
    as_tibble() %>%
    mutate( Month = as.character(Month) ) %>%
    pivot_wider( names_from = Month , values_from = pct.change ) %>%
    View
  

  post.event.change.summary = 
        post.event.change %>%  
        as_tsibble( key = c(orgUnit, var) , index = Month ) %>% 
        group_by( var , Intervention ) %>%
        summarise(
              n_orgUnit = n_distinct( orgUnit ) ,
              mean.pct.change = weighted.mean( pct.change , expected , na.rm = TRUE ) ,
              sd.pct.change = weighted.var.se( pct.change , expected , na.rm = TRUE )^.5 
              , median.pct.change = weighted.median( pct.change , expected , na.rm = TRUE ) ,
              
              )  
  
  post.event.change.summary %>% View

# post.event Totals 
if ( length( unique( actual_expected$var) ) > 1 ){


  post.event.change.total = post.event.change %>%
  group_by( Intervention ) %>%
  summarise(
        mean.pct.change = weighted.mean( pct.change , .median , na.rm = TRUE ) ,
        sd.pct.change = weighted.var.se( pct.change , .median , na.rm = TRUE )^.5 
        , median.pct.change = weighted.median( pct.change , .fitted , na.rm = TRUE )
              
    )  %>%
  mutate( var = 'Total' )

  post.event.summary.total = bind_rows(post.event.change.summary %>% as_tibble() , 
                                post.event.change.total %>% as_tibble() ) %>%
  as_tsibble( key = c( var , Intervention ) , index = Month ) 
}
  

# plot post event change ####
post.event.summary.total %>% 
    # autoplot( mean.pct.change ) + 
    autoplot( median.pct.change ) +
    # ggplot( aes( Month, median.pct.change , group = var ) ) + geom_line() +
    # geom_ribbon( aes(ymin = resid - 2*se, ymax = resid + 2*se) , 
                 # color = 'grey', alpha = .2 ) + 
    facet_grid(  Intervention ~  string_wrap( var , nwrap = 25)  , 
                 scales = 'free',
                 # ncol = 1 , 
                 # strip.position="top" 
                 switch = 'both' ) +
    scale_y_percent(limits = c(-1,2)) +
    expand_limits( y = 0 ) + 
    theme( legend.position = "none" ) 
 

# Mean change in *pre-event* period ####

  pre.model.data = pre.model %>%
    select( -arima ) %>%
    unnest( data ) 
    
  
#### Subset / Intervention here (2) ####
  if( intervention_assignment ){
        pre.model.data = 
        pre.model.data %>%
        inner_join( ous.intervention %>% 
                      select( Facility.id , Facility, Intervention ), 
                by = c('orgUnit' = 'Facility.id') )
        } else { 
          post.event.change %>% mutate( Intervention = "." ) 
        } 
  
  pre.event.change.summary =   pre.model.data %>% 
    filter( str_detect( var, regex( subset , ignore_case = TRUE )) | str_detect( var, 'Total' ) ) %>%
    as_tsibble( key = c(orgUnit, var) , index = Month ) %>% 
    group_by( var, Intervention ) %>%
    summarise(
         n = n() ,
         mean.pct.change = weighted.mean( pct.change , .fitted , na.rm = TRUE ) ,
         sd.pct.change = weighted.var.se( pct.change , .fitted , na.rm = TRUE )^.5 ,
         median.pct.change = weighted.median( pct.change , .fitted , na.rm = TRUE ) 
      ) 

if ( length( unique( pre.model.data$var)) > 1 ){
  
  pre.event.change.total =  pre.model.data %>%
  as_tsibble( key = c(var, orgUnit) , index = Month ) %>%
  group_by( Intervention ) %>%
  summarise(
        mean.pct.change = weighted.mean( pct.change , .fitted , na.rm = TRUE ) ,
        sd.pct.change = weighted.var.se( pct.change , .fitted , na.rm = TRUE )^.5 ,
        median.pct.change = weighted.median( pct.change , .fitted , na.rm = TRUE ) 
    )  %>%
  mutate( var = 'Total' )
  
  pre.event.change.summary = bind_rows( pre.event.change.summary %>% as_tibble(), 
                                pre.event.change.total %>% as_tibble()
                                ) %>%
    as_tsibble( key = c(var, Intervention) , index = Month )
}

# Change ####
  
change = bind_rows( post.event.summary.total  %>% as_tibble() , 
                    pre.event.change.summary %>% as_tibble() ) %>%
         as_tsibble( key = c(var , Intervention), index = Month )

avg.pre.event.change = pre.event.change.summary %>%
  as_tibble() %>%
  rename( pct.change = mean.pct.change ) %>%
  group_by( var , Intervention ) %>%
  summarise(
    mean.pct.change = mean( pct.change, na.rm = TRUE ) ,
    sd.pct.change = sd( pct.change , na.rm = TRUE) ,
    median.pct.change = median(pct.change, na.rm = TRUE )
    
  )

pred.interval = 1.28 #(80%)

# Change with data facets ####

.title = paste( country , subject , ': Deviation from expected')
.title = NULL
.subtitle =  paste0('Key: Line is median deviation from expected.', 
                               '\nGrey rectange indicates 80% confidence interval prior to',  
                              event.month )  
.subtitle = NULL 
.caption = "Adjusted for rainfall and RDT stockouts"

g = 
  change %>% 
  filter( var %in% 'Total' , 
          # Intervention %in% 'Standard' 
          !Intervention %in% 'Standard/PBO' 
          ) %>%
  autoplot( median.pct.change , color = 'dark red',
            size = 1.5 , alpha = .9 ) +
    facet_wrap( Intervention ~ string_wrap( var , nwrap = 25)
                # , scales = 'free'
                ) +
    # facet_wrap(. ~ data  ) +
    theme_ipsum() +
    scale_y_percent( 
      # limits = c(-1, 1.5),
      breaks = seq(-1,1,.1) ) +
    scale_x_yearmonth( labels = date_format("%Y") , date_breaks = "2 years") +
    theme( legend.position = "none") +
    geom_hline( yintercept = 0 , color ='gray20', alpha =.25 ) +

    geom_rect( data = avg.pre.event.change %>%
                 filter( var %in% 'Total' , 
                         # Intervention %in% 'Standard'
                         !Intervention %in% 'Standard/PBO' 
                         )
               , inherit.aes = FALSE ,
             aes( xmin = min(change$Month) %>% as.Date() , 
                  xmax = max(change$Month) %>% as.Date() ,
                  ymin = pred.interval*mean(sd.pct.change, na.rm = TRUE)  ,
                  ymax =  -pred.interval*mean(sd.pct.change, na.rm = TRUE) ) ,
        color = 'gray' , alpha = .2) +
    annotate("text", x = event.month %>% as.Date(),
             y = -.5 , label = event.month , 
             vjust = 0 , hjust = 1 ,
             lineheight = .75 ) +
    expand_limits( y =-.55) +

    theme_ppt( base_size = 10 , ) + 
    theme(  panel.grid.minor = element_blank() ) +
    geom_vline( xintercept  = event.month %>% as.Date(), color = 'gray55') + 
    labs( title = .title ,
          subtitle = .subtitle , 
          caption = .caption 
    )

g
# ggsave( paste0( dir, subject, "_", subset, "_deviation.png") , 
#         width = 12 , height = 8 , dpi = 300  )

# Officer ####
  
  create_pptx( g , 
               paste0( country , "_" , subject , "_deviation.pptx" ) 
               )
  
# End ####
options( warn= warningStart )
end = Sys.time()
paste( "run_time (minutes):" , ( ymd_hms(end) - ymd_hms(start) )  )


