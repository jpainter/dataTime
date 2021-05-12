# DQA STEPs
country = 'Malawi'
indicator = 'confirmed'

## Libraries ####
library(pacman)
p_load( knitr, scales, tidyverse, progress , readxl, patchwork, 
        tsibble, fable, fabletools, feasts, slider, anomalize, 
        furrr, tictoc, ggrepel , sf , ggspatial ,
                mdthemes, extrafont , hrbrthemes , openxlsx ,
        data.tree, igraph , magrittr , progressr )

options(dplyr.summarise.inform=F) 

## Functions ####
source( "data_time_libraries.r")
source( "ingest_formula_data.r")
source( "formula_files.r" )
source( "clean_ts.r" )
source( "outlier.r" )
source( '../dataTime/Deviation_Expected_Functions.R')
# source( '../dataDictionary/dhis2_dictionary/API.r')

# Formulas  ####

# formula.file = files('Formula' , country = country )[1]
# 
# formulas =  read_excel( paste0( dir, formula.file ), sheet = 'Formula') %>% 
#   filter( !is.na(Formula.Name)) 
# 
# formula.names = formulas$Formula.Name 
# 
# formula.elements = read_excel( 
#     paste0( dir , formula.file ), 
#     sheet = 'Formula Elements') 
#   
# formulas = formulas %>%
#     mutate( Formula.id  = map_chr( Formula , 
#                   ~ translate_formula( .x ,
#                            elements = formula.elements ,
#                            translate_from = str ,
#                            translate_to = id ,
#                            brackets = FALSE )
#     )
#     )

# formula data ----
dir = file.dir( country )

formula_data_files = formula_data_files( dir = dir )
f.indicator = grepl( indicator , formula_data_files , ignore.case = TRUE)
formula_data_files = paste0( dir ,  formula_data_files[f.indicator] )

# select most recent
formula_data_file = formula_data_files[1]
print( "Formula files:" ); print( indicator ) 
print( formula_data_file )

formula_definition =  readxl::read_xlsx( formula_data_file , sheet = 'Formula' )

## Ingest ####
d = ingest_formula_data( filename = formula_data_file , 
                         total.name = paste0( 'total_', indicator ) , 
                         leaf = TRUE )
glimpse( d )
count( d, leaf )
count( d, effectiveLeaf )

orgU = readxl::read_xlsx( formula_data_file , 'formulaData' , 
                          guess_max =  21474836 )

glimpse( orgU )

orgDistrict = orgU %>%
  filter( levelName %in% 'District' ) %>%
  rename( District = orgUnitName ) %>%
  # filter( nchar( District ) > 1 ) %>%
  select( orgUnit, District ) %>% unique


# SPecial Country Data-Managaement Tweaks ----
  ## GUINEA: Should submit only one element; Check for duplicate submissions ----

    ### ANC ----
  # if Count.Complete, then double submitted
  d %>% count(Count.Complete)
  
  
  # If double submitted, take mean of two values
  avg = d %>% filter( Count.Complete == 1 )  %>%
      mutate( mean = ( `SNIS Première cpn_default` + `Premières CPN_default` ) / 2 ) %>%
      pull( mean )
  
  d[ d$Count.Complete == 1, 'totalANC'] = avg 
  d = d %>% select( -`SNIS Première cpn_default` ,-`Premières CPN_default`  ) %>%
      rename( ANC1 = totalANC) 
  
    ### IPTp ----
  # if Count.Complete, then double submitted
  d %>% count(Count.Complete)
  names(d)
  
  # take mean of old and new variable
  d.mergeOldNewVars = d %>%
    rowwise() %>%
    mutate(
      ipt1 = mean( 
        c(`Palu Nombre de femmes enceintes ayant reçu la 1ère dose de SP_default` ,
         `Femmes enceintes ayant recu la 1ere dose de SP_default`
        ), na.rm = TRUE ) ,
      ipt2 = mean( 
        c(`Palu Nombre de femmes enceintes ayant reçu la 2ème dose de SP_default` ,
         `Femmes enceintes ayant recu la 2eme dose de SP_default`
        ), na.rm = TRUE ) ,
      ipt3 = mean( 
        c(`Palu Nombre de femmes enceintes ayant reçu 3 doses de SP_default` ,
         `Femmes enceintes ayant recu au moins 3 doses de SP_default`
        ), na.rm = TRUE )
    ) %>%
    select( -`Palu Nombre de femmes enceintes ayant reçu la 1ère dose de SP_default` ,
            -`Femmes enceintes ayant recu la 1ere dose de SP_default` , 
            - `Palu Nombre de femmes enceintes ayant reçu la 2ème dose de SP_default` ,
         - `Femmes enceintes ayant recu la 2eme dose de SP_default` ,
         - `Palu Nombre de femmes enceintes ayant reçu 3 doses de SP_default` ,
         - `Femmes enceintes ayant recu au moins 3 doses de SP_default` ,
         - totalIPT 
            )
  
  glimpse(d.mergeOldNewVars)
  d = d.mergeOldNewVars 
  
    ### OPD ----
  
  # if Count.Complete, then double submitted
  d %>% count(Count.Complete)
  
  # If double submitted, remove two older categories,
  # consultation curative and leave the 6 newer ones
  older_categories = c( "Premiers contacts de consultation curative_default" ,
                        "Premiers contacts des enfants de moins de 5 ans en consultation curative_default")
  
  # update total by subtracting older from total
  d[ d$Count.Complete == 1, 'totalOPD' ] = 
    d[ d$Count.Complete == 1, 'totalOPD'] - 
    d[ d$Count.Complete == 1, "Premiers contacts de consultation curative_default" ]-
    d[ d$Count.Complete == 1, "Premiers contacts des enfants de moins de 5 ans en consultation curative_default" ]
  
  # set older to NA
  d[ d$Count.Complete == 1, older_categories[1] ] = 0 
  d[ d$Count.Complete == 1, older_categories[2] ] = 0 
  
  
  
  ## Nigeria ----
  # ANC , only one var, so pick total
  d = d %>% select(-`Antenatal total attendance_default`)
  
  
# TS ####

  skipCols = c('level' , 'Count.Any' , 'Count.Complete' )
  
  dataCols = setdiff( names( d)[ map_lgl( d, is.numeric ) ]  ,  skipCols ) 

  dTs = d %>% 
    
        select( 'orgUnit' , 'period' , all_of(dataCols) ) %>%
        
        
        as_tsibble( index =  period , 
                            key = orgUnit

                                            ) 
  # fill gaps 
  if ( sum( has_gaps( dTs )$.gaps ) > 0 )  dTs = fill_gaps( dTs, .full = TRUE )
    
  ## Create example data set: Burkina confirmed cases
  # ex = d %>%
  #   select( period, orgUnit, orgUnitName, level, levelName ,
  #           Regional, Province, District, Commune,
  #           totalconfirmed, Count.Any ) %>%
  #   as_tsibble( index =  period , key = orgUnit )
  # saveRDS( ex, "example_cases.rds")
  
  dTs = dTs %>% 
        pivot_longer( {{ dataCols }} ,
                      values_to = 'original' ,
                      names_to = 'vars' )

maxInterval = dTs %>% as_tibble %>% 
    group_by(orgUnit, vars) %>% 
    summarise( n =  sum(!is.na( original ) )) %>% 
    pull(n) %>% max

## Visualize facility-months ####
unique( dTs$orgUnit )

dTs %>% 
    as_tibble %>% 
    group_by( orgUnit , vars ) %>%
    summarise( Months_with_Data = sum(!is.na( original )) ) %>% 
    group_by( orgUnit ) %>%
    summarise( Months_with_Data = max( Months_with_Data )  ) %>% 
    ggplot() + 
    geom_histogram( aes( x = Months_with_Data ) , binwidth = 1 )


dTs %>% 
    group_by( orgUnit , vars ) %>%
    summarise( Months_with_Data = sum(!is.na( original )) ) %>%
    group_by( vars, Months_with_Data ) %>%
    summarise( count = n() ) %>% 
    ggplot() + 
    geom_line( aes( y = count , x = period  , color = vars )  ) +
    facet_wrap( ~ vars ) +
    theme( legend.position = "none" ) 



### Outliers, phase I - Extreme ----
.total = length( key_size( dTs ) )

pb <- progress_bar$new( 
    format = ":current :percent  [:bar] :elapsedfull",
    total = .total, clear = FALSE, width= 50 )

dTs.extreme =  
    dTs %>% 
    filter( orgUnit %in% unique( dTs$orgUnit )[] ) %>%
    pivot_wider( id_cols = c( orgUnit , period ) , 
                 names_from = "vars", 
                 values_from = "original" ) %>%
    group_by( orgUnit ) %>% 
    mutate( across( all_of( dataCols ) , 

            ~clean_ts( . ,
                      interpolate = FALSE ,
                      .clean = "MAD" ,
                      MAD = 15 # median absolute deviation > 5
                      , .pb = pb
                      )  
                    # , otherwise = NA )
    ) 
    ) %>% 

    pivot_longer( {{ dataCols }} , 
                  names_to =  'vars' ,
                  values_to = 'cleanEO' ) 


# test (count number of extreme outliers)
x = bind_cols( dTs, cleanEO = dTs.extreme$cleanEO ); sum( is.na(x$original) != is.na(x$cleanEO))

## Outliers, phase II - Seasonal extremes ---- ####
# (uses same MAD as above, so that it finds outliers *after* removing MAD values)

pb <- progress_bar$new( 
    format = ":current :percent  [:bar] :elapsedfull",
    total = .total, clear = FALSE, width= 50 )

dTs.stl =  
    dTs.extreme %>% 
    filter( orgUnit %in% unique( dTs$orgUnit )[] ) %>%
    pivot_wider( id_cols = c( orgUnit , period ) , 
                 names_from = "vars", 
                 values_from = "cleanEO" ) %>%
    group_by( orgUnit ) %>% 
    mutate( across( all_of( dataCols ) , 

            ~clean_ts( . ,
                      interpolate = FALSE ,
                      .clean = 'tsclean' ,
                      MAD = 15 # median absolute deviation > 5
                      , .pb = pb
                      )  
                    # , otherwise = NA )
    ) 
    ) %>% 
    
    pivot_longer( {{ dataCols }} , 
                  names_to = 'vars' ,
                  values_to = 'cleanEOSO' ) 

# test (count number of extreme outliers)
x = bind_cols( dTs, cleanEO = dTs.extreme$cleanEO ); sum( is.na(x$original) != is.na(x$cleanEO))
x = bind_cols( dTs, cleanEOSO = dTs.stl$cleanEOSO ); sum( is.na(x$original) != is.na(x$cleanEOSO))

###  parallel stl? ####
plan(multisession)

handlers( global = TRUE )
handlers(list(
  handler_progress(
    # format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
    format   = ":spin :total [:bar] :percent in :elapsed ETA: :eta",
    width    = 50,
    complete = "+"
  ) 
)  )

# Rather than tsclean, use fable with STL model, similar to ARIMA. 
model_stl = function( dfvars, data , tsibble_var = 'cleanEO'  , new_data = NA , .model = 'tsclean'){
  p <- progressor( steps = nrow( dfvars ) )
  a = future_map2( 
    dfvars$uniqueOrgs , 
    dfvars$uniqueVars ,
    .f = ~{
      p()
      a = clean_ts( x = data %>% 
                      select( orgUnit, vars, period , {{ tsibble_var }} ) %>%
                      filter( orgUnit %in% .x , 
                              vars %in% .y )  , 
                    tsibble_var = tsibble_var , 
                    .lambda = .5 , 
                    .clean = .model , # 'prophet' , #MAD, arima, prophet
                    interpolate = FALSE ,
                    predict = FALSE , 
                    pred_data = if (!is.na( new_data ) ) new_data %>% 
                      select( vars, period ) %>%
                      filter( orgUnit %in% .x , 
                              vars %in% .y )  ,
                    arima.list = FALSE , # if arima, returns fit, resid, and interp
                    MAD = 15 , 
                    ignoreSmall = TRUE , 
                    smallThreshold = 20 ,
                    .pb = NULL ,
                    timing = FALSE  
      )
      return(a)
    }
  )
}

sample = 1:length(  unique( dTs$orgUnit ) )
uniqueOrgs = unique( dTs$orgUnit )
uniqueVars = unique( dTs$vars )
d1 <- expand_grid( uniqueOrgs = uniqueOrgs[ sample ] ,
                   uniqueVars = uniqueVars )

dTs.stl2 = model_stl( dfvars = d1 , data = dTs.extreme )

dTs.stl2. = bind_rows( dTs.stl2)

x = bind_cols( dTs, cleanEOSO = dTs.stl2$cleanEOSO ); sum( is.na(x$original) != is.na(x$cleanEOSO))

## Model and Interpolate ####

plan(multisession)

handlers( global = TRUE )
handlers(list(
    handler_progress(
        # format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
        format   = ":spin :total [:bar] :percent in :elapsed ETA: :eta",
        width    = 50,
        complete = "+"
    ) 
)  )

model_interp = function( dfvars, data , new_data = NA , .model = 'prophet'){
    p <- progressor( steps = nrow( dfvars ) )
    a = future_map2( 
        dfvars$uniqueOrgs , 
        dfvars$uniqueVars ,
        .f = ~{
            p()
            a = clean_ts( x = data %>% 
                             select( orgUnit, vars, period , cleanEOSO ) %>%
                             filter( orgUnit %in% .x , 
                                     vars %in% .y )  , 
                         tsibble_var = 'cleanEOSO' , 
                        .lambda = .5 , 
                        .clean = .model , # 'prophet' , #MAD, arima, prophet
                        interpolate = FALSE ,
                        predict = FALSE , 
                        pred_data = if (!is.na( new_data ) ) new_data %>% 
                            select( vars, period ) %>%
                            filter( orgUnit %in% .x , 
                                     vars %in% .y )  ,
                        arima.list = TRUE , # if arima, returns fit, resid, and interp
                        MAD = 15 , 
                        ignoreSmall = TRUE , 
                        smallThreshold = 20 ,
                        .pb = NULL ,
                        timing = FALSE  
         )
        return(a)
        }
    )
}

sample = 1:length(  unique( dTs.stl$orgUnit ) )
uniqueOrgs = unique( dTs.stl$orgUnit )
uniqueVars = unique( dTs.stl$vars )
d1 <- expand_grid( uniqueOrgs = uniqueOrgs[ sample ] ,
              uniqueVars = uniqueVars )


dTs.fit = model_interp( dfvars = d1  , 
                       data  = dTs.stl ,
                       .model = 'prophet' )

print( object.size( dTs.fit ) , units = 'auto')

dTs.fit = bind_rows( dTs.fit )
glimpse(dTs.fit)

print( object.size( dTs.fit ) , units = 'auto')

## Interpolate after phase II outliers removed (ARIMA) ----

# TODO:  Also give stl imputes from tsclean.  Consider, when avail
# use tsclean, when not (when original is missing), use model.interpolate


# ARIMA model does better job backcasting.  So can use to fill in missing dat
# for the purpose of estimating the amount that is missing. And to show more 
# stable trends.  But should not use when forecasting because that may 
# bias results and forecast may have less error than it should.  

# tic()
# dts.arima =  
#     dTs.stl %>% 
#     filter( orgUnit %in% unique( dTs$orgUnit )[] ) %>%
#     pivot_wider(
#         id_cols = c( orgUnit , period ) , 
#         names_from = "vars",
#         values_from = "cleanEOSO" ) %>%
#     mutate( across( all_of( dataCols ) ,
#   
#             ~clean_ts( . ,
#                       interpolate = TRUE  ,
#                       .clean = 'arima' ,
#                       MAD = 15 # median absolute deviation > 5
#                       , .pb = pb 
#                       )  
#     ) 
#     ) %>%
# 
#     pivot_longer( {{ dataCols }} , 
#                   names_to = 'vars' , 
#                   values_to = 'cleanInterp' )
# toc()

# pb <- progress_bar$new( 
#     format = ":current :percent  [:bar] :elapsedfull",
#     total = .total, clear = FALSE, width= 50 )

# Parallelize ...
# plan(sequential)
plan(multisession)
# plan(multicore) # mac

handlers(list(
  handler_progress(
    format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
    width    = 50,
    complete = "+"
  )
))


uniqueOrgs = unique( dTs.stl$orgUnit )
uniqueVars = unique( dTs.stl$vars )
d1 <- expand_grid( uniqueOrgs = uniqueOrgs ,
              uniqueVars = uniqueVars )

tic()
# with_progress({
#   p <- progressor( steps = nrow( d1 )  )

  # dts.arima = future_map2(
  dts.arima = future_map2(
                d1$uniqueOrgs , 
                d1$uniqueVars ,
                  .f = ~{
                    # if ( !is.null( pb ) ) pb$tick()
                    # print( .x )
                    # print( paste( .x, .y))
                   if ( exists( 'p' ) ) p()
                    
                    x.ts = dTs.stl %>%
                      select( orgUnit, vars, period , cleanEOSO ) %>%
                      filter( orgUnit %in% .x , vars %in% .y )
                    
                    if( sum( !is.na( x.ts$cleanEOSO ) ) < 24 ) return( x.ts )
                    
                    # TODO: if less than 24, consider using tsclean instead...
                    
                    m = x.ts %>%
                          model( 
                            arima = 
                              # ETS(  box_cox( !! .x  , lambda = .5 ) ) 
                              ARIMA( box_cox( cleanEOSO, lambda = .5 ) ~
                                       pdq(0:2,0:1,0:2) + PDQ(0:2,0:1,0:2) )
                          )
                    
                    if(  "null_mdl"  %in% class(m) )  return( x.ts )
                    
                    o = augment(m) %>% select( -.innov ) %>%
                              mutate( .fitted = round( .fitted ) ) 
                    
                    out.i.arima = interpolate( m , x.ts )$cleanEOSO %>% as.integer() 
                    out.i.stl = tsclean( ts(o$cleanEOSO) , lambda = .5 ) %>% as.integer() 
                    o$interp.arima = out.i.arima 
                    o$interp.stl = out.i.stl 
                    
                    return( o )              
                  } )
# })
toc()

dts.arima = bind_rows( dts.arima )
glimpse( dts.arima )

## dTs.clean ----

dTs.clean = dTs %>% # combine with dTs
  mutate( 
    cleanEO = dTs.extreme$cleanEO ,
    cleanEOSO = dTs.stl$cleanEOSO ,
    cleanInterp.arima = dts.arima$interp.arima ,
    cleanInterp.stl = dts.arima$interp.stl ,
    .resid = dts.arima$.resid , 
    .fitted = dts.arima$.fitted
    , missing = is.na( original )
    , extremeOut = !is.na( original ) &  is.na( cleanEO ) 
    , seasonalOut = !is.na( original ) &  is.na( cleanEOSO ) 
    , Interp = is.na( cleanEOSO ) & ( !is.na( cleanInterp.arima ) | !is.na( cleanInterp.stl ) )
  )

glimpse( dTs.clean )
count( dTs.clean %>% as_tibble(), missing, Interp )

# Save dTs.clean -----
saveRDS( dTs.clean , paste0( dir , indicator , '_dTs_clean.rds' ) )
rm( dTs.extreme, dTs.stl, dts.arima ); gc()
dTs.clean = readRDS( paste0( dir , indicator , '_dTs_clean.rds' )  )


# Visualize month and size of outliers ----

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

# valuea.. (work in progress)
dTs.clean %>% as_tibble() %>% 
  # filter( period > yearmonth( "Jan 1 2019" ) ) %>%
  group_by( vars , missing, extremeOut, seasonalOut ) %>%
  summarise( n = n() , value = sum  ) %>%
  mutate( n_percent = percent( n / sum(n)  , accuracy = .1 ) ) 


# Extreme outlier triptych (monthly total value, number, pct) ----
dTs.clean %>% 
    rename( element = vars ) %>%
    group_by( element ) %>%
    summarise( 
               value = sum( ifelse( extremeOut, original, 0) ) ,
               value.pct = value * 100 / sum( original , na.rm = TRUE ) ,
               number = sum( extremeOut ) , 
                ) %>% 
    pivot_longer( cols = c( value, value.pct, number ) ) %>% 
    autoplot( value ) + 
    facet_grid( rows = vars(name), 
                cols = vars( element ) , 
                scales = 'free' ) +
    theme(legend.position = "none") +
    labs( title = 'Extreme Outliers')


# Seasonal outlier triptych (monthly total value, number, pct)
dTs.clean %>% 
    rename( element = vars ) %>%
    group_by( element ) %>%
    summarise( 
               value = sum( ifelse( seasonalOut, original, 0) ) ,
               value.pct = value * 100 / sum( original , na.rm = TRUE ) ,
               number = sum( seasonalOut ) , 
                ) %>% 
    pivot_longer( cols = c( value, value.pct, number ) ) %>% 
    autoplot( value ) + 
    facet_grid( rows = vars(name), 
                cols = vars( element ) , 
                scales = 'free' ) +
    theme(legend.position = "none") +
    labs( title = "Seasonal Outliers")

# Imputed triptych -- potential missing data ----
dTs.clean %>% 
    rename( element = vars ) %>%
    group_by( element ) %>%
    summarise( 
               value = sum( ifelse( Interp, cleanInterp, 0) ) ,
               value.pct = value * 100 / sum( original , na.rm = TRUE ) ,
               number = sum( seasonalOut ) , 
                ) %>% 
    pivot_longer( cols = c( value, value.pct, number ) ) %>% 
    autoplot( value ) + 
    facet_grid( rows = vars(name), 
                cols = vars( element ) , 
                scales = 'free' ) +
    theme(legend.position = "none") +
    labs( title = "Potential Missing Vaues (imputed)")

# Categories is all the detailed vars versus the total variable.  The chart show the
# difference when cleaning by element versus cleaning by details.  Expect to see 
# greater frequency of errors and percent error among details.  

dTs.summary  = 
    dTs.clean %>% 
    # mutate( Categories  = 
    #             case_when( grepl( 'total' , vars ) ~ 'Total' ,
    #                        TRUE ~ 'Details' ) 
    #         ) %>%
    rename( Categories = vars ) %>%    
    group_by( Categories ) %>%
    summarise( 
               Error = sum( ifelse( extremeOut | seasonalOut, original , 0 ),  na.rm = TRUE ) ,
               adjError = sum( ifelse( extremeOut | seasonalOut, 
                                    sum( c(original, -cleanInterp) , na.rm = T), 0 ) ) ,
               original = sum( original , na.rm = TRUE ) ,
               # Absolut_Percent_Error = Absolute_Error * 100 / original ,
               Percent_Error = Error * 100 / original ,
               Percent_adjError = adjError * 100 / original ,
               number_error = sum( seasonalOut , na.rm = TRUE ) , 
               frequency_error = number_error *100 / n()
                ) %>%
    select( -number_error , -original, -Error , -adjError ) %>%
    pivot_longer( cols = c(contains('error'), contains('original') ) ) 

dTs.summary %>% 
    autoplot( value ) + 
    facet_grid( rows = vars(name), 
                cols = vars(Categories) ,
                scales = 'free' ) +
    theme(legend.position = "none")
    
dTs.summary %>% 
    pivot_wider( names_from = name, values_from = value ) %>%
    ggplot( aes( x = Percent_Error )) + 
    geom_histogram(aes(y=  stat(density) ) ,  binwidth = 1 ) + 
    scale_y_continuous(labels=percent) +
    stat_bin(binwidth=1, geom="text", colour="white", size=3.5,
           aes(label= percent( stat(density) ) ,
               y= stat(density) ), 
           position=position_stack(vjust=0.5)) +
    labs( y = 'Frequency' , x = 'Size of Errror as Percentage of National Total' ,
          title = "Frequency of Data Entry Errors" ,
          subtitle = "Relative to the national aggregate for each category (Details) and 
          when combined (Total) before searching for errors") +
    facet_grid( cols = vars(Categories))


# Consistency ----

harmonic_mean = function( x ){
    1 / mean( 1/x, na.rm = TRUE )
}

# https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

consistency = dTs.clean %>%
  as_tibble() %>% 
  mutate( error = ifelse( is.na(.fitted)|is.na(cleanEOSO), NA ,
                          cleanEOSO - .fitted ) 
  ) %>%
  group_by( orgUnit , vars  ) %>%
  summarise( 
    n_original = sum( !is.na( original)) ,
    n_clean = sum( !is.na( cleanEOSO)) ,
    n_fitted = sum( !is.na( .fitted)) ,
    actual = sum( ifelse( !is.na(.fitted),  cleanEOSO , NA ) , na.rm = TRUE ) ,
    fitted = sum( ifelse( !is.na(cleanEOSO),  .fitted , NA ) , na.rm = TRUE ) ,
    abs_error = sum( abs(error), na.rm = TRUE ) 
             ) %>%
    mutate( 
      deviation = actual / fitted  - 1 ,
      abs_deviation = abs_error / fitted  
    )

median(consistency$deviation, na.rm = TRUE)
median(consistency$abs_deviation, na.rm = TRUE)

qplot( x = n_original , y = deviation , data = consistency)
qplot( x = n_original , y = abs_deviation , data = consistency)
g = ggplot( aes( y = abs(deviation) , x = abs_deviation ) , 
        data = consistency %>% filter( deviation < 1, abs_deviation<1 )) +
  geom_point() 
ggExtra::ggMarginal( g, type = 'histogram')

# Classify error/deviation
consistency.grid = 
  consistency %>% 
  ungroup %>%
  mutate( absDeviationClass = cut( ifelse( is.na(abs_deviation) , Inf, abs_deviation ) ,
                                breaks = c(0, .05, .1, .15, .25, .5 , Inf), 
                                include.lowest = TRUE ) ,
          # to compare deviation with absDeviation on the same scale, take abs of deviation
          deviationClass = cut( ifelse( is.na(deviation) , Inf, abs(deviation) ) ,
                                breaks = c(0, .05, .1, .15, .25, .5, Inf), 
                                include.lowest = TRUE ) ,
          originalClass = cut( ifelse( is.na( n_original ) , 0 , n_original ) , 
                               breaks = c(0, 24, 36 , 48, 54 , 
                                          max(n_original, na.rm = TRUE )  ) ,
                               include.lowest = TRUE )
          ) 

consistency.grid %>%  count( originalClass, deviationClass )  
consistency.grid %>%  count( originalClass, absDeviationClass )  
 
consistency.grid %>%
  count( originalClass, deviationClass )  %>%
  ggplot( aes( originalClass, deviationClass, size = n )  ) +
  geom_point() + labs( title = 'relative deviation from fitted')

consistency.grid %>%
  count( originalClass, absDeviationClass )  %>%
  ggplot( aes( originalClass, absDeviationClass, size = n )  ) +
  geom_point() + labs( title = 'Absoloute deviation from fitted')

consistency.grid %>%
  count( absDeviationClass , deviationClass , )  %>%
  ggplot( aes( deviationClass, absDeviationClass, size = n )  ) +
  geom_point() + labs( title = 'Relative vs Absoloute deviation')

consistency.best = consistency.grid %>%
  filter( originalClass %in% '(48,57]', deviationClass %in% '[0,0.05]')


# DQA SUMMARY ----



# Pre- Post split ----

# Choose whether or not to 
# 1. use total only
# 2. Use categories only (remove total)

pre = dTs.clean %>% 
  # filter( grepl( 'total' , vars , ignore.case = TRUE ) ) %>%
  filter( period < yearmonth( 'February 1 2020') )

post = dTs.clean %>% 
  # filter( grepl( 'total' , vars , ignore.case = TRUE ) ) %>%
  filter( period >= yearmonth( 'February 1 2020') )

future = post %>% as_tibble %>% 
  select( orgUnit, vars, period ) %>% unique %>%
  as_tsibble( index = period , key = c(orgUnit, vars ) )

# Pre-Pred model facilities with prediction ----
plan(multisession)

handlers( global = TRUE )
handlers(list(
    handler_progress(
        # format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
        format   = ":spin :total [:bar] :percent in :elapsed ETA: :eta",
        width    = 50,
        complete = "+"
    ) 
)  )

arima_pred = function( dfvars, data , new_data ){
    p <- progressor( steps = nrow( dfvars ) )
    a = future_map2( 
        dfvars$uniqueOrgs , 
        dfvars$uniqueVars ,
        .f = ~{
            p()
            a = clean_ts( x = data %>% 
                             select( orgUnit, vars, period , cleanEOSO ) %>%
                             filter( orgUnit %in% .x , 
                                     vars %in% .y )  , 
                         tsibble_var = 'cleanEOSO' , 
                        .lambda = .5 , 
                        .clean = 'arima' , #MAD, arima
                        interpolate = FALSE ,
                        predict = TRUE , 
                        pred_data = new_data %>% 
                            select( vars, period ) %>%
                            filter( orgUnit %in% .x , 
                                     vars %in% .y )  ,
                        arima.list = TRUE , # if arima, returns fit, resid, and interp
                        MAD = 10 , 
                        ignoreSmall = TRUE , 
                        smallThreshold = 20 ,
                        .pb = NULL ,
                        timing = FALSE  
         )
        return(a)
        }
    )
}

sample = 1:length(  unique( pre$orgUnit ) )
d2 <- expand_grid( uniqueOrgs = unique( pre$orgUnit )[sample] ,
                   uniqueVars = unique( pre$vars ) ) 

pre_pred = arima_pred( dfvars = d2  , 
                       data  = pre , 
                       new_data = future )

print( object.size( pre_pred ) , units = 'auto')

pre_pred = bind_rows( pre_pred )
glimpse(pre_pred)

print( object.size( pre_pred ) , units = 'auto')

saveRDS( pre_pred , paste0( dir, indicator , "_pre_pred.rds")  )
# Read
# pre_pred = readRDS( paste0( dir, indicator , "_pre_pred.rds")  )

# Pre-model District ----

#- filter by  quality
consistency.best = consistency.grid %>%
  filter( n_original>=55 , 
          absDeviationClass %in% c( '[0,0.05]' , '(0.1,0.15]'  , '(0.15,0.25]' ) 
  )

#- join facility to district
#- filter by quality and aggregate to district 
dTs.clean.district = dTs.clean %>% 
  filter( 
    orgUnit %in% consistency.best$orgUnit ) %>%
  inner_join( orgDistrict , by = "orgUnit") %>%
  group_by( District , vars ) %>%
  summarise( 
    facilities = n_distinct( orgUnit )  ,
    cleanEOSO = sum( cleanEOSO, na.rm = TRUE ) 
             )

count( dTs.clean.district %>% as_tibble(),  District,  facilities )

#- pre_pred district
 pre.district = dTs.clean.district %>% 
   rename( orgUnit = District ) %>%
   filter( period < yearmonth( 'February 1 2020') )
 
 post.district = dTs.clean.district %>% 
   rename( orgUnit = District ) %>%
   filter( period >= yearmonth( 'February 1 2020') )

 future.district = post.district %>% as_tibble %>% 
  select( orgUnit, vars, period ) %>% unique %>%
  as_tsibble( index = period , key = c( orgUnit, vars ) )

  
 sample = 1:length(  unique( pre.district$orgUnit ) )
 d2.district <- expand_grid( uniqueOrgs = unique( pre.district$orgUnit )[sample] ,
                   uniqueVars = unique( pre.district$vars ) ) 

 pre_pred.district = arima_pred( dfvars = d2.district , 
                                 data = pre.district ,
                                 new_data = future.district )

 pre_pred.district = bind_rows( pre_pred.district )
 print( object.size( pre_pred.district ) , units = 'auto')
 glimpse(pre_pred.district)  

# saveRDS( pre_pred.district , paste0( dir, indicator , "_pre_pred.district.rds")  )
# Read
# pre_pred.district = readRDS( paste0( dir, indicator , "_pre_pred.district.rds")  )

# District Impact
 
 ## raw impact among 'best'
 consistency.best = consistency.grid %>%
  filter( n_original>=55 
          , absDeviationClass %in% c( '[0,0.05]' , '(0.1,0.15]'  , '(0.15,0.25]' )
  )
 
 dTs.clean %>% 
   filter( orgUnit %in% consistency.best$orgUnit ) %>%
   group_by( vars ) %>%
   summarise( total = sum( cleanEOSO , na.rm = TRUE ) ) %>%
   autoplot( total )

 preI.district = pre_pred.district %>% 
  filter( period <= yearmonth( 'February 1 2020') ) %>%
  select( orgUnit, vars, period, .resid, .fitted , value ) %>%
  mutate( pctResidual = .resid  / .fitted ) %>% 
  group_by( vars ) %>%
  summarise( 
    actual = sum( ifelse( !is.na(.fitted),  value , NA ) , na.rm = TRUE ) ,
    fitted = sum( ifelse( !is.na( value ),  .fitted , NA ) , na.rm = TRUE ) 
             ) %>%
  mutate( deviation = actual / fitted  - 1 )

postI.district = pre_pred.district %>% 
  filter( period >= yearmonth( 'February 1 2020') ) %>%
  select( orgUnit, vars, period, pred ) %>%
  inner_join( post.district %>% select( orgUnit, vars, period, cleanEOSO    ) ,
              by = c("orgUnit", "vars", "period") ) %>%
  group_by( vars ) %>%
  summarise( 
    actual = sum( ifelse( !is.na(pred),  cleanEOSO , NA ) , na.rm = TRUE ) ,
    predicted = sum( ifelse( !is.na( cleanEOSO), pred , NA ) , na.rm = TRUE )
    ) %>%
  mutate( deviation = actual / predicted  - 1 ) 

# composite chart (District)
  preI.district %>%  
  autoplot( deviation ) + 
  geom_vline( xintercept = as.integer( mdy( "March 1 2020") ) , 
                color = 'black', alpha = .5 ) +
  geom_label( x = as.integer( mdy( "March 1 2020")) ,
                y  = -0.3 , label = "March 2020" , 
                color = 'black', alpha = .5  ,
                hjust = 1 , show.legend = FALSE  ) + 
  # autolayer( preI.forecast , 
  #            vars( deviation) ,  alpha = .5 ,
  #            level = 80 , color = 'white' ) +
  autolayer( postI.district , vars( deviation) ,  color = 'red' ) +
  facet_wrap( ~ vars , 
              labeller = labeller( vars = label_wrap_gen(50))  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-.4, .3) ,
                     breaks = c(-.4,-.3,-.3,-.2,-.1,-.05,.05, .1, .2, .3)) +
  scale_color_discrete( labels = function(x) str_wrap( x, width = 40) ) +
  scale_fill_discrete( labels = function(x) str_wrap( x, width = 40) ) +
  labs( title = paste( country , indicator ) , 
          y = "Percent Change from Expected"  ,
          x = '') +
  theme( legend.position = 'none' ) + # not working!
  theme_bw()
  

# Pre - Model ----
plan(multisession)
# plan( mulitcore ) #mac

# .total = length( key_size( pre ) )
# pb <- progress_bar$new( 
#     format = ":current :percent  [:bar] :elapsedfull",
#     total = .total, clear = FALSE, width= 50 )

handlers(list(
  handler_progress(
    format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
    width    = 50,
    complete = "+"
  )
))

d2 <- expand_grid( uniqueOrgs = unique( pre$orgUnit )[] ,
              uniqueVars = unique( pre$vars ) ) 

tic()
# with_progress({
  # p <- progressor(steps = nrow( d2 ))

    preModel = 
    future_map2( 
      d2$uniqueOrgs , 
      d2$uniqueVars ,
                .f = ~{
                    
                  # if ( !is.null( pb ) ) pb$tick()
                  # print( .x )
                  # if ( !is.null( p ) ) p()
                  
                  x.ts = pre %>%
                    select( orgUnit, vars, period , cleanEOSO ) %>%
                    filter( orgUnit %in% .x , vars %in% .y )
                  
                  if( sum( !is.na( x.ts$cleanEOSO ) ) < 12 ){
                     return( tibble( orgUnit = unique( x.ts$orgUnit ),
                                     vars = unique( x.ts$vars ) ,
                                     arima = NA )
                     )
                  }
                  
                  m = x.ts %>%
                        model( 
                          arima = 
                            # ETS(  box_cox( !! .x  , lambda = .5 ) ) 
                            ARIMA( box_cox( cleanEOSO, lambda = .5 ) ~
                                     pdq(0:2,0:1,0:2) + PDQ(0:2,0:1,0:2) )
                        ) %>% as_tibble()
                  return( m )              
                } 
             # , .progress = TRUE  # Furrr progress bar
             ) 
# })
toc()
preModel = bind_rows( preModel )
saveRDS( preModel , paste0( dir, indicator , "_preModel.rds")  )
# Read
# preModel = readRDS( paste0( dir, indicator , "_preModel.rds")  )


# Prediction ----

tic()
with_progress({
    p <- progressor(steps = nrow( d2 ))

    prediction = 
      map2( 
        d2$uniqueOrgs, 
        d2$uniqueVars  ,
                  .f = ~{

                    if ( !is.null( p ) ) p()
                    
                    m = preModel %>%
                      filter( orgUnit %in% .x , vars %in% .y ) 
                    # %>%
                    #   as_mable( key = c( orgUnit, vars ) ) 
                    
                    if( is.null( m$arima[[1]] ) ) return()
                    
                    new = post %>%
                      filter( orgUnit %in% .x , vars %in% .y ) %>%
                      select( orgUnit, vars , period, .fitted )
                    
                    # if( is.nul,,
                    
                    p = m$arima %>% fable::forecast()
                    
                    f = new %>% 
                      left_join( p[[1]] , by = "period" ) %>%
                      mutate( .mean = ifelse( is.na(.mean), .fitted, .mean ))
  
                    return( f )              
                  } 
               # , .progress = TRUE  # Furrr progress bar
               ) 
})
toc()
prediction = bind_rows( prediction )

saveRDS( prediction , paste0( dir, indicator , "_prediction.rds")  )
# Read
# prediction = readRDS( paste0( dir, indicator , "_prediction.rds")  )

# Impact ----
Stderr <- function(x, na.rm=FALSE) {
  x = ifelse( x == Inf ,NA, x  )
  if (na.rm ) x <- na.omit(x)
  sqrt( var(x)/length(x) )
}

glimpse( pre )
glimpse( post )
glimpse( prediction )

# pctError based on arima of whole dataset
preI = pre %>% 
  filter( orgUnit %in% unique( pre$orgUnit )[]) %>%
  select( orgUnit, vars, period, .resid, .fitted , cleanEOSO ) %>%
  mutate( pctResidual = .resid  / .fitted ) %>% 
  group_by( vars ) %>%
  summarise( 
    actual = sum( ifelse( !is.na(.fitted),  cleanEOSO , NA ) , na.rm = TRUE ) ,
    fitted = sum( ifelse( !is.na(cleanEOSO),  .fitted , NA ) , na.rm = TRUE ) 
             ) %>%
  mutate( deviation = actual / fitted  - 1 )

# pctError based on arima from preModel
tic()
preI.arima = preModel %>%
  # filter( orgUnit %in% 'AVOL0xGSl3Z' ) %>%
  mutate( 
    pctResidual = map( arima , ~{
      if ( is.null( .x$fit$est ) ) return( rep( NA ,length( .x$data$period ) ) )
      ifelse( .x$fit$est$.fitted > 1 , # some estimates close to zero and make percent error explode
              .x$fit$est$.resid / .x$fit$est$.fitted ,
              NA 
      )
    } ),
    
    data = map( arima , ~{
      # if ( is.null( .x$data$`box_cox(cleanEOSO, lambda = 0.5)` ) ) return( rep( NA ,length( .x$data$period ) ) )
      # ifelse( .x$fit$est$.fitted > 1 , # some estimates close to zero and make percent error explode
      #         .x$fit$est$.resid / .x$fit$est$.fitted  ,
      #         NA 
      # )
      
      .x$data$`box_cox(cleanEOSO, lambda = 0.5)` %>% 
        inv_box_cox( . , lambda = 0.5)
    } ),
    
    .fitted = map( arima , ~{
      if ( is.null( .x$fit$est ) ) return( rep( NA ,length( .x$data$period ) ) )
      # ifelse( .x$fit$est$.fitted > 1 , # some estimates close to zero and make percent error explode
      #         .x$fit$est$.fitted  ,
      #         NA 
      # )
      .x$fit$est$.fitted  %>% inv_box_cox( ., lambda = 0.5)
    } ),
    
    period = map( arima , ~ .x$data$period )
          ) %>%
  unnest( c( pctResidual , data, .fitted , period ) )  
toc()



# Classify fit and use to filter summary...


preI2 = preI.arima %>% 
  group_by( period , vars ) %>%
  summarise( 
    actual = sum( ifelse( !is.na(.fitted),  data , NA ) , na.rm = TRUE ) ,
    fitted = sum( ifelse( !is.na( data),  .fitted , NA ) , na.rm = TRUE )
  ) %>%
  mutate( deviation = actual / fitted  - 1 ) %>% 
  as_tsibble( key = vars , index = period )

  
preI %>% autoplot( deviation ) + labs( title = 'preI')
preI2 %>% autoplot( deviation ) + labs( title = 'preI2')

# forecast deviation into the future
## consider non-arima forecasts:  ETS, or rolling mean?
preI.forecast =
  preI2 %>% 
  model( 
    # arima = ARIMA( box_cox( deviation , lambda = .5  ) ) ,
    ets = ETS( box_cox( deviation , lambda = .5  ) )
    ) %>%
  forecast( new_data = future ) 

preI.forecast %>% autoplot() + labs( title = 'preI.forecast')

postI = prediction %>% 
  select( orgUnit, vars, period, .mean ) %>%
  inner_join( post %>% select( orgUnit, vars, period, cleanEOSO    ) ,
              by = c("orgUnit", "vars", "period") ) %>%
  group_by( vars ) %>%
  summarise( 
    actual = sum( ifelse( !is.na(.mean),  cleanEOSO , NA ) , na.rm = TRUE ) ,
    predicted = sum( ifelse( !is.na( cleanEOSO), .mean , NA ) , na.rm = TRUE )
    ) %>%
  mutate( deviation = actual / predicted  - 1 ) 

# Save impact datasets ----
# save( pre, post , preI , postI, preI.variation , 
#       file = paste0( dir, indicator , "_impact.rda") )
# load( paste0( dir, indicator , "_impact.rda") ) 

# mutate( 
#   vars = case_when( 
#     vars %in% "Palu Nombre de femmes enceintes ayant reçu au moins 4 doses de SP_default" ~ 'ipt4',
#     TRUE ~ vars ) 
#   ) %>%

# Impact Vis ----

  # composite chart
  preI2 %>% 
  autoplot( deviation ) + 
  geom_vline( xintercept = as.integer( mdy( "March 1 2020") ) , 
                color = 'black', alpha = .5 ) +
  geom_label( x = as.integer( mdy( "March 1 2020")) ,
                y  = -0.3 , label = "March 2020" , 
                color = 'black', alpha = .5  ,
                hjust = 1 , show.legend = FALSE  ) + 
  autolayer( preI.forecast , 
             vars( deviation) ,  alpha = .5 ,
             level = 80 , color = 'white' ) +
  autolayer( postI , vars( deviation) ,  color = 'red' ) +
  facet_wrap( ~ vars , 
              labeller = labeller( vars = label_wrap_gen(50))  ) +
  scale_y_continuous(labels = scales::percent, limits = c(-.4, .3) ,
                     breaks = c(-.4,-.3,-.3,-.2,-.1,-.05,.05, .1, .2, .3)) +
  scale_color_discrete( labels = function(x) str_wrap( x, width = 40) ) +
  scale_fill_discrete( labels = function(x) str_wrap( x, width = 40) ) +
  labs( title = paste( country , indicator ) , 
          y = "Percent Change from Expected"  ,
          x = '') +
  theme( legend.position = 'none' ) + # not working!
  theme_bw()
  
  # Loess summary 
  bind_rows( preI2 %>% as_tibble() , 
             postI %>% as_tibble() %>% 
               filter( period > yearmonth("March 1 2020") ) ) %>%
    as_tsibble( index = period, key =  vars) %>% 
    autoplot( deviation , alpha = .25 ) +
    geom_vline( xintercept = as.integer( mdy( "March 1 2020") ) , 
                color = 'brown') +
    geom_label( x = as.integer( mdy( "March 1 2020")) ,
                y  = -0.3 , label = "March 2020" , color = 'brown' ,
                hjust = 1 , show.legend = FALSE  ) + 
    geom_smooth( method = 'loess' , formula = 'y ~ x') +
  scale_y_continuous(labels = scales::percent, limits = c(-.4, .3) ,
                     breaks = c(-.4,-.3,-.3,-.2,-.1,-.05,.05, .1, .2, .3)) +
    labs( title = paste( country , indicator ) , 
          y = "Percent Change from Expected"  ,
          x = '') +
    theme_bw()

             