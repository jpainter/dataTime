# Testing dataset reporting downloads

country = "Malawi"

library(pacman)
p_load( knitr, scales, tidyverse, progress , readxl, patchwork, 
        tsibble, fable, fabletools, feasts, slider, anomalize, 
        furrr, tictoc, ggrepel , sf , ggspatial ,
                mdthemes, extrafont , hrbrthemes )

source('../dataTime/dhis2_functions.R')  
source( '../dataTime/Deviation_Expected_Functions.R')

# updated function
api_dataset = function(      periods = "LAST_YEAR" ,
                             level = 'LEVEL-1' ,
                             ou = NA , 
                             datasets = dataSet.ids
){
  
  
  if ( all( is.na( periods )  ) ){
    
    periods = strsplit( date_code(), ";" , fixed = TRUE )[[1]]
    
  }
  
  
  ##### cycle through each period, each data element...
  
  ndei = length( datasets ) * length( periods ) * length( levels )
  pb <- progress_bar$new( total = ndei )
  
  data = list()
  
  # TODO: initialize with expected size: e.g.
  data  = vector(mode = "list",
                 length = length( periods ) )
  
  for ( period in seq_along( periods ) ){
    
    data.de = list()
    
    # todo: allocate size of list :
    data.de = vector(mode = "list",
                     length = length( datasets )
    )
    
    for ( element in  seq_along( data.de ) ){
      
      pb$tick()
      
      
      print( paste( periods[ period ], "Dataset" , element ,
                    "/" , length( datasets ) ,
                    ":" , datasets[ element ])
      )
      
      reports = c( 'ACTUAL_REPORTS', 'ACTUAL_REPORTS_ON_TIME', 'EXPECTED_REPORTS' )
      
      # all permutations of dataset ids with report types
      
      de.ids = outer( datasets[ element ],
                      reports,
                      paste, sep=".")  %>%
        as.character()
      
      de.ids = de.ids[ order(de.ids) ] %>%
        paste(. , collapse = ";")
      
      #Assemble the URL ( before starting, double check semicolons for dx dimension )
      url <- paste0( baseurl, "api/analytics/dataValueSet.json?" ,
                     
                     "&dimension=ou:", level ,
                     
                     "&dimension=pe:" , periods[period] ,
                     
                     "&dimension=dx:" ,
                     
                     # malaria
                     de.ids ,
                     
                     "&displayProperty=NAME")
      
      # print( url )
      
      print( paste( "Level:", level , " ") )
      
      
      # Fetch data
      fetch <- retry( get(url, .print = FALSE )[[1]] ) # if time-out or other error, will retry
      
      # if returns a data frame of values (e.g. not 'server error'), then keep
      if ( is.data.frame( fetch ) ){
        
        data.level = fetch %>%
          # select( -storedBy, -created, -lastUpdated, -comment ) %>%
          mutate(
            level = str_sub( level , -1 ) %>% as.integer()
          )
        
        print( paste( nrow(fetch), "records." ) )
        
      } else {
        
        cat( "no records \n" )
      }
      
      data.de[[ element ]] = data.level
      
      # print( paste( dataElements[ element ]  , "has" ,
      # scales::comma( nrow( data.de[[ element ]] ) ) ,
      # "records"  ) )
      
    }
    
    # combine data
    data[[ period ]] = data.table::rbindlist( data.de , fill = TRUE )
    
    print( paste( "...Period" , periods[period]  , "has",
                  scales::comma( nrow( data[[period]] ) ) ,
                  "records."  ) )
    
  }
  
  # combine period data
  d = data.table::rbindlist( data , fill = TRUE)
  
  print( paste( "TOTAL",
                scales::comma( nrow( d ) ),
                "records"  ) )
  
  return( d )
}


dir = file.dir( country ) # 'dhis2_dictionary/Formulas/Burkina Faso/'
formula.file = files('formula' , country = country )[1]

formulas = read_excel( paste0( dir, formula.file  ), 
                       sheet = 'Formula Elements') %>% 
  select( Formula.Name, dataSet, dataSet.id ) %>% unique

formula.datasets = 
  bind_cols(
  map_df( 1:nrow(formulas) , 
          ~separate_rows( formulas[.x,] , dataSet.id , sep=";\\s+") 
          ) %>% select( dataSet.id ) ,
  
  map_df( 1:nrow(formulas) , 
          ~separate_rows( formulas[.x,] , dataSet , sep=";\\s+") 
          ) %>% select( dataSet )
) %>% unique()

dataSet.ids =  formula.datasets %>%  pull( dataSet.id ) 

# credentials
instances = read_excel("../dataDictionary/dhis2_dictionary/Formulas/_Instances_jp.xlsx")
inst = grepl( country , instances$Instance )

baseurl = instances$IPaddress[ inst ]
loginurl <- paste0( baseurl , "api/me" )
GET( loginurl, authenticate( instances$UserName[ inst ], instances$Password[ inst ] ) ) 


# National
r.n  = api_dataset( periods = NA , level = 'LEVEL-1' )
reporting = r.n %>%  
  separate( dataElement, sep = '\\.' , into=c('dataSet.id' , 'reporting')) %>%
  inner_join( formula.datasets )

saveRDS( reporting , paste0( dir , country , '_0_Reporting_National.rds' ) )

reporting = readRDS( paste0( dir ,  country , '_0_Reporting_National.rds' )  )
  
d.n = reporting %>% 
          mutate( period = as.yearmon(period, "%Y%m") ,
                  value = as.integer( value )
          )

ggplot( d.n, aes( x = period ,  y = value, group = reporting, color = reporting ) ) +
  geom_line() +
  facet_wrap( ~dataSet , scales = 'free') +
  labs( title ='National reporting') +
  scale_color_ipsum() +
  labs( title ='Natioal reporting')


# # # Is is really necessary to get the form reporting rate by facility?  # # #

# Facility (level-6): NB ous and data.leaves can be loaded from EvaluationStepByStep_3.Rmd
# downloads all levels, which is slower than required
# TODO : modify api_download to tak 

levels = data.leaves %>% filter( effectiveLeaf = TRUE ) %>% pull( id )

r[ levels] = list( NA )  

for ( i in  seq_along( levels ) ) {
  print( paste( 'Downloading for level-' , levels[i] , ",", i, "of" , length(levels) , "..." ) ) 
  r[ levels[i] ] = api_dataset( periods = NA , level = levels[i] ) 
} 

r.f  = bind_rows( r ) %>% 
  left_join( data.leaves , by = c( 'orgUnit' = 'id') ) 

reporting = r.f %>%  
  separate( dataElement, sep = '\\.' , into=c('dataSet.id' , 'reporting')) %>%
  left_join( formula.datasets )

saveRDS( reporting , paste0( dir , 'Burkina Faso_0_Reporting_Formation Sanitaire.rds' ) )

reporting = readRDS( paste0( dir , 'Burkina Faso_0_Reporting_Formation Sanitaire.rds' ) )

d.f = reporting %>% 
          mutate( period = as.yearmon(period, "%Y%m") ,
                  value = as.integer( value )
          ) %>%
  group_by( period , reporting , dataSet) %>%
  summarise( value = sum( value ))

ggplot( d.f, aes( x = period ,  y = value, group = reporting, color = reporting ) ) +
  geom_line() +
  facet_wrap( ~dataSet ) +
  labs( title ='Summary of facility level reporting')

ggplot( d.f , 
        aes( x = period ,  y = value, group = reporting, color = reporting ) ) +
  geom_line( size = 1.5) +
  facet_wrap( ~dataSet , scales = 'free') +
  labs( title ='Facility reporting') +
  scale_color_ipsum() +
  md_theme_ipsum()

