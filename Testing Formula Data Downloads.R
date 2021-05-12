# Testing dataset reporting downloads


country = "Uganda-2020"
QR = TRUE 

# libraries and functions ####
library(pacman)
p_load( scales, knitr, scales ,  #gsubfn  for strapplyc, line 221 
        tidyverse, progress , readxl, patchwork ,
        tsibble, fable, fabletools, feasts, slider, anomalize ,
        gsubfn, proto , 
        furrr, tictoc, ggrepel , sf , ggspatial ,
        mdthemes, extrafont , hrbrthemes , openxlsx ,
        data.tree, igraph , magrittr , progressr 
        )


source('../dataDictionary/dhis2_dictionary/dhis2_functions.R')  
source( '../dataTime/Deviation_Expected_Functions.R')
source( '../dataDictionary/dhis2_dictionary/API.r')
source( 'api_data.r')


string_wrap = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}

string_wrap = Vectorize(string_wrap)


stderr <- function(x, na.rm=TRUE ) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}


# Formulas  ####

country.dir = dir( country )

formula.file = files('Formula' , country = country ) %>% most_recent_file()

formulas =  read_excel( paste0( country.dir, formula.file ) , sheet = 'Formula') %>% 
  filter( !is.na(Formula.Name) ) 

formula.names = formulas$Formula.Name 

formula.elements = read_excel( 
    paste0( country.dir , formula.file ), 
    sheet = 'Formula Elements') 
  
# skip if using QR formula
if ( !QR ){ 
formulas = formulas %>%
    mutate( Formula.id  = map_chr( Formula , 
                  ~ translate_formula( .x ,
                           elements = formula.elements ,
                           translate_from = str ,
                           translate_to = id ,
                           brackets = FALSE )
    )
    )
}

# View( formulas )

# Reorder 
formula_order = formulas$Formula.Name %>% 
  map( ., ~ str_split(.x , ' '))  %>%
  map( 1 ) %>% map_chr( 1 ) %>% parse_number() %>% order

formulas = formulas[ formula_order , ]

# Metadata ####
metadata.filename = metadata.file( country.dir, country )
orgUnitLevels = OrgUnitLevels( metadata.filename  )
orgUnits = OrgUnits( metadata.filename )
dataSets = DataSets(  metadata.filename )
dataElements = DataElements( metadata.filename )

# OUS structure ####
ous = read_excel( metadata.filename  , sheet = 'OrgUnits')
ous.levels = read_excel(  metadata.filename , sheet = 'OrgUnitLevels')

ous.id_parent = ous %>% select( id, parent ) 
# Remove top value if it has missing parent.
ous.id_parent = ous.id_parent %>% filter( !is.na( parent ) )


# orgUnit.tree and path ----
if( file.exists( paste0( country.dir , "ous.tree") ) ){
  load( paste0( country.dir , "ous.tree") )
} else {
    ous.tree = FromDataFrameNetwork( ous.id_parent )
    save( ous.tree , file = paste0( country.dir , "ous.tree") )
}

pathsFileName = paste0( country.dir , "paths") 
if ( file.exists( pathsFileName ) ){
  paths = readRDS( paste0( country.dir , "paths") )
} else {
  
  dti = as.igraph.Node( ous.tree )
  
  node.ids = ous %>% 
  filter( !is.na( parent) , 
          !is.na( levelName )) %>% 
  pull(id)
  
  # sllllooowww
  n = length(node.ids)
  pb <- progress_estimated( n ) # dplyr - end of lifcycle
  # pb <- progress_bar$new( total = length(node.ids) ) $ progress pacckage - not working!?
  paths = map( 1:n , ~{
    pb$tick()$print()
    p = all_simple_paths(dti, node.ids[.x]  , 1)[[1]]
    as_ids( p ) %>% rev %>% t %>%
      as_tibble() %>%
      set_colnames( ous.levels$levelName[ 1:length(p) ] ) %>%
      mutate( id = node.ids[.x] ) %>%
      select( id, everything() )
  }
  )
  paths = bind_rows( paths )
  saveRDS( paths,  paste0( country.dir , "paths"))
}

paths.translated = paths %>% 
  pivot_longer( cols = -id , names_to = 'Level') %>%
  left_join( ous %>% select( id, name ) , by = c( 'value' = 'id') ) %>%
  pivot_wider( -value , names_from = Level, values_from = name )


#  NB: ous.leaves unused -- commented out March 15 2021
# ous.id_parent[ is.na( ous.id_parent$parent) , 'parent'] = country
# ou_leaves = ous.tree$Get("isLeaf")
# ous.leaves = tibble( id = attributes(ou_leaves)$name , 
#                      tree.leaf = ou_leaves ) %>%
#     inner_join( ous , by = 'id')
# 
# count( ous.leaves , leaf, tree.leaf ) # confirm dhis2 leaf = tree.leaf 
# count( ous.leaves, level , levelName , tree.leaf ) %>% 
#     pivot_wider( names_from = tree.leaf , values_from = n ) %>%
#     arrange( level )
# 
# # table of leaf facility not at formation sanitaire
# ous.leaves %>% filter( leaf == TRUE , level < 5 ) %>%
#     select( levelName, name )

# Previous data ####

subject = formulas$Formula.Name 

data.files = map( subject ,
                  ~{ 
                    f = files( search = .x , country = country , 
                          other = "All Levels" , type = 'rds' ) 
                    starts_with_country = substr( f , 1, nchar(country)) == country 
                    not_dTs = !grepl('dTs', f , fixed = TRUE  )
                    return( f[ starts_with_country & not_dTs ] )
                  }
)

# data.files

## Find most recent file 

most_recent_data_files = tibble(
  formula = subject ,
  file = map_chr( data.files , most_recent_file ) 
  ) %>%
    mutate( date = map_chr( file, ~{
            ifelse( is.na( .x ) , NA , 
                    gsubfn::strapply( .x, "[0-9-]{10,}", simplify = TRUE) 
            ) } ) ,
            days_old = ifelse( is.na( date ) , NA, Sys.Date() - anydate( date ) ) ,
            update = ifelse( days_old > 30 | is.na( date ) , TRUE , FALSE )
            )


# View( most_recent_data_files )



## login and credentials ----
credentials = read_excel( paste0( "../dataDictionary/dhis2_dictionary/Formulas/",
                                 "_Instances_jp.xlsx")) %>%
  filter( Instance == country )

baseurl = credentials$IPaddress
username = credentials$UserName
password = credentials$Password

loginurl <-paste0( baseurl , "api/me" )
GET( loginurl, authenticate( username, password ) )

# Request data ----

YrsPrevious  = 5 
periods = date_code( YrsPrevious = YrsPrevious ) # 'months_last_5_years' # 

# QR periods 
# periods = "202010;202011;202012;202101;202102;202103"

level = 'All levels'

for ( i in which( most_recent_data_files$update ) ){
  
  print( 'Formula.Name' ) ; print(  most_recent_data_files$formula[i] )
  
  elements = api_formula_elements( most_recent_data_files$formula[i]  ) %>%
    str_replace_all(  '\r' , "") %>%
    str_replace_all(  '\n' , "") %>%
    str_replace_all(  ' ' , "") %>%
    str_trim 
  
  if ( QR ){
    elements = formulas %>%
      filter( Formula.Name %in% most_recent_data_files$formula[i]  ) %>%
      pull( Formula ) %>%
      str_replace_all(  fixed('+') , ";") %>%
      str_replace_all(  ' ' , "") 
  }
  
  
  print( 'Elements' ) ; print( elements )
  
  tic()
  Sys.time()
  
  if ( ! loginDHIS2(baseurl, username, password ) ) stop()
  
  x  = api_data( periods, level , elements, baseurl , 
                 formula = most_recent_data_files$formula[i] ,
                 update = most_recent_data_files$update[i] & !is.na(most_recent_data_files$file[i]) ,
                 check_previous_years = YrsPrevious  , 
                 previous_dataset_file = paste0( dir , 
                                                 most_recent_data_files$file[i]) )
  
  min_period = min(str_split(periods, ";")[[1]])
  max_period = max(str_split(periods, ";")[[1]])
  period_string = ifelse( min_period == max_period , periods , glue::glue( min_period , "_", max_period ) )
  save_to_filename =  paste0( dir, country, "_" , most_recent_data_files$formula[i]  , "_" , level ,"_", 
                              period_string ,"_", Sys.Date() , ".rds") 
  saveRDS( x , save_to_filename )
  print( paste( 'finished downloading' , most_recent_data_files$formula[i] ,
                "saved to:" , save_to_filename) )
  toc()

}



# Update list of downloaded files ----

subject = formulas$Formula.Name 

data.files = map( subject ,
                  ~{ 
                    f = files( search = .x , country = country , 
                               other = "All Levels" , type = 'rds' ) 
                    starts_with_country = substr( f , 1, nchar(country)) == country 
                    not_dTs = !grepl('dTs', f , fixed = TRUE  )
                    return( f[ starts_with_country & not_dTs ] )
                  }
)

# data.files

## Find most recent file 

most_recent_data_files = tibble(
  formula = subject ,
  file = map_chr( data.files , most_recent_file ) 
) %>%
  mutate( date = map_chr( file, ~{
    ifelse( is.na( .x ) , NA , 
            gsubfn::strapply( .x, "[0-9-]{10,}", simplify = TRUE) 
    ) } ) ,
    days_old = ifelse( is.na( date ) , NA, Sys.Date() - anydate( date ) ) ,
    update = ifelse( days_old > 30 | is.na( date ) , TRUE , FALSE )
  )

# View( most_recent_data_files )

# Convert rda to excel ####

reconvert = FALSE   

for ( i in which( !most_recent_data_files$update ) ){
  
  print( i )  ; print( most_recent_data_files$formula[i] ) 

  rdsFile = most_recent_data_files$file[i]
  
  excelFileName = str_replace( rdsFile , ".rds" , "") %>% paste0( ".xlsx" )
  
  if (!file.exists( paste0( country.dir , rdsFile) ) ) next
  
  if ( file.exists( paste0( country.dir , excelFileName) ) & !reconvert  ) next
  
  rdsFileSplit = str_split( rdsFile, "_")[[1]]
  download_date = str_split( rdsFileSplit[length(rdsFileSplit)] , "\\.")[[1]][1]
  
  periods = paste( rdsFileSplit[ 4:7], collapse = "_")
  level = rdsFileSplit[ 3 ]
  
  data = readRDS( paste0( country.dir , rdsFile ) ) %>%
    filter( !is.na( COUNT ) )
  
  # tanslate data 
  if ( 'closedDate' %in% names( orgUnits )) {
    orgUnitCols = c( 'id', 'name', 'leaf', 'closedDate') 
  } else {
    orgUnitCols = c( 'id', 'name', 'leaf' )
  }
  
  dataset = data %>%
    mutate(
      SUM = as.numeric( SUM )  ,
      COUNT = as.numeric( COUNT )
    ) %>% 
    translate_fetch( . , 
                     # formulaElements = formula.elements  , 
                     formulaElements = dataElements ,
                     ous = orgUnits ) %>% 
    inner_join( orgUnits %>% select( {{ orgUnitCols }} ), 
                by = c('orgUnit' = 'id') ) %>%
    select( level, levelName, leaf, orgUnit , orgUnitName, period , 
            dataElement.id , dataElement,  
            categoryOptionCombo.ids , Categories ,
            SUM , COUNT ) %>%
    arrange( level , levelName, orgUnitName, orgUnit, desc( period ) , dataElement , Categories    )

  
  dataset.parent.child = dataset %>% select( orgUnit ) %>%
      distinct( orgUnit) %>%
      inner_join( ous %>% select( id, parent ) , by = c( 'orgUnit' = 'id') ) %>%
      filter( !is.na( parent ))
  
  data.tree <- FromDataFrameNetwork( dataset.parent.child )
  
  data.leaves = data.tree$Get("isLeaf")
  data.leaves = tibble( id = attributes(data.leaves)$name , 
                        effectiveLeaf = data.leaves )
  
  dataset = dataset %>%
      left_join( data.leaves , 
                 by = c( 'orgUnit' = 'id') ) %>%
      left_join( paths.translated , 
                 by = c( 'orgUnit' = 'id') ) 
  
  d.box = dataset %>%
    select( -dataElement , - Categories ) %>% 
    unite( "box" , dataElement.id , categoryOptionCombo.ids,
           sep = ".", remove = TRUE, na.rm = FALSE
    ) %>% 
    complete( box, period , nesting( leaf, effectiveLeaf , level, levelName, orgUnitName, orgUnit ) , 
              fill = list( SUM = 0 ,
                           COUNT = 0 
              ) ) 

  formula_expression = formulas %>% 
    filter( Formula.Name %in% most_recent_data_files$formula[i] ) %>%
    pull( Formula ) #Formula.id
  
  # TODO:
  # Sometimes the download has no values for one of the requested formula elements.
  # In that case, the formula will fail because it the column is missing
  # Revise formula expression to include available columns only
  
  d.box$box %>% unique
    
  sum.fe = paste("sum(c(" , str_replace_all( formula_expression , fixed("+") , "," ) , "), na.rm = TRUE)" )
  min.fe = paste("min(c(" , str_replace_all( formula_expression , fixed("+") , "," ) , "), na.rm = TRUE  )" )
  max.fe = paste("max(c(" , str_replace_all( formula_expression , fixed("+") , "," ) , "), na.rm = TRUE)" )
  

  # Pivot wider and sum 
  dataset_sum = d.box  %>%
    ungroup %>%
    select( - COUNT ) %>% 
    distinct %>% # remove duplicated rows, if exist due to download issues
    pivot_wider(
      names_from = box,
      values_from = SUM ) %>% 
    group_by( levelName, orgUnitName, orgUnit, 
              # parentName,  parent, 
              period, level, leaf ,
              effectiveLeaf )  %>% 
    summarise( 
      Total = eval( parse( text  = sum.fe ) )
      ) 

  # Pivot wider and count 
  dataset_count = d.box %>%
    select( - SUM ) %>%
    distinct %>% # remove duplicated rows, if exist due to download issues
    pivot_wider(
      names_from = box,
      values_from = COUNT ) %>%
    group_by( levelName, orgUnitName, orgUnit, 
              # parentName,  parent, 
              period, level, leaf ,
              effectiveLeaf )  %>% 
    summarise( Count.Complete = eval( parse( text  = min.fe ) ) ,
               Count.Any = eval( parse( text  = max.fe ) )
               # , any.Count = eval( parse( text  = any.fe ) )
    )
  
  dataset_summary = full_join( dataset_sum , 
                       dataset_count ,
                       by = c("levelName", "orgUnit" , "orgUnitName", "period", 
                              "level" , "leaf", "effectiveLeaf" )
  ) %>%
      # left_join( dataset %>% distinct( orgUnit, effectiveLeaf ), by = 'orgUnit' ) %>%
      left_join( paths.translated , 
                 by = c( 'orgUnit' = 'id') ) %>%
      arrange( level, leaf, orgUnitName, period )
  

# Write file 
metadata = tibble( `Formula Name` = formula.names[i] , 
                   `Period` = periods , 
                   `Organization Unit Levels` = level ,
                   Requested = download_date )
formula =  formulas %>% filter( Formula.Name %in% formula.names[i] )
formulaElements = formula.elements
formulaData = dataset
formulaSummaryDataset = dataset_summary

      wb <- createWorkbook()
      
      sheet1  <- addWorksheet( wb, sheetName = "Metadata")
      sheet2  <- addWorksheet( wb, sheetName = "Formula")
      sheet3  <- addWorksheet( wb, sheetName = "Formula Elements")
      sheet4  <- addWorksheet( wb, sheetName = "formulaData")
      sheet5  <- addWorksheet( wb, sheetName = "summaryData")
      
      writeDataTable( wb, sheet1, metadata , rowNames = FALSE)
      writeDataTable(  wb, sheet2, formula , rowNames = FALSE)
      writeDataTable( wb, sheet3,    formulaElements %>% 
                        select( dataElement.id , displayName , everything() ) , 
                      rowNames = FALSE)
      writeDataTable( wb, sheet4, formulaData , rowNames = FALSE)
      writeDataTable( wb, sheet5, formulaSummaryDataset , rowNames = FALSE)
      
      saveWorkbook( wb , paste0( country.dir, excelFileName ) , overwrite = TRUE )   
      
}





