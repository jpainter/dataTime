# Script to download data defined by formulas saved in the Magic Glasses (data dictionary) app

# setup =====

## Replace the text values for country and data.directory.  If working on multiple country instances, 
## suggest a different directory for each.


country = "Sierra Leone Demo" # e.g. "Malawi" must be in quotes

## location of folder with country data, metadata, etc.  
## In R, single backslash (\) slashes need to be replaced with either double back slash (\\), or forward slash(/)  

data.dir = "~/my_data_folder/malaria/Sierra_Leone_Demo"  

## DHIS2 address and credentials 

baseurl = "https://play.dhis2.org/2.36.3/" # note that need trailing "/"
username = "admin"
password = "district"

# Parameters for data requests
level = 'All levels' # all org units
YrsPrevious  = 5 # number of years of data to request 

# First time this file is run, a rather length process maps the facility hierarchy
# if need to update, change to TRUE 
ous_update = FALSE   

# for quarterly reports (ignore) 
QR = FALSE 

# libraries and functions ####
pacman::p_load( scales, knitr, scales ,  #gsubfn  for strapplyc, line 221
        tidyverse, progress , readxl, patchwork ,
        tsibble, fable, fabletools, feasts, slider, anomalize ,
        # gsubfn, proto ,
        furrr, tictoc, ggrepel , sf , ggspatial ,
        mdthemes, extrafont , hrbrthemes , openxlsx ,
        data.tree, igraph , magrittr , progressr
        )


source('dhis2_functions.R')  
source( 'Deviation_Expected_Functions.R')
# source( '../dhis2_dictionary/API.r')
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

if ( is.null( data.dir) ) data.dir = dir( country )

# make sure directory has terminal slash
has.slash.at.end = str_locate_all( data.dir , "/") %>% 
  unlist %in% nchar( data.dir) %>% any 
if ( !has.slash.at.end  ){ data.dir = paste0( data.dir , "/" ) }


formula.file = files( 'Formula' , dir = data.dir , country = country ) %>% most_recent_file()

formulas =  read_excel( paste0( data.dir, formula.file ) , sheet = 'Formula') %>% 
  filter( !is.na( Formula.Name ) ) 

formula.names = formulas$Formula.Name 

formula.elements = read_excel( 
    paste0( data.dir ,  formula.file ), 
    sheet = 'Formula Elements') 
  
## Adjust when using an MDIVE quarterly report formula
## (otherwise ignore) 
QR = FALSE 
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

View( formulas )

# Reorder 
formula_order = formulas$Formula.Name %>%
  map( ., ~ str_split(.x , ' '))  %>%
  map( 1 ) %>% map_chr( 1 ) %>% parse_number() %>% order

formulas = formulas[ formula_order , ]

# Metadata ####
metadata.filename = metadata.file( data.dir, country )
orgUnitLevels = OrgUnitLevels( metadata.filename  )
orgUnits = OrgUnits( metadata.filename )
dataSets = DataSets(  metadata.filename )
dataElements = DataElements( metadata.filename )

# OUS structure ####
ous = orgUnits
ous.levels = orgUnitLevels


# NIGERIA Exception
## Nigeria appears to have 6 levels, the last not named
count( orgUnits , level )
count( orgUnits , levelName )
if ( country == 'Nigeria' ){
  ous.levels = ous.levels %>% 
  bind_rows( tibble(
    level = 6, levelName = "sub_Facility" 
  ))
}

ous.id_parent = ous %>% select( id, parent ) 
# Remove top value if it has missing parent.
ous.id_parent = ous.id_parent %>% filter( !is.na( parent ) )


# orgUnit.tree and path ----
ous.tree.file = files('ous.tree' , country = country , type = 'rds') %>% 
  most_recent_file()

ous.tree.file = paste0( data.dir, ous.tree.file )

if( !ous_update & file.exists( ous.tree.file ) ){
  print( paste( 'reading' , ous.tree.file ))
  ous.tree = readRDS( ous.tree.file  ) %>% as.Node()
} else {
    print( 'preparing ous.tree')
    ous.tree = FromDataFrameNetwork( ous.id_parent )
    ous.tree.list = as.list( ous.tree )
    ous.tree.file = paste0( data.dir, 'ous.tree_' , Sys.Date() , ".rds" )
    saveRDS( ous.tree.list , 
             file = ous.tree.file    ,
             compress = FALSE )
    print( paste( 'ous.tree saved to' , ous.tree.file ) ) ; toc()
}

pathsFileName = files('paths' , country = country , type = 'rds') %>% 
  most_recent_file()
pathsFileName = paste0( data.dir, pathsFileName )


if ( file.exists( pathsFileName )  & !ous_update ){
  print( paste( 'reading', pathsFileName ) )
  paths = readRDS( pathsFileName  )
} else {
  print( 'preparing paths...' )

  print( 'deteriming nodes' ); tic()
  dti = as.igraph.Node( ous.tree )
  nodes = V(dti)
  node.attributes = nodes %>% attributes() 
  ids = node.attributes$names
  toc()
  
  # sllllooowww
  n = length( nodes )
  print( paste( 'there are', n , 'nodes.  Now preparint path for each'))
  tic()

  pb <- progress_bar$new( total = n  ,
                          format = "(:current) [:bar] :percent :eta :elapsedfull" ) 
  paths = map( 1:n , ~{ # skip one becuase it is the root 
    pb$tick()
    
    p = all_simple_paths( dti, nodes[.x]  , 1 )
    
    # for national level, simple path is empty
    if ( is_empty( p ) )  p = list( c( nodes[.x]  ) )
    
    as_ids( p[[1]] ) %>% rev %>% t %>%
      as_tibble() %>%
      set_colnames( ous.levels$levelName[ 1:length(p[[1]]) ] ) %>%
      mutate( id = ids[.x] ) %>%
      select( id, everything() )
  }
  ) 
  paths = bind_rows( paths )
  pathsFileName = paste0( data.dir , country, 
                          '_paths_' , Sys.Date() , '.rds' ) 
  saveRDS( paths, pathsFileName )
  print( 'prepared paths and save to' , pathsFileName  ); toc()
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

if ( any( is.null( c(baseurl, username, password ) ))){
  credentials = read_excel( paste0( "../dataDictionary/dhis2_dictionary/Formulas/",
                                    "_Instances_jp.xlsx")) %>%
    filter( Instance == country )
  
  baseurl = credentials$IPaddress
  username = credentials$UserName
  password = credentials$Password

}

loginurl <-paste0( baseurl , "api/me" )
GET( loginurl, authenticate( username, password ) )



# Request data ----


# QR periods 
# periods = "202010;202011;202012;202101;202102;202103"

for ( i in which( most_recent_data_files$update ) ){
  
  print( 'Formula.Name' ) ; print(  most_recent_data_files$formula[i] )
  
  elements = api_formula_elements( most_recent_data_files$formula[i] , data.dir  ) %>%
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
  
  # Periods
  periodType = formula.elements %>%
    filter( Formula.Name %in% most_recent_data_files$formula[i] ) %>%
    pull( periodType ) %>% max
  
  if ( periodType == 'Monthly') periods = date_code( YrsPrevious = YrsPrevious ) # 'months_last_5_years' # 

  if ( periodType == 'Weekly') periods = date_code_weekly( YrsPrevious = YrsPrevious )

  
  print( 'period' ); print( periods )
  
  tic()
  Sys.time()
  
  if ( ! loginDHIS2(baseurl, username, password ) ) stop()
  
  x  = api_data( periods = periods , 
                 level = level , 
                 elements = elements, 
                 baseurl = baseurl , 
                 formula = most_recent_data_files$formula[i] ,
                 update = most_recent_data_files$update[i] & 
                   !is.na(most_recent_data_files$file[i]) ,
                 check_previous_years = YrsPrevious  , 
                 previous_dataset_file = paste0( data.dir , 
                                                 most_recent_data_files$file[i]) )
  
  min_period = min(str_split(periods, ";")[[1]])
  max_period = max(str_split(periods, ";")[[1]])
  period_string = ifelse( min_period == max_period , periods , glue::glue( min_period , "_", max_period ) )
  save_to_filename =  paste0( data.dir, country, "_" , most_recent_data_files$formula[i]  , "_" , level ,"_", 
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

# Translate, combine ous, and save  ####

reconvert = FALSE    
xlsx = FALSE
summary =  FALSE

### \\TODO: if previous data file does not exist, skip counts

for ( i in which( !most_recent_data_files$update ) ){

  print( i )  ; print( most_recent_data_files$formula[i] ) 

  rdsFile = most_recent_data_files$file[i]
  
  if ( !file.exists( paste0( data.dir , rdsFile) ) & !reconvert ) next
  
  rdsFileSplit = str_split( rdsFile, "_")[[1]]
  download_date = str_split( rdsFileSplit[length(rdsFileSplit)] , "\\.")[[1]][1]
  
  periods = paste( rdsFileSplit[ 4:7], collapse = "_")
  level = rdsFileSplit[ 3 ]
  
  data = readRDS( paste0( data.dir , rdsFile ) ) %>%
    filter( !is.na( COUNT ) )
  
  # tanslate data 
  if ( 'closedDate' %in% names( orgUnits ) ) {
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
    left_join( orgUnits %>% select( {{ orgUnitCols }} ), 
                by = c('orgUnit' = 'id') ) %>%
    select( level, levelName, leaf, orgUnit , orgUnitName, period , 
            dataElement.id , dataElement,  
            categoryOptionCombo.ids , Categories ,
            SUM , COUNT ) %>%
    data.table::as.data.table()
  
  dataset = dataset[ order( level , levelName, orgUnitName, orgUnit, 
                           -period , dataElement , Categories ) ]

  ### problem with network when there are orphan facilities --
  ### the listed parent is not found among orgUnits.  Trying simpler
  ### method based on whether COUNT ==1.  There are some facilties that are both,
  ### so filter to those that are only ever COUNT == 1
  # dataset.parent.child = dataset %>% select( orgUnit ) %>%
  #     distinct( orgUnit) %>%
  #     left_join( ous %>% select( id, parent ) , by = c( 'orgUnit' = 'id') ) %>%
  #     filter( !is.na( parent ))
  # 
  # data.tree <- FromDataFrameNetwork( dataset.parent.child )
  # 
  # data.leaves = data.tree$Get("isLeaf")
  # data.leaves = tibble( id = attributes(data.leaves)$name , 
  #                       effectiveLeaf = data.leaves )
  
  data.leaves = dataset %>%
    as_tibble() %>%
    group_by( orgUnit ) %>%
    summarise( n = max( COUNT ) ) %>%
    mutate( effectiveLeaf = ifelse( n == 1, TRUE, FALSE ) ) %>%
    select( orgUnit , effectiveLeaf ) %>%
    rename( id = orgUnit )
  
  # add paths 
  dataset = dataset %>%
      left_join( data.leaves ,
                 by = c( 'orgUnit' = 'id') ) %>%
      left_join( paths.translated , 
                 by = c( 'orgUnit' = 'id') ) 
  
  formulaData.filename = str_replace( most_recent_data_files$file[i] ,
                                ".rds" , "_formulaData.rds")
  saveRDS( dataset , paste0( data.dir , formulaData.filename ) )

if ( summary | xlsx ){
  ### NB: can make column for each 'box' by id or by label
  ### need to adjust to match the formula expressions
  d.box = dataset %>%
    # select( -dataElement , - Categories ) %>%
    # unite( "box" , dataElement.id , categoryOptionCombo.ids,
    #        sep = ".", remove = TRUE, na.rm = FALSE
    # ) %>%
    select( -dataElement.id , - categoryOptionCombo.ids ) %>%
    unite( "box" , dataElement , Categories ,
           sep = ".", remove = TRUE, na.rm = FALSE
    ) %>%
    complete( box, period , 
              nesting( leaf, effectiveLeaf , level, levelName, orgUnitName, orgUnit ) , 
              fill = list( SUM = 0 ,
                           COUNT = 0 
              ) ) 

  formula_expression = formulas %>% 
    filter( Formula.Name %in% most_recent_data_files$formula[i] ) %>%
    pull( Formula )
  
  # TODO:
  # Sometimes the download has no values for one of the requested formula elements.
  # In that case, the formula will fail because it the column is missing
  # Revise formula expression to include available columns only
  
  box_vars = d.box$box %>% unique
  
  backtick <- function(x) paste0("`", x, "`")
  
  # get formula, and limit to available vars in box
  formula_box = str_replace_all( formula_expression , fixed("+") , "," ) %>%
    str_replace_all( . , fixed("[") , "" ) %>%
    str_replace_all( . , fixed("]") , "" ) %>%
    str_split( " , ") %>% unlist %>% 
    backtick() %>%
    intersect( backtick( box_vars ) ) %>% 
    paste(. , collapse = " , " )
  
  sum.fe = paste("sum(c(" , formula_box , "), na.rm = TRUE)" ) 
  min.fe = paste("min(c(" , formula_box , "), na.rm = TRUE)" ) 
  max.fe = paste("max(c(" , formula_box , "), na.rm = TRUE)" ) 
  

  # Pivot wider and sum 
  tic()
  dataset_sum = d.box  %>%
    ungroup %>%
    select( - COUNT ) %>% 
    distinct %>% # remove duplicated rows, if exist due to download issues
    pivot_wider(
      names_from = box,
      values_from = SUM ) %>%
    group_by( level, levelName, orgUnitName, orgUnit,
             # parentName,  parent,
             period, leaf ,
             effectiveLeaf ) %>%
    summarise(
      Total = eval( parse( text  = sum.fe ) )
      )
  toc()

  # Pivot wider and count 
  tic()
  dataset_count = d.box %>%
    select( - SUM ) %>%
    distinct %>% # remove duplicated rows, if exist due to download issues
    pivot_wider(
      names_from = box,
      values_from = COUNT ) %>%
    group_by( level, levelName, orgUnitName, orgUnit, 
              # parentName,  parent, 
              period, leaf ,
              effectiveLeaf )  %>% 
    summarise( Count.Complete = eval( parse( text  = min.fe ) ) ,
               Count.Any = eval( parse( text  = max.fe ) )
               # , any.Count = eval( parse( text  = any.fe ) )
    )
  toc()
  
  dataset_summary = full_join( dataset_sum , 
                       dataset_count ,
                       by = c("level", "levelName", 
                              "orgUnit" , "orgUnitName", 
                              "leaf", "effectiveLeaf" ,
                              "period"
                                )
  ) %>%
      # left_join( dataset %>% distinct( orgUnit, effectiveLeaf ), by = 'orgUnit' ) %>%
      left_join( paths.translated , 
                 by = c( 'orgUnit' = 'id') ) 
  
  # dataset_count =  dataset_count[ order( level, leaf, orgUnitName, period ) ]
  

# Write file 


  summaryData.filename = str_replace( most_recent_data_files$file[i] ,
                                  ".rds" , "_summaryData.rds")
  saveRDS( dataset_summary , paste0( data.dir , summaryData.filename ) )
  # end summary
  }


if ( xlsx ){
    excelFileName = str_replace( rdsFile , ".rds" , "") %>% paste0( ".xlsx" )

    if ( file.exists( paste0( data.dir , excelFileName) ) & !reconvert  ) next

    metadata = tibble( `Formula Name` = formula.names[i] , 
                       `Period` = periods , 
                       `Organization Unit Levels` = level ,
                       Requested = download_date )
    formula =  formulas %>% filter( Formula.Name %in% formula.names[i] )
    formulaElements = formula.elements %>% filter( Formula.Name %in% most_recent_data_files$formula[i] )
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
      
      saveWorkbook( wb , paste0( data.dir, excelFileName ) , overwrite = TRUE )   
}

# end 
}





