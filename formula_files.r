
file.dir = function( country = 'Sierra Leone' ,
                     dir.base = '../dataDictionary/dhis2_dictionary/Formulas/' ){
  paste0( dir.base , country , "/")
}

formula_data_files = function(  search = 'All' , 
                               dir = NULL ,
                               type = 'xlsx' , 
                               other = "" , ... 
                               ){
                        
                      if (is.null( dir ) ) dir = file.dir( ... )
                      
                      dir.files = list.files( dir, recursive = TRUE )
                      search_and_type =  
                          str_detect( dir.files, fixed( search , ignore_case=TRUE )  )  &
                           grepl( paste0( type , '$' ) , dir.files, 
                                  ignore.case =  TRUE  ) &
                          grepl( other , dir.files , ignore.case = TRUE ) 
                      files = dir.files[  search_and_type   ]
                      return( files[ rev( order( files )) ]  )
                      }