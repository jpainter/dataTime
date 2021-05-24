
# Outlier function

outlier = function( 
    ts ,
    dataCols = dataCols ,
    name = "mad10" ,
    .MAD = 10 ,
    method = "" ,
    ...
){
    
    df =  
        ts %>% 
        
        select( - starts_with('Count') ) %>%
        
        pivot_longer( {{ dataCols }} , 
                      values_to = 'original' ,
                      names_to = 'vars' ) %>% 
        
        
        group_by( orgUnit , vars ) %>% 
        
        mutate( {{ name }} := clean_ts( original , 
                              interpolate = FALSE , 
                              .clean = "" ,
                              MAD = .MAD )  
        )  %>% 
        
        ungroup() %>%
        
        mutate(
            is.outlier = is.na( !! sym( name ) ) & !is.na( original )
        ) 
    
    return( df )
    
}

# TEST : 
extreme = outlier( ts = dTs, name ='extreme', dataCols = dataCols,
                   MAD= 15 ) %>% as_tibble() %>%
    count( is.outlier ) %>%
            mutate( percent = percent( n / sum(n) )
            )


# test = 
    map_df( c(3,5,10) ,
        ~ outlier( ts = dTs, name ='extreme',
                     dataCols = dataCols,
                     .MAD = .x ) %>%
            count( is.outlier ) %>%
            mutate( percent = percent( n / sum(n) ) ,
                    MAD = .x
            )
    )
test

