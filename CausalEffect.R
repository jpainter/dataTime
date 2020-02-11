#kay Brodersen kbrodersen@google.com

# google.github.io/CausalImpact.html

library( CausalImpact )

# Data from 'Malawi compare pos with conf cases.R'

all = confCase.ts %>% 
    index_by( mth ) %>% 
    summarise_all( ~sum(. , na.rm = TRUE ) )

all %>% 
    rename( a = series[5] ) %>% 
    gg_season( a ) +
    xlab( "Month" ) + ggtitle( 'Facility with no gaps' )

f = sample( which( ! hasGaps ) , 1 ) # an orgunit with no gaps

oneFacility = confCase.ts %>% 
    filter( orgUnit %in% units[ f ] )  %>% 
    tsibble::fill_gaps() %>%
    na_kalman() 

matplot( oneFacility, type = "l") # base graphics 
# oneFacility.scale = oneFacility %>% 
#     mutate_if( is_numeric , scale )
# matplot( oneFacility.scale , type = "l") # base graphics 

prepData = bind_cols( oneFacility[, c(7)] , 
                      # all[, c(6)] ,
                      allSotckOut.ts[,2] )  %>% 
    as_tibble() %>%     
    mutate_if( is_numeric , scale ) %>%
    set_colnames( c('y', 'x1' ) ) %>% ts

data = as.zoo( prepData )
matplot(data, type = "l", main = f )

pre.period <- c(1, 24)
post.period <- c(25,36)

impact <- CausalImpact( data , pre.period, post.period ,
                        model.args = list( nseasons = 12 )
                        )

plot( impact )
