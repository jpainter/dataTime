# Rainfall time-series
library( tidyverse )
library( fpp3 )

folder = "../ARC/"
file = 'arc_adm2_rainfall.rds'

arc = readRDS( paste0( folder , file ) )

# glimpse( arc )
# Malawi ts ####
malArc = arc %>% 
    filter( country %in% 'Malawi' ) %>%
    mutate( 
        mnth = yearmonth( paste0( year, "-" , month ))
        ) 

# count( malArc , mnth )

keys = quo( c( country, iso3, NAME_0 , NAME_1 , NAME_2 ) )

# check for duplicates 
 # malArc %>%
 #    select( - year, - month) %>% 
 #     duplicates( key = !!keys , index = mnth ) %>% View
     
malArc.ts = malArc %>%
    select( - year, - month ) %>%
    as_tsibble( index =  mnth , key = !!keys ) %>%
    fill_gaps()  

# Plot by region, with line for each district
malArc.ts %>% autoplot( rain_mean ) + 
    facet_wrap( ~NAME_1 ) +
    guides( color = FALSE )

# Nattional aggregate ####
malArc.ts.0 = malArc.ts %>%
    group_by( country , iso3, NAME_0 ) %>%
    summarise( rain_mean = mean( rain_mean , na.rm = TRUE ) ,
               rain_total = sum( rain_total , na.rm = TRUE )) %>%
    mutate_if( is.numeric , scale  )

malArc.ts.0 %>% autoplot( rain_mean , color = 'blue') + 
    autolayer( malArc.ts.0 , rain_total , color = 'red')

# Season plot - national 
malArc.ts.0 %>% gg_season( rain_mean ) + 
    labs( title = 'Malawi' , 
          subtitle = 'ARC2 mean monthly precipitation')

malArc.ts.0 %>% filter( mnth >= yearmonth( '2015-Jan') ) %>%
    gg_season( rain_total ) + 
    labs( title = 'Malawi' , 
          subtitle = 'ARC2 total monthly precipitation')


# ACF
malArc.ts.0 %>% ACF( rain_mean ) %>% autoplot()

# Regional aggregate ####
malArc.ts.1 = malArc.ts %>%
    group_by( country , iso3, NAME_1 ) %>%
    summarise( rain_mean = mean( rain_mean , na.rm = TRUE ) ,
               rain_total = sum( rain_total , na.rm = TRUE )) %>%
    mutate_if( is.numeric , scale  )

# Season plot - Regional ( Last 5 Years )
malArc.ts.1 %>% filter( mnth >= yearmonth( '2015-Jan') ) %>%
    gg_season( rain_mean ) + 
    facet_wrap( ~NAME_1 ) +
    guides( color = FALSE ) +
    labs( title = 'Malawi' , 
          subtitle = 'ARC2 mean monthly precipitation')

# ACF
malArc.ts.1 %>% 
    filter( !is.na( rain_mean ) ) %>% 
    ACF( rain_mean ) %>% autoplot() + facet_wrap( ~NAME_1 ) +
    guides( color = FALSE )

# Last 5 years ####
malawiRain = malArc.ts.0 %>% 
    as_tsibble( index = mnth , key = country ) %>%
        filter( mnth > yearmonth('2015-Jan')) 
