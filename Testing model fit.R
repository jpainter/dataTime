

fit.train.0 = train.0.cross %>% 
    model( RW( confCases  ~ drift()) 
           # , ets = ETS( confCases  ~ trend("Ad") )
           , fourier1 = ARIMA( confCases ~ fourier(K = 1) + PDQ(0,0,0)) 
           , arima = ARIMA( log( confCases ) )
           ) 
