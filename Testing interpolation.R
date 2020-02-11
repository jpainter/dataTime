# Testing interpolation

olympic_running %>% 
  model(TSLM(Time ~ trend())) %>% 
  interpolate(olympic_running)


# fourier 
olympic_running %>% 
  model( ARIMA( log( Time )  ) ) %>% 
  interpolate( olympic_running )

olympic_running %>% 
  model( ARIMA( log( Time ) ~ fourier(K = 1)  ) ) %>% 
  interpolate( olympic_running )

