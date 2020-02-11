# Testing Outliers

library(tsoutliers)
library(expsmooth)
library(fma)

outlier.chicken <- tsoutliers::tso(chicken,types = c("AO","LS","TC"),maxit.iloop=10)
outlier.chicken
plot(outlier.chicken)

ccts0 = datasetTS.0 %>% as_tibble() %>%
    select( `NMCP OPD Confirmed Malaria Cases Through RDT_<5Yrs`) %>%
    as.ts()

outlier.ccts0 = tsoutliers::tso( ccts0 , types = c("AO"),
                                 cval = 3.5 ,
                                 maxit.iloop=10)
outlier.ccts0
plot( ccts0 )
plot(outlier.ccts0)
