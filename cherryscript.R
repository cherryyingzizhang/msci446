library(rgdal)
library(sp)
library(dplyr)

chicagoparks <- readOGR('4A/MSCI 446/R/chicagoparksshapefile2', 'geo_export_287c1e81-adfc-4076-bbd4-7ac4b1ca62c2')

chicagoparksdf <- data.frame(c(chicagoparks$ward), c(chicagoparks$acres))
names(chicagoparksdf) <- c("ward", "acres")

parkacresbyward <- aggregate(.~ward, data=chicagoparksdf, mean)

plot(chicagoparks)
