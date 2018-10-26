##########################################
#parks by community area
##########################################
library(rgdal)
library(sp)
library(dplyr)

chicagoparks <- readOGR('4A/MSCI 446/R/chicagoparksshapefile2', 'geo_export_287c1e81-adfc-4076-bbd4-7ac4b1ca62c2')

chicagoparksdf <- data.frame(c(chicagoparks$ward), c(chicagoparks$acres))
names(chicagoparksdf) <- c("ward", "acres")

parkacresbyward <- aggregate(.~ward, data=chicagoparksdf, mean)

plot(chicagoparks)

##########################################
#poverty & race by community area
##########################################

#did most of the conversion in excel
library(readxl)
censusdata <- read_excel("4A/MSCI 446/R/Census-Data-by-Chicago-Community-Area-2017 (2).xlsx")
censusdata <- data.frame(censusdata$Community, 
                         censusdata$CommunityAreaNumber, 
                         censusdata$Hispanic, 
                         censusdata$Black,
                         censusdata$White,
                         censusdata$Asian,
                         censusdata$Other,
                         censusdata$PercentChildrenInPoverty)
names(censusdata) <- c('Community', 'communityAreaNumber', 'Hispanic', 'Black', 'White', 'Asian', 'Other', 'PercentChildrenInPoverty')
write.csv(censusdata, 'censusdataByCommunityArea.csv')
