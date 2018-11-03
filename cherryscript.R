##########################################
#parks by community area
##########################################
library(rgdal)
library(sp)
library(dplyr)
library(sf)
library(tidyverse)
library(raster)

chicagoparks <- readOGR('4A/MSCI 446/R/chicagoparksshapefile2', 'geo_export_287c1e81-adfc-4076-bbd4-7ac4b1ca62c2')
chicagocommunityareas <- readOGR('4A/MSCI 446/R/communityareashapefile', 'geo_export_f2c553e7-eb62-4773-9655-8037a1bdd109')

#TEST TO SEE HOW TO ACTUALLY GET INTERSECTION AND AREA OF INTERSECTION
horanpark <- chicagoparks[which(chicagoparks@data$park=="HORAN (ALBERT)"),]
eastgarfield <- chicagocommunityareas[which(chicagocommunityareas@data$community=="EAST GARFIELD PARK"),]
Intersects <- raster::intersect(horanpark, eastgarfield)
area(Intersects)

#RUN THIS CODE FOR TOTAL PARK AREA FOR EACH COMMUNITY AREA
totalParkAreaForCommunityAreas <- rep(0, nrow(chicagocommunityareas))
for(i in 1:nrow(chicagocommunityareas)) {
  totalArea <- 0
  for (j in 1:nrow(chicagoparks)) {
    intersect <- raster::intersect(chicagocommunityareas[i, ], chicagoparks[j, ])
    if (!is.null(intersect)) {
      totalArea <- totalArea + area(intersect)
    }
  }
  totalParkAreaForCommunityAreas[i] <- totalArea
}



#OLD CODE: CHICAGO PARKS BY WARDS
#chicagoparksdf <- data.frame(c(chicagoparks$ward), c(chicagoparks$acres))
#names(chicagoparksdf) <- c("ward", "acres")
#parkacresbyward <- aggregate(.~ward, data=chicagoparksdf, mean)

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
