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
chicagocommunityareas <- readOGR('4A/MSCI 446/R/communityareashapefile', 'geo_export_f2c553e7-eb62-4773-9655-8037a1bdd109', stringsAsFactors = FALSE)

#TEST TO SEE HOW TO ACTUALLY GET INTERSECTION AND AREA OF INTERSECTION
horanpark <- chicagoparks[which(chicagoparks@data$park=="HORAN (ALBERT)"),]
eastgarfield <- chicagocommunityareas[which(chicagocommunityareas@data$community=="EAST GARFIELD PARK"),]
Intersects <- raster::intersect(horanpark, eastgarfield)
area(Intersects)

#RUN THIS CODE FOR TOTAL PARK AREA FOR EACH COMMUNITY AREA#################################
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
#print the dataframe consisting of three columns: commArea, commAreaNum, and totalParkArea
library(readxl)
censusdata <- read_excel("4A/MSCI 446/R/Census-Data-by-Chicago-Community-Area-2017 (2).xlsx")
censusdata <- data.frame(censusdata$Community, censusdata$CommunityAreaNumber)
names(censusdata) <- c('Community', 'communityAreaNumber')
censusdata$Community <- toupper(censusdata$Community)
chicagocommunityareas@data$community[75] <- 'O\'HARE'
communityAreaNumber <- rep(0, nrow(chicagocommunityareas))
for(i in 1:nrow(chicagocommunityareas)) {
  communityAreaNumber[i] <- censusdata[which(censusdata$Community==chicagocommunityareas@data$community[i]),2]
}
totalParkAreaDF <- data.frame(chicagocommunityareas@data$community, communityAreaNumber, totalParkAreaForCommunityAreas)
names(totalParkAreaDF) <- c('Community', 'communityAreaNumber', 'totalParkArea')
write.csv(totalParkAreaDF, 'totalParkAreaByCommunityArea.csv')
#RUN THIS CODE FOR TOTAL PARK AREA FOR EACH COMMUNITY AREA#################################

#OLD CODE: CHICAGO PARKS BY WARDS
#chicagoparksdf <- data.frame(c(chicagoparks$ward), c(chicagoparks$acres))
#names(chicagoparksdf) <- c("ward", "acres")
#parkacresbyward <- aggregate(.~ward, data=chicagoparksdf, mean)

##########################################
#poverty & race by community area
##########################################
#did most of the conversion in excel, and using R to just create a csv of it.
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

##########################################
#Public Safety Data
##########################################
library(rgdal)
library(sp)
library(dplyr)
library(sf)
library(tidyverse)
library(raster)

hospitals <- readOGR('4A/MSCI 446/R/Hospitals', 'Hospitals', stringsAsFactors = FALSE)
numHospitalsPerCommunityArea <- as.data.frame(table(hospitals@data$AREA_NUMBE))
names(numHospitalsPerCommunityArea) <- c('communityAreaNum', 'numHospitals')
numHospitalsPerCommunityArea$communityAreaNum <- as.numeric(levels(numHospitalsPerCommunityArea$communityAreaNum))
#numHospitalsPerCommunityArea$communityAreaNum <- as.numeric(numHospitalsPerCommunityArea$communityAreaNum)

for (i in 1:77) {
  if (sum(numHospitalsPerCommunityArea$communityAreaNum == i) == 0) {
    newDF <- data.frame(i,0)
    names(newDF)<-c('communityAreaNum', 'numHospitals')
    numHospitalsPerCommunityArea <- rbind(numHospitalsPerCommunityArea, newDF)
  }
}


##########################################
#Combining all the data
##########################################

avgSchoolRating <- read.csv("4A/MSCI 446/R/explanatoryvariables/avg_school_rating_by_community.csv")
avgSSLscore <- read.csv("4A/MSCI 446/R/explanatoryvariables/avg_ssl_score_by_community.csv")
censusData <- read.csv("4A/MSCI 446/R/explanatoryvariables/censusdataByCommunityArea.csv")
typesOfCrimes <- read.csv("4A/MSCI 446/R/explanatoryvariables/crime_count_in_community.csv")
predictedVarDF <- read.csv("4A/MSCI 446/R/explanatoryvariables/total_crime_by_community.csv")
totalParkArea <- read.csv("4A/MSCI 446/R/explanatoryvariables/totalParkAreaByCommunityArea.csv")

#because totalParkArea dataframe is not sorted by ascending community area number:
totalParkArea <- totalParkArea[order(totalParkArea$communityAreaNumber),] 

predTable <- data.frame(totalParkArea$Community, 
                        totalParkArea$communityAreaNumber,
                        predictedVarDF$violent_crime * 1000 / (predictedVarDF$population.2010.),
                        avgSchoolRating$avg_rating,
                        avgSSLscore$avg_rating,
                        totalParkArea$totalParkArea,
                        censusData[,4:ncol(censusData)],
                        typesOfCrimes[,3:ncol(typesOfCrimes)]
)
names(predTable) <- c(
  "community",
  "communityAreaNum",
  "percentViolentCrimePer1000Population",
  "avgSchoolRating",
  "avgSSLRating",
  "totalParkArea",
  "hispanic",
  "black",
  "white",
  "asian",
  "other",
  "percentChildrenInPov",
  names(typesOfCrimes[,3:ncol(typesOfCrimes)])
)

#write to csv
write.csv(predTable, 'predTable.csv')