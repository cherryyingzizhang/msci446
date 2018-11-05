# graphics.off()
explanatory <- data.frame(predTable$communityAreaNum,
                          predTable$percentViolentCrimePer1000Population,
                          predTable$avgSchoolRating,
                          predTable$avgSSLRating,
                          predTable$totalParkArea,
                          predTable$numHospitals,
                          predTable$teenMomRate,
                          predTable$infantMortalityRate,
                          predTable$hispanic,
                          predTable$black,
                          predTable$white,
                          predTable$asian,
                          predTable$other,
                          predTable$percentChildrenInPov)
names(explanatory) <- c('community', 'percent_violent_crime_per_1000_population',
                        'avg_school_rating', 'avg_SSL', 'total_park_area',
                        'num_hospitals', 'teen_mom_rate', 'infant_mortality_rate',
                        'race_hispanic', 'race_black', 'race_white',
                        'race_asian', 'race_other', 'percent_children_in_poverty')
col_names <- colnames(explanatory)

# normalize SSL (266.0711 - 304.1068)
explanatory$avg_SSL <- (explanatory$avg_SSL-min(explanatory$avg_SSL))/(max(explanatory$avg_SSL) - min(explanatory$avg_SSL))

# num_hospital to binary
explanatory$num_hospital_greater_than_3 <- NA
explanatory$num_hospital_greater_than_3 <- explanatory$num_hospitals >= 3

for(i in 3:14) {
  plot(explanatory[,i], explanatory$percent_violent_crime_per_1000_population,
       xlab=col_names[i], ylab="% violent crimes per 1000 population", pch=17)
}

plot(explanatory[,15], explanatory$percent_violent_crime_per_1000_population,
     xlab=col_names[15], ylab="% violent crimes per 1000 population", pch=18)
axis(1, at=1:2, labels=c('FALSE', 'TRUE'))

# park hospital teen_mom_rate infant_mortality races(4) poverty_children