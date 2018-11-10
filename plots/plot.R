# graphics.off()
# cat('\014')

explanatory <- data.frame(predTable$communityAreaNum,
                          predTable$percentViolentCrimePer1000Population,
                          predTable$avgSchoolRating,
                          predTable$avgSSLRating,
                          predTable$totalParkArea,
                          predTable$numHospitals,
                          predTable$teenMomRate,
                          predTable$infantMortalityRate,
                          100*predTable$hispanic,
                          100*predTable$black,
                          100*predTable$white,
                          100*predTable$asian,
                          100*predTable$other,
                          100*predTable$percentChildrenInPov)
names(explanatory) <- c('community', 'number_of_violent_crimes_per_1000_population',
                        'Average_School_Rating', 'Normalized_Average_SSL', 'Total_Park_Area_(m2)',
                        'Number_of_Hospitals', 'Number_of_Teen_Moms_/_1000_Female_Teenagers', 'Number_of_Infant_Mortality_/_1000_Live_Births',
                        'Percent_of_Hispanic_(%)', 'Percent_of_Black_(%)', 'Percent_of_White_(%)',
                        'Percent_of_Asian_(%)', 'Percent_of_Other_Race_(%)', 'Percent_of_Children_in_Poverty_(%)')

# normalize SSL (266.0711 - 304.1068)
explanatory$Normalized_Average_SSL <- (explanatory$Normalized_Average_SSL-min(explanatory$Normalized_Average_SSL))/(max(explanatory$Normalized_Average_SSL) - min(explanatory$Normalized_Average_SSL))

# num_hospital to binary
explanatory$Whether_Community_Has_3_or_More_Hospitals <- NA
explanatory$Whether_Community_Has_3_or_More_Hospitals <- explanatory$Number_of_Hospitals >= 3

col_names <- colnames(explanatory)

# plot everything
for(i in 3:14) {
  plot(explanatory[,i], explanatory$number_of_violent_crimes_per_1000_population,
       main=paste('Violent Crime Rate V.S.', gsub( '\\s*\\([^\\)]+\\)', '', gsub('_', ' ', col_names[i]))),
       xlab=gsub('_', ' ', col_names[i]), ylab='Number of Violent Crimes / 1000 Population', pch=18)
}

plot(explanatory[,15], explanatory$number_of_violent_crimes_per_1000_population,
     main=paste('Violent Crime Rate V.S. ', gsub( '\\s*\\([^\\)]+\\)', '', gsub('_', ' ', col_names[15]))),
     xaxt='n', xlim=c(-1,2), xlab=gsub('_', ' ', col_names[15]), ylab='Number of Violent Crimes / 1000 Population', pch=18)
axis(1, at=0:1, labels=c('FALSE', 'TRUE'))

# correlation
# park hospital teen_mom_rate infant_mortality races(4) poverty_children

# histograms
hist(explanatory$number_of_violent_crimes_per_1000_population, 
     col='grey',
     main='Number of Violent Crimes / 1000 Population (Histogram)',
     xlab='Number of Violent Crimes / 1000 Population', ylab='Number of Communities')

for(i in 3:14) {
  hist(explanatory[,i],
       col='grey',
       main=paste(gsub( '\\s*\\([^\\)]+\\)', '', gsub('_', ' ', col_names[i])), '(Histogram)'),
       xlab=gsub('_', ' ', col_names[i]), ylab='Number of Communities')
}

# box plots
boxplot(explanatory$number_of_violent_crimes_per_1000_population, data=explanatory,
        col='grey',
        main="Number of Violent Crimes / 1000 Population (Box Plot)",
        xlab="Number of Violent Crimes / 1000 Population")

for(i in 3:14) {
  boxplot(explanatory[,i], data=explanatory,
          col='grey',
          main=paste(gsub( '\\s*\\([^\\)]+\\)', '', gsub('_', ' ', col_names[i])), '(Box Plot)'),
          xlab=gsub('_', ' ', col_names[i]))
}

# replot park area with supercript
plot(explanatory$`Total_Park_Area_(m2)`, explanatory$number_of_violent_crimes_per_1000_population,
     main='Violent Crime Rate V.S. Total Park Area',
     xlab=expression('Total Park Area' ~ (m^{2})), ylab='Number of Violent Crimes / 1000 Population', pch=18)

hist(explanatory$`Total_Park_Area_(m2)`,
     col='grey',
     main='Total Park Area (Histogram)',
     xlab=expression('Total Park Area' ~ (m^{2})), ylab='Number of Communities')

boxplot(explanatory$`Total_Park_Area_(m2)`, data=explanatory,
        col='grey',
        main='Total Park Area (Box Plot)',
        xlab=expression('Total Park Area' ~ (m^{2})))


# Clustering
library(cluster)
library(fpc)

n = 4
euclidean <- matrix(, nrow = 12, ncol = 12)

for(i in 3:14) {
  for(j in 3:14) {
    if (i != j) {
      subexp <- data.frame(explanatory[,i],
                           explanatory[,j])
      cl <- kmeans(subexp, n)
      plot(subexp, col = cl$cluster, method="euclidean",
           main=paste(gsub( '\\s*\\([^\\)]+\\)', '', gsub('_', ' ', col_names[i])), 'V.S.', gsub( '\\s*\\([^\\)]+\\)', '', gsub('_', ' ', col_names[j]))),
           xlab=gsub('_', ' ', col_names[i]), ylab=gsub('_', ' ', col_names[j]))
      points(cl$centers, col = 1:n, pch = 8, cex = 2)
      euclidean[i-2, j-2] <- sum(cl$withinss)/77
    }
  }
}
