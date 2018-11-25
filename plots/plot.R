# graphics.off()
# cat('\014')
library(bamlss)
library(plotly)
library(magrittr)

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
                          100*predTable$percentChildrenInPov,
                          census_data_by_community_area$Community)
names(explanatory) <- c('community', 'number_of_violent_crimes_per_1000_population',
                        'Average_School_Rating', 'Normalized_Average_SSL', 'Total_Park_Area_(m2)',
                        'Number_of_Hospitals', 'Number_of_Teen_Moms_/_1000_Female_Teenagers', 'Number_of_Infant_Mortality_/_1000_Live_Births',
                        'Percent_of_Hispanic_(%)', 'Percent_of_Black_(%)', 'Percent_of_White_(%)',
                        'Percent_of_Asian_(%)', 'Percent_of_Other_Race_(%)', 'Percent_of_Children_in_Poverty_(%)','Community_Name')

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
hist(explanatory$number_of_violent_crimes_per_1000_population/max(explanatory$number_of_violent_crimes_per_1000_population), 
     col='grey',
     main='Number of Violent Crimes / 1000 Population (Histogram)',
     xlab='Number of Violent Crimes / 1000 Population', ylab='Number of Communities')

for(i in 3:14) {
  hist(explanatory[,i]/max(explanatory[,i]),
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

# Normalize explanatory variables
normalized <- explanatory
for (col in 1:ncol(normalized)) {
  normalized[,col] <- (normalized[,col]-min(normalized[,col]))/(max(normalized[,col]) - min(normalized[,col]))
}

# Clustering Euclidean Distance
euclidean <- matrix(, nrow = 25, ncol = 2)
for(round in 1:2) {
  for(n in 1:25) {
    cl <- kmeans(normalized[, 3:14], n)
    euclidean[n, round] <- cl$tot.withinss
  }
}

plot(euclidean[,2], type='o', col=1, pch=18, lty=1,
     main='Total Within-Cluster Sum of Squares v.s. Number of Centers',
     xlab='Number of Centers', ylab='Total Within-Cluster Sum of Squares')

plot(euclidean[,1], type='o', col=4, pch=18, lty=2,
     main='Results Generated by Different Randomly Chosen Start Point',
     xlab='Number of Centers', ylab='Total Within-Cluster Sum of Squares')
lines(euclidean[,2], type="o", pch=18, lty=1, col=1)
legend(18, 50, c("First Trial","Second Trial"), cex=0.8, 
       col=c(1,4), pch=18:18, lty=1:2)

for(n in 1:25) {
  cl <- kmeans(normalized[, 3:14], n, nstart=50)
  euclidean[n, round] <- cl$tot.withinss
}

plot(euclidean[,2], type='o', col=1, pch=18, lty=1,
     main='Sum of Squares v.s. Number of Centers',
     xlab='Number of Centers', ylab='Total Within-Cluster Sum of Squares')

# Clustering plots with 3 centres, with 50 sets of randomly chosen starting points
cl <- kmeans(normalized[, 3:14], 3, nstart=50)
plot(normalized[, 3:14], col = cl$cluster, pch=20)
points(cl$centers, col = cl$cluster, pch = 8, cex = 2)


# Clustering 3D

####### Races ######
###### TeenMom, InfantMortality, ######
###### ChildrenInPoverty ######

cluster_set <- data.frame(normalized$`Number_of_Teen_Moms_/_1000_Female_Teenagers`,
                          normalized$`Number_of_Infant_Mortality_/_1000_Live_Births`,
                          normalized$`Percent_of_Children_in_Poverty_(%)`,
                          normalized$`Percent_of_Hispanic_(%)`,
                          normalized$`Percent_of_Black_(%)`,
                          normalized$`Percent_of_Asian_(%)`,
                          normalized$`Percent_of_White_(%)`,
                          census_data_by_community_area$communityAreaNumber,
                          census_data_by_community_area$Community,
                          explanatory$number_of_violent_crimes_per_1000_population)
names(cluster_set) <- c('TeenMom', 'InfantMortality', 'ChildPoverty', 'Hispanic', 'Black', 'Asian', 'White', 'Number', 'CommunityName', 'crime')

x=5
y=6
z=7

x_string = 'Teen Mom'
y_string = 'Child Poverty Rate'
z_string = 'Infant Mortality Rate'

cl3 <- kmeans(cluster_set[, c(x,y,z)], 3, nstart=50)
cluster_set$cl <- as.factor(cl3$cluster)
cluster3d <- plot_ly(cluster_set, x = cluster_set[, x],
                     y = cluster_set[, y],
                     z = cluster_set[, z],
                     text = ~paste(Number, CommunityName),
                     color = ~cl, colors = c('#A9BCD0','#58A4B0','#DAA49A')) %>%
  add_markers() %>%
  layout(title = paste('Percent of Asian, White, & Black People'),
         scene = list(xaxis = list(title = 'Percent Black People'),
                      yaxis = list(title = 'Percent Asian People'),
                      zaxis = list(title = 'Percent White People')))
cluster3d

# cl3 <- kmeans(cluster_set[, c(x,y,z)], 3, nstart=50)
# cluster_set$cl <- as.factor(cl3$cluster)
# cluster3d <- plot_ly(cluster_set, x = cluster_set[, x],
#                      y = cluster_set[, y],
#                      z = cluster_set[, z],
#                      color = ~cl, colors = c('#A9BCD0','#58A4B0','#DAA49A')) %>%
#   add_markers() %>%
#   layout(title = paste(colnames(cluster_set)[x],"VS",colnames(cluster_set)[y],"VS",colnames(cluster_set)[z]),
#          scene = list(xaxis = list(title = colnames(cluster_set)[x]),
#                       yaxis = list(title = colnames(cluster_set)[y]),
#                       zaxis = list(title = colnames(cluster_set)[z])))
# cluster3d

# extra 2D scatter plot

p <- plot_ly(data = explanatory, x = explanatory$Average_School_Rating,
             y = ~number_of_violent_crimes_per_1000_population,
             text = ~paste(community, Community_Name),
             marker = list(size = 10,
                           color = '#DAA49A',
                           line = list(color = '#98726B',
                                       width = 2))) %>%
  layout(title = paste('Violent Crime Rate V.S. Average School Rating'),
         xaxis = list(title = 'Average School Rating'),
         yaxis = list(title = 'Violent Crimes per 1000 Population'))

# 3D plot color based on crime rate

x=1
y=2
z=3

x_string = 'Teen Mom'
y_string = 'Child Poverty Rate'
z_string = 'Infant Mortality Rate'

cl3 <- kmeans(cluster_set[, c(x,y,z)], 3, nstart=50)
cluster_set$cl <- as.factor(cl3$cluster)
cluster3d <- plot_ly(cluster_set, x = cluster_set[, x],
                     y = cluster_set[, y],
                     z = cluster_set[, z],
                     text = ~paste(Number, CommunityName),
                     marker = list(color = ~crime, colorscale = c('#84A98C', '#84A98C'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(title = paste('Teen Mom, Infant Mortality, & Child Poverty Rate'),
         scene = list(xaxis = list(title = 'Teen Mom Rate'),
                      yaxis = list(title = 'Infant Mortality Rate'),
                      zaxis = list(title = 'Child Poverty Rate')),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text ='# Violent Crimes<br>1000 Population',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
cluster3d

# '#FFE1A1', '#683531'
