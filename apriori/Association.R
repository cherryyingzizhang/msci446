library(OneR)

# optimal number of bins:
# https://stats.stackexchange.com/questions/798/calculating-optimal-number-of-bins-in-a-histogram
col_names <- colnames(explanatory)
for(i in 2:14) {
  sorted <- sort(explanatory[, i])
  hist(sorted, breaks="FD", xlab=col_names[i])
}
# number bins obtained from histogram
num_bins <- c(6,6,8,12,8,6,12,5,2,5,10,10,8)
strs <- c('crimes', 'school', 'SSL', 'park', 'hospital', 'teenMoms', 'infactMortality', 'hispanic', 'black', 'white', 'asian', 'other', 'childPoverty')

#do fixed width binning
binnedFix <- explanatory[,1:14]

for(i in 2:14) {
  binnedFix[,i] <- paste(strs[i-1], bin(binnedFix[, i], nbins = num_bins[i-1], labels = NULL, method = 'length', na.omit = TRUE))
}

bounds <- list()
bounds[[1]] <- c(105, 550, 991, 1430, 1870, 2320, 2760)
bounds[[2]] <- c(2.14, 2.62, 3.1, 3.57, 4.05, 4.52, 5)
bounds[[3]] <- c(0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1)
bounds[[4]] <- c(4.96e+03, 1.97e+05, 3.86e+05, 5.75e+05, 7.65e+05, 9.54e+05, 1.14e+06, 1.33e+06, 1.52e+06, 1.71e+06, 1.90e+06, 2.09e+06, 2.28e+06)
bounds[[5]] <- c(0,1,2,3,4)
bounds[[6]] <- c(2.14, 21.8, 41.3, 60.8, 80.3, 99.8, 119.3)
bounds[[7]] <- c(1.48, 3.26, 5.02, 6.78, 8.53, 10.3, 12.1, 13.8, 15.6, 17.3, 19.1, 20.8, 22.6)
bounds[[8]] <- c(0.915, 18, 35, 52, 69, 86.1)
bounds[[9]] <- c(0, 48.5, 97.1)
bounds[[10]] <- c(0, 16.8, 33.6, 50.4, 67.2, 84.1)
bounds[[11]] <- c(0, 4.9, 9.8, 14.7, 19.6, 24.5, 29.4, 34.3, 39.2, 44.1, 49)
bounds[[12]] <- c(0,1,2,3,4,5)
bounds[[13]] <- c(2.63, 11.7, 20.7, 29.6, 38.6, 47.6, 56.6, 65.6, 74.6)

mains <- c('Rate of Violent Crimes',
           'Average School Ratings',
           'Average SSL Ratings',
           expression('Total Park Area' ~ (m^{2})),
           'Number of Hospitals',
           'Teen Mom Rate',
           'Infant Mortality Rate',
           'Percent of Hispanic People',
           'Percent of Black People',
           'Percent of White People',
           'Percent of Asian People',
           'Pecent of Other Race',
           'Child Poverty Rate')


for (lol in 2:14) {
  labs <- c()
  for(num in 1:(length(bounds[[lol-1]])-1)) {
    labs[num] <- paste(bounds[[lol-1]][num],'-',bounds[[lol-1]][num+1])
  }
  hist(explanatory[, lol], col='grey', main=mains[lol-1], xlab=mains[lol-1],
       breaks=bounds[[lol-1]], labels=labs, freq = TRUE)
}

#do adaptive width binning
binnedAdaptive <- explanatory
for(i in 2:14) {
  binnedAdaptive[,i] <- paste(strs[i-1], bin(binnedAdaptive[, i], nbins = num_bins[i-1], labels = NULL, method = 'content', na.omit = TRUE))
}

bounds_2 <- list()
bounds_2[[1]] <- c(105, 313, 451, 548, 1020, 1560, 2760)
bounds_2[[2]] <- c(2.14, 3, 3.26, 3.57, 3.86, 4.05, 5)
bounds_2[[3]] <- c(0, 0.221, 0.332, 0.406, 0.436, 0.509, 0.566, 0.682, 1)
bounds_2[[4]] <- c(4.96e+03, 5.37e+04, 8.69e+04, 1.36e+05, 1.64e+05, 1.94e+05, 2.43e+05, 2.88e+05, 3.4e+05, 5.92e+05, 7.61e+05, 1.2e+06, 2.28e+06)
bounds_2[[5]] <- c(0,1,2,3,4)
bounds_2[[6]] <- c(2.14, 22.8, 47.8, 60.4, 77.4, 90.2, 119.3)
bounds_2[[7]] <- c(1.48, 3.73, 4.6, 5.1, 5.63, 6.5, 8.1, 8.97, 10.3, 11.4, 13.2, 14, 22.6)
bounds_2[[8]] <- c(0.915, 4, 11.8, 23.6, 53.8, 86.1)
bounds_2[[9]] <- c(0, 24, 97.1)
bounds_2[[10]] <- c(0, 5, 13, 30.6, 50.8, 84.1)
bounds_2[[11]] <- c(-0.01, 0, 1, 2, 3, 5.2, 10, 16.4, 49)
bounds_2[[12]] <- c(0,1,2,3,4,5)
bounds_2[[13]] <- c(2.63, 11.4, 19.4, 27.5, 30.5, 39.2, 45.8, 51.4, 74.6)
#for each, do association rule mining

for (lol in 2:14) {
  labs <- c()
  for(num in 1:(length(bounds_2[[lol-1]])-1)) {
    labs[num] <- paste(bounds_2[[lol-1]][num],'-',bounds_2[[lol-1]][num+1])
  }
  hist(explanatory[, lol], col='grey', main=mains[lol-1], xlab=mains[lol-1],
       breaks=bounds_2[[lol-1]], labels=labs, freq = TRUE)
}
