library(OneR)
col_names <- colnames(explanatory)
for(i in 2:14) {
  sorted <- sort(explanatory[, i])
  hist(sorted, breaks="FD", xlab=col_names[i])
  # plot(sorted, xlab=col_names[i], pch=18)
}

# optimal number of bins:
# https://stats.stackexchange.com/questions/798/calculating-optimal-number-of-bins-in-a-histogram
num_bins <- c(6,6,8,12,8,6,12,5,2,5,10,10,8)
strs <- c('crimes', 'school', 'SSL', 'park', 'hospital', 'teenMoms', 'infactMortality', 'hispanic', 'black', 'white', 'asian', 'other', 'childPoverty')

#do fixed width binning
binnedFix <- explanatory
for(i in 2:14) {
  binnedFix[,i] <- paste(strs[i-1], bin(binnedFix[, i], nbins = num_bins[i-1], labels = NULL, method = 'length', na.omit = TRUE))
}

#do adaptive width binning
binnedAdaptive <- explanatory
for(i in 2:14) {
  binnedAdaptive[,i] <- paste(strs[i-1], bin(binnedAdaptive[, i], nbins = num_bins[i-1], labels = NULL, method = 'content', na.omit = TRUE))
}

#for each, do association rule mining
