# Creating new dataframe
schooldf <- data.frame(School_Profile_Information$School_ID,
                       School_Profile_Information$Short_Name,
                       School_Profile_Information$Overall_Rating,
                       School_Profile_Information$Rating_Status,
                       School_Locations$WARD_15)
names(schooldf) <- c('id', 'name', 'rating', 'status', 'ward')

# Converting ratings to numbers
schooldf[, 'rating'] = toupper(schooldf[, 'rating'])
schooldf$rating_num <- NA
schooldf$rating_num[schooldf$rating=='INABILITY TO RATE'] <- -1
schooldf$rating_num[is.na(schooldf$rating)] <- -1
schooldf$rating_num[schooldf$rating=='LEVEL 3'] <- 1
schooldf$rating_num[schooldf$rating=='LEVEL 2'] <- 2
schooldf$rating_num[schooldf$rating=='LEVEL 2+'] <- 3
schooldf$rating_num[schooldf$rating=='LEVEL 1'] <- 4
schooldf$rating_num[schooldf$rating=='LEVEL 1+'] <- 5

# Calculating average rating by ward
avg <- aggregate(schooldf$rating_num, list(schooldf$ward), mean)
names(avg) <- c('ward', 'average rating')

# cat("\014") - Clear console
