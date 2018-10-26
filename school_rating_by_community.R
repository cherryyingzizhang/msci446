# Creating new dataframe
schooldf <- data.frame(School_Profile_Information$School_ID,
                       School_Profile_Information$Short_Name,
                       School_Profile_Information$Overall_Rating,
                       School_Profile_Information$Rating_Status,
                       School_Locations$COMMAREA,
                       School_Locations$WARD_15)
community <- data.frame(census_data_by_community_area$communityAreaNumber,
                        census_data_by_community_area$Community)

names(schooldf) <- c('id', 'name', 'rating', 'status', 'community', 'ward')
names(community) <- c('number', 'name')

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

# Calculating average rating by community area
avg <- aggregate(schooldf$rating_num, list(schooldf$community), mean)
names(avg) <- c('community', 'avg_rating')

# Add community number
community[, 'name'] = toupper(community[, 'name'])
community[, 'name'] <- gsub('[^[:alnum:][:space:]]', '', community[, 'name'])

# Join two data frames
avg_commu <- merge(x = community, y = avg, by.x = 'name', by.y = 'community', all = TRUE)
avg_commu_ordered <- avg_commu[, c(2, 1, 3)]
avg_commu_ordered <- avg_commu_ordered[with(avg_commu_ordered, order(number)), ]

# cat("\014") - Clear console
