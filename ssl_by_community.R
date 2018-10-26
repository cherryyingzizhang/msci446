# Create data frame for ssl and community
ssl <- data.frame(Strategic_Subject_List$`SSL SCORE`,
                  Strategic_Subject_List$`COMMUNITY AREA`)
community <- data.frame(census_data_by_community_area$communityAreaNumber,
                        census_data_by_community_area$Community)
names(ssl) <- c('score', 'community')
names(community) <- c('number', 'name')

# Eliminate rows with blank community
ssl <- ssl[!(is.na(ssl$community) | ssl$community==""), ]

# Standardize community names
ssl[, 'community'] = toupper(ssl[, 'community'])
ssl[, 'community'] <- gsub('[^[:alnum:][:space:]]', '', ssl[, 'community'])

# Calculate the average
avg <- aggregate(ssl$score, list(ssl$community), mean)
names(avg) <- c('community', 'avg_rating')

# Add community number
community[, 'name'] = toupper(community[, 'name'])
community[, 'name'] <- gsub('[^[:alnum:][:space:]]', '', community[, 'name'])

# Join two data frames
avg_commu <- merge(x = community, y = avg, by.x = 'name', by.y = 'community', all = TRUE)
avg_commu_ordered <- avg_commu[, c(2, 1, 3)]
avg_commu_ordered <- avg_commu_ordered[with(avg_commu_ordered, order(number)), ]