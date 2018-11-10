library(tidyr)
library(dplyr)
library(plyr)
# cat("\014") - Clear console

# crime data from 2001
crime <- data.frame(crimes2001$X...ID,
                    crimes2001$Date,
                    crimes2001$Primary.Type,
                    crimes2001$Description,
                    crimes2001$Location.Description,
                    crimes2001$Community.Area)
names(crime) <- c('id', 'date', 'type', 'description', 'location', 'community')
crime <- crime[!(is.na(crime$community) | crime$community=='' | crime$community=='0'), ]

# dividing offense involving children into violent vs non-violent
crime$type <- ifelse(grepl(('CRIM SEX ABUSE BY FAM MEMBER'), crime$description)
                   | grepl(('CHILD ABUSE'), crime$description)
                   | grepl(('AGG SEX ASSLT OF CHILD FAM MBR'), crime$description)
                   | grepl(('CHILD ABDUCTION'), crime$description)
                   | grepl(('AGG CRIM SEX ABUSE FAM MEMBER'), crime$description)
                   | grepl(('SEX ASSLT OF CHILD BY FAM MBR'), crime$description)
                   | grepl(('CRIM SEX ABUSE BY FAM MEMBER'), crime$description), 
                     gsub('OFFENSE INVOLVING CHILDREN', 'VIOLENT OFFENSE INVOLVING CHILDREN', crime$type),
                     gsub('OFFENSE INVOLVING CHILDREN', 'NON-VIOLENT OFFENSE INVOLVING CHILDREN', crime$type))

# generating counts
commu_crime <- data.frame(crime$type, crime$community)
names(commu_crime) <- c('type', 'community')

count_commu_crime <- ddply(commu_crime, .(commu_crime$community, commu_crime$type), nrow)
names(count_commu_crime) <- c("community", "type", "count")

count <- spread(count_commu_crime, key = type, value = count)
count[is.na(count)] <- 0

# population
population <- data.frame(population_chicago$GeogKey,
                        population_chicago$Geog,
                        population_chicago$`Total Population`)
names(population) <- c('community', 'community name', 'population(2010)')

# sum violent crimes and total crimes

names(sum_crime) <- c('community')
sum_crime$violent_crime <- NA
sum_crime$total_crime <- NA
sum_crime$violent_crime <- rowSums(count[, c('ASSAULT', 'BATTERY', 'CRIM SEXUAL ASSAULT',
                                             'HOMICIDE', 'KIDNAPPING',
                                             'VIOLENT OFFENSE INVOLVING CHILDREN',
                                             'PUBLIC PEACE VIOLATION', 'RITUALISM',
                                             'ROBBERY', 'SEX OFFENSE', 'WEAPONS VIOLATION')])
sum_crime$total_crime <- rowSums(count[, !(colnames(count) == "community")])

#merge sum_crime and population
sum_crime <- merge(x = sum_crime, y = population, by.x = 'community', by.y = 'community', all = TRUE)
sum_crime <- sum_crime[, c(1, 4, 5, 2, 3)]

#############################################

# location description
violent_crime <- crime[(crime$type=='ASSAULT'
                      | crime$type=='BATTERY'
                      | crime$type=='CRIM SEXUAL ASSAULT'
                      | crime$type=='HOMICIDE'
                      | crime$type=='KIDNAPPING'
                      | crime$type=='VIOLENT OFFENSE INVOLVING CHILDREN'
                      | crime$type=='PUBLIC PEACE VIOLATION'
                      | crime$type=='RITUALISM'
                      | crime$type=='ROBBERY'
                      | crime$type=='SEX OFFENSE'
                      | crime$type=='WEAPONS VIOLATION'),]

# generating counts
location_description <- data.frame(crime$location, crime$community)
names(location_description) <- c('location', 'community')
location_description <- location_description[location_description$location != '',]

count_location_description <- ddply(location_description, .(location_description$community, location_description$location), nrow)
names(count_location_description) <- c("community", "location", "count")

count_loc <- spread(count_location_description, key = location, value = count)
count_loc[is.na(count_loc)] <- 0

# get most frequent location for each community
# NOT using location description as one of the explanatory variables
# near-zero variance predictors are not helpful for predicting
colnames(count_loc)[max.col(count_loc,ties.method="first")]

# output results

# [1] "STREET"           "STREET"           "STREET"           "STREET"           "STREET"           "STREET"          
# [7] "STREET"           "STREET"           "RESIDENCE"        "RESIDENCE"        "STREET"           "RESIDENCE"       
# [13] "STREET"           "STREET"           "STREET"           "STREET"           "RESIDENCE"        "RESIDENCE"       
# [19] "STREET"           "STREET"           "STREET"           "STREET"           "STREET"           "STREET"          
# [25] "STREET"           "STREET"           "STREET"           "STREET"           "STREET"           "STREET"          
# [31] "STREET"           "STREET"           "STREET"           "STREET"           "STREET"           "STREET"          
# [37] "STREET"           "STREET"           "STREET"           "STREET"           "STREET"           "STREET"          
# [43] "APARTMENT"        "STREET"           "RESIDENCE"        "STREET"           "RESIDENCE"        "STREET"          
# [49] "RESIDENCE"        "RESIDENCE"        "RESIDENCE"        "STREET"           "RESIDENCE"        "RESIDENCE"       
# [55] "RESIDENCE"        "STREET"           "STREET"           "STREET"           "STREET"           "STREET"          
# [61] "STREET"           "STREET"           "STREET"           "RESIDENCE"        "RESIDENCE"        "STREET"          
# [67] "STREET"           "STREET"           "STREET"           "RESIDENCE"        "STREET"           "RESIDENCE"       
# [73] "RESIDENCE"        "RESIDENCE"        "RESIDENCE"        "AIRPORT/AIRCRAFT" "STREET"  