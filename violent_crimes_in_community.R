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
names(population) <- c('community', 'community name', 'population')

# sum violent crimes and total crimes
sum_crime <- data.frame(count$community)
names(sum_crime) <- c('community')
sum_crime$violent_crime <- NA
sum_crime$total_crime <- NA
sum_crime$violent_crime <- rowSums(count[, c('ASSAULT', 'BATTERY', 'CRIM SEXUAL ASSAULT',
                                             'HOMICIDE', 'KIDNAPPING',
                                             'VIOLENT OFFENSE INVOLVING CHILDREN',
                                             'PUBLIC PEACE VIOLATION', 'RITUALISM',
                                             'ROBBERY', 'ROBBERY', 'SEX OFFENSE',
                                             'WEAPONS VIOLATION')])
sum_crime$total_crime <- rowSums(count[, !(colnames(count) == "community")])

#merge sum_crime and population
sum_crime <- merge(x = sum_crime, y = population, by.x = 'community', by.y = 'community', all = TRUE)
sum_crime <- sum_crime[, c(1, 4, 5, 2, 3)]