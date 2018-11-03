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

# sum violent crimes and total crimes
sum_crime <- rowSums()

# violent crimes
# assault <- crime[crime$type=='ASSAULT',]
# battery <- crime[crime$type=='BATTERY',]
# sexAssault <- crime[crime$type=='CRIM SEXUAL ASSAULT',]
# domestic_violence <- crime[crime$type=='DOMESTIC VIOLENCE',]
# homicide <- crime[crime$type=='HOMICIDE',]
# kidnapping <- crime[crime$type=='KIDNAPPING',]
# children <- crime[crime$type=='OFFENSE INVOLVING CHILDREN' 
#                   & (crime$description=='CRIM SEX ABUSE BY FAM MEMBER' 
#                      | crime$description=='CHILD ABUSE'
#                      | crime$description=='AGG SEX ASSLT OF CHILD FAM MBR'
#                      | crime$description=='CHILD ABDUCTION'
#                      | crime$description=='AGG CRIM SEX ABUSE FAM MEMBER'
#                      | crime$description=='SEX ASSLT OF CHILD BY FAM MBR'),]
# public_peace_violation <- crime[crime$type=='PUBLIC PEACE VIOLATION',]
# ritualism <- crime[crime$type=='RITUALISM',]
# robbery <- crime[crime$type=='ROBBERY',]
# sexOffense <- crime[crime$type=='SEX OFFENSE',]
# weapon <- crime[crime$type=='WEAPONS VIOLATION',]

# # non-violent crimes
# intimidation <- crime[crime$type=='INTIMIDATION',]
# arson <- crime[crime$type=='ARSON',]
# burglary <- crime[crime$type=='BURGLARY',]
# criminal_damage <- crime[crime$type=='CRIMINAL DAMAGE',]
# trepass <- crime[crime$type=='CRIMINAL TRESPASS',]
# concealed_carry_license_violation <- crime[crime$type=='CONCEALED CARRY LICENSE VIOLATION',]
# deceptive_practice <- crime[crime$type=='DECEPTIVE PRACTICE',]
# gambling <- crime[crime$type=='GAMBLING',]
# human_trafficking <- crime[crime$type=='HUMAN TRAFFICKING',]
# interference_with_public_officer <- crime[crime$type=='INTERFERENCE WITH PUBLIC OFFICER',]
# liquor_law_violation <- crime[crime$type=='LIQUOR LAW VIOLATION',]
# motor_vehicle_theft <- crime[crime$type=='MOTOR VEHICLE THEFT',]
# narcotics <- crime[crime$type=='NARCOTICS',]
# obscenity <- crime[crime$type=='OBSCENITY',]
# other_narcotic_violation <- crime[crime$type=='OTHER NARCOTIC VIOLATION',]
# prostitution <- crime[crime$type=='PROSTITUTION',]
# public_indecency <- crime[crime$type=='PUBLIC INDECENCY',]
# stalking <- crime[crime$type=='STALKING',]
# theft <- crime[crime$type=='THEFT',]
# 
# other <- crime[crime$type=='OTHER OFFENSE',]