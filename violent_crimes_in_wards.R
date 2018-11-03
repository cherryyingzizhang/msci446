library(tidyr)
library(dplyr)
library(plyr)
# cat("\014") - Clear console

crime <- data.frame(crimes2001$X...ID,
                    crimes2001$Date,
                    crimes2001$Primary.Type,
                    crimes2001$Description,
                    crimes2001$Location.Description,
                    crimes2001$Community.Area)
names(crime) <- c('id', 'date', 'type', 'description', 'location', 'community')
crime <- crime[!(is.na(crime$community) | crime$community=='' | crime$community=='0'), ]

commu_crime <- data.frame(crime$type,
                          crime$community)
names(commu_crime) <- c('type', 'community')

count_commu_crime <- ddply(commu_crime, .(commu_crime$community, commu_crime$type), nrow)
names(count_commu_crime) <- c("community", "type", "count")

community_crime_count <- spread(count_commu_crime, key = type, value = count)

# # violent crimes
# assault <- crime[crime$type=='ASSAULT',]
# battery <- crime[crime$type=='BATTERY',]
# sexAssault <- crime[crime$type=='CRIM SEXUAL ASSAULT',]
# trepass <- crime[crime$type=='CRIMINAL TRESPASS',]
# domestic_violence <- crime[crime$type=='DOMESTIC VIOLENCE',]
# homicide <- crime[crime$type=='HOMICIDE',]
# intimidation <- crime[crime$type=='INTIMIDATION',]
# kidnapping <- crime[crime$type=='KIDNAPPING',]
# children <- crime[crime$type=='OFFENSE INVOLVING CHILDREN',]
# public_peace_violation <- crime[crime$type=='PUBLIC PEACE VIOLATION',]
# ritualism <- crime[crime$type=='RITUALISM',]
# robbery <- crime[crime$type=='ROBBERY',]
# sexOffense <- crime[crime$type=='SEX OFFENSE',]
# weapon <- crime[crime$type=='WEAPONS VIOLATION',]
# 
# other <- crime[crime$type=='OTHER OFFENSE',]
# 
# # non-violent crimes
# arson <- crime[crime$type=='ARSON',]
# burglary <- crime[crime$type=='BURGLARY',]
# criminal_damage <- crime[crime$type=='CRIMINAL DAMAGE',]
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
