library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
library(dplyr)

# reading files
Trips_0321 <- read_csv('202103-divvy-tripdata.csv')
Trips_0421 <- read_csv('202104-divvy-tripdata.csv')
Trips_0521 <- read_csv('202105-divvy-tripdata.csv')
Trips_0621 <- read_csv('202106-divvy-tripdata.csv')
Trips_0721 <- read_csv('202107-divvy-tripdata.csv')
Trips_0821 <- read_csv('202108-divvy-tripdata.csv')
Trips_0921 <- read_csv('202109-divvy-tripdata.csv')
Trips_1021 <- read_csv('202110-divvy-tripdata.csv')
Trips_1121 <- read_csv('202111-divvy-tripdata.csv')
Trips_1221 <- read_csv('202112-divvy-tripdata.csv')
Trips_0122 <- read_csv('202201-divvy-tripdata.csv')
Trips_0222 <- read_csv('202202-divvy-tripdata.csv')


#checking column names
colnames(Trips_0321)
colnames(Trips_0421)
colnames(Trips_0521)
colnames(Trips_0621)
colnames(Trips_0721)
colnames(Trips_0821)
colnames(Trips_0921)
colnames(Trips_1021)
colnames(Trips_1121)
colnames(Trips_1221)
colnames(Trips_0122)
colnames(Trips_0222)

# compare dataframes
compare_df_cols(Trips_0321,Trips_0421,Trips_0521,Trips_0621,Trips_0721,
                Trips_0821,Trips_0921,Trips_1021,Trips_1121,Trips_1221,
                Trips_0122,Trips_0222, return = 'mismatch')

#bind into one data frame
Trips <- bind_rows(Trips_0321,Trips_0421,Trips_0521,Trips_0621,Trips_0721,
                    Trips_0821,Trips_0921,Trips_1021,Trips_1121,Trips_1221,
                    Trips_0122,Trips_0222)

# remove unused columns
Trips <- Trips %>%
    select(-c(start_lat,start_lng,end_lat, end_lng))

Trips < Trips %>% rename(ride_type=rideable_type)

# step3 - clean
#inspect data
colnames(Trips)
dim(Trips)
head(Trips)
str(Trips)
summary(Trips)
ggplot(data=Trips)+geom_bar(mapping= aes(rideable_type))
ggplot(data=Trips)+geom_bar(mapping= aes(member_casual,color=rideable_type))

skim(Trips)

Trips$date <- as.Date(Trips$started_at)
Trips$month <- format(as.Date(Trips$date),"%m")
Trips$day <- format(as.Date(Trips$date),"%d")
Trips$year <- format(as.Date(Trips$date),"%y")
Trips$day_of_week <- format(as.Date(Trips$date),"%A")

#calculate ride duration in seconds
Trips$duration <- difftime(Trips$ended_at,Trips$started_at)

#convert from factor to numeric
is.factor(Trips$duration)
Trips$duration <-as.numeric(as.character(Trips$duration))
is.numeric(Trips$duration)

#look for bad data
skim(Trips$duration)

# remove negatives
trips <- Trips[!(Trips$duration<0),]
skim(trips)
summary(trips$duration)
write.csv(trips,'cleaned_bike_data.csv')
