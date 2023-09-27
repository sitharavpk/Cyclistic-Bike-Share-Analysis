## Before starting, install the required packages and call the library needed
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
library(dplyr)
library(ggplot2)
library(scales)
library(chron)
#Setting the path to the working directory
setwd("~/Desktop/RStudio materials/Cyclistic - 2022-2023")


#Loading all the data and renaming
trips_Aug22 <- read_csv('202208-divvy-tripdata.csv')
trips_Sep22 <- read_csv('202209-divvy-tripdata.csv')
trips_Oct22 <- read_csv('202210-divvy-tripdata.csv')
trips_Nov22 <- read_csv('202211-divvy-tripdata.csv')
trips_Dec22 <- read_csv('202212-divvy-tripdata.csv')
trips_Jan23 <- read_csv('202301-divvy-tripdata.csv')
trips_Feb23 <- read_csv('202302-divvy-tripdata.csv')
trips_Mar23 <- read_csv('202303-divvy-tripdata.csv')
trips_Apr23 <- read_csv('202304-divvy-tripdata.csv')
trips_May23 <- read_csv('202305-divvy-tripdata.csv')
trips_Jun23 <- read_csv('202306-divvy-tripdata.csv')
trips_Jul23 <- read_csv('202307-divvy-tripdata.csv')


## Compare the datatypes in each column across all dataframes. While the names don’t have to be in the same order, they DO need to match perfectly before we can join them into one file. This command returns only mismatched rows. If no mismatch then the result would be <0 rows> 
compare_df_cols(trips_Aug22,trips_Sep22,trips_Oct22,trips_Nov22,trips_Dec22,trips_Jan23,trips_Feb23,trips_Mar23,trips_Apr23,trips_May23,trips_Jun23,trips_Jul23,return = "mismatch")



##Combine all dataframes together
all_trips <- bind_rows(trips_Aug22,trips_Sep22,trips_Oct22,trips_Nov22,trips_Dec22,trips_Jan23,trips_Feb23,trips_Mar23,trips_Apr23,trips_May23,trips_Jun23,trips_Jul23)


##Remove unused columns
all_trips <- all_trips %>% select(-c(start_lat,start_lng,end_lat,end_lng))


##Drop observations with null value
all_trips <- drop_na(all_trips)



## All the data has been combined to a single table with 4340442 rows and 9 columns
dim(all_trips)
head(all_trips)


## Summary (Min,Max,Mean,Median,Mode) of columns with numeric datatype. 
summary(all_trips) 


## Summary of dataframe; check missing data 
skim(all_trips)


#Check for any odd data on rider and type of bike
unique(all_trips$member_casual) #Values will be casual or member 
unique(all_trips$rideable_type) #Values will be classic_bike, electric_bike or docked_bike


## Add columns - date,month,year,day of each ride
all_trips$date <- as.Date(all_trips$started_at) ## Default format is yyyy-mm-dd

all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$year <- format(as.Date(all_trips$date),"%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date),"%A")
all_trips$month_of_year <- format(as.Date(all_trips$date),"%B")


##Calculate ride Length from start time and end time of ride
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at, units = 'mins')
all_trips$ride_length <- round(all_trips$ride_length,2)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))


## Remove all negative ride lengths
all_trips_v2 <- all_trips[!(all_trips$ride_length<0),]
summary(all_trips_v2$ride_length)


## Summarise w.r.t. member type
all_trips_v2 %>%  
  group_by(member_casual) %>% 
  summarise(avg_ride_length = mean(ride_length),median_ride_length = median(ride_length),max_ride_length = max(ride_length),min_ride_length=min(ride_length))

##Summarizing each of the metrics (Mean,Median,Max,Min) into separate tables
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)


##Ride length on each day based on Member/Casual rider
aggregate(all_trips_v2$ride_length ~ all_trips_v2$day_of_week +all_trips_v2$member_casual, FUN = mean)


##Order by Days of Week and Months
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

all_trips_v2$month_of_year <- ordered(all_trips_v2$month_of_year, levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))


#Separate dataframe for casual riders and member riders inorder to find patterns in each member type separately
trips_casual <- filter(all_trips_v2, all_trips_v2$member_casual == "casual")
trips_member <- filter(all_trips_v2, all_trips_v2$member_casual == "member")


## Ploting the results
##Total Rides per Month
all_trips_v2 %>% 
  group_by(month_of_year) %>% 
  summarise(num_of_rides = n(), avg_duration = mean(ride_length)) %>%
  arrange(month_of_year) %>% 
  ggplot(aes(x=month_of_year,y=num_of_rides,fill = "pink"))  + geom_col(position='dodge2') + labs(title="Total Rides per Month", x= "Month",y="Number of Rides") + scale_y_continuous(labels = comma)+ theme(axis.text.x = element_text(angle = 60,hjust=1), legend.position = "none")


## Export to csv file for further analysis
write.csv(all_trips_v2,'all_data.csv')