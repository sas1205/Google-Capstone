#Importing libraries 
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(readr)
library(forcats)
library(sp)
library(sf)
library(leaflet)
library(zoo)
library(stringr)
library(chron)
library(waffle)

#importing datasets 
setwd("/Users/shanesescott/Files/Cyclistic_trip_data")
jan_2021 <- read_csv("202101-divvy-tripdata.csv")
feb_2021 <- read_csv("202102-divvy-tripdata.csv")
mar_2021 <- read_csv("202103-divvy-tripdata.csv")
apr_2021 <- read_csv("202104-divvy-tripdata.csv")
may_2021 <- read_csv("202105-divvy-tripdata.csv")
june_2021 <- read_csv("202106-divvy-tripdata.csv")
july_2021 <- read_csv("202107-divvy-tripdata.csv")
aug_2021 <- read_csv("202108-divvy-tripdata.csv")
sep_2021 <- read_csv("202109-divvy-tripdata.csv")
oct_2021 <- read_csv("202110-divvy-tripdata.csv")
nov_2021 <- read_csv("202111-divvy-tripdata.csv")
dec_2021 <- read_csv("202112-divvy-tripdata.csv")

#combining all 12 datasets
df <- bind_rows(jan_2021, feb_2021, mar_2021, apr_2021, may_2021,
                june_2021, july_2021, aug_2021, sep_2021, oct_2021, nov_2021, dec_2021)

#removing old columns from spreadsheets
df <- df %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, start_station_id, end_station_id, ride_length,ride_id, day_of_week, day_of_week))

#converting date and time and adding/renaming new columns for analysis
df$start_time <- as.POSIXct(df$started_at, format="%m/%d/%Y %H:%M", tz="America/Chicago")
df$stop_time <- as.POSIXct(df$ended_at, format="%m/%d/%Y %H:%M", tz="America/Chicago")

df$year <- format(df$start_time, "%Y")
df$month <- format(df$start_time, "%m")
df$day_of_week <- as.factor(as.POSIXlt(df$start_time)$wday)
df$day_of_year <- yday(df$start_time)
df$day <- format(df$start_time, "%d")
df$hour <- format(df$start_time, "%H")
df$minute <- format(df$start_time, "%M")
df$weekend <- is.weekend(df$start_time)

#functions that calculate the ride length and days of the week
df <- df %>% 
  mutate(ride_length = stop_time - start_time) %>% 
  mutate(day_of_week = weekdays(as.Date(df$start_time)))

#function that filters ride lengths that are greater than 0 
df <- df %>%
  filter(ride_length > 0)

# Descriptive analysis on ride_length

mean(df$ride_length) 
median(df$ride_length) 
max(df$ride_length) 
min(df$ride_length) 

# Comparing members and casual users
aggregate(df$ride_length ~ df$member_casual, FUN = mean)
aggregate(df$ride_length ~ df$member_casual, FUN = median)
aggregate(df$ride_length ~ df$member_casual, FUN = max)
aggregate(df$ride_length ~ df$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(df$ride_length ~ df$member_casual + df$day_of_week, FUN = mean)

#run this code 1st, then the code above 
df$day_of_week <- ordered(df$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# ridership data by type and weekday
df %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)	

# number of rides by rider type
df %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# average duration
df %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")



