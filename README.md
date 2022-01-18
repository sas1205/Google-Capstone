# Google-Captstone

## Summary

In this project, I analyze open source data from Divvy Bikes develop better marketing strategies to maximize membership for a fake company called “Cyclistic”. Over the course of this project I analyze ride averages, popular stations, and popular bike types over the course of 2021. These datasets will help us compare how casual riders and annual members use Cyclistic bikes differently. Since the company’s future depends on maximizing annual memberships, we want to convert casual riders to annual members.

## Company Background 

## Business Tasks
1. Maximize the annual memberships with the historical data.
2. Identify how differently annual members and casual riders are using the bikes?
3. Recommendations to increase conversion, in the form of data insights and data visualizations.

## Prepare Phase

As mentioned, Divvy Bikes provide open source of their data for fair use. This data comes from a first party source, it has been provided by the Motivate International Inc which is what Cyclistic’s business is based off of. The city as well as Motivate assure that it accurately represents the population (those in the area, type of bikes, disabled riders etc)  Every dataset is a .csv file that shows trip data from 01/2021 - 12/2021. As for licensing, Motivate grants us a non-exclusive, royalty-free, limited, perpetual license to access, reproduce, analyze, copy, modify, distribute in our product or service and use the Data for any lawful purpose. For security and privacy sake sensitive information such as names, addresses, or credit card info are not included in the datasets. 

## Cleaning Process

I started with individually looking at each dataset in excel. I made sure to take advantage of data validation rules so that the data is uniform for each cell. I also made a checklist:

* Consistent table names
* Starting dates and times in same format
* No negative ride lengths 
* Deleting duplicate rows and replacing all empty cells for station names and ids with NA
* add a ride_length column and format to time (37:30:55), =(D2-C2)
* add day_of_week and format as general =WEEKDAY(C2,1)

While analyzing and doing a basic clean for each dataset I used pivot tables to take monthly averages of rides, members, preferred bike types, popular weekdays, and popular stations. I was able to see a monthly summary for everything. While this didn’t take long it would be impossible to fully combine all spreadsheets to do a full analysis so I moved to R studio. 

Full code in repository 

```{r}
#After importing libraries and datasets I combined them all in a data frame 
df <- bind_rows(jan_2021, feb_2021, mar_2021, apr_2021, may_2021,
                june_2021, july_2021, aug_2021, sep_2021, oct_2021, nov_2021, dec_2021)
```

```{r}
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
```
## Analyze Phase

After moving to R most of my cleaning was already done. Once again I searched for negative ride lengths and converted day_of_week to return characters. After combining all datasets the dataframe had over 5 million entities. 

I was also able to find the mean, median, max, and average which is returned in seconds:

```{r}
> mean(df$ride_length) 
Time difference of 1328.412 secs
> median(df$ride_length) 
Time difference of 720 secs
> max(df$ride_length) 
Time difference of 3356640 secs
> min(df$ride_length) 
Time difference of 60 secs
```

Then I was able to compare members and casual users 
```{r}
aggregate(df$ride_length ~ df$member_casual, FUN = mean)
  df$member_casual df$ride_length
1           casual 1935.7062 secs
2           member  826.4351 secs
> aggregate(df$ride_length ~ df$member_casual, FUN = median)
  df$member_casual df$ride_length
1           casual       960 secs
2           member       600 secs
> aggregate(df$ride_length ~ df$member_casual, FUN = max)
  df$member_casual df$ride_length
1           casual   3356640 secs
2           member     90000 secs
> aggregate(df$ride_length ~ df$member_casual, FUN = min)
  df$member_casual df$ride_length
1           casual        60 secs
2           member        60 secs
```

Finding the average ride time by each day for members vs casual users
```{r}
df$day_of_week <- ordered(df$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
> aggregate(df$ride_length ~ df$member_casual + df$day_of_week, FUN = mean)
   df$member_casual df$day_of_week df$ride_length
1            casual         Sunday 2264.5540 secs
2            member         Sunday  953.4816 secs
3            casual         Monday 2005.9662 secs
4            member         Monday  813.4643 secs
5            casual        Tuesday 1725.0524 secs
6            member        Tuesday  778.0706 secs
7            casual      Wednesday 1683.7172 secs
8            member      Wednesday  772.5140 secs
9            casual       Thursday 1650.8429 secs
10           member       Thursday  774.3132 secs
11           casual         Friday 1809.6571 secs
12           member         Friday  796.5163 secs
13           casual       Saturday 2039.8565 secs
14           member       Saturday  915.0557 secs
```

ridership data by type and weekday
```{r}
df %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()							
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)	
  
  
   member_casual weekday number_of_rides average_duration
   <chr>         <ord>             <int> <drtn>          
 1 casual        Sun              477336 2272.5430 secs  
 2 casual        Mon              284105 1927.7067 secs  
 3 casual        Tue              272172 1692.0191 secs  
 4 casual        Wed              276698 1673.0738 secs  
 5 casual        Thu              283733 1675.8715 secs  
 6 casual        Fri              361098 1835.9249 secs  
 7 casual        Sat              553643 2098.6949 secs  
 8 member        Sun              371981  950.6661 secs  
 9 member        Mon              412095  802.8068 secs  
10 member        Tue              461116  774.6085 secs  
11 member        Wed              472544  776.7377 secs  
12 member        Thu              447217  773.9382 secs  
13 member        Fri              441937  807.6037 secs  
14 member        Sat              428249  926.1613 secs  
  ```
Visualization for number of rides by rider type
```{r}
df %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
```

![alt text](https://github.com/sas1205/Google-Captstone/blob/main/Rplot.png)

Visualization for average duration

```{r}
df %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
   ```
![alt text](https://github.com/sas1205/Google-Captstone/blob/main/Rplot01.png)

## Share 

While I was able to develop visualizations and get calculations in R studio I still decided to create visualizations using Excel as well. As mentioned earlier I had already created a separate sheet containing a monthly analysis so I might as well use them.

Link to google sheets: https://docs.google.com/spreadsheets/d/1TvpZo05JPviv-EYJ6SrAocmnPdoyGvDX8qXa8TvETwQ/edit?usp=sharing

In this first line chart you can see that there are more casual members between June and August. Very close in numbers between the 2 types in September, but it begins to fall off during the fall through winter. I would suggest for Lilly Moreno to increase promotion for the bike-share program during peak months. Due to the weather and increase tourism it is important that they market to casual users to become a member.

![alt text](https://github.com/sas1205/Google-Captstone/blob/main/Number%20of%20members%20for%202021.png)

This bar graph shows that on average casual riders tend to ride for around 38 mins while member riders on average ride for 14 mins:

![alt text](https://github.com/sas1205/Google-Captstone/blob/main/Avg%20ride%20time%20for%20Annual%20and%20Casual%20members.png)

Finally these bar charts compare preferred bike type between casual and annual members:

![alt text](https://github.com/sas1205/Google-Captstone/blob/main/Casual%20Members%20-%20Bike%20type%20for%202021.png)
![alt text](https://github.com/sas1205/Google-Captstone/blob/main/Annual%20Members%20-%20Bike%20type%20for%202021.png)

With the excel sheet I was also able to find the most popular starting stations and ending station names. First, there is a total of 329, 529 starting and ending stations don't have a name or id so we will exclude them. 

```{r}
Top 5 Starting Station Name 
Streeter Dr & Grand Ave, 66,407
Michigan Ave & Oak St, 36,468
Millennium Park, 32,572
Wells St & Concord Ln, 26,035
Clark St & Elm St, 19,766

Top 5 Ending Station Name 
Streeter Dr & Grand Ave, 83,014
Michigan Ave & Oak St, 39,156
Millennium Park, 26,756
Lake Shore Dr & North Blvd, 22,972
DuSable Lake Shore Dr & North Blvd, 18,377
```

## Conclusion 
