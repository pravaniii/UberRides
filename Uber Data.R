library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)


rm(list=ls())

setwd("~/Desktop/DATA 332/Uber Data 332")

April_1<- read.csv("uber-raw-data-apr14.csv")
May_1<-read.csv("uber-raw-data-may14.csv")
June_1<-read.csv("uber-raw-data-jun14.csv")
July_1<-read.csv("uber-raw-data-jul14.csv")
August_1<-read.csv("uber-raw-data-aug14.csv")
September_1<-read.csv("uber-raw-data-sep14.csv")


combined_Data <- rbind(April_1, May_1, June_1, July_1, August_1, September_1)
# # mutate(Date.Time = as.POSIXct(Date.Time, format = "%m/%d/%Y %H:%M:%OS"))


# separate the datetime column into date and time components
combined_Data <- separate(combined_Data, Date.Time, into = c("Date", "Time"), sep = "\\s+(?=[^\\s]+$)")


# Convert the Date column to a date format
combined_Data$Date <- as.Date(combined_Data$Date, format = "%m/%d/%y")

# Add columns for month and day
combined_Data <- mutate(combined_Data,
                        Month = month(Date, label = TRUE),
                        Day = day(Date))

#Separating the time in hour and minutes
combined_Data <- separate(combined_Data, Time, into = c("hour", "minute", "seconds"), sep = ":")

  #A new column for day of the week using the wday() function
combined_Data <- combined_Data %>%
  mutate(day = wday(Date, label = TRUE))

#------------------------PIVOT-----------------------------------------

# Group the data by month and day and count the number of occurrences
everyday_counts <- combined_Data %>%
  group_by(Month, Day) %>%
  summarise(n = n()) %>%
  ungroup()

#Count the number of rides each month 
rides_per_month <- combined_Data %>%
  group_by(Month) %>%
  summarise(num_rides = n())

# Save the pivot table as a CSV file
write.csv(rides_per_month, "rides_per_month.csv", row.names = FALSE)

#Count the number of trips each hour
trips_per_hour <- combined_Data %>%
  group_by(hour, Month) %>%
  summarise(num_rides = n()) %>%
  arrange(hour)

# Save the pivot table as a CSV file
write.csv(trips_per_hour, "trips_per_hour.csv", row.names = FALSE)

#Counting Trips every every day of all the months
Month_day <- combined_Data %>%
  group_by(Month, day) %>%
  summarise(Trips = n())

# Save the pivot table as a CSV file
write.csv(Month_day, "Month_day.csv", row.names = FALSE)

#Bases and number of trips 
Base_Count <- combined_Data %>%
  group_by(Base, Month) %>%
  summarise(n = n())

# Save the pivot table as a CSV file
write.csv(Base_Count, "Base_Count.csv", row.names = FALSE)

Base_week <- combined_Data %>%
  group_by(Base, day) %>%
  summarise(num_trips = n())

# Save the pivot table as a CSV file
write.csv(Base_week, "Base_week.csv", row.names = FALSE)
#--------------------------Graphs----------------------------------------------------
#GRAPH showing the number of rides per month 
ggplot(rides_per_month, aes(x = Month, y = num_rides, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Uber Rides per Month",
       x = "Month",
       y = "Number of Rides") +theme_classic()

#GRAPH showing the number of trips every hour of the day by Month 
ggplot(trips_per_hour, aes(x = reorder(hour, -num_rides), y = num_rides, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Uber Rides Per Hour",
       x = "Time of Day",
       y = "Number of Rides") + theme_classic()

#GRAPH showing the number of trips every hour 
ggplot(trips_per_hour, aes(x = reorder(hour, -num_rides), y = num_rides, fill = hour)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Uber Rides Per Hour",
       x = "Time of Day",
       y = "Number of Rides") + theme_classic()


#GRAPH the number of trips every day for 
ggplot(Month_day, aes(x = Month, y= Trips, fill = day)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Trips every month with day fo the week",
       x = "Month",
       y = "Number of Rides") +theme_minimal()


#GRAPH of the number of bases each month 
ggplot(Base_Count, aes(x = Month, y= n, fill = Base)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Trips according to Bases",
       x = "Month",
       y = "Number of Rides") +theme_minimal()

#-------------------------------------------------------------------------------

#Group the number of trips everyday of all the months
All_Month_Trips <- combined_Data %>%
  group_by(Month, Day) %>%
  summarise(num_rides = n())

# Save the pivot table as a CSV file
write.csv(All_Month_Trips, "All_Month_Trips.csv", row.names = FALSE)


#--------------------------Heat Map--------------------------------------------
# Create a new data frame with the hour, day, and count information
Hour_Day_Count <- combined_Data %>%
  group_by(hour, day) %>%
  summarise(n = n())

# Save the pivot table as a CSV file
write.csv(Hour_Day_Count, "Hour_Day_Count.csv", row.names = FALSE)

#Heatmap plot by Hour and Day 
ggplot(Hour_Day_Count, aes(x = hour, y = day, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Uber Rides by Hour and Day",
       x = "Hour of the Day",
       y = "Day of the Week")

  
#Heatmap plot by Month and Day 
ggplot(Month_day, aes(x = Month, y = day, fill = Trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Uber Rides by Month and Day",
       x = "Hour of the Day",
       y = "Day of the Week")

ggplot(Base_week, aes(x = Base, y = day, fill = num_trips)) +
  geom_tile() +
  scale_fill_gradient(low = "light yellow", high = "Deep Pink") +
  labs(title = "Base and Day of the week",
       x = "Hour of the Day",
       y = "Day of the Week")
#----------------------Prediction Model------------------------------------------
                  
prediction_model <- combined_Data %>%
  group_by(hour, Month, day, Day) %>%
  summarise(Total_Trips = n())

# Save the pivot table as a CSV file
write.csv(prediction_model, "prediction_model.csv", row.names = FALSE)

#Graphing the model where it shows the number of rides depending on the day 
ggplot(prediction_model, aes(x = Month, y = Total_Trips, color = factor(day =="Sat"))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("lightgreen", "deep pink"), guide = "none")+
  labs(color = "day == Sat")









