
# Uber Data 🚘

A brief description of what this project does and who it's for


## Library📚
    library(dplyr)
    library(tidyverse)
    library(readxl)
    library(lubridate)
    library(ggplot2)

## Data Cleaning 🧼
1. rbind 
* Combined 6 csv files in one by using rbind. 


        combined_Data <- rbind(April_1, May_1, June_1, July_1, August_1, September_1)

2. Separate Date, Time, Hour, minutes 
* Separated the date, time, hour, minutes, and seconds in differet columns using the 'separate' function.
                                                                

      combined_Data <- separate(combined_Data, Date.Time, into = c("Date", "Time"), sep = "\\s+(?=[^\\s]+$)")  

      combined_Data$Date <- as.Date(combined_Data$Date, format = "%m/%d/%y")  
      
      combined_Data <- separate(combined_Data, Time, into = c("hour", "minute", "seconds"), sep = ":")                                               
## Data Summary 📝
## Data Analysis 📊
1. Pivot table to plot number of trips  per month 
    
        rides_per_month <- combined_Data %>%
        group_by(Month) %>%
        summarise(num_rides = n())
2. Pivot table to plot number of trips per hour 

        trips_per_hour <- combined_Data %>%
        group_by(hour, Month) %>%
        summarise(num_rides = n()) %>%
        arrange(hour)
3. Pivot table to plot the number of trips in each base 

        Base_Count <- combined_Data %>%
        group_by(Base, Month) %>%
        summarise(n = n())
## Shiny App ✨
In the user interface(ui) section I added sidebar panels to custom the inputs. 

In the mainPanel I added a different tab for every graph to be displayed 

    ui <- fluidPage (
  
     titlePanel("Uber Rides"),
  
    sidebarPanel(
    selectInput("month_select", "Select a Month",
                choices = c("Apr", "May", "Jun", "Jul", "Aug", "Sep"),
                selected = "Apr"),
    selectInput("Day_select","Select a day", 
                choices = c("Mon", "Tue", "Wed", "Thu", "Fri",  "Sat", "Sun"),
                selected = "Sat")),
  
    mainPanel(
    tabsetPanel(
      tabPanel("Rides per Month", plotOutput("rides_per_month")),
      tabPanel("Trips per Hour by Month", plotOutput("trips_per_hour_month")),
      tabPanel("Trips per Hour", plotOutput("trips_per_hour")),
      tabPanel("Trips per Day of the Week", plotOutput("trips_per_day")),
      tabPanel("Map", leafletOutput("map")),
      tabPanel("Trips Acc To Base", plotOutput("Base_Count")),
      tabPanel("Heat Map 1", plotOutput("Hour_Day_Count")),
      tabPanel("Heat Map 2", plotOutput("Month_day")),
      tabPanel("Heat Map 3", plotOutput("Base_week")), 
      tabPanel("Prediction Model", plotOutput("prediction_model"))
   
   In the server panel, I added all the graphs that needed to be displayed along with input functions to make the charts dynamic. 
        
        server <- function(input, output) {
           
       output$trips_per_hour_month <- renderPlot({
        trips_per_hour_filtered <- combined_Data %>%
        filter(Month %in% input$month_select) %>%
        group_by(hour, Month) %>%
        summarise(num_rides = n()) %>%
        arrange(hour)
    
 
         ggplot(trips_per_hour_filtered, aes(x = reorder(hour-num_rides), 
         y = num_rides, fill = Month)) +
         geom_bar(stat = "identity") +
         labs(title = "Number of Uber Rides Per Hour by Month",
           x = "Time of Day",
           y = "Number of Rides") + theme_classic()})
  
