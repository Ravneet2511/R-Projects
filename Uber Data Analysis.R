library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse) 
library(scales)
library(magrittr)
library(ggthemes)

colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")
colors

apr <- read.csv("C:/Users/DELL/Desktop/SEM5/R CLASS/Project/uber-raw-data-apr14.csv")
may <- read.csv("C:/Users/DELL/Desktop/SEM5/R CLASS/Project/uber-raw-data-may14.csv")
june <- read.csv("C:/Users/DELL/Desktop/SEM5/R CLASS/Project/uber-raw-data-jun14.csv")
july <- read.csv("C:/Users/DELL/Desktop/SEM5/R CLASS/Project/uber-raw-data-jul14.csv")
aug <- read.csv("C:/Users/DELL/Desktop/SEM5/R CLASS/Project/uber-raw-data-aug14.csv")
sept <- read.csv("C:/Users/DELL/Desktop/SEM5/R CLASS/Project/uber-raw-data-sep14.csv")

# Combine the data together .................................................
data <- rbind(apr, may, june, july, aug, sept)
cat("The dimensions of the data are:", dim(data))

# data of first 6 rows......................................................
head(data)
summary(data)

data$Date.Time <- as.POSIXct(data$Date.Time, format="%m/%d/%Y %H:%M:%S")
data$Time <- format(as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data$Date.Time <- ymd_hms(data$Date.Time)

#data cleaning and manupulation........................

data$day <- factor(day(data$Date.Time))
data$month <- factor(month(data$Date.Time, label=TRUE))
data$year <- factor(year(data$Date.Time))
data$dayofweek <- factor(wday(data$Date.Time, label=TRUE))

data$second = factor(second(hms(data$Time)))
data$minute = factor(minute(hms(data$Time)))
data$hour = factor(hour(hms(data$Time)))


#visualisation 1..................................................................
#ggplot2 and scales used here
#trips every hour
hourly_data <- data %>% group_by(hour) %>% dplyr::summarize(Total = n())

ggplot(hourly_data, aes(hour, Total)) + 
  geom_bar(stat="identity", 
           fill="skyblue", 
           color="red") + 
  ggtitle("Trips Every Hour", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)





#magrittr and dplyr used here.........................

month_hour_data <- data %>% group_by(month, dayofweek) %>%  dplyr::summarize(Total = n())

#trips by months and weekday.......................................................

ggplot(month_hour_data, aes(dayofweek, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by WeekDay") + 
  scale_y_continuous(labels = comma)



#trips by months....................................................................

day_data <- data %>% group_by(month) %>% dplyr::summarize(Trips = n())
day_data
ggplot(day_data, aes(month, Trips)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  ggtitle("Trips by month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma)


#day of week and trips...............................................................


' WKday_month_data '


wkday_month_data <- data %>% group_by(dayofweek, month) %>% dplyr::summarize(Trips = n())
wkday_month_data
ggplot(wkday_month_data, aes(dayofweek, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trips by weekDays and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)


#HEATMAP OF DAYS AND MONTH.........................................................

' day_hour_data'

day_month_data <- data %>% group_by(day, month) %>% dplyr::summarize(Total = n())
day_month_data


ggplot(day_month_data, aes(day, month, fill = Total)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Month and Day")

#HEATMAP OF MONTH AND WEEKDAY......................................................

ggplot(wkday_month_data, aes(dayofweek, month, fill = Trips)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Weekday and Month")

# NYC MAP...........................................................................

''' WARNING : TAKES A LOT OF TIME TO RUN '''


min_lat <- 40 
max_lat <- 40.91
min_long <- -74.15
max_long <- -73.7004

ggplot(data, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES")


# trips by bases....................................................................

ggplot(data,aes(x=Base)) +
 geom_bar(fill="purple")+
 scale_y_continuous(labels = comma)+
 ggtitle("Trips By bases")

# trips by hour and month..............................................................
month_hour_data <- data %>% group_by(hour,month) %>% dplyr::summarize(Total = n())
month_hour_data

ggplot(month_hour_data, aes(hour,Total,fill=month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips By hour and month") +
  scale_y_continuous(labels = comma)





