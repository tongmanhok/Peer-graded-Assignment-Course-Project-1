### Peer-graded Assignment: Course Project 1
# Sherwood Tang

rm(list=ls())

library(dplyr)
library(ggplot2)
library(lattice)

# Code for reading in the dataset and/or processing the data
data <- read.csv("activity.csv",sep=",",header=TRUE)

# see the data
summary(data)
str(data)
dim(data)
head(data)

data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- factor(data$interval)

# subsetting NA data
data1<-data[!is.na(data$steps),]
head(data1)

# Mean and median number of steps taken each day
step_by_date  <- data1 %>%
  group_by(date) %>%
  summarize(sum_step=sum(steps))
medians <- median(step_by_date$sum_step)
means <- round(mean(step_by_date$sum_step))

# Histogram of the total number of steps taken each day
hist(step_by_date$sum_step, breaks = 15, col = "blue", 
     xlab = "Sum of Steps per Day", main= "Histogram of the total number of steps taken each day")

# Time series plot of the average number of steps taken
step_by_interval  <- data1 %>%
  group_by(interval) %>%
  summarize(avstep=mean(steps,na.rm=TRUE))
index=which.max(step_by_interval$avstep)
max_mean<-max(step_by_interval$avstep)
which_interval<-step_by_interval$interval[index]

title1 <- "Mean Steps per Interval"
ggplot(step_by_interval,aes(x=interval,y=avstep,group = 1))+
  geom_line()+
  geom_vline(data=step_by_interval,aes(xintercept=as.numeric(which_interval),col="red"))+
  xlab("Interval")+
  ylab("Mean Steps per Interval")+
  ggtitle(title1)

# Imputing missing data
totna<-sum(is.na(data$steps))

# There are 'r totna' missing data in the original data.
naindex<-which(is.na(data$steps))
input_data <- data
nainput<-function(x){
  step_by_interval[data[x,]$interval==step_by_interval$interval,]$avstep
}

input_data[naindex,]$steps<-unlist(lapply(naindex,FUN=nainput))
summary(input_data)

step_by_date_input  <- input_data %>%
  group_by(date) %>%
  summarize(sum_step_input=sum(steps))

# Histogram of the total number of steps taken each day after missing values are imputed
hist(step_by_date_input$sum_step_input, breaks = 15, col = "green", 
     xlab = "Sum of Steps per Day", main= "Histogram of the total number of steps taken each day after missing values are imputed")

# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
input_data$day <- as.factor(weekdays(input_data$date))
head(input_data)
input_data$is_weekday<-ifelse(input_data$day %in% c("Saturday","Sunday"),FALSE,TRUE)

weekday_data<-input_data[input_data$is_weekday==TRUE,]
weekend_data<-input_data[input_data$is_weekday==FALSE,]

weekday_interval  <- weekday_data %>%
  group_by(interval) %>%
  summarize(avstep=mean(steps,na.rm=TRUE))
weekday_interval$day<-"Weekday"

weekend_interval  <- weekend_data %>%
  group_by(interval) %>%
  summarize(avstep=mean(steps,na.rm=TRUE))
weekend_interval$day<-"Weekend"

week_data<-rbind(weekday_interval,weekend_interval)

xyplot(avstep ~  interval | day, data = week_data, layout = c(1,2), type ="l", ylab="Number of Steps")
