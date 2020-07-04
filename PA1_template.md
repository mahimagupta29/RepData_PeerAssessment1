---
title: "Reproducible Research"
author: "Mahima Gupta"
date: "03/07/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

This assignment consists of 5 task.    
They are as follows:
```{r,echo=TRUE}
#Load Libraries
library(ggplot2)
library(dplyr)
```



## Loading and preprocessing the data.

Downloand & unzip file for processing. Read and load CSV file into a Data Frame.

```{r, echo=TRUE}
# download file from web
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/activity.zip",method="curl")
# unzip data and read 
unzip(zipfile="./data/activity.zip",exdir="./data")
activity <- read.csv("./data/activity.csv")
activity$date <- as.Date(activity$date)
head(activity)
```

## What is mean total number of steps taken per day?

For this part of the assignment, we ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day.  
(For readability, only ‘steps taken’ for first 10 days are displayed)

```{r, echo=TRUE}
stepsPerDay <- activity %>%
        group_by(date) %>%
        summarize(sumsteps = sum(steps, na.rm = TRUE)) 
#Display first 10 rows of data
head(stepsPerDay,10)
```

2. Make a histogram of the total number of steps taken each day.

```{r, echo=TRUE}
hist(stepsPerDay$sumsteps, main = "Histogram of Daily Steps", breaks = 20,
     col="cyan", xlab="Steps", ylim = c(0,12))
```


3. Calculate and report the mean and median of the total number of steps taken per day.

```{r, echo=TRUE}
mean <- round(mean(stepsPerDay$sumsteps),digits = 2)
median <- round(median(stepsPerDay$sumsteps),digits = 2)

print(paste("The mean is: ", mean))
print(paste("The median is: ", median))
```



## What is the average daily activity pattern?

1. Make a time series plot (type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```{r, echo=TRUE}
stepsPerInterval <- activity %>%
        group_by(interval) %>%
        summarize(meansteps = mean(steps, na.rm = TRUE)) 
#Display first 10 rows of data
head(stepsPerInterval,10)
```


```{r, echo=TRUE}
plot(stepsPerInterval$meansteps ~ stepsPerInterval$interval,
     col="blue", 
     type="l",
     xlab = "5 Minute Intervals",
     ylab = "Average Number of Steps",
     main = "Steps By Time Interval")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r, echo=TRUE}
print(paste("Interval containing the most steps on average:",stepsPerInterval$interval[which.max(stepsPerInterval$meansteps)]))
print(paste("Average steps for that interval:",round(max(stepsPerInterval$meansteps),digits=2)))
```



## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the      total number of rows with NAs)

```{r, echo=TRUE}
print(paste("The total number of rows with NA is: ",sum(is.na(activity$steps))))
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the            mean/median for that day, or the mean for that 5-minute interval, etc.

I will use the mean for that 5 -minute interval to replace all the missing values in the dataset. At the end, I will check if all the NAs have been replaced
NA Strategy - To solve for the missing NA values the average for the associated interval will be used. The average was built in a prior step and is readily available: First, loop through all records of a copy of the ‘activity’ data. Then, look for records containing NA values. Finally, transform the ‘steps’ value based on matching the interval in the ‘stepsPerInterval’ data frame created in a prior step.

3. Create a new dataset that is equal to the original dataset but with the missing       data filled in.

```{r, echo=TRUE}
library(magrittr)
#Before
#Display first 10 rows of data
head(activity,10)

#After
replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
#meandata <- activity%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
meandata <- activity  
for (i in 1:nrow(activity)){
        if(is.na(activity$steps[i])){
                meandata$steps[i]<- stepsPerInterval$meansteps[meandata$interval[i] == stepsPerInterval$interval]
        }
}
#Display first 10 rows of data
head(meandata,10)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values       differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r, echo=TRUE}
stepsPerDay <- meandata %>%
        group_by(date) %>%
        summarize(sumsteps = sum(steps, na.rm = TRUE)) 
head(stepsPerDay,10)
```

```{r,echo=TRUE}
summary(stepsPerDay)
```

Making Histogram
```{r, echo=TRUE}
hist(stepsPerDay$sumsteps, main = "Histogram of Daily Steps", 
     col="yellow", xlab="Steps", ylab = "Frequency",breaks = 20,ylim = c(0,20))
```

```{r, echo=TRUE}
meanPostNA <- round(mean(stepsPerDay$sumsteps), digits = 2)
medianPostNA <- round(median(stepsPerDay$sumsteps), digits = 2)

print(paste("The mean is: ", mean(meanPostNA)))
```
Once logic is applied to imput missing values it allows for all detail records to be included increasing both the mean and median. The mean increases from 9354.23 to 10766.19. It is important to note that NA values in the first part of the project were ignored (na.rm = TRUE). Once averages were applied to the missing values the overall mean increased.

```{r,echo=TRUE}
NACompare <- data.frame(mean = c(mean,meanPostNA),median = c(median,medianPostNA))
rownames(NACompare) <- c("Pre NA Transformation", "Post NA Transformation")
print(NACompare)
```



## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r,echo=TRUE}
activityDoW <- meandata
activityDoW$date <- as.Date(activityDoW$date)
activityDoW$day <- ifelse(weekdays(activityDoW$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activityDoW$day <- as.factor(activityDoW$day)
```

2. Make a panel plot containing a time series plot (i.e. type=“l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r,echo=TRUE}
activityWeekday <- filter(activityDoW, activityDoW$day == "weekday")
activityWeekend <- filter(activityDoW, activityDoW$day == "weekend")

activityWeekday <- activityWeekday %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) 
activityWeekday$day <- "weekday"

activityWeekend <- activityWeekend %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) 
activityWeekend$day <- "weekend"

wkdayWkend <- rbind(activityWeekday, activityWeekend)
wkdayWkend$day <- as.factor(wkdayWkend$day)


g <- ggplot (wkdayWkend, aes (x=interval, y=steps,color=day))
g + geom_line() + facet_grid (day~.) + 
        theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14)) + 
        labs(y = "Number of Steps") + labs(x = "Interval") + 
        ggtitle("Average Number of Steps - Weekday vs. Weekend") + 
        theme(plot.title = element_text(hjust = 0.5))
```

The chart shows slight differences in the step patterns throughout the average daily intervals. 
- For weekdays there is a larger spike in the morning intervals that could indicate commuters on their way to work. 
- Spikes during the late morning and early afternoon seem to be higher on the weekends. One can assume that this might be related to subjects running errands, doing yardwork, exercising, etc.
- Those same intervals for weekdays appear to be less perhaps due to subjects sitting at their desk, driving a truck or standing in front of a machine in a factory.
