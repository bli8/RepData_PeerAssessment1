---
title: "Assignment 1"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Load the data

```{r activity}
library(readr)
activity <- read_csv("C:/Users/lbw92/Desktop/activity.csv")
summary(activity)
```

## What is the total number of steps taken every day?

Make a histogram of the total number of steps taken each day.

Calculate and report the mean and median total number of steps taken each day.

```{r,echo=TRUE}
steps.date <- aggregate(activity$steps,list(activity$date),sum,na.rm=TRUE)
colnames(steps.date ) <- c("date","steps")
head(steps.date)

hist(steps.date$steps,ylab="Number of days",xlab="Number of steps",col="red",main="Histogram of steps each day")

mean(steps.date$steps)
median(steps.date$steps)
```

## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r,echo=TRUE}
interval <- aggregate(activity$steps,list(activity$interval),mean,na.rm=TRUE)
colnames(interval) <- c("interval","steps_mean")
head(interval)

plot(interval$interval,interval$steps_mean,type="l",xlab="Interval in minutes",ylab="Average number of steps",main="Time series plot")

interval[which.max(interval$steps),]
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r,echo=TRUE}
sum(is.na(activity$steps))

interval.mean <- aggregate(steps ~ interval, activity , FUN = mean)
for (i in 1:nrow(activity)){
     tmp <- activity$steps[i]
     if(is.na(tmp)){
         for(j in 1:nrow(interval.mean)){
             if(activity$interval[i] == interval.mean$interval[j]){
                 activity$steps[i] = interval.mean$steps[j]
                 break
             }
         }
     }  
 }
head(activity)

steps.all <- aggregate(activity$steps,list(activity$date),sum)
colnames(steps.all ) <- c("date","sum")
hist(steps.all$sum,ylab="Number of days",xlab="Total number of steps",col="blue",main="Histogram of total number of steps each day")

mean(steps.all$sum)
median(steps.all$sum)
```
Both mean and median values differ from the first part of assignment. Inputting missing data increase the estimate of total daily number of steps

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r, echo=TRUE}
library(timeDate)
activity$day <- ifelse(isWeekday(activity$date) == TRUE,"weekday","weekend")
activity$day <- as.factor(activity$day)

weekday.mean <- aggregate(steps ~ interval+day,activity,mean)
library(lattice)
xyplot(steps ~ interval|day,data=weekday.mean,type="l",lwd=2,layout=c(1,2),
       xlab="5-minute interval",ylab="Number of steps")
```

```{r,include=FALSE}
  file.rename(from="PA1_template.Rmd", to="PA1_template.md")
```
