
Reproducible Research - Monitoring Personal Data - Project 1
==============================================================
Date: June 29, 2016



##Loading and preprocessing the data##

Show any code that is needed to

1.Load the data (i.e. read.csv())

2.Process/transform the data (if necessary) into a format suitable for your analysis

```r
library(plyr)
library(lattice)

setwd("~/Coursera/DataScience/Reproducible Research/Week 2 Assignment")
activity <-read.csv("./activity.csv")
activity$date <- as.Date(activity$date,"%Y-%m-%d")
```

##What is mean total number of steps taken per day?##

For this part of the assignment, you can ignore the missing values in the dataset.

1.Calculate the total number of steps taken per day

```r
sumSteps = ddply(activity, c("date"), numcolwise(sum),na.rm=TRUE)
```


2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(sumSteps$steps,breaks=20,main="",xlab="total steps per day",col="blue")
```

![plot of chunk totalStepsPerDay](figure/totalStepsPerDay-1.png)

3.Calculate and report the mean and median of the total number of steps taken per day

```r
mean(sumSteps$steps,na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(sumSteps$steps,na.rm=TRUE)
```

```
## [1] 10395
```
##What is the average daily activity pattern?##

3.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgInterval <- aggregate(x = list(steps = activity$steps), by = list(interval = activity$interval), 
                      FUN = mean, na.rm = TRUE)
with(avgInterval,plot(interval,steps,type="l"))
```

![plot of chunk AvgStepsPerDay](figure/AvgStepsPerDay-1.png)

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgInterval[which.max(avgInterval$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

##Imputing missing values##

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

*Assigned the steps mean for the 5-minute interval to NA values.* 

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity2 <- merge(activity,avgInterval,by.x="interval",by.y="interval")
names(activity2) <- c("interval","steps","date","avgSteps")
  
activity2$steps[is.na(activity2$steps)] <- activity2[is.na(activity2$steps),"avgSteps"]
```


4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
sumSteps2<- aggregate(steps ~ date, data=activity2,sum)

hist(sumSteps2$steps,breaks=20,main="",xlab="total steps per day",col="green")
```

![plot of chunk totalStepsPerDay2](figure/totalStepsPerDay2-1.png)

```r
mean(sumSteps2$steps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(sumSteps2$steps,na.rm=TRUE)
```

```
## [1] 10766.19
```
Do these values differ from the estimates from the first part of the assignment? 

*Yes.The values do differ after populating new NA values*

What is the impact of imputing missing data on the estimates of the total daily number of steps?

*It increased the mean and median of the total number of steps*


##Are there differences in activity patterns between weekdays and weekends?##

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
weekPer <- function(date) {
   if (weekdays(date,abbr=TRUE) %in% c("Sat","Sun"))
      return("Weekend")
   else return("Weekday")
}
activity2$weekPeriod <- as.factor(sapply(activity2$date, FUN=weekPer))
```


2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
avgSteps2 <- aggregate(steps ~ interval + weekPeriod, data = activity2, mean)

xyplot(steps ~ interval|weekPeriod, data=avgSteps2, layout = c(1,2),type="l")
```

![plot of chunk totalStepsPerInterval](figure/totalStepsPerInterval-1.png)

