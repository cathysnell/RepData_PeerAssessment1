---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

  
## Loading and preprocessing the data  

Load the data and process/transform the data into a format suitable for analysis.


```r
url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- tempfile()
download.file(url, temp)
act_mon <- read.csv(unzip(temp))
```

The variables included in this dataset are:

- **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- **date**: The date on which the measurement was taken in YYYY-MM-DD format

- **interval**: Identifier for the 5-minute interval in which measurement was taken

## What is mean total number of steps taken per day?  

For this part, the missing values have been ignored in the dataset.

First, calculate the total number of steps taken per day.


```r
tot_steps <- aggregate(act_mon$steps, list(act_mon$date), FUN=sum)
```

Display the total number of steps taken each day as a histogram and calculate the mean and median.


```r
hist(tot_steps$x)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

```r
mean_st <- mean(tot_steps$x, na.rm=TRUE)
median_st <- median(tot_steps$x, na.rm=TRUE)
```

The mean of the total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup> and the median is 10765.

## What is the average daily activity pattern?  

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avg_steps <- aggregate(act_mon$steps, list(act_mon$interval), FUN=mean, na.rm=TRUE)
plot(avg_steps, type = "l",
    ylab = "Average Steps Taken Across All Days",
    xlab = "5-Minute Interval")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_int <- which.max(avg_steps$x)
```

Interval 104, on average across all days in the dataset, contains the maximum number of steps (206.1698113 steps)

## Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

So, calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
na_count <- sum(is.na(act_mon$steps))
```

The total number of rows with NAs is 2304 or 13.1147541%.

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
cleaned_am <- act_mon
for (i in 1:nrow(cleaned_am)) {
    if (is.na(cleaned_am[i,1])) {
        avg <- avg_steps[avg_steps$Group.1==cleaned_am[i,3],2]
        cleaned_am[i,1] <- avg
    }
}
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
cleaned_steps <- aggregate(cleaned_am$steps, list(cleaned_am$date), FUN=sum)
hist(cleaned_steps$x)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
mean_clean <- mean(cleaned_steps$x)
median_clean <- median(cleaned_steps$x)
```

The mean of the total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup> and the median is 1.0766189 &times; 10<sup>4</sup>.

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
cleaned_am$day <- factor(weekdays(as.Date(cleaned_am$date)))
cleaned_am$day_type <- factor(ifelse(cleaned_am$day=="Saturday" | cleaned_am$day=="Sunday", "weekend", "weekday"))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
avg_cleaned_steps <- aggregate(cleaned_am$steps, list(cleaned_am$interval, cleaned_am$day_type), FUN=mean)
library(lattice)
xyplot(x ~ Group.1|Group.2, 
           data = avg_cleaned_steps,
           type = "l",
           xlab = "5-Minute Interval",
           ylab = "Average Steps Taken",
           layout=c(1,2))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 
