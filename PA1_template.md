# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Load the data (i.e. read.csv())


```r
data <- read.csv("activity.csv", na.strings="NA", stringsAsFactors=FALSE)

library("plyr")
library("dplyr")
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```



## What is mean total number of steps taken per day?

Make a histogram of the total number of steps taken each day


```r
steps_pr_day <- data %>%
     group_by(date) %>%
     summarise(Tot.steps = sum(steps))

hist(steps_pr_day$Tot.steps, main="Total number of steps taken each day", xlab="Number of steps", ylab="Number of days")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 


Calculated and reported the mean:


```r
mean(steps_pr_day$Tot.steps, na.rm=TRUE)
```

```
## [1] 10766.19
```
Calculated and reported the median:


```r
median(steps_pr_day$Tot.steps, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

A time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_pr_interval <- data %>% 
     group_by(interval) %>%
     summarise(mean.steps = mean(steps, na.rm=TRUE))

plot(steps_pr_interval$mean.steps, type="l", main="Average daily activity", xlab="Intervals", ylab="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

The 5-minute interval, that on average across all the days in the dataset, contains the maximum number of steps:


```r
which.max(steps_pr_interval$mean.steps)
```

```
## [1] 104
```

## Imputing missing values

Calculated and reported the total number of missing values in the dataset (i.e. the total number of rows with NAs):


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

My strategy for filling in all of the missing values in the dataset is to use the mean for that 5-minute interval and insert it where it is missing.

Created a new dataset that is equal to the original dataset but with the missing data filled in:


```r
cleaned_data <- data
steps_pr_interval_extended <- steps_pr_interval[rep(seq_len(nrow(steps_pr_interval)), 61), ]
for (i in 1:nrow(data) ) {
        if(is.na(data[i,1])) {
                cleaned_data[i,1] <- as.integer(steps_pr_interval_extended[i,2])
        }
}
```

Made a histogram of the total number of steps taken each day and Calculated and reported the mean and median total number of steps taken per day. THe histogram looks the same but the mean and median values are lower. Imputing the values makes the average steps pr day less.


```r
steps_pr_day_cleaned <- cleaned_data %>%
     group_by(date) %>%
     summarise(Tot.steps = sum(steps))

hist(steps_pr_day_cleaned$Tot.steps, main="Total number of steps taken each day", xlab="Number of steps", ylab="Number of days")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 


Calculated and reported the mean with imputed values:

```r
mean(steps_pr_day_cleaned$Tot.steps)
```

```
## [1] 10749.77
```
Calculated and reported the median with imputed values:

```r
median(steps_pr_day_cleaned$Tot.steps)
```

```
## [1] 10641
```


## Are there differences in activity patterns between weekdays and weekends?

Created a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
cleaned_data$day <- c("weekend", "weekday", "weekday", "weekday", "weekday", "weekday", "weekend")[as.POSIXlt(cleaned_data$date)$wday + 1]
cleaned_data$day <- as.factor(cleaned_data$day)
```

Made a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
weekdays <- cleaned_data[which(cleaned_data$day=="weekday"),]
weekends <- cleaned_data[which(cleaned_data$day=="weekend"),]

steps_pr_weekday <- weekdays %>%
     group_by(interval) %>%
     summarise(mean.steps = mean(steps))

steps_pr_weekend <- weekends %>%
     group_by(interval) %>%
     summarise(mean.steps = mean(steps))


par(mfrow=c(2,1))
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 4))
plot(steps_pr_weekday$mean.steps, type="l", xaxt='n')
plot(steps_pr_weekend$mean.steps, type="l")
mtext("Interval", side = 1, outer = TRUE)
mtext("Weekend     Weekdays", side = 4, outer = TRUE)
mtext("Number of steps", side = 2, outer = TRUE)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 
