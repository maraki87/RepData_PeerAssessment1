---
title: "PA1_template"
author: "Maria Luppi"
date: "Sunday, November 15, 2015"
output: html_document
---
## A. Basic settings


```r
echo = TRUE  # Always make code visible
options(scipen = 1)  # Turn off scientific notations for numbers
```

## B. Loading and processing the data


```r
unzip("activity.zip")
```

```
## Warning in unzip("activity.zip"): error 1 in extracting from zip file
```

```r
data = read.csv("activity.csv", colClasses = c("integer", "Date", "factor"), header = TRUE)
data$month = as.numeric(format(data$date, "%m"))
noNA = na.omit(data)
rownames(noNA) = 1:nrow(noNA)
head(noNA)
```

```
##   steps       date interval month
## 1     0 2012-10-02        0    10
## 2     0 2012-10-02        5    10
## 3     0 2012-10-02       10    10
## 4     0 2012-10-02       15    10
## 5     0 2012-10-02       20    10
## 6     0 2012-10-02       25    10
```

```r
dim(noNA)
```

```
## [1] 15264     4
```

```r
library(ggplot2)
```

## C. What is mean total number of steps taken per day?

### For this part of the assignment, you can ignore the missing values in the dataset.

### Calculate the total number of steps taken per day The results obtained are the following:
  
  date total

2012-10-01 0
2012-10-02 126
2012-10-03 11352
2012-10-04 12116
2012-10-05 13294
2012-10-06 15420
### Make a histogram of the total number of steps taken each day


```r
ggplot(noNA, aes(date, steps)) + geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

### Calculate and report the mean and median total number of steps taken per day
Mean total number of steps taken per day:
  

```r
totalSteps = aggregate(noNA$steps, list(Date = noNA$date), FUN = "sum")$x
mean(totalSteps)
```

```
## [1] 10766.19
```

The mean total number of steps taken per day is 9354.

Median total number of steps taken per day:


```r
median(totalSteps)
```

```
## [1] 10765
```

The median total number of steps taken per day is 10395.

## D. What is the average daily activity pattern?

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgSteps = aggregate(noNA$steps, list(interval = as.numeric(as.character(noNA$interval))), FUN = "mean")
names(avgSteps)[2] = "meanOfSteps"

ggplot(avgSteps, aes(interval, meanOfSteps)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avgSteps[avgSteps$meanOfSteps == max(avgSteps$meanOfSteps), ]
```

```
##     interval meanOfSteps
## 104      835    206.1698
```

The 5-minute interval that contains the maximum of steps, on average across all days, is 835.

## E. Imputing missing values

The total number of rows with NAs:


```r
sum(is.na(data))
```

```
## [1] 2304
```

The number of NA's is 2304.

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
My strategy is to use the mean for that 5-minute interval to fill each NA value in the steps column.

### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newData = data 
for (i in 1:nrow(newData)) {
  if (is.na(newData$steps[i])) {
    newData$steps[i] <- avgSteps[which(newData$interval[i] == avgSteps$interval), ]$meanOfSteps
  }
}

head(newData)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
sum(is.na(newData))
```

```
## [1] 0
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
ggplot(newData, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "steelblue",
                                             fill = "steelblue",
                                             width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

### Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
Mean total number of steps taken per day:
  

```r
newTotalSteps = aggregate(newData$steps, 
                            list(Date = newData$date), 
                            FUN = "sum")$x
newMean = mean(newTotalSteps)
newMean
```

```
## [1] 10766.19
```

The mean total number of steps taken per day is 10766.

Median total number of steps taken per day:

```r
newMedian = median(newTotalSteps)
newMedian
```

```
## [1] 10766.19
```

The median total number of steps taken per day is 10765.

### Compare them with the two before imputing missing data:

```r
oldMean = mean(totalSteps)
oldMedian = median(totalSteps)
newMean - oldMean
```

```
## [1] 0
```

```r
newMedian - oldMedian
```

```
## [1] 1.188679
```

These values differ greatly from the estimates from the first part of the assignment. The impact of imputing the missing values is to have more data, hence to obtain a bigger mean and median value.

## F. Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
head(newData)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
newData$weekdays = factor(format(newData$date, "%A"))
levels(newData$weekdays)
```

```
## [1] "domenica"  "giovedì"   "lunedì"    "martedì"   "mercoledì" "sabato"   
## [7] "venerdì"
```

```r
levels(newData$weekdays) = list(weekday = c("Monday", "Tuesday",
                                            "Wednesday", 
                                            "Thursday", "Friday"),
                                weekend = c("Saturday", "Sunday"))
levels(newData$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(newData$weekdays)
```

```
## 
## weekday weekend 
##       0       0
```

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avgSteps = aggregate(newData$steps, list(interval = as.numeric(as.character(newData$interval)), weekdays = newData$weekdays), FUN = "mean")
names(avgSteps)[3] = "meanOfSteps"
library(lattice)
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

```
## Error in limits.and.aspect(default.prepanel, prepanel = prepanel, have.xlim = have.xlim, : need at least one panel
```

