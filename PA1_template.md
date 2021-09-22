---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
setwd('C:\\Users\\DELL\\Desktop\\RStudio\\dir')

activity <- read.csv('activity.csv')
```


## What is mean total number of steps taken per day?


```r
steps_per_day <- aggregate(steps ~ date, activity, sum)
hist(steps_per_day$steps, main= paste("Total steps each day"), col="blue",
     xlab= "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
rmean <- mean(steps_per_day$steps)
rmean
```

```
## [1] 10766.19
```

```r
rmedian <- median(steps_per_day$steps)
rmedian
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
steps_per_interval <- aggregate(steps ~ interval, activity, mean)

plot(steps_per_interval$interval, steps_per_interval$steps, type='l', xlab='Interval', 
     ylab='Number of Steps', main= 'Average steps per day per interval')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
max_interval <- steps_per_interval[which.max(steps_per_interval$steps),1]
max_interval
```

```
## [1] 835
```




## Imputing missing values

## a.calculate the total number of missing values

```r
NATotal <- sum(!complete.cases(activity))
NATotal
```

```
## [1] 2304
```
## b.use mean to compute missing values

```r
stepsavg   <- aggregate(steps ~ interval, activity, mean)

fillNA <- numeric()

for (i in 1:nrow(activity)){
  obs <- activity[i, ]
  if (is.na(obs$steps)) {
    steps <- subset(stepsavg, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  fillNA <- c(fillNA, steps)
}
```

## c.new dataset with missing values

```r
newdata <- activity
newdata$steps <- fillNA
```

## d.histogram with total steps, mean and median

```r
stepstotal <- aggregate(steps ~ date, newdata, sum, na.rm = TRUE)
hist(stepstotal$steps, main = paste("Total Steps"), col = "blue", xlab= "No: Steps")
#Create histogram to show differences
hist(steps_per_day$steps, main = paste("Total Steps Each Day"), col="green", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "green"), lwd=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
rmeantotal <- mean(stepstotal$steps)
rmeantotal
```

```
## [1] 10766.19
```

```r
rmediantotal <- median(stepstotal$steps)
rmediantotal
```

```
## [1] 10766.19
```

```r
rmeandiff <- rmediantotal - rmedian
rmediandiff <- rmediantotal - rmedian

rmeandiff
```

```
## [1] 1.188679
```

```r
rmediandiff
```

```
## [1] 1.188679
```


## Are there differences in activity patterns between weekdays and weekends?

```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
newdata$dow <- as.factor(ifelse(is.element(weekdays(as.Date(newdata$date)),
                                           weekdays),"Weekday","Weekend"))
StepsTotalUnion <- aggregate(steps ~ interval + dow, newdata, mean)
library(lattice)
xyplot(StepsTotalUnion$steps ~ StepsTotalUnion$interval|StepsTotalUnion$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")              
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
