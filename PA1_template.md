# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
unzip(zipfile = "activity.zip")
csv <- read.csv("activity.csv")
csv$date <- as.Date(csv$date,"%Y-%m-%d")
```

## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day
2. Calculate and report the mean and median total number of steps taken per day

```r
library(data.table)
```

```
## Warning: package 'data.table' was built under R version 3.2.3
```

```r
table <- data.table(csv)
table_sum <- as.data.frame(table[, list(sum_steps=sum(steps,na.rm=TRUE)),by=date])
mean0 <- mean(table_sum$sum_steps)
median0 <- median(table_sum$sum_steps)
hist(table_sum$sum_steps, 
    breaks=30, 
    main="Total number of steps taken each day",xlab="Total number of steps"
)
abline(v=mean0, col = 'blue')
abline(v=median0, col = 'green')
legend('topright', lty=1, col=c("blue","green"),
       legend = c(paste('Mean = ', round(mean0,2)),paste('Median = ', median0)))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
table_mean <- as.data.frame(table[, list(average_steps=mean(steps,na.rm=TRUE)),by=interval])
plot(table_mean$interval, table_mean$average_steps, type="l", ylab="Avg # steps", xlab="5-minute Interval")
maxInterval <- table_mean[which.max(table_mean$average_steps),]
points(maxInterval$interval,  maxInterval$average_steps, col = 'red', pch = 19)
legend('topright', legend = paste("Max avg number of steps \n occurs on interval #",maxInterval$interval))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(csv))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
We will use the mean of the respective 5-min interval (see line 2 of the code below).

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
csv_without_NA <- csv
csv_without_NA$steps[is.na(csv_without_NA$steps)] <- tapply(csv_without_NA$steps, csv_without_NA$interval, mean, na.rm = TRUE)
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
table <- data.table(csv_without_NA)
table_sum <- as.data.frame(table[, list(sum_steps=sum(steps,na.rm=TRUE)),by=date])
mean1 <- mean(table_sum$sum_steps)
median1 <- median(table_sum$sum_steps)
hist(table_sum$sum_steps, 
     breaks=30, 
     main="Total number of steps taken each day",xlab="Total number of steps"
)
abline(v=mean1, col = 'blue')
abline(v=median1, col = 'green')
legend('topright', lty=1, col=c("blue","green"),
       legend = c(paste('Mean = ', round(mean1,2)),paste('Median = ', round(median1,2))))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)

Both mean and median values are changed (in this case, increased).

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

```r
csv$daytype = as.factor(c("weekday","weekend"))
csv$daytype[weekdays(as.Date(as.character(csv$date))) %in% c("sábado","domingo")] = "weekend"
csv$daytype[weekdays(as.Date(as.character(csv$date))) %in% c("segunda-feira","terça-feira","quarta-feira","quinta-feira","seta-feira")] = "weekday"
tableweekend <- data.table(csv[csv$daytype == "weekend",])
table_mean_weekend <- as.data.frame(tableweekend[, list(average_steps2=mean(steps,na.rm=TRUE)),by=interval])
tableweekday <- data.table(csv[csv$daytype == "weekday",])
table_mean_weekday <- as.data.frame(tableweekday[, list(average_steps1=mean(steps,na.rm=TRUE)),by=interval])
merge <- merge(table_mean_weekend, table_mean_weekday)
par(mfrow = c(1, 1))
plot(1, type = 'n', ylim = c(0,10+maxInterval$average_steps), xlim = c(0, 2360), 
     ylab="Avg # steps", xlab="5-minute Interval")
lines(merge$interval, merge$average_steps1, col=rgb(0,1,0,alpha=0.6), lwd=2 )
lines(merge$interval, merge$average_steps2, col=rgb(1,0,0,alpha=0.6), lwd=2 )
legend('topright', lty=1, lwd=2, col=c("green","red"),legend = c("Weekdays","Weekends"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)

The weekday pattern has a strong peek in the morning compared to the weekend pattern, which seems to have less variation across the day.
