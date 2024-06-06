---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

```r
data <- read.table(unz("activity.zip", "activity.csv"), header=T, quote="\"", sep=",")
data$date <- as.Date(data$date, format = "%Y-%m-%d")
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?

```r
totStepsDay <- aggregate(steps ~ date, data = data ,FUN=sum, na.rm = T)
hist(totStepsDay$steps, main = "Total number of steps per day", xlab = "Steps", col= "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
paste0("Mean of total steps per day is ", mean(totStepsDay$steps))
```

```
## [1] "Mean of total steps per day is 10766.1886792453"
```

```r
paste0("Median of total steps per day is ", median(totStepsDay$steps))
```

```
## [1] "Median of total steps per day is 10765"
```

## What is the average daily activity pattern?

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.3.2
```

```r
avgStepsDay <- aggregate(steps ~ interval, data=data,FUN=mean, na.rm = T)

ggplot(avgStepsDay, aes(x = interval, y = steps, color = steps))+ geom_line(data = avgStepsDay[!is.na(avgStepsDay$steps), ]) + ggtitle("Average daily activity pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
paste0("This 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps = ", avgStepsDay$interval[which.max(avgStepsDay$steps)])
```

```
## [1] "This 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps = 835"
```


## Imputing missing values

```r
paste0("Total number of missing values in the dataset = ",sum(is.na(data)))
```

```
## [1] "Total number of missing values in the dataset = 2304"
```

```r
imputed_data <- data.frame(steps = replace(data$steps, is.na(data$steps), floor(mean(data$steps, na.rm = TRUE))), date = as.Date(data$date), interval = data$interval)

totStepImp <- aggregate(steps ~ date , data=imputed_data,FUN=sum, na.rm = T)

hist(totStepImp$steps, main = "Total number of steps per day with imputed data", xlab = "Steps", col= "green")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
paste0("Mean of total steps per day with imputed data is ", mean(totStepImp$steps, na.rm = TRUE))
```

```
## [1] "Mean of total steps per day with imputed data is 10751.737704918"
```

```r
paste0("Median of total steps per day with imputed data is ", median(totStepImp$steps, na.rm = TRUE))
```

```
## [1] "Median of total steps per day with imputed data is 10656"
```

```r
paste0("Imputed data differs very little from the original data. The mean and median have only slightly changed after imputing the data.")
```

```
## [1] "Imputed data differs very little from the original data. The mean and median have only slightly changed after imputing the data."
```


## Are there differences in activity patterns between weekdays and weekends?

```r
imputed_data$days <- weekdays(imputed_data$date, TRUE)
imputed_data[imputed_data$days!='Sat'  & imputed_data$days!="Sun",]$days <- "weekday"
imputed_data[imputed_data$days=='Sat'  | imputed_data$days=="Sun",]$days <- "weekend"

avgStepImp <- aggregate(steps ~ interval + days, data = imputed_data, mean)

ggplot(avgStepImp, aes(x = interval, y = steps, color = days))+ geom_line() + facet_grid(rows = vars(days))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
paste0("There is a difference in activity patterns between weekends and weekdays. Weekdays have the peak activity levels between the 500-1000 interval but activity levels drop drastically. However, during the weekends, the activity starts late and does not peak as much yet it does not drop throughout the day until much later.")
```

```
## [1] "There is a difference in activity patterns between weekends and weekdays. Weekdays have the peak activity levels between the 500-1000 interval but activity levels drop drastically. However, during the weekends, the activity starts late and does not peak as much yet it does not drop throughout the day until much later."
```
