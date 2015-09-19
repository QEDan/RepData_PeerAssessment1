# Reproducible Research: Peer Assessment 1
## Loading and preprocessing the data


```r
#Load the data to a data frame

activity.df <- read.csv("activity.csv")

#Convert the date field to a date data type
activity.df$date <- as.Date(activity.df$date)
```
## What is mean total number of steps taken per day?


```r
steps.per.day <- aggregate( 
  x = activity.df$steps,
  by = list(activity.df$date),
  FUN = sum,
  na.rm=T)
  
names(steps.per.day) <- c("Date", "Steps")
mean.spd <- mean(steps.per.day$Steps, na.rm=TRUE)
median.spd <- median(steps.per.day$Steps, na.rm=TRUE)
library(ggplot2)
hist <- ggplot(steps.per.day, aes(x=Steps)) +
  ggtitle("Steps per day") +
  xlab("Steps") +
  geom_histogram(binwidth=2000) +
  annotate("text", label = paste("Mean = ", format(mean.spd)), 
               x = 0, hjust = 0, y = Inf, vjust = 2, color = "darkred") +
    annotate("text", label = paste("Median = ", format(median.spd)), 
               x = 15000, hjust = 0, y = Inf, vjust = 2, color = "darkgreen") +
  geom_vline(xintercept = mean.spd, color = "darkred") +
  geom_vline(xintercept = median.spd, color = "darkgreen")
hist
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

The mean total number of steps:

```r
print(mean.spd)
```

```
## [1] 9354.23
```

The median total number of steps:

```r
print(median.spd)
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
mean.steps  <- aggregate(
  x = activity.df$steps , 
  by = list(activity.df$interval), 
  FUN = mean,
  na.rm=TRUE)


names(mean.steps) <- c("interval","steps")

steps.ts <- ggplot(mean.steps,aes(interval,steps)) +
  ggtitle("Mean Steps vs. Time") +
  geom_line() +
  xlab("Time (5 minute intervals)")          
steps.ts 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 



```r
mean.steps[which.max(mean.steps$steps),c("interval")]
```

```
## [1] 835
```
## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
