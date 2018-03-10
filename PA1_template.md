---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)

sessionInfo()
```

```
## R version 3.4.3 (2017-11-30)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS High Sierra 10.13.3
## 
## Matrix products: default
## BLAS: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] lattice_0.20-35 dplyr_0.7.4    
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.15     digest_0.6.15    rprojroot_1.3-2  assertthat_0.2.0
##  [5] grid_3.4.3       R6_2.2.2         backports_1.1.2  magrittr_1.5    
##  [9] evaluate_0.10.1  pillar_1.2.1     rlang_0.2.0      stringi_1.1.6   
## [13] bindrcpp_0.2     rmarkdown_1.9    tools_3.4.3      stringr_1.3.0   
## [17] glue_1.2.0       yaml_2.1.17      compiler_3.4.3   pkgconfig_2.0.1 
## [21] htmltools_0.3.6  bindr_0.1        knitr_1.20       tibble_1.4.2
```

## Loading and preprocessing the data


```r
activity <- read.csv(unz("activity.zip", filename = "activity.csv"))

activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?

### Calculate the total number of steps taken per day


```r
activity.by_day <- activity %>%
    group_by(date) %>%
    summarize(total_steps = sum(steps, na.rm = TRUE))
```

### Make a histogram of the total number of steps taken each day


```r
hist(
    activity.by_day$total_steps,
    breaks = 10,
    main = "Total steps per day",
    xlab = "Total steps")
```

![](PA1_template_files/figure-html/hist_total_steps_by_day-1.png)<!-- -->

### Calculate and report the mean and median of the total number of steps taken per day


```r
mean(activity.by_day$total_steps, na.rm = TRUE)
```

```
## [1] 9354.23
```


```r
median(activity.by_day$total_steps, na.rm = TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

### Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days


```r
activity.by_interval <- activity %>%
    group_by(interval) %>%
    summarize(average_steps = mean(steps, na.rm = TRUE))

plot(
    activity.by_interval$interval,
    activity.by_interval$average_steps,
    type = "l",
    main = "Average steps by interval",
    xlab = "Interval",
    ylab = "Average steps")
```

![](PA1_template_files/figure-html/plot_average_steps_by_interval-1.png)<!-- -->

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
activity.by_interval[
    activity.by_interval$average_steps
        == max(activity.by_interval$average_steps), ]
```

```
## # A tibble: 1 x 2
##   interval average_steps
##      <int>         <dbl>
## 1      835          206.
```

## Imputing missing values

### Calculate and report the total number of missing values in the dataset


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

### Devise a strategy for filling in all of the missing values in the dataset

Will use the average for the interval for missing values for that interval. The average for the interval was calculated earlier, first will join the two data frames.


```r
activity.strategy_missing_values <- activity %>%
    inner_join(activity.by_interval, by = "interval")
```

### Create a new dataset that is equal to the original dataset but with the missing data filled in

Check if the number of steps is NA, and use the average if so. Otherwise, just reuse the number of steps. Finally, select the columns of interest (can drop average_steps at this point).


```r
activity.fill_missing <- activity.strategy_missing_values %>%
    mutate(steps = ifelse(is.na(steps), average_steps, steps)) %>%
    select(steps, date, interval)
```

### Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day


```r
activity.by_day.after_imputation <- activity.fill_missing %>%
    group_by(date) %>%
    summarize(total_steps = sum(steps, na.rm = TRUE))

hist(
    activity.by_day.after_imputation$total_steps,
    breaks = 10,
    main = "Total steps per day after imputation",
    xlab = "Total steps")
```

![](PA1_template_files/figure-html/total_steps_by_day_after_imputation-1.png)<!-- -->

```r
mean(activity.by_day.after_imputation$total_steps)
```

```
## [1] 10766.19
```

```r
median(activity.by_day.after_imputation$total_steps)
```

```
## [1] 10766.19
```

#### Do these values differ from the estimates from the first part of the assignment?

Yes.

#### What is the impact of imputing missing data on the estimates of the total daily number of steps?

Using this imputation strategy, the mean and median are now the same. The mean has increased and the median has decreased; in addition the mean has increased more than the median has decreased.

After imputation, the distribution appears closer to a normal distribution.

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend”


```r
activity.fill_missing.add_factor <- activity.fill_missing %>%
    mutate(date.type = ifelse(
               weekdays(date, abbr = TRUE) %in% c("Sat", "Sun"),
               "weekend",
               "weekday"))
activity.fill_missing.add_factor$date.type <-
    as.factor(activity.fill_missing.add_factor$date.type)
```

### Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
activity.by_interval.by_date_type <- activity.fill_missing.add_factor %>%
    group_by(date.type, interval) %>%
    summarize(average_steps = mean(steps))

xyplot(
    average_steps ~ interval | date.type,
    data = activity.by_interval.by_date_type,
    type = "l",
    ylab = "average steps",
    main = "average steps on weekend vs weekday",
    layout = c(1, 2))
```

![](PA1_template_files/figure-html/compare_days-1.png)<!-- -->
