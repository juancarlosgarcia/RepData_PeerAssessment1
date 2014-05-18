# Reproducible Research : Peer Assessment 1
========================================================

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit][1], [Nike Fuelband][2], or [Jawbone Up][3]. These type of devices are part of the quantified self movement  a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

##Assigment
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data
* Loading the data

```r
library(RCurl)  #if the data source does not exists we need to download from the repository.
```

```
## Loading required package: bitops
```

```r
file_name = "activity.csv"
setInternet2(use = TRUE)  #required to support SSL in the url
url_source = "https://raw.githubusercontent.com/juancarlosgarcia/RepData_PeerAssessment1/master/activity.csv"
if (!file.exists(file_name)) {
    download.file(url_source, destfile = file_name, method = "auto")
}
activity_data <- read.csv(file_name, colClasses = c("numeric", "character", 
    "numeric"), header = T)
```

* Transform the data into an aggregate format suitable for the analysis

```r
activity_day <- aggregate(steps ~ date, data = activity_data, FUN = sum)
activity_inv <- aggregate(activity_data$steps, by = list(interval = activity_data$interval), 
    FUN = mean, na.rm = T)
colnames(activity_inv) <- c("interval", "steps")
activity_inv$interval <- as.integer(activity_inv$interval)
```



## What is mean total number of steps taken per day?
* This is a histogram of the total number of steps taken each day


```r
library(ggplot2)  #ggplot2 is used for plotting

steps_mean <- round(mean(activity_day$steps), 0)
steps_median <- round(median(activity_day$steps), 0)
point_labels <- c(paste(" Mean:", steps_mean), paste(" Median:", steps_median))
color_names <- c("red", "black")
ggplot(activity_day, aes(x = steps)) + geom_histogram(fill = "orange", binwidth = 800) + 
    geom_point(aes(x = steps_mean, y = 0, color = color_names[1]), size = 4, 
        shape = 12) + geom_point(aes(x = steps_median, y = 0, color = color_names[2]), 
    size = 4, shape = 10) + scale_color_manual(name = element_blank(), labels = point_labels, 
    values = color_names) + labs(title = "Histogram of Steps per Day", x = "Number of Steps", 
    y = "Count") + theme_bw() + theme(legend.position = "bottom")
```

![plot of chunk make_histogram](figure/make_histogram.png) 

* The mean and median total number of steps taken per day are :
 - **Mean: 10766**
 - **Median: 10765**

## What is the average daily activity pattern?
This is a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_max <- which.max(activity_inv$steps)
interval_max <- activity_inv[steps_max, ]$interval
point_labels <- c(paste(" Maximun Activity Interval:", interval_max))
ggplot(activity_inv, aes(x = interval, y = steps)) + geom_line(color = "orange", 
    size = 1) + geom_point(aes(x = interval_max, y = 0, color = "red"), size = 4, 
    shape = 12) + scale_color_manual(name = element_blank(), labels = point_labels, 
    values = c("red")) + labs(title = "Average daily activity pattern", x = "5-minute Interval", 
    y = "Average Steps") + theme_bw() + theme(legend.position = "bottom")
```

![plot of chunk daily_pattern](figure/daily_pattern.png) 

* The 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps is :
 - **5-minute interval: 835**

## Imputing missing values
There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.





[1]:http://www.fitbit.com/ec
[2]:http://www.nike.com/us/en_us/c/nikeplus-fuelband
[3]:https://jawbone.com/up
