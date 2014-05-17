# Reproducible Research : Peer Assessment 1
========================================================

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit][1], [Nike Fuelband][2], or [Jawbone Up][3]. These type of devices are part of the quantified self movement  a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

##Assigment
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data
* Loading the data

```r
library(RCurl)
```

```
## Loading required package: bitops
```

```r
load_data <- function() {
    file_name = "activity.csv"
    setInternet2(use = TRUE)
    url_source = "https://raw.githubusercontent.com/juancarlosgarcia/RepData_PeerAssessment1/master/activity.csv"
    if (!file.exists(file_name)) {
        download.file(url_source, destfile = file_name, method = "auto")
    }
    activity_data <- read.csv(file_name, colClasses = c("numeric", "character", 
        "numeric"), header = T)
    activity_data
}
activity_data <- load_data()
```

* Transform the data into an aggregate format suitable for the analysis

```r
tidy_data <- aggregate(steps ~ date, activity_data, sum)
```



## What is mean total number of steps taken per day?
* This is a histogram of the total number of steps taken each day


```r
library(ggplot2)  #ggplot2 is used for plotting


steps_mean <- round(mean(tidy_data$steps), 0)
steps_median <- round(median(tidy_data$steps), 0)
point_labels <- c(paste(" Mean:", steps_mean), paste(" Median:", steps_median))
color_names <- c("red", "black")
ggplot(tidy_data, aes(x = steps)) + geom_histogram(fill = "orange", binwidth = 800) + 
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

[1]:http://www.fitbit.com/ec
[2]:http://www.nike.com/us/en_us/c/nikeplus-fuelband
[3]:https://jawbone.com/up
