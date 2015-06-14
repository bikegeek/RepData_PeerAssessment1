<!-- rmarkdown v1 -->

---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    fig_height: 7
    keep_md: yes
  pdf_document: default
  word_document: default
---


## Loading and preprocessing the data

```r
    library("reshape2")
    df<-read.csv("activity.csv",header=TRUE, stringsAsFactors=FALSE)
    raw<-df
    data<-na.omit(df)
    data$steps <- as.numeric(data$steps)
    data$interval <- as.numeric(data$interval)
    melted <- melt(data,id=c(2:3),measure=c(1),na.rm=TRUE)
    sumsteps <- dcast(melted, date~variable,sum)
```


## What is mean total number of steps taken per day?

```r
    meansteps <- mean(sumsteps$steps)
    print(meansteps)
```

```
## [1] 10766.19
```


## What is the average daily activity pattern?

```r
    mediansteps <- median(sumsteps$steps)
    print(mediansteps)
```

```
## [1] 10765
```

####Histogram
![plot of chunk histogram](figure/histogram-1.png) 

```
## quartz_off_screen 
##                 2
```

##Average Daily Activity Pattern

```r
    #Calculate the average for each 5 minute interval over all the days
    meantimeseries <- dcast(melted,interval~variable,mean)

    #Create a separate figure that can be checked into github or attached to
    #another document...
    png(file="timeseries.png", height=480, width=480)
    plot(meantimeseries$interval,
         meantimeseries$steps,type="l",
         xlab="Time Interval (seconds)",
         ylab="Avg # steps across all days")
    dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
    #Create a figure that is viewed in the knitr output
    plot(meantimeseries$interval,
         meantimeseries$steps,type="l",
         xlab="Time Interval (seconds)",
         ylab="Avg # steps across all days",
         main="Timeseries plot (ignore NA's)")
```

![plot of chunk timeseries](figure/timeseries-1.png) 

###Time interval with the maximum number of steps (averaged across all days):

```r
    index<-which.max(meantimeseries$steps)
    print(cat( meantimeseries[index,1], " second interval"))
```

```
## 835  second intervalNULL
```

## Imputing missing values

####1.  Total number of missing values (NA) in dataset

```r
    num.missing <- as.numeric(sum(is.na(df$steps)))
    print("Number of NA's for step data: ")
```

```
## [1] "Number of NA's for step data: "
```

```r
    print(num.missing)
```

```
## [1] 2304
```

####2.  Strategy for filling in the missing values
Replace each *NA* with the mean value that corresponds to the 5-minute interval
of the missing value.  The mean value for each 5-minute interval is available
from the time-series calculation we performed earlier.  


####3.  Create a new dataset with missing values filled in with the median value

```r
    missingindices <- which(is.na(raw$steps))
    rawdata <- df
    #Replace each NA with the average number of steps for the interval that
    #corresponds to this.
    for(i in 1:length(missingindices)){
       #get the time interval for each element in the missingindices list/vector
       rawinterval <- rawdata[i,3]

       #get the mean number of steps for this interval (from the
       #meantimeseries data frame)
       row <- which(meantimeseries$interval == rawinterval)
       meanforinterval<- meantimeseries[row,2]

       #Replace the NA for this row with the meanforinterval value
       rawdata[missingindices[i],1] <- meanforinterval

   }
  #print(head(rawdata,500))
```

####4. Make histogram, calculate and report the mean and median total number of steps taken per day.

####Histogram- Filled Data
![plot of chunk histogram filled data](figure/histogram filled data-1.png) 

```
## quartz_off_screen 
##                 2
```
####Comments:
Replacing the missing data with the average number of steps (that correspond to
the time interval) increased the frequency of the number of averages steps but
did not change the "shape" of the histogram.


####Filled Data-Mean total number of steps taken per day:

```r
    meansteps2 <- mean(sumsteps2$steps)
    print(meansteps2)
```

```
## [1] 10766.19
```

####Filled Data-Median number of total daily steps:

```r
    mediansteps2 <- median(sumsteps2$steps)
    print(mediansteps2)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```
## quartz_off_screen 
##                 2
```

![plot of chunk daysofweek](figure/daysofweek-1.png) 

####Comments:
There are more steps taken throughout the day on weekends, especially from the
1000 sec to ~1800 sec interval.  The number of steps taken are also taken later
on the weekend mornings, consistent with sleeping-in on the weekends.





