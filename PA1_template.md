---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Two datasets are needed to complete this project.  The orignal data set contains missing values for one of the variables(steps).  The second data set is a tranform of the original where the rows with missing values are removed.


```r
        # Original dataset
        activity <- read.csv("data/activity.csv", stringsAsFactors=FALSE,  header=TRUE)
        
        # Transformed data set 
        activityNoNA <- activity[which(is.na(activity$steps)== FALSE),]
```

## What is mean total number of steps taken per day?

This is based on the transformed data set, i.e. there are no missing values in the steps variable. The following code calculates the total number of steps across all days and plots the histogram:

```r
totalSteps <- aggregate(activityNoNA$steps, by=list(date=activityNoNA$date), FUN = sum )
# histogram of total number of steps (x) taken each day
stepsPlot <- hist(totalSteps$x, main = "Total Number of Steps Taken Each Day", ylim = c(0, 40),
                  sub= "Excluded missing values in steps",xlab = "Total Steps", col = "Yellow")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

Calculate mean and median values:

```r
mean(totalSteps$x)  # mean number of steps taken per day
```

```
## [1] 10766
```

```r
median(totalSteps$x) # median number of steps taken per day
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Contintue to use the transformed data set. The pattern is best viewed by creating a plot of the average number of steps taken across all days for each 5-minute interval.  First, the average number of steps for each interval is calculated.

```r
avgSteps <- aggregate(activityNoNA$steps, by=list(interval=activityNoNA$interval), FUN=mean) 
```
The sample results indicate the average number of steps across all days for each specfic 5-minute interval. A day has 288 5-minute intervals. The interval id indicates the interval starting point of the interval in military time, hence

interval | Time

       0 | 12:00 AM       
     500 | 05:00 AM      
    1000 | 10:00 AM      
    1500 | 03:00 PM    
    2000 | 08:00 PM       
    2355 | 11:55 PM


```r
avgStepsPlot <- plot(avgSteps$interval, avgSteps$x, type= 'l', 
                main = "Average Daily Activity", xlab = "Interval", 
                ylab = "Average Number of Steps", 
                col = "Dark Green", sub= "Excluded missing values in steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

The 5-minute interval, on average across all the days in the dataset that contains, the maximum number of steps is:

```r
maxNumSteps <-avgSteps[which(avgSteps$x == max(avgSteps$x)),]
maxNumSteps
```

```
##     interval     x
## 104      835 206.2
```
The most active time of day is at 08:35AM. 

## Imputing missing values

The original data set contains missing values and is used to impute missing values. 

Missing values in the dataset may introduce a bias into the calculations or summaries of the data. To determine if there are missing values in the steps and date variables:

```r
sum(is.na(activity$steps)) # number of missing values in steps
```

```
## [1] 2304
```

```r
sum (is.na(activity$date)) # number of missing values in date
```

```
## [1] 0
```
The above calculations indicate there an 2304 missing (NA) values in steps and no missing (NA) values in date.

The missing values in steps will be filled with the mean for the 5-minute interval.

- calculate the mean of each interval over all days
- merge the datasets on interval 
- replace the NA in steps with the corresponding interval mean

```r
intervalMean <- aggregate(activityNoNA$steps, by=list(m=activityNoNA$interval), FUN=mean)

iSteps <- merge(activity, intervalMean, by.x="interval", by.y="m")

iSteps$steps[is.na(iSteps$steps)] <-  iSteps$x[is.na(iSteps$steps)]
```
There are no missing (NA) values for steps.  Value of steps is >= 0 for each interval in each day.  

The new data set is created by subsetting and excluding the x (mean) variable, then sorting the data set by date:

```r
newAct <-  subset(iSteps, select = c(steps, date, interval))  #180 obs of 68 variables
newActivity <- newAct[order(newAct$date),]
str(newActivity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
The new (imputed) dataset is equal in number of observations and variables to the original dataset and there are no missing values. 

The following uses the imputed dataset (newActivity) to create the histogram of the total number of steps taken per day. To facilitate the comparison with the histogram of the original data (activity), the plots are placed side-by-side. The mean and median of each dataset is calculated. 


```r
totalStepsAct <- aggregate(newActivity$steps, by=list(date=newActivity$date), FUN = sum )

par (mfrow=c(1,2))
stepsPlotAct <- hist(totalStepsAct$x, main = "Total Number of Steps Taken Each Day", xlab="Total Steps",
                     ylim = c(0, 40),
                     sub = "Imputed missing values",
                     col = "Sky Blue")
        abline(v=median(totalStepsAct$x), col = "red", lwd=2)

stepsPlot <- hist(totalSteps$x, main = "Total Number of Steps Taken Each Day", ylim = c(0, 40),
                  sub= "Excluded missing values",xlab = "Total Steps", col = "Yellow")
        abline(v=median(totalSteps$x), col = "red", lwd=2)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

```r
# original data set with missing values in steps 
stepsPlot$counts # frequency count
```

```
## [1]  5 12 28  6  2
```

```r
mean(totalSteps$x) # mean number of steps 
```

```
## [1] 10766
```

```r
median(totalSteps$x) # median number of steps
```

```
## [1] 10765
```

```r
# imputed data set
stepsPlotAct$counts # frequency count 
```

```
## [1]  5 12 36  6  2
```

```r
mean(totalStepsAct$x) # mean number of steps 
```

```
## [1] 10766
```

```r
median(totalStepsAct$x) # median number of steps 
```

```
## [1] 10766
```

```r
# calcuate percent change
(stepsPlotAct$count[(3)] - stepsPlot$counts[(3)]) / stepsPlot$counts[(3)] * 100
```

```
## [1] 28.57
```
Imputing the data has no impact on the mean or the median number of steps taken each day.  There is an 29% increase in the frequency in the 10,000 - 15,000 range of total steps using the imputed data set. 

## Are there differences in activity patterns between weekdays and weekends?

Using the imputed data set, the function weekdays is used to convert dates to days of the week. It is then necessary to group days into type: "Saturday" and "Sunday" are of type "weekend"" and the remaining days of the week are of type "weekday". The average number steps taken is calculated across all weekday days or weekend days. 

```r
newActivity$weekday <- weekdays(as.Date(newActivity$date, '%Y-%m-%d'))
newActivity$weekday.type <- ifelse(newActivity$weekday == "Sunday" 
                                   | newActivity$weekday == "Saturday", "weekend", "weekday")
newActivity$weekday.type <-factor(newActivity$weekday.type)
```

Using the lattice system, the panel plots are created to compare weekend vs. weekday activity.

```r
data <- aggregate(newActivity$steps, by=list(interval=newActivity$interval, 
                                             type=newActivity$weekday.type), FUN=mean)

library(lattice)
xyplot(x ~interval | type, data, type = "l", layout=c(1,2), ylab = "Average number of steps taken")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

Examining the chart the maximum average number of steps for the week occurs on weekday mornings between 5:00 AM and 10:00 AM. The calcuation below establish the exact interval at 8:35 AM.

```r
data[which(data$x == max(data$x)),]
```

```
##     interval    type     x
## 104      835 weekday 230.4
```

There is another weekday peak of activity between 3:00 PM and 8:00 PM.  Weekend activity levels differ.  The maxiumn average number of steps is below the weekday maximun, but the participant is more active throughout the day from 8:00 AM to 8:00 PM. 
