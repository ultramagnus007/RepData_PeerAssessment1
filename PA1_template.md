---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

# 1. Loading and preprocessing the data

```r
data <- read.table("activity.csv", header = TRUE, sep = ",",
			 colClasses = c("numeric", "Date", "integer"))
```
### ignore the missing values in the dataset

```r
cleandata <- data[is.na(data$steps) == FALSE,]
library(dplyr)
```


# 2. What is mean total number of steps taken per day?

## 1. Calculate the total number of steps taken per day

```r
daygroupdata<-group_by(cleandata, date)
stepsdaydata<-summarise(daygroupdata, total = sum(steps))
```
## 2. histogram of the total number of steps taken each day

```r
hist(stepsdaydata$total, xlab = "total number of steps taken each day",
		main = "using Data with NAs removed, Histogram of total no of steps each days ")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 
## 3. mean and median of the total number of steps taken per day

```r
medianday<-median(stepsdaydata$total)
meanday<-mean(stepsdaydata$total)
print("median = ")
```

```
## [1] "median = "
```

```r
print(medianday)
```

```
## [1] 10765
```

```r
print("mean = ")
```

```
## [1] "mean = "
```

```r
print(meanday)
```

```
## [1] 10766.19
```


# 3. What is the average daily activity pattern?
intervalStepData contains the average of 5 minutes interval avergaed acrossed all days

```r
intervalGroupData<-group_by(cleandata, interval)
intervalStepdata<-summarise(intervalGroupData, avg = mean(steps))
```
## 1. time series plot of the 5-minute interval (x-axis) vs the average number of steps taken, averaged across all days (y-axis)

```r
plot(intervalStepdata$interval, intervalStepdata$avg, type = "l",
		col = "red", xlab = "5 minute interval", ylab = "average number of steps in a day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 
## 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
maxInterval contains the interval which have maximum average steps across all the days

```r
maxInterval <- intervalStepdata[intervalStepdata$avg==max(intervalStepdata$avg),]
maxInterval$interval
```

```
## [1] 835
```



# 4. Imputing missing values

## 1. total number of missing values
number of missing values = total no of rows in original data - total no of rows in cleandata(which contains No NA values)

```r
numrow <-nrow(data)
nrowclean <- nrow(cleandata)
nmissing <- numrow - nrowclean
print("number of missing values = ")
```

```
## [1] "number of missing values = "
```

```r
print(nmissing)
```

```
## [1] 2304
```
## 2. Create a new dataset that is equal to the original dataset but with the missing data filled in

intervalStepData contains the no of steps in the interval averaged accross all days, there are 288 such 5 minute intervals in a day
in the folloing loop NA values are replaced with corresponding values from intervalStepData

```r
j<-1
#making copy of original data
newdata <- data
for( i in c(1:numrow))
{
        if(is.na(newdata[i,1]) == TRUE)
                newdata[i, 1] = intervalStepdata$avg[j]
        j = (j+1)
        if(j == 289)
                j = 1
}
```
## 3. histogram of the total number of steps taken each day

```r
daygroupdata<-group_by(newdata, date)
stepsdaydata<-summarise(daygroupdata, total = sum(steps))
hist(stepsdaydata$total, xlab = "total number of steps taken each day",
		main = "using Data NAs with replaced, Histogram of total no of steps each days ")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
## 4. mean and median of the total number of steps taken per day

```r
medianday<-median(stepsdaydata$total)
meanday<-mean(stepsdaydata$total)
print("new median = ")
```

```
## [1] "new median = "
```

```r
print(medianday)
```

```
## [1] 10766.19
```

```r
print("new mean = ")
```

```
## [1] "new mean = "
```

```r
print(meanday)
```

```
## [1] 10766.19
```

# 5. Are there differences in activity patterns between weekdays and weekends?


```r
weekDay<-function(dt)
{
        s = weekdays(dt)
        for(i in 1:length(dt))
        {
                if(s[i] == "Saturday" | s[i] == "Sunday")
                        s[i]="weekend"
                else
                        s[i]="weekday"

        }
        s
}
```
adding a new column 'WEKKDAY' in the data indicating weekday or weekend

```r
newdata<- mutate(newdata, WEEKDAY = weekDay(date))
library(lattice)
```
grouping the data by interval and WEEKDAY

```r
newdata<-group_by(newdata, interval, WEEKDAY)
```

adding the mean number of steps grouped by interval and WEEKDAY

```r
newdata<-summarise(newdata, avg = mean(steps))
```
making xyplot, mean number of steps as Y axis, interval as X axis, grouped by WEEKDAY

```r
xyplot(newdata$avg ~ newdata$interval| newdata$WEEKDAY, type = "l", xlab = "5 minute interval",
					ylab = "mean number of steps")
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png) 


