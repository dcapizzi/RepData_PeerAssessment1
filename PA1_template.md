### Reproducible Research: Peer Assessment 1

## Assignment Overview
This assignment analyzes mean daily patterns using data from a personal activity monitoring device. The device collects data at 5- minute intervals throughout the day. The dataset consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and includes the number of steps taken in 5 minute intervals each day.

Data was downloaded from the following address:
https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

The variables included in this dataset are:  
- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)  
- date: The date on which the measurement was taken in YYYY-MM-DD format  
- interval: Identifier for the 5-minute interval in which measurement was taken  

There are a total of 17,568 observations in this dataset.

#### Load packages
To begin, we load the primary packages to be used during the session, ggplot2 and plyr.


```r
library(ggplot2)
library(plyr) 
```

#### Step 1: Load the data

Our first step is to unzip and load the data into R. We transform the data variable into a POSIXlt date to facilitate the analysis.


```r
unzip("activity.zip")  # unzip the data file
data <- read.csv("activity.csv", stringsAsFactors=FALSE) # read the data into R
str(data) # print summary of the data
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

#### Step 2: Histogram of the total number of steps taken each day

First, we develop a histogram of total steps taken per day. To do this, we first use the ddply function to calculate the total steps taken by day (excluding NAs). We then plot the histogram using this data.


```r
sums <- ddply(data,  # create new data table, sums, with total steps per day
      .(date),  # aggregate by date
      summarise,  # summarize the data
      sum_steps = sum(steps, na.rm = TRUE)) # sum steps to the aggration of date

hist <- ggplot(sums, aes(x=sum_steps)) +  # plot sum_steps
      geom_histogram(binwidth = 1000, # add histogram with bins of 1000
                     aes(fill = ..count..)) + # color by count
      labs(x = "Steps Taken (Bins 1000)",  # add labels
            y = "Count of Steps", 
            title = "Histogram of Total Steps Taken")

print(hist)
```

![plot of chunk hist_stepstaken](figure/hist_stepstaken-1.png)

Interestingly, over 10 days have values from zero to 1,000 (the first bin.) We investigate these further, to find 8 days with zero steps per day.


```r
sums[sums$sum_steps==0,]
```

```
##          date sum_steps
## 1  2012-10-01         0
## 8  2012-10-08         0
## 32 2012-11-01         0
## 35 2012-11-04         0
## 40 2012-11-09         0
## 41 2012-11-10         0
## 45 2012-11-14         0
## 61 2012-11-30         0
```

#### Step 3: Mean and median number of steps taken each day

We then calculate the mean and median steps taken per day, store, and print the results. We use the sum_steps variable calculated previously.


```r
step_mean <- mean(sums$sum_steps) # take mean total steps per day
step_median <- median(sums$sum_steps) # take median of total steps per day

step_mean
```

```
## [1] 9354.23
```

```r
step_median
```

```
## [1] 10395
```

#### Step 4: Time series plot of the average number of steps taken

We now seek to identify the daily pattern (the average number of steps for each 5-minute period during the day.) 

*4.1 Time Series Plot*

We again use ddply to summarize the data set, this time to calculate the average number of steps for each interval independent of the day. We plot the data in a line graph to better understand the distribution.


```r
avgs <- ddply(data,  # use data
      .(interval),  # aggregate by interval (across days)
      summarise,  # summarize table
      avg_steps = mean(steps, na.rm = TRUE)) # calculate average steps

line <- ggplot(avgs, aes(x = interval, y = avg_steps)) +
      geom_line(colour="blue") +
      labs(x = "5-Minute Interval", 
            y = "Average Steps per Interval (across 61 days)", 
            title = "Average Steps Per Interval (exclude NAs)")

print(line)
```

![plot of chunk time_series](figure/time_series-1.png)

#### Step 5: The 5-minute interval that, on average, contains the maximum number of steps
Based on the histogram, it appears that peak movement occurs between the 500th and 1000th interval. We now calculate the interval with the largest average steps, which is the 835th interval, in which approximately 206 steps are taken on average across our days.


```r
max_steps <- max(avgs$avg_steps) # calculate highest average number of steps
max_interval <- avgs$interval[avgs$avg_steps == max_steps] # identify related interval

print(max_steps)
```

```
## [1] 206.1698
```

```r
print(max_interval)
```

```
## [1] 835
```

This implies that the most active time (on average) across the 61-day sample set was from 1:55 pm to 2:00pm.


```r
max_interval/60  # max steps divided by 60 minutes in an hour
```

```
## [1] 13.91667
```

```r
max_interval - (13*60) # remaining minutes 
```

```
## [1] 55
```


#### Step 6: Code to describe and show a strategy for imputing missing data

*3.1 Missing Values*  
There are approximately 2,304 NA values in the data set.


```r
sum(is.na(data$steps)) # add up the times when data$steps is NA
```

```
## [1] 2304
```

*3.2 Devise a Strategy to Address NAs*  
These NAs are concentrated into the 8 specific days which we earlier discovered had zero steps. For these days, all values are NAs and all NAs fall within these days. As such, I will replace the NA values with the average for that specific interval for the other days in the sample set.  

To demonstrate this phenomenon, I first create a table of NAs by day and plot it:

```r
countNAs <- ddply(data,  # count NAs in data 
      .(date), # aggregated by date
      summarise, 
      avg_steps = mean(steps, na.rm = TRUE), # count average steps
      NAcount = sum(is.na(steps))) # count NAs for that date



NAplot <-ggplot(countNAs, # plot NA count
            aes(x = strptime(date,"%Y-%m-%d"),  # x is date (stripped of time)
            y = NAcount)) + # y is NA count
            geom_line(colour="darkgreen") # color dark green

print(NAplot)
```

![plot of chunk NA_strategy](figure/NA_strategy-1.png)

It is clear that the graph "spikes" on 8 particular days and there are no NAs at any other time. The days with NAs are summarized below:

```r
NAdays <- countNAs[rowSums(is.na(countNAs)) > 0,]
NAdays
```

```
##          date avg_steps NAcount
## 1  2012-10-01       NaN     288
## 8  2012-10-08       NaN     288
## 32 2012-11-01       NaN     288
## 35 2012-11-04       NaN     288
## 40 2012-11-09       NaN     288
## 41 2012-11-10       NaN     288
## 45 2012-11-14       NaN     288
## 61 2012-11-30       NaN     288
```
      
Given that the NAs are highly concentrated by day, it seems logical to apply the average steps at that time interval on other days.

*3.3 Create a New Data Set* 

To create a new data set, I merge the existing averages by interval into my original data set to create a new data set. I create a new variable, newsteps, which is either (1) original value if available or (2) the interval average if it is NA. 


```r
new_data <- merge(data, avgs, by="interval") # merge existing averages into a new data set by interval
new_data <- new_data[order(new_data$date,  # reorder by date
                         new_data$interval), # and interval
                   c(3,1,2,4)] # rearrange columns to date, interval, steps, newsteps

new_data$new_steps <- ifelse(is.na(new_data$steps), # if steps is NA
                   new_data$avg_steps, # replace with avg steps per interval
                   new_data$steps) # else it remains steps

new_data[sample(nrow(new_data), 20), ] # display random data to applied criteria
```

```
##             date interval steps  avg_steps new_steps
## 14300 2012-10-10     1930     0  27.396226   0.00000
## 11409 2012-10-27     1535     0  65.320755   0.00000
## 4468  2012-10-08      605    NA  49.264151  49.26415
## 3087  2012-10-13      410     0   2.566038   0.00000
## 14733 2012-11-04     2005    NA  19.018868  19.01887
## 17012 2012-10-10     2310     0   0.000000   0.00000
## 10510 2012-11-01     1420    NA  35.471698  35.47170
## 10474 2012-10-31     1415     0  48.698113   0.00000
## 12614 2012-10-29     1710     0  50.716981   0.00000
## 5649  2012-11-29      740    24  52.264151  24.00000
## 1094  2012-10-30      125     0   1.113208   0.00000
## 7576  2012-11-02     1020     0  38.924528   0.00000
## 9757  2012-10-17     1315     8  40.981132   8.00000
## 5025  2012-10-03      650   533  37.358491 533.00000
## 554   2012-11-25       45     0   1.471698   0.00000
## 7021  2012-11-22      935     0  45.226415   0.00000
## 6129  2012-10-24      820     0 171.150943   0.00000
## 4565  2012-10-01      610    NA  53.773585  53.77358
## 6955  2012-10-13      930   488  66.207547 488.00000
## 31    2012-11-03        0     0   1.716981   0.00000
```

#### Step 7: Histogram of the total number of steps taken each day after missing values are imputed

I now plot the new data set with the same parameters as the original. It is clear that the NAs have been addressed and the distribution is more appropriate, with most days (18) falling between 10,000 and 11,000 days. (Note 8 of these are formerly NAs) 


```r
new_sums <- ddply(new_data, # aggregate the new data set
      .(date),  # by day
      summarise,  
      new_sum_steps = sum(new_steps, na.rm = TRUE)) # add to total steps per day

new_hist <- ggplot(new_sums, aes(x=new_sum_steps)) +  # graph new data
      geom_histogram(binwidth = 1000, # bins of 1,000
                     aes(fill = ..count..)) + # fill with count
      labs(x = "Steps Taken (Bins 1000)",  # add labels
            y = "Count of Steps", 
            title = "Histogram of Total Steps Taken (NAs Replaced)")

print(new_hist)
```

![plot of chunk new_hist](figure/new_hist-1.png)

#### Step 8: Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

We now want to identify weekdays vs. weekends and identify and differences. 

*4.1 Create a Factor Variable for Weekday/Weekend* 

So, I use the date varaible to first determine the weekday (with the weekdays function), and create a factor variable for weekday vs. weekend. I add the factor variable to my new data frame.


```r
new_data$weekday <- weekdays(strptime(new_data$date,"%Y-%m-%d")) # add weekday (day of week) to data

factor <- ifelse(new_data$weekday==c("Saturday", # if weekday is Sat / Sun...
                                     "Sunday"),
                 "weekend", # name factor weekend
                 "weekday") # else name it weekday

factor <- as.factor(factor) # convert factor to a factor-type variable
new_data <- cbind(new_data,factor) # bind factor to my data frame

new_data[sample(nrow(new_data), 20), ] # print out samples
```

```
##             date interval steps   avg_steps  new_steps   weekday  factor
## 965   2012-11-07      115     0   0.3396226  0.0000000 Wednesday weekday
## 16096 2012-11-16     2155     0   2.6226415  0.0000000    Friday weekday
## 389   2012-10-31       30     0   0.5283019  0.0000000 Wednesday weekday
## 9368  2012-11-29     1245     0  37.7358491  0.0000000  Thursday weekday
## 2945  2012-10-19      400     0   1.1886792  0.0000000    Friday weekday
## 5984  2012-11-03      810    31 129.4339623 31.0000000  Saturday weekend
## 11584 2012-10-01     1545    NA  98.6603774 98.6603774    Monday weekday
## 7897  2012-10-13     1045    22  28.3396226 22.0000000  Saturday weekday
## 2710  2012-11-07      340     0   0.4905660  0.0000000 Wednesday weekday
## 8841  2012-11-23     1200     0  63.8679245  0.0000000    Friday weekday
## 3820  2012-10-29      510     0   3.0000000  0.0000000    Monday weekday
## 2034  2012-11-06      245     0   0.0000000  0.0000000   Tuesday weekday
## 16081 2012-11-12     2155    11   2.6226415 11.0000000    Monday weekday
## 7780  2012-10-03     1035     0  37.4150943  0.0000000 Wednesday weekday
## 4724  2012-11-15      625     0  47.0754717  0.0000000  Thursday weekday
## 7171  2012-11-16      945     9  38.7547170  9.0000000    Friday weekday
## 2452  2012-11-10      320    NA   0.2075472  0.2075472  Saturday weekend
## 15440 2012-10-24     2105    42  17.2264151 42.0000000 Wednesday weekday
## 14540 2012-11-22     1950     0  45.6603774  0.0000000  Thursday weekday
## 15141 2012-11-29     2040    17  19.5471698 17.0000000  Thursday weekday
```

*4.2 Plot the data using a panel plot* 

Finally, we plot the new data into a data frame. First, we have to aggregate again by interval across the different days. We then plot the new data in a panel plot.


```r
new_avgs <- ddply(new_data,  # use data
      .(interval, factor),  # aggregate by interval (across days) and factor
      summarise,  # summarize table
      avg_weekday_steps = mean(new_steps, na.rm = TRUE)) # calculate average weekday steps

line2 <- ggplot(new_avgs, # plot new average with weekday factor
                aes(x = interval,  # x is interval
                    y = avg_weekday_steps,  # y is average steps
                    colour = factor)) + # color is the factor variable
      geom_line() + # plot line chart
      facet_grid(factor ~ .) +  # facet with factor
      labs(x = "5-Minute Interval",  # add labels
            y = "Average Steps per Interval (across 61 days)", 
            title = "Average Steps Per Interval (replace NAs)")

print(line2)
```

![plot of chunk new_plot](figure/new_plot-1.png)

It appears that there are significant differences on the weekend, as people tend to get up earlier on the weekday (walk less in the early morning) and walk much more during the day.
