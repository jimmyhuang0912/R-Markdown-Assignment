Code for reading in the dataset and/or processing the data
----------------------------------------------------------

1.  Load the data
2.  Process/transform the data (if necessary) into a format suitable for
    your analysis

<!-- -->

    data <- read.csv(file="activity.csv", header=TRUE)

Mean Total Number of Steps Taken per Day
----------------------------------------

1.  Calculate the total number of steps taken per day
2.  If you do not understand the difference between a histogram and a
    barplot, research the difference between them. Make a histogram of
    the total number of steps taken each day

<!-- -->

    install.packages("dplyr",repos="http://cran.rstudio.com/")

    ## 
    ##   There is a binary version available but the source version is
    ##   later:
    ##       binary source needs_compilation
    ## dplyr  0.7.5  0.7.6              TRUE

    library("dplyr")
    date <- group_by(data, date)
    data1 <- summarize(date, totalsteps = sum(steps,na.rm=TRUE))
    hist(data1$totalsteps,main="Total Number of Steps Taken Each Day",xlab="Total Steps per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

1.  Calculate and report the mean and median of the total number of
    steps taken per day

<!-- -->

    meantotal <- mean(data1$totalsteps)
    mediantotal <- median(data1$totalsteps)

-   Mean Number of Steps Taken per Day = 9354.2295082
-   Median Number of Steps Taken per Day = 10395

Averag Daily Activity Pattern
-----------------------------

1.  Make a time series plot (i.e. type="l") of the 5-minute interval
    (x-axis) and the average number of steps taken, averaged across all
    days (y-axis)
2.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    int <- group_by(data, interval)
    data2 <-summarize(int, meansteps = mean(steps,na.rm=TRUE))
    plot(data2$interval, data2$meansteps, type="l", 
         main="Daily Activity Pattern", 
         xlab="5 Min Internval", ylab=" Average Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    data2[which.max(data2$meansteps),]

    ## # A tibble: 1 x 2
    ##   interval meansteps
    ##      <int>     <dbl>
    ## 1      835      206.

Missing Value
-------------

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs)
2.  Devise a strategy for filling in all of the missing values in the
    dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.
3.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.
4.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

<!-- -->

    numNA <- sum(is.na(data$steps))
    numNA

    ## [1] 2304

    data3 <- data %>% group_by(interval) %>% 
      mutate(meansteps=mean(steps,na.rm=TRUE))
    for (i in 1:17568){
      if(is.na(data3$steps[i])) {
      data3$steps[i] = data3$meansteps[i]  
      }
    }
    data3 <- ungroup(data3)
    data3 <- select(data3,-(meansteps))

    date <- group_by(data3, date)
    data4 <- summarize(date, totalsteps = sum(steps,na.rm=TRUE))
    hist(data4$totalsteps, 
         main="Total Number of Steps Taken Each Day", 
         xlab="Total Steps per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    meantotal2 <- mean(data4$totalsteps)
    mediantotal2 <- median(data4$totalsteps)

-   Mean Number of Steps Taken per Day = 1.076618910^{4}
-   Median Number of Steps Taken per Day = 1.076618910^{4}

Weekday and Weekend
-------------------

1.  Create a new factor variable in the dataset with two levels -
    "weekday" and "weekend" indicating whether a given date is a weekday
    or weekend day.
2.  Make a panel plot containing a time series plot (i.e.type="l") of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated data.

<!-- -->

    data3$date <- as.Date(data3$date)
    dataA <- mutate(data3,day=weekdays(date))
    for (i in 1:17568){
      if(dataA$day[i] %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')) {
        dataA$day[i] = "Weekday"  
      }
      else if(dataA$day[i] %in% c('Saturday', 'Sunday'))
        dataA$day[i] = "weekend"
    }
    dataB <- aggregate(steps ~ interval + day, dataA,mean)
    install.packages("ggplot2",repos="http://cran.rstudio.com/")

    ## package 'ggplot2' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\jimmy\AppData\Local\Temp\RtmpsJn6bK\downloaded_packages

    library("ggplot2")
    ggplot(data = dataB, aes(x = interval, y = steps)) + 
      geom_line() +
      facet_grid(day ~ .) +
      ggtitle("Average Daily Activity Pattern") +
      xlab("5 Min Interval") +
      ylab("Average Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)
