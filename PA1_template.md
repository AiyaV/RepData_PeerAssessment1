
Loading and preprocessing the data

Load the data (i.e. read.csv())

```{r echo=TRUE, cache=TRUE}
if(!file.exists("ActivityMonitoringData"))
  dir.create("ActivityMonitoringData")
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL,  destfile = "./ActivityMonitoringData/activity.zip")
unzip("./ActivityMonitoringData/activity.zip")
fileAMD <- file("./ActivityMonitoringData/activity.csv")
dataAMD <- read.csv("activity.csv")
head(dataAMD)
```

What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

- Calculate the total number of steps taken per day;
- Make a histogram of the total number of steps taken each day;
- Calculate and report the mean and median of the total number of steps taken per day
```{r}
steps_per_day <- aggregate(steps ~ date, dataAMD, sum)
head(steps_per_day)
hist(steps_per_day$steps, main = paste("Total number of steps taken per day"), xlab = paste("Number of steps"))
mean(steps_per_day$steps)
median(steps_per_day$steps)
```

What is the average daily activity pattern?

-Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis);
-Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
avg_daily_activity <- aggregate(steps~interval, dataAMD, mean)
plot(avg_daily_activity$interval, avg_daily_activity$steps, type = "l", main="Average Number of Steps per Day by Interval", xlab="Interval", ylab="Number of Steps")
avg_daily_activity[which.max(avg_daily_activity$steps),]
```

Imputing missing values

-Calculate and report the total number of missing values in the dataset;

```{r}
missing <- is.na(dataAMD$steps)
table(missing)
```

-Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
-Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r cache=TRUE}

library(mice)
md.pattern(dataAMD)
imputed_dataAMD <- mice(dataAMD)
summary(imputed_dataAMD)
imputed_dataAMD2 <- complete(imputed_dataAMD,1)
summary(imputed_dataAMD2)

summary(dataAMD)

head(imputed_dataAMD2)

```

-Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 

```{r}

steps_per_day_narm <- aggregate(steps~date, imputed_dataAMD2, sum)
head(steps_per_day_narm)
hist(steps_per_day_narm$steps, main = ("Total Steps Each Day"), xlab=("Number of Steps"))
mean(steps_per_day_narm$steps)
median(steps_per_day_narm$steps)

```

-What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}

 sum(imputed_dataAMD2$steps)
 sum(dataAMD$steps, na.rm = T)

```

Are there differences in activity patterns between weekdays and weekends?
Use the dataset with the filled-in missing values for this part.

-Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}

imputed_dataAMD2$date <- as.Date(imputed_dataAMD2$date)
weekdays1 <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
imputed_dataAMD2$wDay <- c("weekend", "weekday")[(weekdays(imputed_dataAMD2$date) %in% weekdays1) + 1L]

```

-Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```{r}

library(ggplot2)
averages1 <- aggregate(steps ~ interval + wDay, data=imputed_dataAMD2, mean)

ggplot(averages1, aes(interval, steps)) + geom_line() + facet_grid(wDay ~ .) +
   xlab("Interval") + ylab("Number of steps")

```


