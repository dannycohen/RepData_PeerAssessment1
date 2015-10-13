## Clear environment
## rm(list = ls())

## Loading and preprocessing the data

activityData_all <- read.csv("activity.csv")
head(activityData,100)

# remove NAs
activityData <- activityData_all[complete.cases(activityData_all),]

# fix dates

fixdates <- function(activityData){
    activityData$date <- as.Date(activityData$date, "%Y-%m-%d")
    activityData$interval <- sprintf("%04d",activityData$interval)
    activityData$interval_time <- paste(substr(activityData$interval, 0, 2), ":",substr(activityData$interval, 3, 4), sep="")
    activityData$date_time <- strptime(paste(activityData$date, activityData$interval_time), "%Y-%m-%d %H:%M")
    return(activityData)
}

activityData <- fixdates(activityData)


## What is mean total number of steps taken per day?

totalStepsPerDay <- aggregate(steps ~ date , activityData, FUN = sum) 


par(mfrow=c(1,1), mar=c(4,4,4,1))

histstepsperday <- function(totalStepsPerDay, main){
    hist(totalStepsPerDay$steps
         , main = main 
         , xlab = "Total number of daily steps "
         , ylab = "Frequency"
         , labels = TRUE
         , col="gray"
         , breaks = 10
    )
}


histstepsperday(totalStepsPerDay, "Histogram of total steps per day")


# mean and median calculation

median(totalStepsPerDay$steps)

mean(totalStepsPerDay$steps)


## What is the average daily activity pattern?

interval_mean <- aggregate(steps ~ interval , activityData, FUN = mean) 

names(interval_mean)[2] <- "steps_mean"

plotstepsinterval <- function(data, main){
    plot(data$interval, data$steps_mean
         , type = "l"
         , main = main # "Steps per 5 min. interval\n(averaged across all days)"
         , xlab = "Interval (time of day)"
         , ylab = "Number of steps"
    )
    
}

plotstepsinterval(interval_mean, "Steps per 5 min. interval\n(averaged across all days)")


#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

intervals_sorted <- interval_mean[order(-interval_mean$steps_mean),] 

head(intervals_sorted)


## Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

nrow(activityData_all) - nrow(activityData)


#### 2. Devise a strategy for filling in all of the missing values in the dataset. 
####    The strategy does not need to be sophisticated. 
####    For example, you could use the mean/median for that day, 
####    or the mean for that 5-minute interval, etc.


#### 3. Create a new dataset that is equal to the original dataset 
####    but with the missing data filled in.

activityData_all <- fixdates(activityData_all)

activityData_all <- merge(activityData_all,interval_mean,by="interval")

activityData_all[is.na(activityData_all$steps),2] <- as.integer(activityData_all[is.na(activityData_all$steps),6])


#### 4. Make a histogram of the total number of steps taken each day 
####    and Calculate and report the mean and 
####    median total number of steps taken per day. 
####    Do these values differ from the estimates from the first part of the assignment?
####    What is the impact of imputing missing data on the estimates of the total daily number of steps?


totalStepsPerDay_all <- aggregate(steps ~ date , activityData_all, FUN = sum) 

histstepsperday(totalStepsPerDay_all, "Histogram of total steps per day\n(with imputed missing values)")

par(mfrow=c(1,2))
histstepsperday(totalStepsPerDay_all, "Histogram of total steps per day\n(with imputed missing values)")
histstepsperday(totalStepsPerDay, "Histogram of total steps per day")


#### Mean and Median

median(totalStepsPerDay$steps)
median(totalStepsPerDay_all$steps)

mean(totalStepsPerDay$steps)
mean(totalStepsPerDay_all$steps)




## Are there differences in activity patterns between weekdays and weekends?

weekend <- c("Saturday", "Sunday")

new_activitydata_all <- activityData_all
new_activitydata_all$is_weekend <- weekdays(activityData_all$date) %in% weekend
new_activitydata_all$weekday <- weekdays(activityData_all$date)

#### Weekend

weekend_activity <- new_activitydata_all[new_activitydata_all$is_weekend == TRUE,]

weekend_activity_mean <- aggregate(steps ~ interval , weekend_activity, FUN = mean) 
names(weekend_activity_mean)[2] <- "steps_mean"
weekend_activity_mean$steps_mean <- as.integer(weekend_activity_mean$steps_mean)
    
par(mfrow=c(2,1))
plotstepsinterval(weekend_activity_mean
                  , "Weekend\nsteps per 5 min. interval (averaged across all days)"
                  )

#### Weekday

weekday_activity <- new_activitydata_all[new_activitydata_all$is_weekend == FALSE,]

weekday_activity_mean <- aggregate(steps ~ interval , weekday_activity, FUN = mean) 
names(weekday_activity_mean)[2] <- "steps_mean"
weekday_activity_mean$steps_mean <- as.integer(weekday_activity_mean$steps_mean)


plotstepsinterval(weekday_activity_mean
                  , "Weekday\nsteps per 5 min. interval (averaged across all days)"
)
