# Code for reading in the dataset and/or processing the data
if (!file.exists("data/activity.zip")) {
  dir.create("data", showWarnings = FALSE)
  datUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  datName <- "data/activity.zip"
  download.file(datUrl, datName)
}

unzip(datName)
stepDat <- read.csv("activity.csv", colClasses = c("integer", "Date", "integer"))

attach(stepDat)
  # the dataset has 3 variables: steps, date, interval

# 2. histogram of total number of steps per day
total_steps <- tapply(steps, date, sum, na.rm = TRUE)
plot(as.Date(row.names(total_steps)), total_steps, type = "h",
     main = "Total Steps Taken Each Day",
     ylab = "Total Steps")

hist(total_steps,breaks = 10,
     main = "Total Steps Taken Each Day",
     xlab = "Daily Total Steps")

# 3. Mean and median number of steps taken each day
mean_steps <- mean(total_steps)
median_steps <- median(total_steps)

# Time series plot of the average number of steps taken
avg_interval_steps <- tapply(steps, interval, mean, na.rm = TRUE)
interval_level <- unique(interval)
plot(interval_level, avg_interval_steps, type = "l",
     main = "Time Series Plot of Average Steps",
     xlab = "Time", ylab = "Average Steps")

# The 5-minute interval that, on average, contains the maximum number of steps
max_interval <- as.integer(names(avg_interval_steps)[which.max(avg_interval_steps)])
max_value <- max(avg_interval_steps)
max_label <- paste0("(",max_interval,",",round(max_value,2),")")
text(max_interval+100, max_value+3, max_label, cex = 0.7)
lines(c(max_interval,max_interval), c(max_value, -10), col = "red")

# Code to describe and show a strategy for imputing missing data
na_value <- as.integer(is.na(steps))
na_value_byDate <- tapply(na_value, date, sum)
table(na_value_byDate) # it can be seen 8 days value missing
na_value_interval <- tapply(na_value, as.factor(interval), sum)
plot(unique(date), na_value_byDate, type = "l")
plot(interval_level, na_value_interval, type = "l")

steps2 <- vector(mode = "numeric", length = length(steps))
for (i in 1:length(steps)) {
  if (is.na(steps[i])) {
    steps2[i] <- avg_interval_steps[as.character(interval[i])]
  } 
  else {
    steps2[i] <- steps[i]
  }
}
  # check there is no missing value in steps2
sum(is.na(steps2))

# Histogram of the total number of steps taken each day after missing values are imputed
total_steps2 <- tapply(steps2, date, sum)
plot(as.Date(unique(date)), total_steps2, type = "h",
     main = "Adjusted Total Steps Each Day",
     ylab = "Total Steps per Day")
hist(ttotal_steps2, breanks = )

# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
wday <- weekdays(date)
stepDat2 <- cbind(stepDat, wday)
steps_weekdays <- stepDat2[!wday %in% c("Saturday", "Sunday"),]
steps_weekends <- stepDat2[wday %in% c("Saturday", "Sunday"),]

avg_steps_weekdays <- tapply(steps_weekdays$steps, steps_weekdays$interval, 
                             mean, na.rm =TRUE)
avg_steps_weekends <- tapply(steps_weekends$steps, steps_weekends$interval,
                             mean, na.rm = TRUE)
plot(interval_level, avg_steps_weekdays, type = "l",
     main = "Weekday Walking Pattern")
plot(interval_level, avg_steps_weekends, type = "l",
     main = "Weekends Walking Pattern")


