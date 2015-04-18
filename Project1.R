# Qn1: Getting and loading data
setwd("/Users/yingjiang/Dropbox/Education/Coursera/Data_science_spec/Data_science_C5/Projects/RepData_PeerAssessment1")
unzip("activity.zip")
activity <- read.csv("activity.csv")

summary(activity)
colnames(activity)
class(activity$steps) # integer
class(activity$date) # factor
sum(activity$steps, na.rm = T) # 570608

# Qn2: mean total number of steps taken per day
# Remove cases (observations) with missing values
activity_compl <- activity[complete.cases(activity), ]
activity_compl$date <- as.factor(as.character(activity_compl$date))
# Calculate total number of steps taken each day, and store as a vector element.
dailysums <- as.numeric(by(activity_compl$steps, activity_compl$date, sum))
# Create vector of dates (for the 2 months of the study) and stitch to the daily total steps data.
# Transform into plotting-ready data
# Make plot
dailytotalsteps <- as.data.frame(cbind(levels(activity_compl$date), dailysums), stringsAsFactors = F)
colnames(dailytotalsteps) <- c("Date", "Total.steps.taken")
dailytotalsteps$Date <- as.Date(dailytotalsteps$Date)
dailytotalsteps$Total.steps.taken <- as.numeric(dailytotalsteps$Total.steps.taken)
hist(dailytotalsteps$Total.steps.taken,
     breaks = 25,
     main = "Histogram of daily total steps taken",
     xlab = "Total steps taken daily",
     ylab = "Frequency")

# Calculate mean and median of daily total steps
mean(dailysums)
median(dailysums)

# Qn3: Average daily activity pattern
# Calculate mean activity values at each time interval.
# as.factor(activity$interval)
# levels(as.factor(activity$interval))
intervalmeans <- as.numeric(by(activity$steps, as.factor(activity$interval), mean, na.rm = TRUE))
# Stich mean activity values to the interval vector.
dailyactivitymeans <- as.data.frame(cbind(as.numeric(levels(as.factor(activity$interval))), intervalmeans), stringsAsFactors = F)
colnames(dailyactivitymeans) <- c("Time.interval", "Mean.steps.taken")
plot(x = dailyactivitymeans$Time.interval,
     y = dailyactivitymeans$Mean.steps.taken,
     main = "Average activity through a day",
     xlab = "Time interval through a day (min)",
     ylab = "Average number of steps taken over 2 months",
     type = "l")

# Qn4: Imputing missing values
# Calculate total number of missing values
sum(is.na(activity$steps)) # 2304
# Fill in missing values for the dataset.
# Strategy: for each missing value at a particular time interval, fill in the 2-month average value for that interval.
# Create new dataset (activity_NAimputed) with missing values filled in.
activity_NAimputed <- activity
for(i in 1:nrow(activity)) {
    if(is.na(activity_NAimputed$steps[i])) {
        activity_NAimputed$steps[i] <- dailyactivitymeans$Mean.steps.taken[dailyactivitymeans$Time.interval == activity_NAimputed$interval[i]]
    }
}
sum(is.na(activity_NAimputed$steps)) # 0
# Make new histogram for daily sums; calculate new daily means and medians.
dailysums_NAimputed <- as.numeric(by(activity_NAimputed$steps, activity_NAimputed$date, sum))
dailytotalsteps_NAimputed <- as.data.frame(cbind(levels(activity_NAimputed$date), dailysums_NAimputed), stringsAsFactors = F)
colnames(dailytotalsteps_NAimputed) <- c("Date", "Total.steps.taken")
dailytotalsteps_NAimputed$Date <- as.Date(dailytotalsteps_NAimputed$Date)
dailytotalsteps_NAimputed$Total.steps.taken <- as.numeric(dailytotalsteps_NAimputed$Total.steps.taken)
hist(dailytotalsteps_NAimputed$Total.steps.taken,
     breaks = 25,
     main = "Histogram of daily total steps taken (NAs approximated by the mean at any given interval)",
     xlab = "Total steps taken daily",
     ylab = "Frequency")

# Calculate mean and median of daily total steps
mean(dailysums_NAimputed)
median(dailysums_NAimputed)

# Qn5: 
# Created a new factor variable in the dataset, "day", with 2 levels "weekday" and "weekend".
activity_wk <- cbind(activity_NAimputed, factor(length(nrow(activity_NAimputed)), levels = c("weekday", "weekend")))
colnames(activity_wk)[4] <- "day"
activity_wk$date <- as.Date(activity_wk$date)
ifweekday <- weekdays(activity_wk$date)
for(i in 1:length(ifweekday)) {
    if(ifweekday[i] == "Saturday" | ifweekday[i] == "Sunday") {
        activity_wk$day[i] <- "weekend"
    } else {
        activity_wk$day[i] <- "weekday"
    }
}

# Average activity across all weekdays or all weekends.
activity_split <- split(activity_wk, activity_wk$day)
intervalmeans_wkday <- as.numeric(by(activity_split[[1]]$steps, as.factor(activity_split[[1]]$interval), mean, na.rm = TRUE))
intervalmeans_wkend <- as.numeric(by(activity_split[[2]]$steps, as.factor(activity_split[[2]]$interval), mean, na.rm = TRUE))

dailyactivitymeans_wkday <- as.data.frame(cbind(as.numeric(levels(as.factor(activity$interval))), intervalmeans_wkday, rep("Weekday", length(intervalmeans_wkday))), stringsAsFactors = F)
dailyactivitymeans_wkend <- as.data.frame(cbind(as.numeric(levels(as.factor(activity$interval))), intervalmeans_wkend, rep("Weekend", length(intervalmeans_wkend))), stringsAsFactors = F)
colnames(dailyactivitymeans_wkday) <- c("Time.interval", "Mean.steps.taken", "Day")
colnames(dailyactivitymeans_wkend) <- c("Time.interval", "Mean.steps.taken", "Day")
dailyactivitymeans_wk <- as.data.frame(rbind(dailyactivitymeans_wkday, dailyactivitymeans_wkend))

###################
# Using base plot #
###################
par(mfrow = c(2, 1))
plot(x = dailyactivitymeans_wkday$Time.interval,
     y = dailyactivitymeans_wkday$Mean.steps.taken,
     main = "Average activity through a day",
     xlab = "",
     ylab = "",
     type = "l",
     col = "red")
legend("topright",
       lty = 1,
       col = c("red", "blue"),
       legend = c("Weekdays", "Weekends"))
plot(x = dailyactivitymeans_wkend$Time.interval,
     y = dailyactivitymeans_wkend$Mean.steps.taken,
     xlab = "Time interval through a day (min)",
     ylab = "Average number of steps taken over 2 months",
     type = "l",
     col = "blue")
     
#################
# Using ggplot2 #
#################
# intervalmeans_wk <- as.numeric(by(activity_wk$steps, as.factor(activity_wk$interval), mean, na.rm = TRUE))
# wkmeans <- as.data.frame(cbind(as.numeric(levels(as.factor(activity_NAimputed$interval))), intervalmeans_wk), stringsAsFactors = F)
# colnames(dailyactivitymeans) <- c("Time.interval", "Mean.steps.taken")
# 
# Make a 2-panel plot of the average values
dailyactivitymeans_wk$Time.interval <- as.numeric(dailyactivitymeans_wk$Time.interval)
dailyactivitymeans_wk$Mean.steps.taken <- as.numeric(dailyactivitymeans_wk$Mean.steps.taken)
dailyactivitymeans_wk$Day <- as.factor(dailyactivitymeans_wk$Day)

library(ggplot2)

g <- ggplot(dailyactivitymeans_wk, aes(x = Time.interval,
                                       y = Mean.steps.taken,
                                       colour = Day)) +
     geom_line(aes(colour = Day, group = Day)) + 
     facet_wrap(~Day, nrow = 2, ncol = 1) +
     labs(x = "Time interval through a day (min)",
          y = "Average number of steps taken over 2 months")

print(g)