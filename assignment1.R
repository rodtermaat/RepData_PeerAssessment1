##install.packages("dplyr", dependencies=TRUE)
library(dplyr)
library(xtable)
library(lattice)

dat <- read.csv("./activity.csv")
dat1 <- filter(dat, steps!='NA')
dat2 <- summarise(group_by(dat1, date), sum(steps))
names(dat2)[names(dat2)=="sum(steps)"] <- "steps"

hist(dat2$'steps',
     col="red",
     xlab="Steps per day",
     main="Steps per day Freq")

dat_mean <- mean(dat2$steps)
dat_med <- median(dat2$steps)
dat_mean <- summarise(group_by(dat1, date), mean(steps))
dat_med <- summarise(group_by(dat1, date), median(steps))

xt_mean <- as.data.frame(dat_mean[1:53, 1:2])
print(xtable(xt_mean), type='html', floating=FALSE, include.rownames=F)


dat_interval <- summarise(group_by(dat1, interval), mean(steps))
plot(dat_interval$interval, dat_interval$'mean(steps)', type="l", xlab="Interval", ylab="Avereage Steps")
title(main = "Average Steps per Interval")

names(dat_interval)[names(dat_interval)=="mean(steps)"] <- "steps"
dat_interval2 <- arrange(dat_interval, desc(steps))
big_interval <- dat_interval2$interval[1]
big_value <- dat_interval2$steps[1]

## finds row with NA
dat_na <- filter(dat, is.na(steps))
names(dat_na)[names(dat_na)=="steps"] <- "nosteps"

## determine the mean by interval
dat_mean_int <- summarise(group_by(dat1, interval), round(mean(steps),0))

## join the mean interval with NA records on 
dat_na_join <- inner_join(dat_na, dat_mean_int, by='interval')
names(dat_na_join)[names(dat_na_join)=="round(mean(steps), 0)"] <- "steps"
dat_na_join2 <- select(dat_na_join, steps, date, interval)
dat_na_final <- rbind(dat_na_join2,dat1)

## summarize by 

dat_no_NA <- summarise(group_by(dat_na_final, date), sum(steps))
names(dat_no_NA)[names(dat_no_NA)=="sum(steps)"] <- "steps"

hist(dat_no_NA$'steps',
     col="red",
     xlab="Steps per day",
     main="Steps per day Freq with NA replacement")

dat_mean <- mean(dat2$steps)
dat_med <- median(dat2$steps)
dat_mean <- summarise(group_by(dat1, date), mean(steps))
dat_med <- summarise(group_by(dat1, date), median(steps))

## weekdays versus weekends
x <- weekdays(as.Date(Sys.Date))
dat_weekday <- mutate(dat_na_final, weekday = weekdays(as.Date(date)))
dat_weekday$weekday[which(dat_weekday$weekday=="Monday")]<-"Weekday"
dat_weekday$weekday[which(dat_weekday$weekday=="Tuesday")]<-"Weekday"
dat_weekday$weekday[which(dat_weekday$weekday=="Wednesday")]<-"Weekday"
dat_weekday$weekday[which(dat_weekday$weekday=="Thursday")]<-"Weekday"
dat_weekday$weekday[which(dat_weekday$weekday=="Friday")]<-"Weekday"
dat_weekday$weekday[which(dat_weekday$weekday=="Saturday")]<-"Weekend"
dat_weekday$weekday[which(dat_weekday$weekday=="Sunday")]<-"Weekend"

## First we group the dat_weekday data frame by weekday and interval with the mean(steps)
## Then we plot the data using some mystical magic.
dat_weekday_summary <- summarise(group_by(dat_weekday, weekday, interval), sum(steps))
names(dat_weekday_summary)[names(dat_weekday_summary)=="sum(steps)"] <- "steps"

xyplot(steps ~ interval| weekday, 
       data = dat_weekday_summary,
       type = "l",
       xlab = "Interval",
       ylab = "Number of steps",
       layout=c(1,2))


