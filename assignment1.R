install.packages("dplyr", dependencies=TRUE)
library(dplyr)

dat <- read.csv("./activity.csv")
dat1 <- filter(dat, steps!='NA')
dat2 <- summarise(group_by(dat1, date), sum(steps))

hist(dat2$'sum(steps)',
     col="red",
     xlab="Steps per day",
     main="Steps per day Freq")

dat_mean <- summarise(group_by(dat1, date), mean(steps))
dat_med <- summarise(group_by(dat1, date), median(steps))

dat_interval <- summarise(group_by(dat1, interval), mean(steps))
plot(dat_interval$interval, dat_interval$'mean(steps)', type="l", xlab="Interval", ylab="Avereage Steps")
title(main = "Average Steps per Interval")

names(dat_interval)[names(dat_interval)=="mean(steps)"] <- "steps"
dat_interval2 <- arrange(dat_interval, desc(steps))
big_interval <- dat_interval2$interval[1]
big_value <- dat_interval2$steps[1]

dat_na <- filter(dat, steps=='NA')

