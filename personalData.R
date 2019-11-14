# load tidyverse, gridExtra, and missForest library
library(tidyverse)
library(gridExtra)

# read csv file & convert date string to date
#
PAdata <- read.csv("activity.csv", stringsAsFactors = FALSE)
PAdata$date <- as.Date(PAdata$date, "%Y-%m-%d")
# 
# group by date & summarize
PAdata_bg <- group_by(PAdata, date)
PAdata_summary <- summarize(PAdata_bg, totalSteps=sum(steps, na.rm=TRUE))
#
# plot histogram

p <- ggplot(PAdata_summary, aes(x=date, y=totalSteps))+
    geom_bar(stat="identity", color="dark blue", fill="light blue")
print(p)

# get means and medians
s1 <- summarize(PAdata_bg, mean(steps, na.rm=TRUE), median(steps, na.rm=TRUE))
names(s1) <- c("date", "means", "medians")
# time series plot of averages
p2 <- ggplot(s1, aes(x=date, y=means, na.rm=TRUE )) + 
  geom_bar(stat="identity", color="dark blue", fill="steelblue4")
print(p2)

# the 5-minute interval, on an average, with the maximum number of steps
s2 <- aggregate(steps ~ date, PAdata_bg, max)
s2 <- merge(s2, PAdata_bg)
print(paste("interval with max number of steps on average:", 
            round(mean(s2$interval),0)))

# impute missing data 
# add library(Amelia)
<<<<<<< HEAD
# use bound parameter to limit to positive numbers for steps
library(Amelia)
m <- 5
amelia.out <- amelia(PAdata, m=m, bound=matrix(c(1,0,500), nrow=1), p2s=0)
#
# use mi.meld to combine output from amelia
b.out <- NULL     # coefficients
se.out <- NULL    # standard error dataframe
for (i in 1:m){
  ols.out <- lm(steps ~ interval, data=amelia.out$imputations[[i]])
  b.out <- rbind(b.out, ols.out$coef)
  se.out <- rbind(se.out, coef(summary(ols.out))[,2])
}

PAdata_impute <- mi.meld(q=b.out, se=se.out)

# Histogram of total steps each day after imputing missing data
PAdata_impute_gb <- group_by(PAdata_impute, date)
PAdata_impute_summ <- summarize(PAdata_impute_gb, totalSteps=sum(steps))
=======
totalNA <- sum(complete.cases(PAdata))
print(paste("Number of rows with NA values:", totalNA))

# set average number of steps by interval where value was NA
averageSteps <- aggregate(steps ~ interval, PAdata, FUN=mean)
newSet <- numeric()
for (i in 1:nrow(PAdata)) {
  if(is.na(PAdata[i,]$steps)) {
    steps <- subset(averageSteps, interval == PAdata[i,]$interval)$steps
  }
  else
  {
    steps <- PAdata[i,]$steps
  }
  newSet <- c(newSet, steps)
}
# create new dataframe with NA values substituted
PAdata_new <- PAdata
PAdata_new$steps <- newSet
>>>>>>> 7ad60dbc7950baad139b1046e5b29b646c3d83c5

# Histogram of total steps each day after imputing missing data
PAdata_new_summ <- PAdata_new %>% group_by(date) %>% summarize(sum=sum(steps))
PAdata_newplot <- ggplot(PAdata_new_summ, aes(x=date, y=sum)) + 
  geom_bar(stat="identity",color="dark blue", fill="steelblue") + 
  ylab("totalSteps by day - no NAs")
print(PAdata_newplot)
#
# Panel plot comparing average number of steps by interval over weekdays vs weekends
# x axis: intervals
# y axis: average number of steps

PAdata_int <- PAdata %>% mutate(Weekday=ifelse(weekdays(date)=="Saturday" |
                                                 weekdays(date)=="Sunday",0,1))
PAdata_int <- PAdata_int %>% group_by(interval, Weekday)
PAdata_int_summary <- PAdata_int %>% summarize(AvgSteps=mean(steps, na.rm=TRUE))
wkdayPlot <- ggplot(filter(PAdata_int_summary,Weekday==1),aes(x=interval, y=AvgSteps))+
  geom_line(col="dark blue") + ylab("Avg Steps on Weekdays")
wkendPlot <- ggplot(filter(PAdata_int_summary,Weekday==0),aes(x=interval, y=AvgSteps))+
  geom_line(col="dark red") + ylab("Avg Steps on Weekends")

# load library gridExtra
library(gridExtra)
grid.arrange(wkdayPlot, wkendPlot, nrow=2)
# All of R code in the report