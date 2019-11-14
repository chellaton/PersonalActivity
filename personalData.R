# load tidyverse library
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
print(mean(s2$interval))

# impute missing data 
# add library(Amelia)
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

p_impute <- ggplot(PAdata_impute_summ, aes(x=date, y=totalSteps)) +
  geom_bar(stat="identity", color="dark green", fill="green3")
print(p_impute)
#
# Panel plot comparing average number of steps by interval over weekdays vs weekends
# x axis: intervals
# y axis: average number of steps
#
PAdata_int <- PAdata %>% mutate(Weekday=ifelse(weekdays(date)=="Saturday" |
                                                 weekdays(date)=="Sunday",0,1))
PAdata_int <- PAdata_int %>% group_by(interval, Weekday)
PAdata_int_summary <- PAdata_int %>% summarize(AvgSteps=mean(steps, na.rm=TRUE))
wkdayPlot <- ggplot(filter(PAdata_int_summary,Weekday==1),aes(x=interval, y=AvgSteps))+
  geom_line(col="dark blue") + ylab("Avg Steps on Weekdays")+geom_smooth(method="auto")
wkendPlot <- ggplot(filter(PAdata_int_summary,Weekday==0),aes(x=interval, y=AvgSteps))+
  geom_line(col="dark red") + ylab("Avg Steps on Weekends")+geom_smooth(method="auto")

# load library gridExtra
library(gridExtra)
grid.arrange(wkdayPlot, wkendPlot, nrow=2)
# All of R code in the report