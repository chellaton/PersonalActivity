# load tidyverse library
library(tidyverse)
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
p2 <- ggplot(s1, aes(x=date, y=means, na.rm=TRUE )) + geom_bar(stat="identity", color="dark blue", fill="steelblue4")
print(p2)