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
hist(PAdata_summary$date)