################################################################

#bring in harmonia at KBS data

harmonia<-read.csv(file="casestudydata/kbs_harmonia94-17.csv", header=T)

#this is raw trap data, and will require manipulation

#first, handle the dates, which are horrible in the original data
library(lubridate)

harmonia$newdate<-mdy(harmonia$DATE)
#extract year
harmonia$year<-year(harmonia$newdate)
#extract day of year
harmonia$doy<-yday(harmonia$newdate)

#because sampling periods varied year to year, sometimes with long tails, let's cull the data at Aug 10

harmonia<-harmonia[which(harmonia$doy<222),]

#also get rid of nulls
harmonia<-harmonia[complete.cases(harmonia),]

#now, reshape and summarize. We want to get average counts per trap, by year- let's use plyr

library(plyr)

harmonia.year<-ddply(harmonia, "year", summarize,
                     avg=sum(ADULTS)/length(ADULTS))

#now, we run it through the RS detector
source("regime_shift_detector.R")

RSdetector(harmonia.year)

