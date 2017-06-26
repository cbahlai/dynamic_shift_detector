################################################################

#bring in harmonia at KBS data

harmonia<-read.csv(file="casestudydata/kbs_harmonia94-15.csv", header=T)

#this is raw trap data, and will require manipulation

#first, handle the dates, which are horrible in the original data
library(lubridate)

harmonia$newdate<-mdy(harmonia$DATE)
#extract year
harmonia$year<-year(harmonia$newdate)
#extract day of year
harmonia$doy<-yday(harmonia$newdate)

#because sampling periods varied year to year, sometimes with long tails, so let's cull the data at the end of July

harmonia<-harmonia[which(harmonia$doy<240),]

#also get rid of nulls
harmonia<-harmonia[complete.cases(harmonia),]

#now, reshape and summarize. We want to get average counts per trap, by year- let's use plyr

library(plyr)

harmonia.year<-ddply(harmonia, "year", summarize,
                     avg=sum(ADULTS)/length(ADULTS))

#now, we run it through the RS detector
source("regime_shift_detector.R")

RSdetector(harmonia.year)

#so this is really interesting. The year 2015 is going back to the outbreak dynamic,
# making the RS detector no longer flag the post-2006 shift. Let's look at the model outputs
# on the original data that only went to 2013

harmonia.2013<-harmonia.year[which(harmonia.year$year<2014),]
RSdetector(harmonia.2013)

#estimate year to year sampling error to compare to simulation results
harmonia.year.for.error<-ddply(harmonia, "year", summarize,
                     sum=sum(ADULTS), avg=mean(ADULTS), sd=sd(ADULTS), n=length(ADULTS), sem=sd/sqrt(n), perc.err=100*sem/avg )
mean(harmonia.year.for.error$perc.err)
