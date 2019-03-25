################################################################

#bring in sprat data

sprat<-read.csv(file="C:/Users/cbahlai/Dropbox/Old_gigs/Zipkin/sprat.csv", header=T)


#okay, let's see if it'll work
source("regime_shift_detector.R")

#not sure which column to work on so I'll do both

#here's C

spratC<-sprat
spratC$obsC<-NULL

RSdetector(spratC, criterion="AICc")

spratC1<-addNt1(spratC)

breakweights(spratC1, "AICc")

#here's l

spratl<-sprat
spratl$obsl<-NULL

RSdetector(spratl, criterion="AICc")

spratl1<-addNt1(spratl)

breakweights(spratl1, "AICc")
