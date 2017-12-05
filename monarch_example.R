################################################################

#bring in Monarch overwintering data

monarch<-read.csv(file="C:/Users/cbahlai/Dropbox/Old_gigs/Zipkin/MonarchOW.csv", header=T)


#okay, let's see if it'll work
source("regime_shift_detector.R")

RSdetector(monarch)



