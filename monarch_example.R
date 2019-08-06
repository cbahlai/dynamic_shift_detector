################################################################

#bring in Monarch overwintering data

monarch<-read.csv(file="C:/Users/cbahlai/Dropbox/Old_gigs/Zipkin/MonarchOW.csv", header=T)


#okay, let's see if it'll work
source("dynamic_shift_detector.R")

DSdetector(monarch, criterion="AIC")

DSdetector(monarch, criterion="AICc")

monarch1<-addNt1(monarch)

breakweights(monarch1, "AIC")

breakweights(monarch1, "AICc")