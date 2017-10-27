################################################################

#bring in Monarch overwintering data

monarch<-read.csv(file="C:/Users/cbahlai/Dropbox/Old_gigs/Zipkin/MonarchOW.csv", header=T)


#okay, let's see if it'll work
source("regime_shift_detector.R")

RSdetector(monarch)

#it looks like the AICcorrection is indicating the a model with two breaks is nearly identically ranked
#to a model with just one break, but by AIC, the two break model is performing dramatically better, so let's 
#also examine the two break point model

monarch1<-addNt1(monarch)

best.2.break<-equivalentfit(monarch1)[which(equivalentfit(monarch1)$Nbreaks==2),]

modelspecification(best.2.break, monarch1)

