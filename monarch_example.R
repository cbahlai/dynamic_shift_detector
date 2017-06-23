################################################################

#bring in Monarch overwintering data

monarch<-read.csv(file="C:/Users/cbahl/Dropbox/Zipkin/monarchOW.csv", header=T)


#okay, let's see if it'll work
source("regime_shift_detector.R")

RSdetector(monarch)

#it looks like the AICcorrection is indicating the a model with two breaks is nearly identically ranked
#to a model with just one break, but by AIC, the two break model is performing dramatically better, so let's 
#also examine the two break point model

monarch1<-addNt1(monarch)

best.2.break<-equivalentfit(monarch1)[which(equivalentfit(monarch1)$Nbreaks==2),]

modelspecification(best.2.break, monarch1)

#So we haven't considered a case where there's a linear decline in k. 
# we asumed abrupt transitions, but this may not be the case. Let's see what happens when 
#we look at the data this way. 

# a good guess for the form of a linear decline in k, if it's occuring, would be a simple 
# linear regression of Nt over years. So let's get that out:

monarch.regression<-lm(Nt~year, data=monarch1)
summary(monarch.regression)

#extract slope and intercept
intercept.k<-summary(monarch.regression)$coefficients[1,1]
slope.k<-summary(monarch.regression)$coefficients[2,1]

#predict k over time
monarch1$k.linear<-intercept.k+slope.k*monarch1$year

# now let's look at the
# fit by AICc using this linear declining k

monarch.linear.decline.k<-nlsLM(Nt1~ Nt*exp(r*(1- Nt/k.linear)), 
                                start=list(r=1.5), data=monarch1)
summary(monarch.linear.decline.k)
AIC(monarch.linear.decline.k) #get AIC
AIC(monarch.linear.decline.k)+AICcorrection(monarch1, 1)#add correction to get AICc 
#use the correction for 1 break to account for the fact that K was estimated first 
#what essentially is a nested model

#so this is interesting- the regime shift model with two breaks still fits well, but just assuming
#a linear decline in k produced an equivalent (but slightly better) AICc.