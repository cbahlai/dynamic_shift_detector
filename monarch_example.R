################################################################

#bring in Monarch overwintering data

monarch<-read.csv(file="C:/Users/cbahl/Dropbox/Zipkin/monarchOW.csv", header=T)

#data requires some light cleaning. We want year to be a continuous variable
#and data starts in 1994- take the earlier year in the range given- replace

monarch$Year<-1994:2015

#okay, let's see if it'll work

RSdetector(monarch)


#let's also look at our instantaneous r and k values
monarch1<-addNt1(monarch)

monarch1$r<-r.est(monarch1)
monarch1$k<-k.est(monarch1)

#so what I'm seeing in these data is it looks like the assumtion that k is constant 
#is a really bad one. the regime shift model seems to suggest that r is even going up as 
# k drops dramatically. Not to speculate or anything, but this is what we'd expect in
# a case where habitat destruction is limiting breeding habitat, but there could be something 
#compensatory going on with r.

#that's interesting. 

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

#now see what instantaneous r looks like, using this fixed vector for k
monarch1$r.linear.k<- (log(monarch1$Nt1/monarch1$Nt))/(1-(monarch1$Nt/monarch1$k.linear))

#hmm! Hmm! Interesting!! to be sure the evidence doesn't support this, let's look at the
# fit by AICc using this linear declining k

monarch.linear.decline.k<-nlsLM(Nt1~ Nt*exp(r*(1- Nt/k.linear)), 
                                start=list(r=1.5), data=monarch1)
summary(monarch.linear.decline.k)
AIC(monarch.linear.decline.k)+AICcorrection(monarch1, 0)#add correction to get AICc

#so this is interesting- the regime shift model with two breaks still fits best, but just assuming
#a linear decline in k produced a nearly as good AIC. Not equivalent, but getting close.