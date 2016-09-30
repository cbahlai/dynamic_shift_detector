#script to analyse time series data to etermine break points

#import test data for calibrating functions
test<-read.csv(file="test.csv", header=TRUE)


#assume data is in form of data frame where first column is year, second is abundance (Nt)

#create function that makes an additional response variable Nt1
#where Nt1 is the abundance in the next year

addNt1<-function(data){
  #create empty vector to feed 'next year' abundance into
  Nt1 = c()
  #pull out values from the abundance column-
  #for each year, go to the NEXT year's abundance, put that in the vector
  for (i in 1:(length(data[,2])-1)) {
    Nt1 = c(Nt1,(data[,2])[i+1])
  }
  #cut out last sampling year, because there is no Nt+1 for that year
  data<-data[which(data[,1]<max(data[,1])),]
  #add the vector of new values to the data fram
  data$Nt1<-Nt1
  #rename columns in data set so that they're consistent no matter 
  #what they're named before importing
  names(data)<-c("year", "Nt", "Nt1")
  #spit out that new data frame
  return(data)
}

#test that our function does what we want it to do
test1<-addNt1(test)
View(test1)

#looks good! now we can begin building the model-fitting function

#load minpack.lm
#this is my prefered nonlinear package- fewer convergence issues.
#we'll want to use its nlsLM function to fit the Ricker equation.
library(minpack.lm)

#create function that fits the ricker model

rickerfit<-function (data){
  #fit the model
  ricker.model<-nlsLM(Nt1~ Nt*exp(r*(1- Nt/k)), start=list(r=1.5, k=0.5), data=data)
  #What outputs do we need from each run? AIC, r and k, and their resepective errors.
  #we'll want to create a vecor with this information in it so we can use this information later
  output<-(stuff)
  return(output)
}



