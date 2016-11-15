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
  output<-c(AIC(ricker.model), #AIC
            summary(ricker.model)$coefficients[1,1], # r
            summary(ricker.model)$coefficients[1,2], # se for r
            summary(ricker.model)$coefficients[2,1], # k
            summary(ricker.model)$coefficients[2,2]) # se for k
  return(output)
}

#test to see if the fit function works

testfit<-rickerfit(test1)
#should spit out a vector of AIC, r, rse, k, kse.

testfit

#okay, seems to work! 

#now we need to build a tool that will cut a time series up,
#fit the model, and spit out relevant parameters

#we probably want to write a function for each break point combo
#then smash them into a function that figures out what the best model is

#so to make them comparable, we're going to want each function to output
#number of breaks, break1, break2, AIC. We can then go back and pull the coefficients
#for the 

#start with function for no breaks

nobreaks<-function(data){
  fit<-rickerfit(data) #fit the model
  out<-c(0, NA, NA, fit[1]) #output vector with no breaks
  return(out)
}

#test it to see if it's spitting out the right stuff
nobreaks(test1)

#next, a function for 1 break
onebreak<-function(data){
  Break1<-min(data$year)+2 #create first breakpoint three years into the time series to avoid overfitting
  out.frame<-data.frame(matrix(vector(), 0, 4,
                               dimnames=list(c(), c("Number", "Break1", "Break2", "AIC"))),
                        stringsAsFactors=F)#Create a place to put our data
  while(Break1<(max(data$year))){
    part1<-data[which(data$year<Break1),] #create subsets at the breakpoint
    part2<-data[which(data$year>(Break1-1)),]
    if(nrow(part1)>3 & nrow(part2)>3){
      fit1<-rickerfit(part1) #fit the model to part 1
      fit2<-rickerfit(part2)
      out<-c(1, max(part1$year), NA, fit1[1]+fit2[1])#create output vector
      out.frame<-rbind(out.frame, out) #bind it to previous results
    }
    Break1<-Break1+1 #move the break to next year
  }
  return(out.frame)
}

#test it to see if it's spitting out the right stuff
onebreak(test1)
