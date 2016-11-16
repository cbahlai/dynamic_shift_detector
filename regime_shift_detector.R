#script to analyse time series data to etermine break points

#import test data for calibrating functions
test<-read.csv(file="test.csv", header=TRUE)
plot(test)

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
  #create an initial estimate of k to aide model convergence
  kest<-mean(data$Nt)
  #fit the model
  ricker.model<-nlsLM(Nt1~ Nt*exp(r*(1- Nt/k)), start=list(r=1.5, k=kest), data=data)
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

#next, a function for 1 break, with dummy variables in it so we can use it for the 2 break model
onebreak<-function(data, Break2, fit3, breaks){
  if(missing(Break2)){ #if no value for Break2 is provided, it is NA
    Break2 <- NA  
  }
  if(missing(fit3)){ #if no value for fit3 is provided,it is 0
    fit3<-0  
  }
  if(missing(breaks)){ #if no value for breaks is provided,it is 1
    breaks<-1  
  }
  Break1<-min(data$year)+2 #create first breakpoint three years into the time series to avoid overfitting
  out.frame<-data.frame(matrix(vector(), 0, 4,
                               dimnames=list(c(), c("Number", "Break1", "Break2", "AIC"))),
                        stringsAsFactors=F)#Create a place to put our data
  while(Break1<(max(data$year))){
    part1<-data[which(data$year<Break1),] #create subsets at the breakpoint
    part2<-data[which(data$year>(Break1-1)),]
    if(nrow(part1)>2 & nrow(part2)>2){ #constrain model to run only when 3 or more points are present
      fit1<-rickerfit(part1) #fit the model to part 1
      fit2<-rickerfit(part2) #fit the model to part 2
      out<-c(breaks, max(part1$year), Break2, fit1[1]+fit2[1]+fit3)#create output vector
      out.frame<-rbind(out.frame, out) #bind it to previous results
    }
    Break1<-Break1+1 #move the break to next year
  }
  #rename columns in output for some reason
  colnames(out.frame)<- c("Number", "Break1", "Break2", "AIC")
  return(out.frame)
}

#test it to see if it's spitting out the right stuff
onebreak(test1)
#does maniputlating the second break point carry correctly?
onebreak(test1, 2005, 0, 2)
#cool.

#okay, now we need to build another function that does this same analysis for two break points
# we'll break the time series into two, use the onebreak function on the first part
# and then feed the break point and AIC from that section into the one break model
# break things up the same way as previously, but instead of calling sections 1 and 2
# call them sections A and B. Run onebreak on section A, just fit and feed results to onebreak
# from section B.

twobreaks<-function(data){
  Break1<-min(data$year)+2 #create first breakpoint two years into the time series to avoid overfitting
  Break2<-Break1+2 #make breakpoint two start later
  out.frame<-data.frame(matrix(vector(), 0, 4,
                               dimnames=list(c(), c("Number", "Break1", "Break2", "AIC"))),
                        stringsAsFactors=F)#Create a place to put our data
  
  while(Break2<(max(data$year))){
    partA<-data[which(data$year<Break2),] #create subsets at the breakpoint
    partB<-data[which(data$year>(Break2-1)),]
    if(nrow(partA)>2 & nrow(partB)>2){ #constrain model to run only when 3 or more points are present
      fitB<-rickerfit(partB) #fit the model to part B so we have the parameters to feed in
      fitA<-onebreak(partA, Break2, fitB[1], 2) #do onebreak with the AIC from part B's fit
      out.frame<-rbind(out.frame, fitA)
    }
    Break2<-Break2+1 #move the break to next year
  }
  #rename columns in output for some reason
  colnames(out.frame)<- c("Number", "Break1", "Break2", "AIC")
  return(out.frame)
}


#and let's gve that a try
twobreaks(test1)

#looks like we got it! 

#Now we need to put these functions together into a break point finder function. 
#Let's first create a function that builds a data frame of AICs for all possible 
#fits from all break point combinations

breakfit<-function(data){
  no.breaks<-nobreaks(data) #fit no break model
  one.break<-onebreak(data) #fit one break model set
  two.breaks<-twobreaks(data) #fit two breaks model set
  out.frame<-rbind(two.breaks, one.break, no.breaks) #stick the results together
  out.frame<-out.frame[order(out.frame$Number, out.frame$Break1, out.frame$Break2),] #sort output
  return(out.frame)
}

#give that a test
breakfit(test1)

#okay, next, a function that pulls out the set of equivalent performance best models
# remember that delta AIC <2 is considered equivalent and lowest AIC is best

equivalentfit<-function(data){
  breakset<-breakfit(data) #generate matrix of fits by breakpoints
  AICbest<-min(breakset$AIC) #find best AIC in the set
  deltaAIC<-AICbest+2 # create rule for equivalent models
  out.frame<-breakset[which(breakset$AIC<deltaAIC),] #cut out all data but equivalent models
  return(out.frame)
}

#and test that
equivalentfit(test1)

#but equivalent fit is one thing- if there's equivalent fit and one of the model set has
# fewer parameters, we obviously want to go with that. create a function that does that

bestfit<-function(data){
  breakset<-equivalentfit(data)#
}