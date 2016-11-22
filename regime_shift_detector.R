#script to analyse time series data to determine break points

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

#looks good!

#lets also create a function that will calculate an AICc correction factor
#which can be added to the AIC values we compute- because we have small sample
#sizes we need to more heavily penalize models with more parameters

AICcorrection<-function(data, breaks){
  a<-2*(breaks+1) # a is the number of parameters estimated each fit (r, k)
  correction<-(2*a*(a+1))/(length(data$year)-a-1)
  return(correction)
}

AICcorrection(test1, 0)

#now we can begin building the model-fitting function


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
  correction<-AICcorrection(data, 0) #calculate AICc correction
  out<-c(0, NA, NA, fit[1]+correction) #output vector with no breaks
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
                               dimnames=list(c(), c("Number", "Break1", "Break2", "AICc"))),
                        stringsAsFactors=F)#Create a place to put our data
  while(Break1<(max(data$year))){
    part1<-data[which(data$year<Break1),] #create subsets at the breakpoint
    part2<-data[which(data$year>(Break1-1)),]
    if(nrow(part1)>2 & nrow(part2)>2){ #constrain model to run only when 3 or more points are present
      fit1<-rickerfit(part1) #fit the model to part 1
      fit2<-rickerfit(part2) #fit the model to part 2
      out<-c(breaks, max(part1$year), Break2, fit1[1]+fit2[1]+fit3)#create output vector
      if (breaks==1){
        out[4]<-out[4]+AICcorrection(data, 1)
      }
      if (breaks==2){
        out[4]<-out[4]+AICcorrection(data, 1)
      }
      out.frame<-rbind(out.frame, out) #bind it to previous results
    }
    Break1<-Break1+1 #move the break to next year
  }
  #rename columns in output for some reason
  colnames(out.frame)<- c("Number", "Break1", "Break2", "AICc")
  return(out.frame)
}

#test it to see if it's spitting out the right stuff
onebreak(test1)
#does maniputlating the second break point carry correctly?
onebreak(test1, 2005, 5, 2)
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
                               dimnames=list(c(), c("Number", "Break1", "Break2", "AICc"))),
                        stringsAsFactors=F)#Create a place to put our data
  
  while(Break2<(max(data$year))){
    partA<-data[which(data$year<Break2)+1,] #create subsets at the breakpoint
    partB<-data[which(data$year>(Break2)),]
    if(nrow(partA)>2 & nrow(partB)>2){ #constrain model to run only when 3 or more points are present
      fitB<-rickerfit(partB) #fit the model to part B so we have the parameters to feed in
      fitA<-onebreak(partA, max(partA$year), fitB[1], 2) #do onebreak with the AIC from part B's fit
      out.frame<-rbind(out.frame, fitA)
    }
    Break2<-Break2+1 #move the break to next year
  }
  #rename columns in output for some reason
  colnames(out.frame)<- c("Number", "Break1", "Break2", "AICc")
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
  breakset<-equivalentfit(data) #get set of eqivalent models
  fewest.parameters<-min(breakset$Number) #choose model with fewest break points
  out.frame<-breakset[which(breakset$Number==fewest.parameters),] #pull models with fewest parameters
  if(length(out.frame$Number>1)){ #if there is more than one model with the same # of parameters
    AICbest<-min(out.frame$AIC) #find best AIC in the set
    out.frame<-out.frame[which(out.frame$AIC==AICbest),] #only bring forward the model with the best AIC
  }
  return(out.frame)
}

#and give that one a go
bestfit(test1)

#and now let's calculate the particulars of the model with the best fit
# we need a function that can handle all cases 

bestmodel<-function(data){
  modelspecs<-bestfit(data) #get the particulars of the best model
  out.frame<-data.frame(matrix(vector(), 0, 7,
                               dimnames=list(c(), 
                                             c("Year1", "Year2", "AIC", "r", "rse", "k", "kse"))),
                        stringsAsFactors=F)#Create a place to put our data
  
  if (modelspecs$Number[1]==0){ #if there's no breaks
    fit<-rickerfit(data)
    output<-c(min(data$Year), max(data$Year), fit) #fit whole data series + output results
    out.frame<-rbind(out.frame, output)
    
  } else if (modelspecs$Number[1]==1){
    part1<-data[which(data$year<modelspecs$Break1[1]+1),] #create subsets at the breakpoint
    part2<-data[which(data$year>(modelspecs$Break1[1])),]
    fit1<-rickerfit(part1) #fit first segment
    output1<-c(min(part1$year), max(part1$year), fit1)
    fit2<-rickerfit(part2) #fit second segment
    output2<-c(min(part2$year), max(part2$year), fit2)
    out.frame<-rbind(out.frame, output1, output2)#put outputs for each segment in a data frame
    
  } else if (modelspecs$Number[1]==2){
    part1<-data[which(data$year<modelspecs$Break1[1]+1),] #create subsets at the breakpoint
    part2<-data[which(data$year>(modelspecs$Break1[1])& data$year<modelspecs$Break2[1]+1),]
    part3<-data[which(data$year>(modelspecs$Break2[1])),]
    fit1<-rickerfit(part1) #fit first segment
    output1<-c(min(part1$year), max(part1$year), fit1)
    fit2<-rickerfit(part2) #fit second segment
    output2<-c(min(part2$year), max(part2$year), fit2)
    fit3<-rickerfit(part3) #fit third segment
    output3<-c(min(part3$year), max(part3$year), fit3)
    out.frame<-rbind(out.frame, output1, output2, output3)#put outputs for each segment in a data frame
    
  }
  colnames(out.frame)<- c("Year1", "Year2", "AIC", "r", "rse", "k", "kse")
  return(out.frame)
}


#and test it
bestmodel(test1)

#finally let's write a function that feeds data through the relevant functions and gives
#us a report of the relevant things

rsdetector<-function(data){ #use raw time series data
  #plot the data
  plot(data)
  data1<-addNt1(data)
  #give an output of all possible break point combinations tested
  writeLines(paste("Here are the break points for all models tested"))
  print(breakfit(data1))
  #output models with equivalent performance
  writeLines(paste("Here is the set of best performing models"))
  print(equivalentfit(data1))
  #output model with best performance
  writeLines(paste("Here is the best model- the one with the fewest parameters and/or lowest AICc"))
  print(bestfit(data1))
  # output regression parameters of best model
  writeLines(paste("Here is the set of regression parameters"))
  writeLines(paste("Note AIC is used here for individual segments,\n decisions based on AICc for whole model"))
  print(bestmodel(data1))
}

rsdetector(test)
#looks good!! Now it's time to try this in the wild

#I'm also wondering what would happen if we solved for r while holding k constant
# and vice versa. This would provide an 'instantaneous' r and k  that we could also
# look at to see if there's support for changes
# if the Ricker model takes the form Nt1~ Nt*exp(r*(1- Nt/k)), let's solve for each
# r and k, then use the k or r values for the no break model to create a vector for 
# each parameter over time

r.est<-function(data){
  k<-rickerfit(data)[4]# fit model without breakpints to get the estimated overall k
  r<- (log(data$Nt1/data$Nt))/(1-(data$Nt/k)) #solve for r
  return(r)
}

test2<-test1
test2$r<-r.est(test2)

k.est<-function(data){
  r<-rickerfit(data)[2]# fit model without breakpints to get the estimated overall r
  k<- data$Nt/(1-(log(data$Nt1/data$Nt)/r)) #solve for k
  return (k)
}

test2$k<-k.est(test2)


# Okay, so now we want to generalize this so it can handle N break point cases
# this is a bit more complex because it'll require some recursion from within the function
# and because there will be N break cases, we won't be able to plan a 2 dimensional data frame 
# that can have columns for all break cases. I think the best way to do this is by creating a 
# list of break points and an associated list of AICs for the fit, and then use the sum function
# for the AICs when including them in the output, but include the break point list as a single
#element in the output data frame
# so we want a 3 column data frame with number of breaks, a list of the breaks, and the 
#resultant AICc as columns, respectively.




#to-do list from lab meeting
#generalize model to handle N break point cases
#create simulations to test robustness of picking up regime shifts at different break point spacings
#Create simulations to test robustness of picking up regime shifts of different sizes
#create simulations to test robustness of picking up regime shifts in different noise scenarios


