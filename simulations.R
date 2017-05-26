#script for creating simulated data under a variety of parameters
#and then determining if RSdetector function correctly determines these parameters

#to-do list from lab meeting:

#create simulations to test robustness of picking up regime shifts at different break point spacings
#Create simulations to test robustness of picking up regime shifts of different sizes
#create simulations to test robustness of picking up regime shifts in different noise scenarios
#and while we're at it, simulations to test how this all works given different lengths of time series

#get the regime shift detector functions into memory
source("regime_shift_detector.R")

#create a function that will make fake data based on specified parameters
#assume change, noise is given in percent (0-100) scale, as is change to r, k

fakedata<-function(startyear, Nyears, startPop, noise, startK, startR, breaks, changeK, changeR){
  if(missing(startyear)){ #set default values for all paremeters
    startyear<-1900
  }
  if(missing(Nyears)){
    Nyears<-20
  }
  #in order to set the default breaks, I need to know what the last year in the time range is
  #so let's create a vector of years going into the time series now
  year<-seq(startyear, (startyear+Nyears-1)) #-1 because time range is inclusive of year 0
  lastyear<-max(year)-1# max is second last year because we don't have Nt1 for last year
  
  if(missing(startPop)){#let's make popultion size default to 1000
    startPop<-1000
  }
  if(missing(noise)){#no noise by default
    noise<-0
  }
  if(missing(startK)){ #so start population can grow by default
    startK<-1500
  }
  if(missing(startR)){
    startR<-1.5
  }
  if(missing(breaks)){
    breaks<-list()#no break model, null list
  }
  if(missing(changeK)){ # by default, don't change K after a break
    changeK<-0
  }
  if(missing(changeR)){ #same with r
    changeR<-0
  }
  
  #create a vector for noise for each year- it will be random, in the range of % given
  noisevector<-c()# make an empty vector
  for (i in 1:(length(year))){
    instant.buzz<-1+runif(1, -noise, noise)/100 #generate an instantaneous buzz :)
    noisevector<-c(noisevector, instant.buzz) #add that to the vector
  }
  
  #create a vector of when regime shifts will occur
  change<-c(FALSE)# make a vector with first value false- cannot have a change in first year
  for (i in 1:(length(year)-1)){
    if(any(breaks==year[i])){
      switch<-TRUE
    }else{
      switch<-FALSE
    }
        change<-c(change, switch) #add that to the vector
  }
  
  #create a vector of changes to k 
  k<-c(startK)# initiate vector with start value at k
  for (i in 1:length(year)-1){
    if (change[i+1]){
      changesetK<-c(changeK, -changeK)
      nextk<-k[i]*(100+(sample(changesetK, 1)))/100 #randomly chose an increase or decrease in % change
    } else{
      nextk<-k[i] # or if it's not a break year, don't change k
    }
    k<-c(k, nextk)
  }

  # #create a vector of changes to r
  r<-c(startR)# initiate vector with start value at r
  for (i in 1:length(year)-1){
    if (change[i+1]){
      changesetR<-c(changeR, -changeR)
      nextr<-r[i]*(100+(sample(changesetR, 1)))/100 #randomly chose an increase or decrease in % change
    } else{
      nextr<-r[i] # or if it's not a break year, don't change r
    }
    r<-c(r, nextr)
  }
  #calculate Nt vector
  Nt<-c(startPop) #create population vector with starting population as entry 1
  for(i in 1:length(year)){
    Nt1<-Nt[i]*exp(r[i]*(1- Nt[i]/k[i]))*noisevector[i]
    Nt<-c(Nt, Nt1)

  }
  #now we need to make the simulated data into a data frame which would look like
  #one fed into the analysis
  addyear<-max(year)+1
  year<-c(year, addyear)
  simdata<-as.data.frame(cbind(year, Nt))
  
  return(simdata)
}
fakedata(noise=5, changeK=50, changeR=10, breaks=list("1905", "1910"))

#now we need to create a function that will take the simulated data, find the best break combination
#and compare the ones it finds to the ones the data was built with


detect.fake.shifts<-function(startyear, Nyears, startPop, noise, startK, startR, breaks, changeK, changeR){
  #create simulated data based on input parameters
  test<-fakedata(startyear, Nyears, startPop, noise, startK, startR, breaks, changeK, changeR)
  #run the data thtrough the script that finds the best model
  #and pull out a list of the breaks it found
  breaksfound<-unlist(bestmodel(addNt1(test))[2])
  #cull out the 'break' at the end of the data
  endbreak<-as.numeric(length(breaksfound))-1
  breaksfound<-breaksfound[1:endbreak]
  # test if we found the right breaks
  if(length(breaksfound)==length(breaks)){
    if(all(breaksfound==breaks)){
      victory<-1
    }else{
      victory<-0
    }
  }else{
    victory<-0
  }
  #also want to output number of breaks input
  nbreaks<-length(breaks)
  #output needed information
  testconditions<-unlist(c(Nyears, startPop, noise, nbreaks, startK, startR, changeK, changeR, victory))
  return(testconditions)
  
}

#create a function that compiles sucesses and failures for iterations of fitting the model
# on simulated data produced under given conditions

break.it.down<-function(startyear, Nyears, startPop, noise, 
                        startK, startR, breaks, changeK, changeR, nIter){
  out.frame<-data.frame(matrix(vector(), 0, 9,
                               dimnames=list(c(), 
                                             c("Nyears", "startPop", "noise", "nbreaks",
                                               "startK", "startR", "changeK", "changeR", "victory"))),
                        stringsAsFactors=F)#Create a place to put our data
  for (i in 1:nIter){
    test<-detect.fake.shifts(startyear, Nyears, startPop, noise, startK, 
                             startR, breaks, changeK, changeR)
    out.frame<-rbind(out.frame, test)#put output for segment in a data frame
  }
  colnames(out.frame)<- c("Nyears", "startPop", "noise", "nbreaks",
                          "startK", "startR", "changeK", "changeR", "victory")
  return(out.frame)
  
}



#okay, now that we've got it all working, it's time to build out the tests. To prevent the permutations
# of possible tests from going to infinity, let's create a 'base scenario' that we modify one parameter
# at a time, and let's choose 1,2,3,4 break point scenarios in which to test these

#choose base parameters

startyear<-1 #should not affect output at all
Nyears<-22 #processing time goes up considerably with length of time series, so make this the base scenario
startPop<-3000 # arbtrary start point, but r, K need to be chosen in reasonable scale with this
noise<-1 #base scenario should have very little %noise, but needs some so  there's a wee bit of error in the fit 
startK<-2000 #seems reasonable for a startpop of 1500
startR<-1.5 #also reasonable r
changeK<-50# start with big, easily detected shifts
changeR<-70 # as with changeK
nIter<-5 # keep this low while we build the code

# create some script that randomly chooses the breaks, given certain rules
# recall that the model assumes breaks cannot occur less than three years apart 
# or from the start or end of the time series because of overfitting issues

#minumum break must be three years in or later
minbreak<-startyear+3
#maximum break must be three years prior to the end of the series or before, plus we lose the last year
maxbreak<-startyear+Nyears-4

#create a sequence of all posible breaks
possibleBreaks<-seq(minbreak, maxbreak)

#create a function that generates a list of breaks randomly from the available set of breaks

breaklist<-function(possibleBreaks, howmany){ #we'll cap it at 3 breaks for the simulations
  if (howmany>3){ #no cheating, we're capping this at 3 breaks for the simulations
    howmany<-3
  }
  if (howmany<1){ #seriously, don't try to break this here
    howmany<-1
  }
  firstbreak<-sample(possibleBreaks, 1)
  eliminatedSecondBreaks<-seq(firstbreak-3, firstbreak+3)
  possibleSecondBreaks<-possibleBreaks[!is.element(possibleBreaks, eliminatedSecondBreaks)]
  secondbreak<-sample(possibleSecondBreaks, 1)
  eliminatedThirdBreaks<-seq(secondbreak-3, secondbreak+3)
  possibleThirdBreaks<-possibleSecondBreaks[!is.element(possibleSecondBreaks, eliminatedThirdBreaks)]
  thirdbreak<-sample(possibleThirdBreaks, 1)

  if (howmany==1){
    #for one break, this is simple
    breaks=sample(possibleBreaks, 1)
  }else if (howmany==2){
    #for two breaks
        breaks<-sort(c(firstbreak, secondbreak))
  }else if (howmany==3){
    #for three breaks, follow from 2
    breaks<-sort(c(firstbreak, secondbreak, thirdbreak))
  }
  return(breaks)
}

numLoops<-2 #how many times to we want to iterate through new break point choice set
results.matrix<-data.frame(matrix(vector(), 0, 9, 
                                  dimnames=list(c(), c("Nyears", "startPop", "noise", "nbreaks",
                                                       "startK", "startR", "changeK", "changeR", 
                                                       "victory"))),
                           stringsAsFactors=F)#Create a place to put our data

#base scenario

while (numLoops>0){
  #we want to test each scenario with  1-3 breaks
  breaks1<-breaklist(possibleBreaks, 1)
  breaks2<-breaklist(possibleBreaks, 2)
  breaks3<-breaklist(possibleBreaks, 3)
 
  
  result.matrix1<-break.it.down(startyear=startyear, Nyears=Nyears, startPop=startPop, 
                                noise=noise, startK=startK, startR=startR, 
                                breaks=breaks1, changeK=changeK, changeR=changeR, nIter=nIter)
  result.matrix2<-break.it.down(startyear=startyear, Nyears=Nyears, startPop=startPop, 
                                noise=noise, startK=startK, startR=startR, 
                                breaks=breaks2, changeK=changeK, changeR=changeR, nIter=nIter)
  result.matrix3<-break.it.down(startyear=startyear, Nyears=Nyears, startPop=startPop, 
                                noise=noise, startK=startK, startR=startR, 
                                breaks=breaks3, changeK=changeK, changeR=changeR, nIter=nIter)

  results.matrix<-rbind(results.matrix, result.matrix1, result.matrix2, result.matrix3)
  numLoops<-numLoops-1
}
