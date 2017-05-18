#script for creating simulated data under a variety of parameters
#and then determining if RSdetector function correctly determines these parameters

#to-do list from lab meeting:

#create simulations to test robustness of picking up regime shifts at different break point spacings
#Create simulations to test robustness of picking up regime shifts of different sizes
#create simulations to test robustness of picking up regime shifts in different noise scenarios
#and while we're at it, simulations to test how this all works given different lengths of time series

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
    if(any(breaks==year[i+1])){
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
      nextk<-k[i]*runif(1, 100-changeK, 100+changeK)/100 #randomly chose an increase or decrease in % change
    } else{
      nextk<-k[i] # or if it's not a break year, don't change k
    }
    k<-c(k, nextk)
  }

  # #create a vector of changes to r
  r<-c(startR)# initiate vector with start value at r
  for (i in 1:length(year)-1){
    if (change[i+1]){
      nextr<-r[i]*runif(1, 100-changeR, 100+changeR)/100 #randomly chose an increase or decrease in % change
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



fakedata(noise=5, changeK=5, changeR=2, breaks=list("1905", "1910"))


