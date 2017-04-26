######################################################
# Code for figures generated in "Regime shifts in monarchs" 
# adapted from Bahlai et al 2015 ecological applications
######################################################

#first, monarch population data
#pre-process data as it was used in analysis
#read in raw data
monarch<-read.csv(file="C:/Users/cbahl/Dropbox/Zipkin/monarchOW.csv", header=T)

#data requires some light cleaning. We want year to be a continuous variable
#and data starts in 1994- take the earlier year in the range given- replace

monarch$Year<-1994:2015


library(reshape)
library(ggplot2)
library(gridExtra)
# for graphics, we will use the color palette GrandBudapest
# from Karthik Ram's wesanderson package
library(wesanderson)

#create dataset culled to a standard sampling date
cullpoint=241
datahaxyweekly<-datahaxyweeklyunculled[which(datahaxyweeklyunculled$Ordinal_date<cullpoint),]


#reshape the weekly observations to provide yearly total captures and traps
datahaxymelt<-melt(datahaxyweekly, id=1:2)
datahaxyraw<-cast(datahaxymelt, Year~variable, sum)

#compute Nt and Nt+1 in ladybeetles per trap based on yearly totals
datahaxyraw$Nt<-datahaxyraw$Captures/datahaxyraw$Traps
Nt1 = c()
for (i in 1:(length(datahaxyraw$Nt)-1)) {
  Nt1 = c(Nt1,datahaxyraw$Nt[i+1])
}

#assign sampling year a phase, based on output of model selection

phase = c()
for (i in 1:(length(datahaxyraw$Year))) {
  if(datahaxyraw$Year[i]<2001){
    phase = c(phase, "A")
  }
  else if (datahaxyraw$Year[i]>2000& datahaxyraw$Year[i]<2006){
    phase = c(phase, "B")
  }
  else {
    phase = c(phase, "C")
  }
}
datahaxyraw$phase<-phase

#phases for the lines that are to join time series points
phasea = c()
for (i in 1:(length(datahaxyraw$Year))) {
  if(datahaxyraw$Year[i]<2001.1){
    phasea = c(phasea, "A")
  }
  else if (datahaxyraw$Year[i]>2000& datahaxyraw$Year[i]<2006.1){
    phasea = c(phasea, "B")
  }
  else if (datahaxyraw$Year[i]>2005.9){
    phasea = c(phasea, "C")
  }
}
datahaxyraw$phasea<-phasea

#cut out last sampling year, because there is no Nt+1 for that year
datahaxy<-datahaxyraw[which(datahaxyraw$Year<max(datahaxyraw$Year)),]

#append Nt+1 column to dataset
datahaxy$Nt1<-Nt1

######################################
#
# Generate time series figure
#
######################################
pal<-wes.palette(3, "GrandBudapest")
axis.text.theme<-element_text(size=14)
axis.title.theme<-element_text(face="bold", size=16)
harmonia.timeseries<-ggplot(datahaxyraw, aes(Year, Nt, colour=phase, cex=1))+geom_point(size=4)+scale_color_manual(values = pal)+geom_line(data=datahaxyraw, aes(x=Year, y=Nt, group=phasea), size=1)+geom_line(size=1)+xlab("\nYear")+ylab("\nAverage captures per trap\n")+theme_bw()+coord_equal(ratio=8)+geom_vline(xintercept=c(2000.5, 2005.5), colour="blue", linetype="longdash")+ theme(legend.key = element_blank())+theme(axis.text=axis.text.theme, axis.title=axis.title.theme, legend.title=axis.title.theme, legend.text=axis.text.theme)

harmonia.timeseries

######################################
#
# Generate Ricker model figure
#
######################################
phase.a<-function(x){x*exp(1.28*(1- x/0.33))}
phase.b<-function(x){x*exp(2.17*(1- x/0.47))}
phase.c<-function(x){x*exp(1.54*(1- x/0.30))}
phase.ac<-function(x){x*exp(1.47*(1- x/0.31))}



harmonia.ricker<-ggplot(datahaxy, aes(Nt, Nt1, colour=phase, label=Year))+geom_point(size=4)+scale_color_manual(values = wes.palette(3, "GrandBudapest"))+xlab("\nN(t)")+ylab("\nN(t+1)\n")+theme_bw()+ theme(legend.key = element_blank())+stat_function(fun=phase.a, colour=pal[1], size=1)+stat_function(fun=phase.b, colour=pal[2], size=1)+stat_function(fun=phase.c, colour=pal[3], size=1)+stat_function(fun=phase.ac, colour="black", size=1, linetype="longdash")+coord_equal(ratio=1)+geom_text(hjust=1.3, vjust=0, color="black")+theme(axis.text=axis.text.theme, axis.title=axis.title.theme, legend.title=axis.title.theme, legend.text=axis.text.theme)
harmonia.ricker