######################################################
# Code for figures generated in "Regime shifts in monarchs" 
# adapted from Bahlai et al 2015 ecological applications
######################################################

#first, monarch population data
#pre-process data as it was used in analysis
#read in raw data
monarch<-read.csv(file="C:/Users/cbahlai/Dropbox/Old_gigs/Zipkin/MonarchOW.csv", header=T)

#data requires some light cleaning. We want year to be a continuous variable
#and data starts in 1994- take the earlier year in the range given- replace

monarch$Year<-1995:2017
colnames(monarch)[1]<-"year"
colnames(monarch)[2]<-"Nt"

# lets use some already built functions to do our data manipulation
source("regime_shift_detector.R")

library(reshape)
library(ggplot2)
library(gridExtra)
# for graphics, we will use the color palette GrandBudapest
# from Karthik Ram's wesanderson package
library(wesanderson)
#for bug icons, run rphylopic
library(rphylopic)

monimg<-image_data("281ee51c-f772-4ebc-b58c-469122406a78", size = "256")[[1]]



#assign sampling year a phase, based on output of model selection

phase = c()
for (i in 1:(length(monarch$year))) {
  if(monarch$year[i]<2003.1){
    phase = c(phase, "A")
  }
  else if (monarch$year[i]>2002.9& monarch$year[i]<2009){
    phase = c(phase, "B")
  }
  else {
    phase = c(phase, "C")
  }
}
monarch$phase<-phase

#phases for the lines that are to join time series points
phasea = c()
for (i in 1:(length(monarch$year))) {
  if(monarch$year[i]<2002.1){
    phasea = c(phasea, "A")
  }
  else if (monarch$year[i]>2002& monarch$year[i]<2009.1){
    phasea = c(phasea, "B")
  }
  else if(monarch$year[i]>2008.9){
    phasea = c(phasea, "C")
  }
}
monarch$phasea<-phasea



######################################
#
# Generate time series figure
#
######################################
pal<-wes_palette("GrandBudapest", 3)
axis.text.theme<-element_text(size=14)
axis.title.theme<-element_text(face="bold", size=16)
monarch.timeseries<-ggplot(monarch, aes(year, Nt, colour=phase, cex=1))+
  scale_color_manual(values = pal)+
  geom_line(data=monarch, aes(x=year, y=Nt, group=phasea), size=1)+
  geom_point(size=3)+
  geom_point(colour="black", pch=21, size=3)+
  xlab("\nYear")+
  ylab("\nHectares occupied\n")+
  theme_bw()+
  coord_equal(ratio=0.45)+
  geom_vline(xintercept=c(2003.5, 2008.5), colour="blue", linetype="longdash")+ 
  geom_vline(xintercept=c(2006.5), colour="grey62", linetype="longdash")+
  theme(legend.key = element_blank(), plot.margin=unit(c(15,0,0,0), "mm"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=axis.text.theme, axis.title=axis.title.theme, 
        legend.title=axis.title.theme, legend.text=axis.text.theme)+
  annotate("text", x=1993.9, y=17, label="A", size=6)+
  add_phylopic(monimg, 1, x=2016.4, y=15.5, ysize=4, color="black")

monarch.timeseries





monarch1<-addNt1(monarch)

######################################
#
# Generate Ricker model figure
#
######################################
phase.a<-function(x){x*exp(0.99*(1- x/10.11))}
phase.b<-function(x){x*exp(1.57*(1- x/5.65))}
phase.c<-function(x){x*exp(1.16*(1- x/2.83))}

monarch.ricker<-ggplot(monarch1, aes(Nt, Nt1, colour=phase, label=year))+
  scale_color_manual(values = pal)+
  xlab("\nN(t)")+ylab("\nN(t+1)\n")+
  theme_bw()+ 
  theme(legend.key = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  stat_function(fun=phase.a, colour=pal[1], size=1)+
  stat_function(fun=phase.b, colour=pal[2], size=1)+
  stat_function(fun=phase.c, colour=pal[3], size=1)+
  geom_point(size=3)+
  geom_point(colour="black", pch=21, size=3)+
  coord_equal(ratio=1)+
  xlim(-2,19)+ylim(-2,19)+
  #geom_text(hjust=1.3, vjust=0, color="black", size=3)+
  theme(axis.text=axis.text.theme, 
        axis.title=axis.title.theme, legend.title=axis.title.theme, legend.text=axis.text.theme)+
  annotate("text", x=-1.6, y=18.3, label="B", size=6)


monarch.ricker


monarch.ricker.nolegend<-monarch.ricker+ 
  theme(legend.position = "none", plot.margin=unit(c(0,30,10,0), "mm"))
monarch.ricker.nolegend

pdf("figs/Figure_4_monarch.pdf", height=8, width=6)
grid.arrange(arrangeGrob(monarch.timeseries, monarch.ricker.nolegend, 
                         heights=c(1, 1)))
dev.off()
