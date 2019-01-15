

#read in all the sim data files- this was done in pieces to keep it computationally doable


#get a list of file names:
setwd("simresults/Break_weights_AICc")
file_list<-list.files()

#loop through the files, merge 'em together
simulation.results <- do.call("rbind",lapply(file_list,
                                             FUN=function(files){read.csv(files, header=TRUE)}))

setwd("../..")


#let's cull out the 15 year scenarios with 3 breaks- this usually violates the constraints of the 
#model and thus isn't an honest test- let's also get rid of it for 20, and 2 break scenarios for 20, as these often fail
simulation.results<-simulation.results[-which(simulation.results$Nyears=="15" & 
                                                simulation.results$nbreaksin=="3"),]
simulation.results<-simulation.results[-which(simulation.results$Nyears=="15" & 
                                                simulation.results$nbreaksin=="2"),]
#also, the negative starting values for r don't work well...Ricker model works best for a population
#that is K limited, so this results in nonsensical output for this particular implimentation
simulation.results<-simulation.results[-which(simulation.results$startR=="-0.5"),]

#now we need to take th data produced and summarize it for plotting
library(plyr)

#results will need to be summarized completely differently for this set- we're not counting,
#we're averaging the weight of the result

#average outcome for each unique observation
summarize.results<-ddply(simulation.results,
                         c("Nyears", "startPop", "noise", "nbreaksin",
                           "startK", "startR", "changeK", "changeR"), summarize,
                         rightweight=mean(rightweight), wrongweight=mean(wrongweight),
                         rightmin=mean(rightmin), wrongmax=mean(wrongmax))

#now, because zero break scenerios have been defined as 1 when they're correct, they're giving a
#misleading trend, so let's just take them out of the plot
#they're not helping to interpret anything

#define a vector to keep it concise
vec<-summarize.results$nbreaksin

summarize.results$rightweight<-ifelse(vec==0, NA, summarize.results$rightweight)
summarize.results$rightmin<-ifelse(vec==0, NA, summarize.results$rightmin)


#all right, let's get plotting!
library(ggplot2)


#choose a color palette
pal<-c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c")
pal.nozero<-c("#fecc5c", "#fd8d3c", "#e31a1c") #for cases where no zero break scenarios are plotted
pal.noone<-c("#fd8d3c", "#e31a1c")
pal.notwo<-c("#e31a1c")

#we need to subset the data by factor we're varying.

###############
# Noise experiment

#start with successes

noise.experiment.correct<-summarize.results[which(summarize.results$changeK==75 & 
                                                    summarize.results$changeR==25 & 
                                                    summarize.results$startR==2 &
                                                    summarize.results$Nyears==20),]
noiseplot.correct<-ggplot(noise.experiment.correct, aes(noise, rightweight, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="gam", se=F, color="grey", formula=y ~ poly(x, 3), span=0.1, show.legend=F)+
  geom_point(colour="black", pch=23, size=3, show.legend=F)+
  geom_smooth(aes(noise, wrongweight), method="gam", se=F, color="grey", formula=y ~ poly(x, 3), span=0.1, show.legend=F)+
  geom_point(aes(noise, wrongweight), colour="black", pch=25, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% noise")+
  ylab("break weight")+
  xlim(0,15)+ylim(-0.2,1.1)

noiseplot.correct


###############
# startR experiment

#start with successes

startr.experiment.correct<-summarize.results[which(summarize.results$changeK==75 & 
                                                    summarize.results$changeR==25 & 
                                                    summarize.results$noise==2 &
                                                    summarize.results$Nyears==20),]
startr.correct<-ggplot(startr.experiment.correct, aes(startR, rightweight, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="gam", se=F, color="grey", formula=y ~ poly(x, 3), span=0.1, show.legend=F)+
  geom_point(colour="black", pch=23, size=3, show.legend=F)+
  geom_smooth(aes(startR, wrongweight), method="gam", se=F, color="grey", formula=y ~ poly(x, 3), span=0.1, show.legend=F)+
  geom_point(aes(startR, wrongweight), colour="black", pch=25, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("r starting value")+
  ylab("break weight")+
  xlim(0.5, 2)+ylim(-0.2,1.1)

startr.correct



###############
# K experiment

#start with successes

changeK.experiment.correct<-summarize.results[which(summarize.results$noise==5 & 
                                                      summarize.results$changeR==25 &
                                                      summarize.results$startR==2 &
                                                      summarize.results$Nyears==20),]
changeKplot.correct<-ggplot(changeK.experiment.correct, aes(changeK, rightweight, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="gam", se=F, color="grey", formula=y ~ poly(x, 3), span=0.1, show.legend=F)+
  geom_point(colour="black", pch=23, size=3, show.legend=F)+
  geom_smooth(aes(changeK, wrongweight), method="gam", se=F, color="grey", formula=y ~ poly(x, 3), span=0.1, show.legend=F)+
  geom_point(aes(changeK, wrongweight), colour="black", pch=25, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% change in K")+
  ylab("break weight")+
  xlim(0,75)+ylim(-0.2,1.1)

changeKplot.correct



###############
# r experiment

#start with successes

changeR.experiment.correct<-summarize.results[which(summarize.results$noise==5 & 
                                                      summarize.results$changeK==75 &
                                                      summarize.results$startR==2 &
                                                      summarize.results$Nyears==20),]
changeRplot.correct<-ggplot(changeR.experiment.correct, aes(changeR, rightweight, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="gam", se=F, color="grey", formula=y ~ poly(x, 3), span=0.1, show.legend=F)+
  geom_point(colour="black", pch=23, size=3, show.legend=F)+
  geom_smooth(aes(changeR, wrongweight), method="gam", se=F, color="grey", formula=y ~ poly(x, 3), span=0.1, show.legend=F)+
  geom_point(aes(changeR, wrongweight), colour="black", pch=25, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% change in r")+
  ylab("break weight")+
  xlim(0,75)+ylim(-0.2,1.1)

changeRplot.correct



###############
# Time series length experiment

#start with successes

Nyears.experiment.correct<-summarize.results[which(summarize.results$noise==2 & 
                                                     summarize.results$changeK==75 &
                                                     summarize.results$startR==2 &
                                                     summarize.results$changeR==25),]
Nyearsplot.correct<-ggplot(Nyears.experiment.correct, aes(Nyears, rightweight, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="lm", se=F, color="grey", show.legend=F)+
  geom_smooth(aes(Nyears, wrongweight), method="lm", se=F, color="grey", show.legend=F)+
  geom_point(colour="black", pch=23, size=3, show.legend=F)+
  geom_point(aes(Nyears, wrongweight), colour="black", pch=25, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("Series length")+
  ylab("break weight")+
  xlim(14,31)+ylim(-0.2,1.1)

Nyearsplot.correct

#dummy up a data series for the shape legend

veccat<-c("true","erroneous", "true", "true")
veccat<-factor(veccat, levels=c("true", "erroneous"))
vecx<-c(20, 40, 66, 50)
vecy<-c(0.5, 0.5, 0.8, 0.4)
dummy<-as.data.frame(cbind(veccat,vecx,vecy))
shapepal<-c(23, 25)

plot.for.leg1<-ggplot(changeR.experiment.correct, aes(changeR, rightweight, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_point(colour="black", pch=22, size=3, show.legend=T)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme_bw(base_size = 12)

plot.for.leg1

plot.for.leg2<-ggplot(data=dummy, aes(vecx, vecy, shape=as.factor(veccat)))+
  scale_shape_manual(values=shapepal, labels=c("true", "erroneous"))+
  geom_point(color="black", fill="black", size=3, show.legend=T)+
  theme_bw(base_size = 12)+
  guides(shape=guide_legend(title="Type\nof break"))

plot.for.leg2


# need to stack together noiseplot.correct, changeKplot.correct, changeRplot.correct, Nyearsplot.correct

#stack plots together
library(gridExtra)
library(grid)

noiseplot.correct.1<-noiseplot.correct+
  guides(fill=FALSE)+
  ylab(NULL)+
  #xlab(NULL)+
  coord_fixed(ratio=15)+
  ggtitle(label="A")+
  theme(plot.title = element_text(size = 12, margin = margin(t = 10, b = -1)))

startr.correct.1<-startr.correct+
  guides(fill=FALSE)+
  ylab(NULL)+
  #xlab(NULL)+
  coord_fixed(ratio=1.5)+
  ggtitle(label="B")+
  theme(plot.title = element_text(size = 12, margin = margin(t = 10, b = -1)))

changeKplot.correct.1<-changeKplot.correct+
  guides(fill=FALSE)+
  ylab(NULL)+
  #xlab(NULL)+
  coord_fixed(ratio=75)+
  ggtitle(label="C")+
  theme(plot.title = element_text(size = 12, margin = margin(t = 10, b = -1)))

changeRplot.correct.1<-changeRplot.correct+
  guides(fill=FALSE)+
  ylab(NULL)+
  #xlab(NULL)+
  coord_fixed(ratio=75)+
  ggtitle(label="D")+
  theme(plot.title = element_text(size = 12, margin = margin(t = 10, b = -1)))

Nyearsplot.correct.1<-Nyearsplot.correct+
  guides(fill=FALSE)+
  ylab(NULL)+
  #xlab(NULL)+
  coord_fixed(ratio=17)+
  ggtitle(label="E")+
  theme(plot.title = element_text(size = 12, margin = margin(t = 10, b = -1)))

#pull legend out of plot
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

leg1<-g_legend(plot.for.leg1)
leg2<-g_legend(plot.for.leg2)

#create a blank grob to hold space where the legend would go next to D
blank <- grid.rect(gp=gpar(col="white"))

grid.arrange(arrangeGrob(noiseplot.correct.1, startr.correct.1, 
                         changeKplot.correct.1, changeRplot.correct.1,
                         Nyearsplot.correct.1, leg, 
                         ncol=6, widths=c(35,35,35,35,35,30)), 
             left=textGrob("\n                  Break weight", rot=90,
                           gp=gpar(fontsize=16, fontface="bold")))



pdf("figs/Figure_2_AICc_average_breaks.pdf", height=3.4, width=11)
grid.arrange(arrangeGrob(noiseplot.correct.1, startr.correct.1, 
                         changeKplot.correct.1, changeRplot.correct.1,
                         Nyearsplot.correct.1, arrangeGrob(blank, leg1, leg2,
                                                           ncol=1, heights=c(0.2,0.4,0.4)), 
                         ncol=6, widths=c(35,35,35,35,35,30)), 
             left=textGrob("\n  Break weight", rot=90,
                           gp=gpar(fontsize=16, fontface="bold")))


dev.off()
