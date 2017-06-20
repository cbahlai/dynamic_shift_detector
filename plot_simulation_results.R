

#read in all the sim data files- this was done in pieces to keep it computationally doable


#get a list of file names:
setwd("simresults/")
file_list<-list.files()

#loop through the files, merge 'em together
simulation.results <- do.call("rbind",lapply(file_list,
                                  FUN=function(files){read.csv(files, header=TRUE)}))

setwd("..")

#Encoding results
#All scripted breaks found =1
#extra breaks found = 2
#missing breaks (when more than one break in sim data) =3
#right number of breaks but not all match =4
# total failure to find correct breaks =0 

#because our function was throwing an error about results as factors, we encoded 
#them numerically as above. But before we do any operations on it we should 
#at least convert the outcome integers to factors so we don't accidentally 
#do any numeric operations on them. 

#note the other columns of the data frame are also basically ordinal/catagorical, but
#we want to retain their order for plotting purposes, so we'll just proceed
#with caution there


simulation.results$victory<-as.factor(simulation.results$victory)


#now we need to take th data produced and summarize it for plotting
library(plyr)


#count number of times a unique observation was recorded
summarize.results<-count(simulation.results,
                         c("Nyears", "startPop", "noise", "nbreaksin",
                           "startK", "startR", "changeK", "changeR", "victory"))

#count the number of times a unique scenario was attemped (should be pretty uniform but 
# there are someetimes cases where the fit failed) (for a denominator!)

tot.tests<-count(simulation.results,
                 c("Nyears", "startPop", "noise", "nbreaksin",
                   "startK", "startR", "changeK", "changeR"))
#rename the freq column so we don't have naming issues with a merge
colnames(tot.tests)[colnames(tot.tests) == 'freq']<-'total.tests'

summarize.results<-merge(summarize.results, tot.tests)

summarize.results$proportion<-summarize.results$freq/summarize.results$total

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

noise.experiment.correct<-summarize.results[which(summarize.results$changeK==40 & 
                                                    summarize.results$changeR==20 &
                                                    summarize.results$victory==1 &
                                                    summarize.results$Nyears==25),]
noiseplot.correct<-ggplot(noise.experiment.correct, aes(noise, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% noise")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

noiseplot.correct

#next do partial sucesses
# case 2 is extra breaks found

noise.experiment.extra<-summarize.results[which(summarize.results$changeK==40 & 
                                                  summarize.results$changeR==20 &
                                                  summarize.results$victory==2 &
                                                  summarize.results$Nyears==25),]

noiseplot.extra<-ggplot(noise.experiment.extra, aes(noise, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal.nozero)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% noise")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

noiseplot.extra

#missing breaks in output

noise.experiment.missing<-summarize.results[which(summarize.results$changeK==40 & 
                                                    summarize.results$changeR==20 &
                                                    summarize.results$victory==3 &
                                                    summarize.results$Nyears==25),]

noiseplot.missing<-ggplot(noise.experiment.missing, aes(noise, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal.notwo)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% noise")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

noiseplot.missing

#mismatched breaks in output

noise.experiment.mismatch<-summarize.results[which(summarize.results$changeK==40 & 
                                                     summarize.results$changeR==20 &
                                                     summarize.results$victory==4 &
                                                     summarize.results$Nyears==25),]

noiseplot.mismatch<-ggplot(noise.experiment.mismatch, aes(noise, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal.noone)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% noise")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

noiseplot.mismatch

#complete failure to find breaks in output

noise.experiment.fail<-summarize.results[which(summarize.results$changeK==40 & 
                                                 summarize.results$changeR==20 &
                                                 summarize.results$victory==0 &
                                                 summarize.results$Nyears==25),]

noiseplot.fail<-ggplot(noise.experiment.fail, aes(noise, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% noise")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

noiseplot.fail

#stack plots together
library(gridExtra)
library(grid)

noiseplot.correct1<-noiseplot.correct+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="A", size=5)

noiseplot.extra1<-noiseplot.extra+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="B", size=5)

noiseplot.missing1<-noiseplot.missing+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="C", size=5)

noiseplot.mismatch1<-noiseplot.mismatch+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="D", size=5)

noiseplot.fail1<-noiseplot.fail+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="E", size=5)


grid.arrange(arrangeGrob(arrangeGrob(noiseplot.correct1, noiseplot.extra1, ncol=2), 
                         arrangeGrob(noiseplot.missing1, noiseplot.mismatch1, ncol=2),
                         arrangeGrob(noiseplot.fail1, ncol=1, widths=0.6), ncol=1,
                         left=textGrob("\n                  Proportion of outcomes", rot=90,
                                       gp=gpar(fontsize=16, fontface="bold")), 
                         sub=textGrob("% noise", 
                                      gp=gpar(fontsize=16, fontface="bold"), vjust=-2)))



pdf("figs/noise_simulation_outcomes.pdf", height=10, width=5)
grid.arrange(arrangeGrob(arrangeGrob(noiseplot.correct1, noiseplot.extra1, ncol=2), 
                         arrangeGrob(noiseplot.missing1, noiseplot.mismatch1, ncol=2),
                         arrangeGrob(noiseplot.fail1, ncol=1, widths=0.6), ncol=1,
                         left=textGrob("\n                  Proportion of outcomes", rot=90,
                                       gp=gpar(fontsize=16, fontface="bold")), 
                         sub=textGrob("% noise", 
                                      gp=gpar(fontsize=16, fontface="bold"), vjust=-2)))
dev.off()


###############
# K experiment

#start with successes

changeK.experiment.correct<-summarize.results[which(summarize.results$noise==5 & 
                                                      summarize.results$changeR==20 &
                                                      summarize.results$victory==1 &
                                                      summarize.results$Nyears==25),]
changeKplot.correct<-ggplot(changeK.experiment.correct, aes(changeK, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% change in K")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

changeKplot.correct

#next do partial sucesses
# case 2 is extra breaks found

changeK.experiment.extra<-summarize.results[which(summarize.results$noise==5 & 
                                                    summarize.results$changeR==20 &
                                                    summarize.results$victory==2 &
                                                    summarize.results$Nyears==25),]

changeKplot.extra<-ggplot(changeK.experiment.extra, aes(changeK, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal.nozero)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% change in K ")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

changeKplot.extra

#missing breaks in output

changeK.experiment.missing<-summarize.results[which(summarize.results$noise==5 & 
                                                      summarize.results$changeR==20 &
                                                      summarize.results$victory==3 &
                                                      summarize.results$Nyears==25),]

changeKplot.missing<-ggplot(changeK.experiment.missing, aes(changeK, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal.notwo)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% change in K ")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

changeKplot.missing

#mismatched breaks in output

changeK.experiment.mismatch<-summarize.results[which(summarize.results$noise==5 & 
                                                       summarize.results$changeR==20 &
                                                       summarize.results$victory==4 &
                                                       summarize.results$Nyears==25),]

changeKplot.mismatch<-ggplot(changeK.experiment.mismatch, aes(changeK, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal.noone)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% change in K ")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

changeKplot.mismatch

#complete failure to find breaks in output

changeK.experiment.fail<-summarize.results[which(summarize.results$noise==5 & 
                                                   summarize.results$changeR==20 &
                                                   summarize.results$victory==0 &
                                                   summarize.results$Nyears==25),]

changeKplot.fail<-ggplot(changeK.experiment.fail, aes(changeK, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% change in K ")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

changeKplot.fail

#stack plots together
library(gridExtra)
library(grid)

changeKplot.correct1<-changeKplot.correct+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="A", size=5)

changeKplot.extra1<-changeKplot.extra+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="B", size=5)

changeKplot.missing1<-changeKplot.missing+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="C", size=5)

changeKplot.mismatch1<-changeKplot.mismatch+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="D", size=5)

changeKplot.fail1<-changeKplot.fail+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="E", size=5)


grid.arrange(arrangeGrob(arrangeGrob(changeKplot.correct1, changeKplot.extra1, ncol=2), 
                         arrangeGrob(changeKplot.missing1, changeKplot.mismatch1, ncol=2),
                         arrangeGrob(changeKplot.fail1, ncol=1, widths=0.6), ncol=1,
                         left=textGrob("\n                  Proportion of outcomes", rot=90,
                                       gp=gpar(fontsize=16, fontface="bold")), 
                         sub=textGrob("% change in K ", 
                                      gp=gpar(fontsize=16, fontface="bold"), vjust=-2)))



pdf("figs/changeK_simulation_outcomes.pdf", height=10, width=5)
grid.arrange(arrangeGrob(arrangeGrob(changeKplot.correct1, changeKplot.extra1, ncol=2), 
                         arrangeGrob(changeKplot.missing1, changeKplot.mismatch1, ncol=2),
                         arrangeGrob(changeKplot.fail1, ncol=1, widths=0.6), ncol=1,
                         left=textGrob("\n                  Proportion of outcomes", rot=90,
                                       gp=gpar(fontsize=16, fontface="bold")), 
                         sub=textGrob("% change in K ", 
                                      gp=gpar(fontsize=16, fontface="bold"), vjust=-2)))
dev.off()


###############
# r experiment

#start with successes

changeR.experiment.correct<-summarize.results[which(summarize.results$noise==5 & 
                                                      summarize.results$changeK==40 &
                                                      summarize.results$victory==1 &
                                                      summarize.results$Nyears==25),]
changeRplot.correct<-ggplot(changeR.experiment.correct, aes(changeR, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% change in r")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

changeRplot.correct

#next do partial successes
# case 2 is extra breaks found

changeR.experiment.extra<-summarize.results[which(summarize.results$noise==5 & 
                                                    summarize.results$changeK==40 &
                                                    summarize.results$victory==2 &
                                                    summarize.results$Nyears==25),]

changeRplot.extra<-ggplot(changeR.experiment.extra, aes(changeR, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal.nozero)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% change in r ")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

changeRplot.extra

#missing breaks in output

changeR.experiment.missing<-summarize.results[which(summarize.results$noise==5 & 
                                                      summarize.results$changeK==40 &
                                                      summarize.results$victory==3 &
                                                      summarize.results$Nyears==25),]

changeRplot.missing<-ggplot(changeR.experiment.missing, aes(changeR, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal.notwo)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% change in r")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

changeRplot.missing

#mismatched breaks in output

changeR.experiment.mismatch<-summarize.results[which(summarize.results$noise==5 & 
                                                       summarize.results$changeK==40 &
                                                       summarize.results$victory==4 &
                                                       summarize.results$Nyears==25),]

changeRplot.mismatch<-ggplot(changeR.experiment.mismatch, aes(changeR, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal.noone)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% change in r ")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

changeRplot.mismatch

#complete failure to find breaks in output

changeR.experiment.fail<-summarize.results[which(summarize.results$noise==5 & 
                                                   summarize.results$changeK==40 &
                                                   summarize.results$victory==0 &
                                                   summarize.results$Nyears==25),]

changeRplot.fail<-ggplot(changeR.experiment.fail, aes(changeR, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% change in r ")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

changeRplot.fail

#stack plots together
library(gridExtra)
library(grid)

changeRplot.correct1<-changeRplot.correct+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="A", size=5)

changeRplot.extra1<-changeRplot.extra+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="B", size=5)

changeRplot.missing1<-changeRplot.missing+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="C", size=5)

changeRplot.mismatch1<-changeRplot.mismatch+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="D", size=5)

changeRplot.fail1<-changeRplot.fail+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="E", size=5)


grid.arrange(arrangeGrob(arrangeGrob(changeRplot.correct1, changeRplot.extra1, ncol=2), 
                         arrangeGrob(changeRplot.missing1, changeRplot.mismatch1, ncol=2),
                         arrangeGrob(changeRplot.fail1, ncol=1, widths=0.6), ncol=1,
                         left=textGrob("\n                  Proportion of outcomes", rot=90,
                                       gp=gpar(fontsize=16, fontface="bold")), 
                         sub=textGrob("% change in r ", 
                                      gp=gpar(fontsize=16, fontface="bold"), vjust=-2)))



pdf("figs/changeR_simulation_outcomes.pdf", height=10, width=5)
grid.arrange(arrangeGrob(arrangeGrob(changeRplot.correct1, changeRplot.extra1, ncol=2), 
                         arrangeGrob(changeRplot.missing1, changeRplot.mismatch1, ncol=2),
                         arrangeGrob(changeRplot.fail1, ncol=1, widths=0.6), ncol=1,
                         left=textGrob("\n                  Proportion of outcomes", rot=90,
                                       gp=gpar(fontsize=16, fontface="bold")), 
                         sub=textGrob("% change in r ", 
                                      gp=gpar(fontsize=16, fontface="bold"), vjust=-2)))
dev.off()

###############
# Time series length experiment

#start with successes

Nyears.experiment.correct<-summarize.results[which(summarize.results$noise==5 & 
                                                     summarize.results$changeK==40 & summarize.results$changeR==20 &
                                                     summarize.results$victory==1),]
Nyearsplot.correct<-ggplot(Nyears.experiment.correct, aes(Nyears, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("Series length")+
  ylab("proportion of outcomes")+
  xlim(24,35)+ylim(-0.2,1.1)

Nyearsplot.correct

#next do partial successes
# case 2 is extra breaks found

Nyears.experiment.extra<-summarize.results[which(summarize.results$noise==5 & 
                                                   summarize.results$changeK==40 & summarize.results$changeR==20 &
                                                   summarize.results$victory==2),]

Nyearsplot.extra<-ggplot(Nyears.experiment.extra, aes(Nyears, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal.nozero)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("Series length ")+
  ylab("proportion of outcomes")+
  xlim(24,35)+ylim(-0.2,1.1)

Nyearsplot.extra

#missing breaks in output

Nyears.experiment.missing<-summarize.results[which(summarize.results$noise==5 & 
                                                     summarize.results$changeK==40 & summarize.results$changeR==20 &
                                                     summarize.results$victory==3),]

Nyearsplot.missing<-ggplot(Nyears.experiment.missing, aes(Nyears, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal.notwo)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("Series length")+
  ylab("proportion of outcomes")+
  xlim(24,35)+ylim(-0.2,1.1)

Nyearsplot.missing

#mismatched breaks in output

Nyears.experiment.mismatch<-summarize.results[which(summarize.results$noise==5 & 
                                                      summarize.results$changeK==40 & summarize.results$changeR==20 &
                                                      summarize.results$victory==4),]

Nyearsplot.mismatch<-ggplot(Nyears.experiment.mismatch, aes(Nyears, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal.noone)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("Series length ")+
  ylab("proportion of outcomes")+
  xlim(24,35)+ylim(-0.2,1.1)

Nyearsplot.mismatch

#complete failure to find breaks in output

Nyears.experiment.fail<-summarize.results[which(summarize.results$noise==5 & 
                                                  summarize.results$changeK==40 & summarize.results$changeR==20 &
                                                  summarize.results$victory==0),]

Nyearsplot.fail<-ggplot(Nyears.experiment.fail, aes(Nyears, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("Series length ")+
  ylab("proportion of outcomes")+
  xlim(24,35)+ylim(-0.2,1.1)

Nyearsplot.fail

#stack plots together
library(gridExtra)
library(grid)

Nyearsplot.correct1<-Nyearsplot.correct+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=10)+
  annotate("text", x=34.2, y=1.03, label="A", size=5)

Nyearsplot.extra1<-Nyearsplot.extra+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=10)+
  annotate("text", x=34.2, y=1.03, label="B", size=5)

Nyearsplot.missing1<-Nyearsplot.missing+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=10)+
  annotate("text", x=34.2, y=1.03, label="C", size=5)

Nyearsplot.mismatch1<-Nyearsplot.mismatch+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=10)+
  annotate("text", x=34.2, y=1.03, label="D", size=5)

Nyearsplot.fail1<-Nyearsplot.fail+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=10)+
  annotate("text", x=34.2, y=1.03, label="E", size=5)


grid.arrange(arrangeGrob(arrangeGrob(Nyearsplot.correct1, Nyearsplot.extra1, ncol=2), 
                         arrangeGrob(Nyearsplot.missing1, Nyearsplot.mismatch1, ncol=2),
                         arrangeGrob(Nyearsplot.fail1, ncol=1, widths=0.6), ncol=1,
                         left=textGrob("\n                  Proportion of outcomes", rot=90,
                                       gp=gpar(fontsize=16, fontface="bold")), 
                         sub=textGrob("Series length ", 
                                      gp=gpar(fontsize=16, fontface="bold"), vjust=-2)))



pdf("figs/Nyears_simulation_outcomes.pdf", height=10, width=5)
grid.arrange(arrangeGrob(arrangeGrob(Nyearsplot.correct1, Nyearsplot.extra1, ncol=2), 
                         arrangeGrob(Nyearsplot.missing1, Nyearsplot.mismatch1, ncol=2),
                         arrangeGrob(Nyearsplot.fail1, ncol=1, widths=0.6), ncol=1,
                         left=textGrob("\n                  Proportion of outcomes", rot=90,
                                       gp=gpar(fontsize=16, fontface="bold")), 
                         sub=textGrob("Series length ", 
                                      gp=gpar(fontsize=16, fontface="bold"), vjust=-2)))
dev.off()


###############
# Outcome by incoming data experiment

#now we're looking at the data in the oposite way- so, we have an outcome, what is the 
# probability of error of that outcome
summarize.results.breaksout<-count(simulation.results,
                         c("Nyears", "noise", "nbreaksin","nbreaksout",
                            "changeK", "changeR"))

#cut out scenarios changing k and R abecause this wouldn't be 'known' from this side

breaksout.results<-summarize.results.breaksout[which(summarize.results.breaksout$Nyears==25 & 
                                                       summarize.results.breaksout$changeK==40 & 
                                                       summarize.results.breaksout$changeR==20 & 
                                                       summarize.results.breaksout$nbreaksout<4),]
breaksout.tot.tests<-ddply(breaksout.results,
                                   c("Nyears", "noise", "nbreaksout",
                                     "changeK", "changeR"), summarise,
                           tot.tests=sum(freq))


#count the number of times a unique scenario was attemped (should be pretty uniform but 
# there are someetimes cases where the fit failed) (for a denominator!)


summarize.results.breaksout.1<-merge(breaksout.results, breaksout.tot.tests)


summarize.results.breaksout.1$proportion<-summarize.results.breaksout.1$freq/summarize.results.breaksout.1$tot.tests

# Scenario- 0 breaks observed
breaksout.results.0<-summarize.results.breaksout.1[which(summarize.results.breaksout.1$nbreaksout==0),]


#plot by outcome
pal<-c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c")

noiseplot.breaksout.0<-ggplot(breaksout.results.0, aes(noise, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Actual number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% noise")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

noiseplot.breaksout.0

# Scenario- 1 breaks observed
breaksout.results.1<-summarize.results.breaksout.1[which(summarize.results.breaksout.1$nbreaksout==1),]


#plot by outcome
pal<-c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c")

noiseplot.breaksout.1<-ggplot(breaksout.results.1, aes(noise, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Actual number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% noise")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

noiseplot.breaksout.1

# Scenario- 2 breaks observed
breaksout.results.2<-summarize.results.breaksout.1[which(summarize.results.breaksout.1$nbreaksout==2),]


#plot by outcome
pal<-c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c")

noiseplot.breaksout.2<-ggplot(breaksout.results.2, aes(noise, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Actual number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% noise")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

noiseplot.breaksout.2

# Scenario- 3 breaks observed
breaksout.results.3<-summarize.results.breaksout.1[which(summarize.results.breaksout.1$nbreaksout==3),]


#plot by outcome
pal<-c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c")

noiseplot.breaksout.3<-ggplot(breaksout.results.3, aes(noise, proportion, fill=as.factor(nbreaksin)))+
  scale_fill_manual(values=pal)+
  geom_smooth(method="gam", se=TRUE, formula=y ~ poly(x, 3), span=0.1)+
  geom_point(colour="black", pch=21, size=3)+
  theme_bw(base_size = 12)+
  guides(fill=guide_legend(title="Actual number\nof breaks"))+
  theme(legend.key=element_blank())+
  xlab("% noise")+
  ylab("proportion of outcomes")+
  xlim(0,90)+ylim(-0.2,1.1)

noiseplot.breaksout.3


#stack plots together
library(gridExtra)
library(grid)

noiseplot.breaksout.01<-noiseplot.breaksout.0+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="A", size=5)

noiseplot.breaksout.11<-noiseplot.breaksout.1+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="B", size=5)

noiseplot.breaksout.21<-noiseplot.breaksout.2+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="C", size=5)

noiseplot.breaksout.31<-noiseplot.breaksout.3+
  guides(fill=FALSE)+
  ylab(NULL)+
  xlab(NULL)+
  coord_fixed(ratio=80)+
  annotate("text", x=85, y=1.03, label="D", size=5)

#pull legend out of plot
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

leg<-g_legend(noiseplot.breaksout.3)

#create a blank grob to hold space where the legend would go next to D
blank <- grid.rect(gp=gpar(col="white"))

grid.arrange(arrangeGrob(arrangeGrob(noiseplot.breaksout.01, noiseplot.breaksout.11, leg, ncol=3, widths=c(30,30,40)), 
                         arrangeGrob(noiseplot.breaksout.21, noiseplot.breaksout.31, blank,  ncol=3, widths=c(30,30,40)),
                         ncol=1,
                         left=textGrob("\n                  Proportion of outcomes", rot=90,
                                       gp=gpar(fontsize=16, fontface="bold")), 
                         sub=textGrob("% noise", 
                                      gp=gpar(fontsize=16, fontface="bold"), vjust=-2)))



pdf("figs/observed_outcomes.pdf", height=5, width=5)
grid.arrange(arrangeGrob(arrangeGrob(noiseplot.breaksout.01, noiseplot.breaksout.11, leg, ncol=3, widths=c(35,35,30)), 
                         arrangeGrob(noiseplot.breaksout.21, noiseplot.breaksout.31, blank,  ncol=3, widths=c(35,35,30)),
                         ncol=1,
                         left=textGrob("\n                  Proportion of outcomes", rot=90,
                                       gp=gpar(fontsize=16, fontface="bold")), 
                         sub=textGrob("% noise", 
                                      gp=gpar(fontsize=16, fontface="bold"), vjust=-2)))

dev.off()
