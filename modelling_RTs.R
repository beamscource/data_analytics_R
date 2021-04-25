##################################################################
# modelling reaction times
# Author: Eugen Klein, Dezember 2014
##################################################################

# for linear mixed modeling
library(lme4)
# for p-values in linear mixed modeling
library(lmerTest)
# 
library(LMERConvenienceFunctions)
# for changing contrasts
library(MASS)
# for qqplots
library(lattice)
# for qqplots
library(languageR)
# load ggplot2 for plots
library(ggplot2)
# load plyr for data frame manipulatios
library(plyr)


# order the levels of distractor condition
big$distractor.condition = (factor(big$distractor.condition,
                                  c("none", "tone", "match", "mismatch")))

# define sliding contrasts
contrasts(big$distractor.condition.sliding) = contr.sdif(4)
# default contrast treatment
contrasts(big$distractor.condition) = contr.treatment(4)

##################################################################
#### compile a model
##################################################################

m1rt = lmer(log.rt ~ distractor.condition +
              pre.log.rt +
              response +
              (1 |subject.ID) +
              (1|block), data = big, REML = T)

m2rt = lmer(log.rt ~ distractor.condition.sliding +
              pre.log.rt +
              response +
              (1 + response |subject.ID) +
              (1|observation), data = big, REML = T)

m2 = glmer(rt ~ distractor.condition + pre.rt +
             (1 |subject.ID) +
             (1|block), data = big, family = inverse.gaussian(link = "1/mu^2"),
              REML = F)

##################################################################
#### model diagnostics
##################################################################

qqnorm(resid(m1))
plot(fitted(m1), resid(m1))
qqnorm(ranef(m1)$subject.ID[[1]])

##################################################################
# plot each participant's RTs as bar plot
##################################################################

for (i in 1:length(IdList)){
  # calculate the mean RT for each condition
  mm = ddply(big[big$subject.ID == IdList[i],], "distractor.condition", summarise, mrt = mean(rt),
             sert = sd(rt)/sqrt(length(rt)))
  # order the condition for RTs
  mm$distractor.condition = (factor(mm$distractor.condition,
                                    c("none", "tone", "match", "mismatch")))
  
  p = ggplot(mm, aes(x = factor(distractor.condition), y = mrt, fill=factor(distractor.condition)))
  (p + geom_bar(stat = "identity")
   + ggtitle(paste("RTs", strsplit(fileList[i], split = "\\.")[[1]][1]))
   + labs(x = "Distractor condition", y = "Reaction times (in ms)", fill= "Distractor condition")
   + scale_fill_brewer(palette = "Set2")
   + theme_bw()
   + theme(text = element_text(size=26))
   + geom_errorbar(aes(ymin=mrt-sert, ymax=mrt+sert), width=.1)
   + coord_cartesian(ylim=c(325,825)))
  
  ggsave(filename = paste(savDir, "RTs_", strsplit(fileList[i], split = "\\.")[[1]][1],".jpg", sep = "", collapse = NULL),
         width = 16, height = 11.8, pointsize = 20)
  
  rm(list = "p","mm")
}

##################################################################
# plot mean RTs as bar plot
##################################################################

# calculate the mean RT for each condition
mm = ddply(big, "distractor.condition", summarise, mrt = mean(rt),
           sert = sd(rt)/sqrt(length(rt)))
# order the condition for RTs
mm$distractor.condition = (factor(mm$distractor.condition,
                                  c("none", "match", "mismatch"))) # tone

p = ggplot(na.omit(mm), aes(x = distractor.condition, y = mrt, fill = distractor.condition))
(p + geom_bar(stat = 'identity') #
 #+ ggtitle("Mean RTs for all participants")
 + labs(y = "RTs (in ms)") # fill = "Distractor condition") # x = "Distractor condition",
 + geom_errorbar(aes(ymin=mrt-sert, ymax=mrt+sert), width=.1)
 #+ scale_fill_brewer(palette = "Set2")
 + scale_fill_grey(start = .8, end = .5)
 #theme with white background
 + theme_classic()
 + theme(text = element_text(size=30))
#  +
#    #eliminates background, gridlines, and chart border
#    theme(
#      plot.background = element_blank()
#      ,panel.grid.major = element_blank()
#      ,panel.grid.minor = element_blank()
#      ,panel.border = element_blank()
#    ) +
#    #draws x and y axis line
#    theme(axis.line = element_line(color = 'black'))
 + coord_cartesian(ylim=c(450,510)))

ggsave(filename = paste(savDir, "RTs_mean.jpg", sep = "", collapse = NULL),
       width = 16, height = 11.8, pointsize = 20)

##################################################################
# plot each participant's RTs with distractor's VOT separated
##################################################################

# define speech subset
speech.frame1 = big[big$distractor.condition == "match",]
speech.frame2 = big[big$distractor.condition == "mismatch",]
speech.frame = rbind(speech.frame1, speech.frame2)
rm(list = "speech.frame1","speech.frame2")

for (i in 1:length(IdList)){
  
  mm = ddply(speech.frame[speech.frame$subject.ID == IdList[i],], c("steps", "distractor.condition"), summarise, mrt = mean(rt),
             sert = sd(rt)/sqrt(length(rt)))
  
  p = ggplot(mm, aes(fill = factor(steps), x = factor(distractor.condition), y = mrt))
  (p + geom_bar(stat="identity", position = "dodge")
   + ggtitle(paste("RTs", strsplit(fileList[i], split = "\\.")[[1]][1]))
   + labs(x = "Distractor' VOT ", y = "Participant's RT (in ms)",
          fill = "Distractor condition")
   + geom_errorbar(aes(ymin=mrt-sert, ymax=mrt+sert), position=position_dodge(.9), width = .3)
   + scale_fill_brewer(palette = "Blues")
   #theme with white background
   + theme_bw()
   + theme(text = element_text(size=26))
   + coord_cartesian(ylim=c(350,850)))
  
  ggsave(filename = paste(savDir, "RTs_withVOT_", strsplit(fileList[i], split = "\\.")[[1]][1],".jpg", sep = "", collapse = NULL),
         width = 16, height = 11.8, pointsize = 20)
  
  
}

##################################################################
# plot mean RTs with distractor's VOT separated 1
##################################################################

mm = ddply(speech.frame, c("steps", "distractor.condition", "response"), summarise, mrt = mean(rt),
           sert = sd(rt)/sqrt(length(rt)))

# get rid of steps with too few observtions
mm = mm[c(7:38),]

# # order the condition for RTs
# mm$distractor.condition = (factor(mm$distractor.condition,
#                                   c("tone", "none", "match", "mismatch")))

p = ggplot(mm, aes(fill = factor(steps), x = factor(distractor.condition), y = mrt))
(p + geom_bar(stat="identity", position = "dodge")
 #+ ggtitle("Mean RTs")
 + labs(y = "RTs (in ms)")
        #fill = "Distractor VOT")
 + geom_errorbar(aes(ymin=mrt-sert, ymax=mrt+sert), position = position_dodge(.9), width = .3)
 + facet_wrap(~response)
 + scale_fill_grey(start = .8, end = .3) #scale_fill_brewer(palette = "Set2")
 + theme_classic() #theme_bw()
 + theme(text = element_text(size=26))
 + coord_cartesian(ylim=c(450,570)))

ggsave(filename = paste(savDir, "RTs_withVOT_mean.jpg", sep = "", collapse = NULL),
       width = 16, height = 11.8, pointsize = 20)

##################################################################
# plot mean RTs with distractor's VOT separated 2
##################################################################

mm = ddply(speech.frame, c("vot.step", "distractor.condition"), summarise, mrt = mean(rt),
           sert = sd(rt)/sqrt(length(rt)))
# # order the condition for RTs
# mm$distractor.condition = (factor(mm$distractor.condition,
#                                   c("tone", "none", "match", "mismatch")))

p = ggplot(mm, aes(fill = factor(distractor.condition), x = factor(vot.step), y = mrt))
(p + geom_bar(stat="identity", position = "dodge")
 + ggtitle("Mean RTs")
 + labs(x = "Distractor's VOT", y = "Participant's RT (in ms)",
        fill = "Distractor condition")
 + geom_errorbar(aes(ymin=mrt-sert, ymax=mrt+sert), position = "dodge", width=0.25)
 + scale_fill_brewer(palette = "Set2")
 + coord_cartesian(ylim=c(400,600)))

ggsave(filename = paste(savDir, "RTs_withVOT_mean.jpg", sep = "", collapse = NULL),
       width = 16, height = 11.8, pointsize = 20)

##################################################################

mm = ddply(speech.frame[speech.frame$subject.ID == IdList[i],], c("vot.step", "distractor.condition"), summarise, mrt = mean(rt),
           sert = sd(rt)/sqrt(length(rt)))
# order the condition for RTs
mm$distractor.condition = (factor(mm$distractor.condition,
                                  c("tone", "none", "match", "mismatch")))

p = ggplot(mm, aes(x = factor(distractor.condition), y = mrt, fill = factor(vot.step)))
(p + geom_bar(stat="identity", position = "dodge")
 + ggtitle(paste("RTs", strsplit(fileList[i], split = "\\.")[[1]][1]))
 + labs(x = "Distractor condition", y = "Participant's RT (in ms)",
        fill = "Distractor's VOT")
 + scale_fill_brewer(palette = "Blues")
 + coord_cartesian(ylim=c(400,600)))

##################################################################
#### plotting RTs as box plots
##################################################################

# order the condition for RTs
big$condition.rt = (factor(big$distractor.condition,
                           c("none", "tone", "match", "mismatch")))

# plot RTs without distractor VOT separation
p = ggplot(big, aes(factor(condition.rt), logrt))
(p + geom_boxplot()
 + ggtitle("Response times")
 + labs(x = "Distractor", y = "Participant's RT"))

# plot RTs with distractor VOT separated
p = ggplot(big, aes(factor(condition.rt), rt))
(p + geom_boxplot(aes(fill = factor(vot.step)))
 + ggtitle("Response times")
 + labs(x = "Distractor", y = "Participant's RT",
        fill = "Distractor's VOT"))

##################################################################
#### plotting RTs scatterplot
##################################################################

graphics.off()

p = ggplot(big, aes(x=observation, y=c(rt),
                          color=condition.rt))
# method "lm" linear model
p +(geom_point(shape=4, alpha=0.2)) +geom_smooth(method=lm, se=FALSE)+ 
  ggtitle("Perceptuo-motor effects") +
  labs(x = "Trial number",
       y = "Participants' response time",
       color = "Distractor's condition") +scale_y_continuous(limits=c(400,550))
