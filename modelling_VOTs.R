##################################################################

# modelling VOT
# Author: Eugen Klein, January 2014
##################################################################

# load lme4 for linear mixed modeling
library(lme4)
library(lmerTest)
# qqplots
library(lattice)
#
library(languageR)
# load ggplot2 for plots
library(ggplot2)
# load plyr for data frame manipulatios
library(plyr)


##################################################################
#### compile a model
##################################################################

# with distractor VOT as continous predictor
m1 = lmer(log.vot ~ distractor.log.vot +
            pre.vot +
            distractor.condition +
            response +
            (1 + response |subject.ID) +
            (1|block), data = big, REML = T) # observation

m6 = lmer(log.vot ~ distractor.log.vot +
            pre.vot +
            distractor.condition +
            response +
            (1 + response + distractor.log.vot |subject.ID) +
            (1|observation), data = big, REML = T)

m8 = lmer(log.vot.ratio ~ distractor.log.vot +
            pre.vot +
            distractor.condition +
            response +
            (1 + response + distractor.log.vot |subject.ID) +
            (1|observation), data = big, REML = T)

m7 = lmer(log.vot ~ distractor.log.vot +
            pre.vot +
            distractor.condition +
            response +
            (1 + response + distractor.log.vot |subject.ID) +
            (1|observation), data = big, REML = T)

# with descrete VOT steps as factor m3 / with steps as numeral
m2 = lmer(log.vot ~ steps +
             pre.log.vot +
             distractor.condition +
             response +
             (1|subject.ID) +
             (1|block), data = big, REML = T)

# 
m5 = lmer(log.vot ~ distractor.log.vot +
            pre.vot +
            pairs +
            response +
            (1 + response |subject.ID) +
            (1|block), data = big, REML = T)

# effect of tone on VOT
m4 = lmer(log.vot ~ pre.vot +
            distractor.condition +
            response +
            (1|subject.ID) +
            (1|block), data = big, REML = T)


##################################################################
#### model diagnostics
##################################################################

qqnorm(resid(m1))
plot(fitted(m1), resid(m1))
qqnorm(ranef(m1)$subject.ID[[1]])

##################################################################
# trial to trial vot correlation
##################################################################

acf.fnc(big, group = "subject.ID", time = "observation", x = "response.vot", plot = TRUE)

##################################################################
# plot VOTs scaterplot evolution for each participant
##################################################################

for (i in 1:length(IdList)){
  
  p = ggplot(big[big$subject.ID == IdList[i],], aes(x=observation, y=center.vot,
                                                                colour = distractor.condition))
  # method "lm" linear model; loess - curvy
  (p + geom_point(size = 1.5)
   + geom_smooth(method=loess, se=F, size = 1.5)
   + ggtitle(paste("VOTs", strsplit(fileList[i], split = "\\.")[[1]][1]))
   + labs(x = "Trial number", y = "Participant's VOT (in ms)", colour = "Distractor's condition")
   + theme_bw()
   + theme(text = element_text(size=26))
   + coord_cartesian(ylim=c(-50,50)))
  
  ggsave(filename = paste(savDir, "VOTs_", strsplit(fileList[i], split = "\\.")[[1]][1],".jpg", sep = "", collapse = NULL),
         width = 16, height = 11.8, pointsize = 20)
  
}

##################################################################
# scatter for all participants in panels
##################################################################

# for ka responses
p = ggplot(aes(x = distractor.vot, y = center.vot), # steps
                    data = big[big$response == 'ka',])

# for ta responses
p = ggplot(aes(x = distractor.vot, y = center.vot),
           data = big[big$response == 'ta',])

p +
  #geom_point() +
  geom_smooth(method=lm, colour="blue", size = 1, fullrange=TRUE) +
  #theme_classic() +
  theme(text = element_text(size=18)) +
  geom_vline(xintercept=0, colour = 'red', size = 0.25) +
  facet_wrap(~subject.ID) +
  coord_cartesian(ylim=c(-20,20)) +
  ggtitle("Centered VOT (ms) for /ka/ responses") +
  labs(x = "Distractor VOT",
       y = "Response VOT")


##################################################################
### VOT scatterplots for each participant
##################################################################

for (i in 1:length(IdList)){
  
  p = ggplot(na.omit(big[big$subject.ID == IdList[i], ]), # na.omit
             aes(x = distractor.vot, y= center.vot)) # x = distractor.vot
  
  (p + (geom_point(shape=1, alpha=1)) +geom_smooth(method=lm, colour="blue", size = 1)
   + geom_hline(yintercept=0, colour = 'red', size = 0.5)
   + geom_vline(xintercept=0, colour = 'red', size = 0.5)
   + scale_fill_brewer(palette = "Set2")
   + facet_wrap(~response) # pairs
   
   + ggtitle(paste("VOTs (ms),", strsplit(fileList[i], split = "\\.")[[1]][1]))
   + labs(x = "Distractor's VOT centered around response mean", y = "Response VOT centered around response mean")
   + theme_bw()
   + theme(text = element_text(size=26))
   + coord_cartesian(ylim=c(-50,50)))
  
  ggsave(filename = paste(savDir, "VOT_scatter_", strsplit(fileList[i], split = "\\.")[[1]][1],".jpg", sep = "", collapse = NULL),
         width = 16, height = 11.8, pointsize = 20)
  
}

for (i in 1:length(IdList)){

  jpeg(filename=paste(savDir, "VOT_scatter_", strsplit(fileList[i], split = "\\.")[[1]][1],".jpeg", sep = "", collapse = NULL))
  print(xyplot(response.vot ~ distractor.vot|pairs, big[big$subject.ID == IdList[i], ],
         group = pairs,
         grid = TRUE,
         type = c("p", "lm"), col.line = "darkorange", lwd = 3))
  dev.off()
  
}

########## all 40 participants

# positive slopes
p = ggplot(na.omit(big[big$subject.ID == 3|big$subject.ID == 4|big$subject.ID == 5|
                         big$subject.ID == 7|big$subject.ID == 13|big$subject.ID == 17|
                         big$subject.ID == 19|big$subject.ID == 23|big$subject.ID == 25|
                         big$subject.ID == 27|big$subject.ID == 29|big$subject.ID == 32|
                         big$subject.ID == 35|big$subject.ID == 36|big$subject.ID == 38|
                         big$subject.ID == 39|big$subject.ID == 40|big$subject.ID == 41|
                         big$subject.ID == 42|big$subject.ID == 43, ]),
           aes(x = distractor.vot, y= center.vot))

# rest
p = ggplot(na.omit(big[big$subject.ID == 6|big$subject.ID == 8|big$subject.ID == 8|
                         big$subject.ID == 11|big$subject.ID == 12|big$subject.ID == 14|
                         big$subject.ID == 15|big$subject.ID == 16|big$subject.ID == 20|
                         big$subject.ID == 21|big$subject.ID == 24|big$subject.ID == 26|
                         big$subject.ID == 31|big$subject.ID == 33|big$subject.ID == 34|
                         big$subject.ID == 37|big$subject.ID == 44|big$subject.ID == 45|
                         big$subject.ID == 46|big$subject.ID == 47, ]),
           aes(x = distractor.vot, y= center.vot))

(p #+ (geom_point(shape=1, alpha=1))
 + geom_smooth(method=lm, colour="black", size = .5, fullrange=TRUE)
 + geom_hline(yintercept=0, colour = 'grey', size = 0.25, linetype="dotted")
 + geom_vline(xintercept=0, colour = 'grey', size = 0.25, linetype="dotted")
 + scale_fill_brewer(palette = "Set2")
 + facet_wrap(~subject.ID, scales='free_x')
 
 #+ ggtitle("VOTs (ms), all")
 + labs(x = "Distractor VOT", y = "Response VOT")

 + theme_classic()
 + theme(text = element_text(size=30),
         axis.text.x = element_text(size=16),
         axis.text.y = element_text(size=16))
 + coord_cartesian(ylim=c(-5,5)))
#+ scale_y_continuous(breaks=c(-3,0,3)))

##################################################################
# plot each participant's VOTs with distractor's VOT separated
##################################################################

for (i in 1:length(IdList)){
  
  mm = ddply(big, c("response", "steps", "distractor.condition"), summarise, mvot = mean(response.vot, na.rm = TRUE),
             sevot = sd(response.vot, na.rm=TRUE)/sqrt(length(response.vot)))
  mm[mm$distractor.condition == "none",]$steps = 10
  mm[mm$distractor.condition == "tone",]$steps = 10
  
  mm = mm[!(mm$steps == -5 | mm$steps == -4| mm$steps == -3| mm$steps ==-2| mm$steps ==5| mm$steps ==6),]
  
  p = ggplot(mm, aes(fill = factor(steps), x = factor(distractor.condition), y = mvot))
  (p + geom_bar(stat="identity", position = "dodge")
   + ggtitle(paste("VOTs", strsplit(fileList[i], split = "\\.")[[1]][1]))
   + labs(x = "Distractor' VOT ", y = "Participant's VOT (in ms)",
          fill = "Distractor condition")
   + facet_wrap(~response)
   + geom_errorbar(aes(ymin=mvot-sevot, ymax=mvot+sevot), position=position_dodge(.9), width = .3)
   + scale_fill_grey(start = .8, end = .3) #scale_fill_brewer(palette = "Blues")
   + theme_classic()
   + theme(text = element_text(size=30),
           axis.text.x = element_text(size=16),
           axis.text.y = element_text(size=16)))
   #+ coord_cartesian(ylim=c(50,110)))
  
  ggsave(filename = paste(savDir, "VOTs_withVOT_", strsplit(fileList[i], split = "\\.")[[1]][1],".jpg", sep = "", collapse = NULL),
         width = 16, height = 11.8, pointsize = 20)
  
  
}
  
##################################################################
# none/tone densities for all participants in panels
##################################################################

# define no-distractor subset
ka.frame = big[(big$distractor.condition == "none" |
                 big$distractor.condition == "tone") &
                 big$response == "ka",]
ka.frame$distractor.condition = NULL
ka.frame$pairs = NULL

# define tone-distractor subset
ta.frame = big[(big$distractor.condition == "none" |
                 big$distractor.condition == "tone") &
                 big$response == "ta",]
ta.frame$distractor.condition = NULL
ta.frame$pairs = NULL

p = ggplot(aes(x = center.vot, y = ..density..),
           data = ka.frame)

p = ggplot(aes(x = center.vot, y = ..density..),
           data = ta.frame)

p = ggplot(aes(x = center.vot, y = ..density..),
           data = big)

p + geom_density(aes(colour = distractor.condition), size=0.25) +
  facet_wrap(~subject.ID) +
  ggtitle("VOTs (ms) for /ka/ responses") +
  labs(x = "Response VOT centered around response mean", y = "Density",
       colour = 'Condition') +
  theme_bw() +
  theme(text = element_text(size=26)) +
  coord_cartesian(ylim=c(-5,5)))

##################################################################
#### plotting accommodation of VOTs scatterplot
##################################################################

# compute distance between participant's VOT and distractor VOT
big$distance[big$distractor.condition == "match"] = (
  big$response.vot[big$distractor.condition == "match"]
  -big$vot.step[big$distractor.condition == "match"])

# compute distance between participant's VOT and distractor VOT
big$distance[big$distractor.condition == "mismatch"] = (
  big$response.vot[big$distractor.condition == "mismatch"]
  -big$vot.step[big$distractor.condition == "mismatch"])

# compute distance between participant's mean VOT and distractor VOT
# for match condition
big$distance[big$distractor.condition == "match"] = (
  big$response.vot[big$distractor.condition == "match"]
  -big$vot.step[big$distractor.condition == "match"])

# compute distance between participant's mean VOT and distractor VOT
# for mismatch condition
big$distance[big$distractor.condition == "mismatch"] = (
  big$response.vot[big$distractor.condition == "mismatch"] 
  -big$vot.step[big$distractor.condition == "mismatch"])

# replace tone condition with none condition
big$distractor.condition[big$distractor.condition == "tone"] = "none"

# compute distance from mean VOT from none distractor trials
big$distance[big$distractor.condition == "none"] = (
  big$response.vot[big$distractor.condition == "none"] -
    mean(big$response.vot[big$distractor.condition == "none"]))

# compute distance from mean VOT from non-speech distractor trials
big$distance[big$distractor.condition == "tone"] = (
  big$response.vot[big$distractor.condition == "tone"] -
    mean(big$response.vot[big$distractor.condition == "tone"]))

p = ggplot(big, aes(x=observation, y=c(distance),
                          color=distractor.condition))
# method "lm" linear model; loess - curvy
p +(geom_point(shape=4, alpha=0.1)) +geom_smooth(method=lm, se=FALSE)+ #alpha=0.2
  ggtitle("Accommodation of VOT (measured at onset of F0)") +
  labs(x = "Trial number",
       y = "Distance between Response's and Distractor's VOT",
       color = "Distractor's condition")
#coord_cartesian(ylim=c(-100,150))

##################################################################
#### no-distractor condition: plot VOT distribution for each participant
##################################################################

for (i in 1:length(IdList)){
  
  p = ggplot(big[big$subject.ID == IdList[i] & big$vot.step == "none",],
             aes(x=response.vot, y = ..density..))
  
  
  mka = round(mean(big[big$subject.ID == IdList[i] & big$vot.step == "none"
                             & big$response == "ka",]$response.vot))
  mta = round(mean(big[big$subject.ID == IdList[i] & big$vot.step == "none"
                             & big$response == "ta",]$response.vot))
  sdka = round(sd(big[big$subject.ID == IdList[i] & big$vot.step == "none"
                            & big$response == "ka",]$response.vot))
  sdta = round(sd(big[big$subject.ID == IdList[i] & big$vot.step == "none"
                            & big$response == "ta",]$response.vot))
  (p + geom_histogram(colour = "blue", fill = "grey",, binwidth=1) + geom_density()
   + ggtitle(paste("No-distractor VOTs", strsplit(fileList[i], split = "\\.")[[1]][1], "Gender: ", 
                   big[big$subject.ID == IdList[i],]$gender[1]))
   + facet_wrap(~response)
   + labs(x = paste("KA mean =", mka, "ms", "SD =", sdka, "         TA mean =", mta, "ms", "SD =", sdta), y = "Density")
   + coord_cartesian(xlim=c(20,150)))
  
  ggsave(filename = paste(savDir, "VOT_no_dist_", strsplit(fileList[i], split = "\\.")[[1]][1],".jpg", sep = "", collapse = NULL),
         width = 16, height = 11.8, pointsize = 20)
  
}

##################################################################
#  modify data for VOT density plots
##################################################################

# add tone and none conditions as VOT step
big$vot.step[big$distractor.condition == "tone"] = "tone"
big$vot.step[big$distractor.condition == "none"] = "none"

# order the vot condition and code them as factors
big$vot.step = (factor(big$vot.step,
                             c("none", "tone", "45", "60", "75", "90", "105", "120")))

##################################################################
# include the no-distractor condition as zero
##################################################################

big[big$distractor.condition == 'none',]$steps = as.character('no')

# order the vot condition and code them as factors
big$steps = (factor(big$steps,
                          c("-5", "-4", "-3", "-2", "-1", "no", "1", "2", "3",
                            "4", "5", "6")))

##################################################################
# define sub-frames
##################################################################

# define no-distractor subset fo ka
no.frame = big[big$distractor.condition == "none",]
no.frame$distractor.condition = NULL
no.frame$pairs = NULL

# define no-distractor subset fo ta
no.frame.ta = big[big$response == "ta" & 
                    big$distractor.condition == "none",]
no.frame.ta$distractor.condition = NULL
no.frame.ta$pairs = NULL

# define tone-distractor subset for ka
to.frame.ka = big[big$response == "ka" & 
                    big$distractor.condition == "tone",]
to.frame.ka$distractor.condition = NULL
to.frame.ka$pairs = NULL

# define tone-distractor subset for ta
to.frame.ta = big[big$response == "ta" & 
                    big$distractor.condition == "tone",]
to.frame.ta$distractor.condition = NULL
to.frame.ta$pairs = NULL

# define speech subset
speech.frame1 = big[big$distractor.condition == "match",]
speech.frame2 = big[big$distractor.condition == "mismatch",]
speech.frame = rbind(speech.frame1, speech.frame2)
rm(list = "speech.frame1","speech.frame2")

##################################################################
#### plot VOT distributions for each participant with steps
##################################################################

# plotting the ka responses
for (i in 1:length(IdList)){
  
  # means for PPH
  M = mean(no.frame.ka[no.frame.ka$subject.ID == IdList[i], ]$response.vot)
  SD = sd(no.frame.ka[no.frame.ka$subject.ID == IdList[i], ]$response.vot)
  M = round(M, digits = 2)
  SD = round(SD, digits = 2)
  
  # create plot for ka responses
  p = ggplot(speech.frame[speech.frame$subject.ID == IdList[i] &
                            speech.frame$response == 'ka', ],
             aes(x = response.vot, color=factor(steps)))
  
  (p + geom_density(size=1)
   + scale_fill_brewer(palette = "Blues")
   + facet_wrap(~pairs)
   + geom_density(data=no.frame.ka[no.frame.ka$subject.ID == IdList[i],], aes(response.vot), size=2, colour="black")
   #+ geom_density(data=to.frame.ka[to.frame.ka$subject.ID == IdList[i],], aes(response.vot), size=1.5, colour="red")
   + ggtitle(paste("VOTs, ka-responses,", strsplit(fileList[i], split = "\\.")[[1]][1]))
   + labs(x = paste("PPH: M =", M, "SD =", SD), y = "Density", color = "Distractor attractors")
   + theme_bw()
   + theme(text = element_text(size=26))
   + coord_cartesian(xlim=c(-50,50)))
  
  ggsave(filename = paste(savDir, "VOT_dens_ka_", strsplit(fileList[i], split = "\\.")[[1]][1],".jpg", sep = "", collapse = NULL),
         width = 16, height = 11.8, pointsize = 20)
  
}

# plotting the ta responses
for (i in 1:length(IdList)){
  M = mean(no.frame.ta[no.frame.ta$subject.ID == IdList[i], ]$response.vot)
  SD = sd(no.frame.ta[no.frame.ta$subject.ID == IdList[i], ]$response.vot)
  M = round(M, digits = 2)
  SD = round(SD, digits = 2)
  
  p = ggplot(speech.frame[speech.frame$subject.ID == IdList[i] &
                            speech.frame$response == 'ta', ],
             aes(x = response.vot, color=factor(steps)))
  
  (p + geom_density(size=1)
   + scale_fill_brewer(palette = "Blues")
   + facet_wrap(~pairs)
   + geom_density(data=no.frame.ta[no.frame.ta$subject.ID == IdList[i],], aes(response.vot), size=2, colour="black")
   #+ geom_density(data=to.frame.ta[to.frame.ta$subject.ID == IdList[i],], aes(response.vot), size=1.5, colour="red")
   + ggtitle(paste("VOTs, ta-responses,", strsplit(fileList[i], split = "\\.")[[1]][1]))
   + labs(x = paste("PPH: M =", M, "SD =", SD), y = "Density", color = "Distractor attractors")
   + theme_bw()
   + theme(text = element_text(size=26))
   + coord_cartesian(xlim=c(-50,50)))
  
  ggsave(filename = paste(savDir, "VOT_dens_ta_", strsplit(fileList[i], split = "\\.")[[1]][1],".jpg", sep = "", collapse = NULL),
         width = 16, height = 11.8, pointsize = 20)
  
}

##################################################################
#### plotting VOTs as box plot
##################################################################

# replace tone condition with none condition
big$distractor.condition[big$distractor.condition == "tone"] = "none"
# order the condition for VOTs
big$condition = (factor(big$distractor.condition,
                              c("none", "match", "mismatch")))

# plot VOTs
p = ggplot(big, aes(factor(condition), response.vot))
(p + geom_boxplot(aes(fill = factor(vot.step)))
 + ggtitle("VOT measured at onset of periodicity")
 + labs(x = "Distractor", y = "Participant's VOT",
        fill = "Distractor's VOT"))
#+ coord_cartesian(ylim=c(40,150)))

##################################################################
#### plot VOTs for several participants
##################################################################

# add tone and none conditions as VOT step
big$vot.step[big$distractor.condition == "tone"] = "tone"
big$vot.step[big$distractor.condition == "none"] = "none"

# order the condition for VOTs and code them as factors
big$vot.step = (factor(big$vot.step,
                             c("none", "tone", "45", "60", "75", "90", "105", "120")))
# order the condition for rts
big$condition.rt = (factor(big$distractor.condition,
                                 c("none", "identity", "voicing match", "tone" )))

# plot VOTs for several participants
p = ggplot(big, aes(factor(condition.rt), normal.vot))
(p + geom_boxplot(aes(factor(subject.ID), fill = factor(vot.step)))
 + ggtitle("VOT measured at onset of periodicity")
 + labs(x = "Participant", y = "Participant's VOT",
        fill = "Distractor's VOT"))


