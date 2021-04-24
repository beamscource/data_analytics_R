##################################################################
# Analysis for IcPhs 15
# Perceptuomotor interactions across and within phonemic categories
# Eugen Klein, 21.01.2015

library(lmerTest)
# for changing contrasts
library(MASS)
# load ggplot2 for plots
library(ggplot2)
# load plyr for data frame manipulatios
library(plyr)

# all analyses are done only for speech.frame (no tone, no none condition)

# define sliding contrasts
contrasts(speech.frame$distractor.condition.sliding) = contr.sdif(4)
# default contrast treatment
contrasts(speech.frame$distractor.condition) = contr.treatment(4)

# define sliding contrasts for right pull
contrasts(exit$distractor.condition) = contr.sdif(4)
contrasts(right.pull$distractor.condition) = contr.treatment(4)
# check number of removed observations
# 1024*40 = 40 960
sum(is.na(speech.frame$log.vot)) #63

# define data subset with speech distractors
left.pull = big[!is.na(big$steps) & (big$steps < -1) & (big$steps > -4),]
left.pull$distractor.site = "negative"
right.pull = big[!is.na(big$steps) & (big$steps > -2) & (big$steps < 6) & !(big$steps == 0),]
right.pull$distractor.site = "positive"
speech.dist = rbind(left.pull, right.pull)
speech.dist$distractor.site = as.factor(speech.dist$distractor.site)

##################################################################
# models
##################################################################

# VOT steps (with less than 500 observations taken out (-5,-4,6)
m1bicphs = lmer(log.rt ~ distractor.condition +
                  pre.log.rt +
                  steps +
                 distractor.condition:steps +
                 distractor.condition:steps:response +
                 distractor.vot +
                 distractor.condition:distractor.vot +
                 distractor.site +
                 (1 + response + distractor.condition |subject.ID) +
                 (1|observation), data = speech.dist, REML = T)

m1aicphs = lmer(log.rt ~ distractor.condition +
                 pre.log.rt +
                 response +
                 distractor.condition:log.vot.step + # added
                 as.factor(soa) +
                 (1 + response + distractor.condition |subject.ID) +
                 (1|observation), data = big[big$distractor.condition == "match"|big$distractor.condition == "mismatch", ], REML = T)

m2icphs = lm(mrt ~ mvot + response, data = vort.log)

# NEGATIVE PULLERS TAKEN OUT FROM THE DATA:
# with tone and no-distractor sliding contrast
m3icphs = lmer(log.rt ~ distractor.condition +
                 pre.log.rt +
                 response +
                 (1 + response|subject.ID) +
                 (1|observation), data = exit, REML = T)

##################################################################
# plot mean RTs as bar plot
##################################################################

# calculate the mean RT for each condition
mm = ddply(na.omit(big), "distractor.condition", summarise, mrt = mean(rt, na.rm = TRUE),
           sert = sd(rt, na.rm = TRUE)/sqrt(length(rt)))
# order the condition for RTs
mm$distractor.condition = (factor(mm$distractor.condition,
                                  c("match", "mismatch")))

p = ggplot(mm, aes(x = distractor.condition, y = mrt, fill = distractor.condition))
(p + geom_bar(stat = 'identity')
 + labs(x = "Distractor condition", y = "RT (ms)")
 + geom_errorbar(aes(ymin=mrt-sert, ymax=mrt+sert), width=.1)
  + scale_fill_grey(start = .8, end = .3, guide=FALSE)
  + theme_classic()
  + theme(text = element_text(size=30))
  + coord_cartesian(ylim=c(450,510)))

# take out pullers
right.pull = big[!(big$steps < -1) & !(big$steps > 5) & !(big$steps == 0)|big$distractor.condition == "tone",]
mm = ddply(exit, "distractor.condition", summarise, mrt = mean(rt),
           sert = sd(rt)/sqrt(length(rt)))

##################################################################
# plot mean RTs with distractor's VOT separated
##################################################################

mm = ddply(na.omit(speech.dist), c("steps", "distractor.condition"), summarise, mrt = mean(rt),
           sert = sd(rt)/sqrt(length(rt)))

# get rid of steps with too few observations
mm = mm[c(7:38),]

# get rid of steps with too few observations
mm = mm[c(c(7:18),c(21:40),c(45,46)),]

# get rid of all steps but 1 (distractor VOT closest to participants VOT)
mm = mm[c(39:42),]

p = ggplot(mm, aes(fill = factor(steps), x = factor(distractor.condition), y = mrt))
(p + geom_bar(stat="identity", position = "dodge")
 + labs(x = "Distractor condition binned by VOT step", y = "RT (ms)", fill = "VOT step")
 + geom_errorbar(aes(ymin=mrt-sert, ymax=mrt+sert), position = position_dodge(.9), width = .3)
 #+ facet_wrap(~response)
 + scale_fill_grey(start = .8, end = .3)
 + theme_classic()
 + theme(text = element_text(size=28))
 + coord_cartesian(ylim=c(450,560)))

##################################################################
# with the whole big frame/distractor's VOT
##################################################################

mm = ddply(na.omit(big), c("vot.step", "distractor.condition"), summarise, mrt = mean(rt),
           sert = sd(rt)/sqrt(length(rt)))

p = ggplot(mm, aes(fill = factor(vot.step), x = factor(distractor.condition), y = mrt))
(p + geom_bar(stat="identity", position = "dodge")
 + labs(x = "Distractor condition", y = "RTs (in ms)", fill = "Distractor \nVOT (in ms)")
 + geom_errorbar(aes(ymin=mrt-sert, ymax=mrt+sert), position = position_dodge(.9), width = .3)
 #+ facet_wrap(~response)
 + scale_fill_grey(start = .8, end = .3)
 + theme_classic()
 + theme(text = element_text(size=28))
 + coord_cartesian(ylim=c(450,520)))

##################################################################
# plot scatter log.RT to VOT steps (polynomial regression)
##################################################################

p = ggplot(speech.dist, aes(x=steps, y=rt))
# method "lm" linear model; loess - curvy
(p #+ geom_point(alpha = 1/10)
 + geom_smooth(method=loess, se=F, size = 1, colour = "black")
 + geom_vline(xintercept=-1, colour = 'grey', size = 2, linetype = "dashed")
 + labs(x = "VOT step", y = "RTs (in ms)")
 + theme_classic()
 + theme(text = element_text(size=28)))
 #+ coord_cartesian(ylim=c(-50,50)))

##################################################################
# plot scatter log.RT to log.response VOT
##################################################################

# create empty vector
speech$response.duration = rep(NA, length(speech$vot.step))

for (j in 1:length(IdList)){
  
  duration = rep(NA, length(speech[speech$subject.ID == IdList[j],]))
  check_vector = speech[speech$subject.ID == IdList[j],]
  
  for (i in 1:nrow(check_vector)){
    # VOT devided by syllable duration
    duration[i] = check_vector$vowel.length[i]+check_vector$response.vot[i]
  }
  
  speech[speech$subject.ID == IdList[j],]$response.duration = duration
}

mm = ddply(na.omit(speech.dist), c("subject", "distractor.condition", "response"), summarise,
           mvot = mean(response.vot), mrt = mean(rt))

vort = mm[mm$distractor.condition == "match",]

p = ggplot(speech, aes(x=(response.vot), y=rt))
# method "lm" linear model; loess - curvy
(p + geom_point(alpha = 1/10)
 + geom_smooth(method=lm, se=T, size = 1, colour = "black")
 + labs(x = "Response VOT (in ms)", y = "RTs (in ms)")
 + theme_classic()
 + theme(text = element_text(size=28))
 + coord_cartesian(ylim=c(200,1000)))

p = ggplot(vort, aes(x=(mvot), y=mrt, shape = response))
# method "lm" linear model; loess - curvy
(p + geom_point()
 + geom_smooth(method=lm, se=F, size = 1, colour = "black")
 + labs(x = "Mean response VOT (in ms)", y = "Mean RTs (in ms)", shape = "Syllable")
 + theme_classic()
 + theme(text = element_text(size=28))
 + scale_shape_manual(values=c(1,2)))
 + coord_cartesian(ylim=c(200,1000)))

##################################################################
# plot scatter RT to VOT-distance
##################################################################

p = ggplot(na.omit(big), aes(x=distractor.vot, y=rt, linetype=distractor.condition))
(p + geom_smooth(method=lm, se=T, colour = "black")
+ labs(x = "Relative distractor VOT (ms)", y = "RT (ms)", linetype = "Distractor \ncondition")
#+ facet_wrap(~distractor.condition)
+ theme_classic()
+ theme(text = element_text(size=28)))
+ coord_cartesian(ylim=c(200,1000))) geom_point(alpha = 1/10)
