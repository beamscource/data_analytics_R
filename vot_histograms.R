

# add tone and none conditions as VOT step
big$vot.step[big$distractor.condition == "tone"] = "tone"
big$vot.step[big$distractor.condition == "none"] = "none"

# order the vot condition code them as factors
big$vot.step = (factor(big$vot.step,
                             c("none", "tone", "45", "60", "75", "90", "105", "120")))

# order the distractor condition and code them as factors
big$distractor.condition = (factor(big$distractor.condition,
                                         c("mismatch", "match", "tone", "none")))


##################################################################
#### plot VOT distributions for each participant with facets
##################################################################

p = ggplot(big[big$subject.ID == IdList[4],], aes(x=response.vot, y = ..density..,
                                                              fill=factor(vot.step)))
(p + geom_histogram(binwidth=2) + facet_wrap(~distractor.condition, scales = "free_x", ncol=2)
 + ggtitle(paste("VOTs", strsplit(fileList[4], split = "\\.")[[1]][1]))
 + labs(x = "VOT (in ms)", y = "Density",
      fill = "Distractor conditon")
 + coord_cartesian(xlim=c(30,130)))

##################################################################
#### plot VOT distributions for each participant with denstity curves
##################################################################

# define no-distractor subset fo ka
no.frame.ka = big[big$response == "ka" & 
                          big$distractor.condition == "none",]
no.frame.ka$distractor.condition = NULL
no.frame.ka$pairs = NULL

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

# order the response pairs and code them as factors
big$pairs = (factor(big$pairs,
                          c("ka-ka", "ka-ta", "ta-ta", "ta-ka")))

p = ggplot(speech.frame[speech.frame$subject.ID == IdList[4],],aes(response.vot))
(p + geom_density(aes(color=factor(vot.step)))
 + facet_wrap(~distractor.condition)
+ geom_density(data=no.frame, aes(response.vot), size=1, colour="black")
+ geom_density(data=to.frame, aes(response.vot), size=1, colour="gold")
+ ggtitle(paste("VOTs", strsplit(fileList[4], split = "\\.")[[1]][1]))
+ labs(x = "VOT (in ms)", y = "Density",
       color = "Distractor conditon")
+ coord_cartesian(xlim=c(20,140)))

##################################################################
#### plot VOT distributions for each participant with attractors
##################################################################

# define no-distractor subset fo ka
no.frame.ka = big[big$response == "ka" & 
                          big$distractor.condition == "none",]
no.frame.ka$distractor.condition = NULL
no.frame.ka$pairs = NULL

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

# plotting the ka responses
M = mean(no.frame.ka[no.frame.ka$subject.ID == IdList[4], ]$response.vot)
SD = sd(no.frame.ka[no.frame.ka$subject.ID == IdList[4], ]$response.vot)
M = round(M, digits = 2)
SD = round(SD, digits = 2)

M.l = mean(speech.frame[speech.frame$subject.ID == IdList[4] &
                        speech.frame$attractor == "left" &
                        speech.frame$response ==  "ka", ]$response.vot)
SD.l = sd(speech.frame[speech.frame$subject.ID == IdList[4] & 
                        speech.frame$attractor == "left" &
                        speech.frame$response ==  "ka", ]$response.vot)
M.l = round(M.l, digits = 2)
SD.l = round(SD.l, digits = 2)

M.r = mean(speech.frame[speech.frame$subject.ID == IdList[4] &
                          speech.frame$attractor == "right" &
                          speech.frame$response ==  "ka", ]$response.vot)
SD.r = sd(speech.frame[speech.frame$subject.ID == IdList[4] & 
                        speech.frame$attractor == "right" &
                         speech.frame$response ==  "ka", ]$response.vot)
M.r = round(M.r, digits = 2)
SD.r = round(SD.r, digits = 2)

p = ggplot(speech.frame[speech.frame$subject.ID == IdList[4] &
                          speech.frame$response == 'ka', ],
           aes(x = response.vot, color=factor(attractor)))

(p + geom_density()
 + scale_fill_brewer(palette = "Set1")
 + facet_wrap(~pairs)
 + geom_density(data=no.frame.ka[no.frame.ka$subject.ID == IdList[4],], aes(response.vot), size=1.5, colour="black")
 + geom_density(data=to.frame.ka[to.frame.ka$subject.ID == IdList[4],], aes(response.vot), size=1.5, colour="red")
 + ggtitle(paste("VOTs, ka-responses,", strsplit(fileList[4], split = "\\.")[[1]][1]))
 + scale_color_discrete(name = "Distractor attracts",
                       breaks=c("left", "right"),
                       labels=c(paste("left", "M =", M.l, "SD =", SD.l), 
                                paste("right", "M =", M.r, "SD =", SD.r)))
 + labs(x = paste("PPH: M =", M, "SD =", SD), y = "Density")
 + coord_cartesian(xlim=c(20,140)))

# plotting the ta responses
M = mean(no.frame.ta[no.frame.ta$subject.ID == IdList[4], ]$response.vot)
SD = sd(no.frame.ta[no.frame.ta$subject.ID == IdList[4], ]$response.vot)
M = round(M, digits = 2)
SD = round(SD, digits = 2)

M.l = mean(speech.frame[speech.frame$subject.ID == IdList[4] &
                          speech.frame$attractor == "left" &
                          speech.frame$response ==  "ta", ]$response.vot)
SD.l = sd(speech.frame[speech.frame$subject.ID == IdList[4] & 
                         speech.frame$attractor == "left" &
                         speech.frame$response ==  "ta", ]$response.vot)
M.l = round(M.l, digits = 2)
SD.l = round(SD.l, digits = 2)

M.r = mean(speech.frame[speech.frame$subject.ID == IdList[4] &
                          speech.frame$attractor == "right" &
                          speech.frame$response ==  "ta", ]$response.vot)
SD.r = sd(speech.frame[speech.frame$subject.ID == IdList[4] & 
                         speech.frame$attractor == "right" &
                         speech.frame$response ==  "ta", ]$response.vot)
M.r = round(M.r, digits = 2)
SD.r = round(SD.r, digits = 2)

p = ggplot(speech.frame[speech.frame$subject.ID == IdList[4] &
                          speech.frame$response == 'ta', ],
           aes(x = response.vot, color=factor(attractor)))

(p + geom_density()
 + scale_fill_brewer(palette = "Set1")
 + facet_wrap(~pairs)
 + geom_density(data=no.frame.ta[no.frame.ta$subject.ID == IdList[4],], aes(response.vot), size=1.5, colour="black")
 + geom_density(data=to.frame.ta[to.frame.ta$subject.ID == IdList[4],], aes(response.vot), size=1.5, colour="red")
 + ggtitle(paste("VOTs, ta-responses,", strsplit(fileList[4], split = "\\.")[[1]][1]))
 + scale_color_discrete(name = "Distractor attracts",
                        breaks=c("left", "right"),
                        labels=c(paste("left", "M =", M.l, "SD =", SD.l), 
                                 paste("right", "M =", M.r, "SD =", SD.r)))
 + labs(x = paste("PPH: M =", M, "SD =", SD), y = "Density")
 + coord_cartesian(xlim=c(20,140)))
  
##################################################################
#### plot VOT distribution for each participant for no-distractor condition
##################################################################

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
 + coord_cartesian(xlim=c(30,140)))

##################################################################
#### plot VOT t for no-distractor condition
##################################################################