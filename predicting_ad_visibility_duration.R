
# clear workspace
rm(list = ls())

# load the gdata package to read xls-files
library(gdata)
# load plyr for data frame manipulatios
library(plyr)
# load ggplot for visualization
library(ggplot2)
# for qqmath
library(lattice)
# for power transormation
library(car)

##############################
# color scheme
# Blau 4	0	180	255		#00B4FF
# Blau 5	0	62	115		#003E73
# Balu 6	0	39	72		
# Gelb	251	228	0		#FBE400
# Violett	129	27	127		#811B7F
# Grau #A6A6A6


# main directory
mainDir = "C:/Users/Klein/Desktop/ads/"
# save directory
savDir = "C:/Users/Klein/Desktop/ads/computations/figures/"

fileList = dir(mainDir)

# get ad title info

file7 = paste(mainDir, fileList[15], sep = "", collapse = NULL)
# read data table for current file
data7 = read.csv(file7, header = TRUE)

file1 = paste(mainDir, fileList[9], sep = "", collapse = NULL)
# read data table for current file
data1 = read.csv(file1, header = TRUE)

file2 = paste(mainDir, fileList[10], sep = "", collapse = NULL)
# read data table for current file
data2 = read.csv(file2, header = TRUE)

file3 = paste(mainDir, fileList[11], sep = "", collapse = NULL)
# read data table for current file
data3 = read.csv(file3, header = TRUE)

file4 = paste(mainDir, fileList[12], sep = "", collapse = NULL)
# read data table for current file
data4 = read.csv(file4, header = TRUE)

file5 = paste(mainDir, fileList[13], sep = "", collapse = NULL)
# read data table for current file
data5 = read.csv(file5, header = TRUE)

file6 = paste(mainDir, fileList[14], sep = "", collapse = NULL)
# read data table for current file
data6 = read.csv(file6, header = TRUE)

all = rbind(data1, data2, data3, data4, data5, data6, data7)
names(all)[names(all) == "Media.url"] = "AdMediaSrc"

# current file's path
currentFile = paste(mainDir, fileList[7], sep = "", collapse = NULL)
# read data table for current file
data = read.xls(currentFile, sheet = 1, header = TRUE)
data$Media.url = data$AdMediaSrc

# remove outliers
data = data[!data$visibility_Duration > 90000,]

# convert variables to factors
data$taskId = as.factor(data$taskId)
data$visibility_Time = as.factor(data$visibility_Time)
data$cursorHover_Time = as.factor(data$cursorHover_Time)
data$playButton_ClickTime = as.factor(data$playButton_ClickTime)
data$unmuteButton_ClickTime = as.factor(data$unmuteButton_ClickTime)
data$moreButton_ClickTime = as.factor(data$moreButton_ClickTime)
data$Platform = as.factor(data$Platform)
data$Type = as.factor(data$Type)
data$BIC = as.factor(data$BIC)
data$AdMediaSrc = as.factor(data$AdMediaSrc)
data$AdBrandName = as.factor(data$AdBrandName)
data$Media.url = as.factor(data$Media.url)

all$AdMediaSrc = str_replace(all$AdMediaSrc, "index.m3u8", "add_video.webm")


# split string
data$Media.url = sapply(strsplit(as.character(data$Media.url), split='/', fixed=TRUE),function(x) (x[7]))
all$Media.url = sapply(strsplit(as.character(all$Media.url), split='/', fixed=TRUE),function(x) (x[7]))

# merge data with ad titles
clean = data
data = merge(x = data, y = all, by = "Media.url", all.x = TRUE)

# ##################################################
# # visibility duration across ad types
# ##################################################
# 
# 
# # calculate the mean visibility duration for each ad type
# vis_dur = ddply(data, "Type", summarise, mvd = mean(visibility_Duration, na.rm = TRUE)/1000,
#            sevd = (sd(visibility_Duration, na.rm = TRUE)/sqrt(length(visibility_Duration)))/1000)
# 
# 
# # order the types
# vis_dur$Type = (factor(vis_dur$Type, c("autoplay", "clicktoplay", "static")))
# 
# p = ggplot(vis_dur, aes(x = Type, y = mvd, fill = Type))
# (p + geom_bar(stat = 'identity')
# + labs(x = "Ad type", y = "Mean Visibility Duration (in sec)")
# + geom_errorbar(aes(ymin=mvd-sevd, ymax=mvd+sevd), width=.1)
# #+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
# + scale_fill_manual(values = c("#00B4FF", "#811B7F", "#A6A6A6"))
# + theme_bw()
# + guides(fill = FALSE)
# + ggtitle("Mean Visibility Duration across ad types")
# + theme(text = element_text(size=5)))
# #+ coord_cartesian(ylim=c(0,)))
# 
# ggsave(filename = paste(savDir, "Mean_VD_all_ad_types.jpg", sep = "", collapse = NULL),
#        width = 4, height = 2.95)

##################################################
# visibility duration for static across platforms
##################################################
# 
# # calculate the mean visibility duration for static ads (facebook/twitter split)
# vis_dur_stat = ddply(data, c("Type", "Platform"), summarise, mvd = mean(visibility_Duration, na.rm = TRUE)/1000,
#            sevd = (sd(visibility_Duration, na.rm = TRUE)/sqrt(length(visibility_Duration)))/1000)
# vis_dur_stat = vis_dur_stat[3:4,]
# 
# p = ggplot(vis_dur_stat, aes(x = Platform, y = mvd, fill = Platform))
# (p + geom_bar(stat = 'identity')
# + labs(x = "Static ads", y = "Visibility duration (in sec)")
# + geom_errorbar(aes(ymin=mvd-sevd, ymax=mvd+sevd), width=.1)
# #+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
# + scale_fill_manual(values = c("#00B4FF", "#811B7F"))
# + theme_bw()
# + guides(fill = FALSE)
# + ggtitle("Mean Visibility Duration for static ads across platforms")
# + theme(text = element_text(size=5)))
# #+ coord_cartesian(ylim=c(450,510)))
# 
# ggsave(filename = paste(savDir, "Mean_VD_static.jpg", sep = "", collapse = NULL),
#        width = 4, height = 2.95)

#######################################################
# calculate the mean visibility duration for each project
#######################################################

vis_dur_proj = ddply(data, c("projectId", "elementId",  "Ad.title", "AdMediaSrc", "AdBrandName", "Type", "BIC"), summarise, mvd = mean(visibility_Duration, na.rm = TRUE)/1000,
           sevd = (sd(visibility_Duration, na.rm = TRUE)/sqrt(length(visibility_Duration)))/1000)

# order by Type
#vis_dur_proj[order(vis_dur_proj$Type, mm$BIC),]

# order by Type and visibility duration
vis_dur_proj = vis_dur_proj[order(vis_dur_proj$Type, -vis_dur_proj$mvd),]

vis_dur_proj$BIC = factor(vis_dur_proj$BIC, c("Yes", "No"))

# rename the duplicated

vis_dur_proj$Ad.title = as.character(vis_dur_proj$Ad.title)

vis_dur_proj[vis_dur_proj$Type == "static", ]$Ad.title[33] = "I deserve it. I mean I worked four days out last week. (Get 50% More Data)"
vis_dur_proj[vis_dur_proj$Type == "static", ]$Ad.title[40] = "I deserve it. I mean I worked four days out last week."
vis_dur_proj[vis_dur_proj$Type == "static", ]$Ad.title[45] = "My horoscope said I should follow my bliss."
vis_dur_proj[vis_dur_proj$Type == "static", ]$Ad.title[47] = "My horoscope said I should follow my bliss. (Get 50% More Data)"
vis_dur_proj[vis_dur_proj$Type == "static", ]$Ad.title[49] = "I'm celebrating father's day in Norway. (Get 300$ Credit)"
vis_dur_proj[vis_dur_proj$Type == "static", ]$Ad.title[54] = "I'm celebrating father's day in Norway."
vis_dur_proj[vis_dur_proj$Type == "static", ]$Ad.title[41] = "My eight-month-old is going to want to take great pictures some day."
vis_dur_proj[vis_dur_proj$Type == "static", ]$Ad.title[55] = "My eight-month-old is going to want to take great pictures some day. (Get Rollover Data)"

vis_dur_proj[vis_dur_proj$Type == "static", ]$Ad.title[33] = "I deserve it. (Get 50% More Data) FB"
vis_dur_proj[vis_dur_proj$Type == "static", ]$Ad.title[40] = "I deserve it. FB"
vis_dur_proj[vis_dur_proj$Type == "static", ]$Ad.title[45] = "My horoscope said.. FB"
vis_dur_proj[vis_dur_proj$Type == "static", ]$Ad.title[47] = "My horoscope said.. (Get 50% More Data) FB"
vis_dur_proj[vis_dur_proj$Type == "static", ]$Ad.title[49] = "Father's day. (Get 300$ Credit) FB"
vis_dur_proj[vis_dur_proj$Type == "static", ]$Ad.title[54] = "Father's day. FB"
vis_dur_proj[vis_dur_proj$Type == "static", ]$Ad.title[41] = "Eight-month-old.. FB"
vis_dur_proj[vis_dur_proj$Type == "static", ]$Ad.title[55] = "Eight-month-old.. (Get Rollover Data) FB"

vis_dur_proj$Ad.title = as.factor(vis_dur_proj$Ad.title)


# # write a nice header
# colnames(vis_dur_proj) <- c("projectId","adId","adType", "BIC", "meanVisibilityduration", "SD")

# # split the data by ad type
# static = mm[mm$adType == "static",]
# auto = mm[mm$adType == "autoplay",]
# click = mm[mm$adType == "clicktoplay",]

#######################################################
# top 10 visibility static only
#######################################################

static = vis_dur_proj[vis_dur_proj$Type == "static", ] [1:10,]
static$ID = 1:nrow(static)

p = ggplot(static, aes(x = reorder(Ad.title, -mvd), y = mvd, fill = BIC))
(p + geom_bar(stat = 'identity', position="dodge")
+ labs(x = "Ad title", y = "Mean Visibility Duration (in sec)", fill = "Best-in-Class ad:")
+ geom_errorbar(aes(ymin=mvd-sevd, ymax=mvd+sevd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#811B7F", "#A6A6A6"))
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Top 10 Mean Visibility Duration for static ads")
+ theme(text = element_text(size=5))
+ coord_cartesian(ylim=c(0,20))
+theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1)))

ggsave(filename = paste(savDir, "Top10_Mean_VD_static.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)

static = vis_dur_proj[vis_dur_proj$Type == "static", ] # [1:25,]
static = vis_dur_proj[vis_dur_proj$Type == "static", ][26:length(vis_dur_proj$Type == "static"),]
static$ID = 1:nrow(static)

p = ggplot(static[1:34,], aes(x = reorder(Ad.title, -mvd), y = mvd, fill = BIC))
(p + geom_bar(stat = 'identity', position="dodge")
+ labs(x = "Ad title", y = "Mean Visibility Duration (in sec)", fill = "Best-in-Class ad:")
+ geom_errorbar(aes(ymin=mvd-sevd, ymax=mvd+sevd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#811B7F", "#A6A6A6"))
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Mean Visibility Duration for static ads (part 1)")
+ theme(text = element_text(size=5))
+ coord_cartesian(ylim=c(0,20))
+theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1)))

ggsave(filename = paste(savDir, "All_Mean_VD_static_new_1.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)

#######################################################
# top 10 visibility autoplay only
#######################################################

auto = vis_dur_proj[vis_dur_proj$Type == "autoplay", ][1:10,]
auto$ID = 1:nrow(auto)


p = ggplot(auto, aes(x = reorder(Ad.title, -mvd), y = mvd, fill = BIC))
(p + geom_bar(stat = 'identity', position="dodge")
+ labs(x = "Ad title", y = "Mean Visibility Duration (in sec)", fill = "Best-in-Class ad:")
+ geom_errorbar(aes(ymin=mvd-sevd, ymax=mvd+sevd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#811B7F", "#A6A6A6"))
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Top 10 Mean Visibility Duration for autoplay (Facebook) ads")
+ theme(text = element_text(size=5))
+ coord_cartesian(ylim=c(0,45))
+theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1)))

ggsave(filename = paste(savDir, "Top10_Mean_VD_autoplay.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)



auto = vis_dur_proj[vis_dur_proj$Type == "autoplay", ] #[1:10,]
auto$ID = 1:nrow(auto)


p = ggplot(auto[24:55,], aes(x = reorder(Ad.title, -mvd), y = mvd, fill = BIC))
(p + geom_bar(stat = 'identity', position="dodge")
+ labs(x = "Ad title", y = "Mean Visibility Duration (in sec)", fill = "Best-in-Class ad:")
+ geom_errorbar(aes(ymin=mvd-sevd, ymax=mvd+sevd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#A6A6A6", "#811B7F")) #c("#811B7F", "#A6A6A6")
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Mean Visibility Duration for autoplay (Facebook) ads (part 2)")
+ theme(text = element_text(size=5))
+ coord_cartesian(ylim=c(0,45))
+theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1)))

ggsave(filename = paste(savDir, "All_Mean_VD_autoplay_new_2.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)

#######################################################
# top 10 visibility clicktoplay only
#######################################################

click = vis_dur_proj[vis_dur_proj$Type == "clicktoplay", ][1:10,]
click$ID = 1:nrow(click)

p = ggplot(click, aes(x = reorder(Ad.title, -mvd), y = mvd, fill = BIC))
(p + geom_bar(stat = 'identity', position="dodge")
+ labs(x = "Ad title", y = "Mean Visibility Duration (in sec)", fill = "Best-in-Class ad:")
+ geom_errorbar(aes(ymin=mvd-sevd, ymax=mvd+sevd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#811B7F", "#A6A6A6"))
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Top 10 Mean Visibility Duration for clicktoplay (Twitter) ads")
+ theme(text = element_text(size=5))
+ coord_cartesian(ylim=c(0,45))
+theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1)))

ggsave(filename = paste(savDir, "Top10_Mean_VD_clicktoplay.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)


click = vis_dur_proj[vis_dur_proj$Type == "clicktoplay", ] #[1:10,]
click$ID = 1:nrow(click)

p = ggplot(click[24:55, ], aes(x = reorder(Ad.title, -mvd), y = mvd, fill = BIC))
(p + geom_bar(stat = 'identity', position="dodge")
+ labs(x = "Ad title", y = "Mean Visibility Duration (in sec)", fill = "Best-in-Class ad:")
+ geom_errorbar(aes(ymin=mvd-sevd, ymax=mvd+sevd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#A6A6A6", "#811B7F")) #c("#811B7F", "#A6A6A6")
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Mean Visibility Duration for clicktoplay (Twitter) ads (part 2)")
+ theme(text = element_text(size=5))
+ coord_cartesian(ylim=c(0,45))
+theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1)))

ggsave(filename = paste(savDir, "All_Mean_VD_clicktoplay_new_2.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)

#######################################################
# Bottom 10 visibility static only
#######################################################

static_b = vis_dur_proj[vis_dur_proj$Type == "static", ][(length(vis_dur_proj[vis_dur_proj$Type == "static",]$mvd)-9):length(vis_dur_proj[vis_dur_proj$Type == "static",]$mvd),]
static_b$ID = 1:nrow(static)

p = ggplot(static_b, aes(x = reorder(Ad.title, -mvd), y = mvd, fill = BIC))
(p + geom_bar(stat = 'identity', position="dodge")
+ labs(x = "Ad title", y = "Mean Visibility Duration (in sec)", fill = "Best-in-Class ad:")
+ geom_errorbar(aes(ymin=mvd-sevd, ymax=mvd+sevd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#811B7F", "#A6A6A6"))
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Bottom 10 Mean Visibility Duration for static ads")
+ theme(text = element_text(size=5))
+ coord_cartesian(ylim=c(0,20))
+theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1)))

ggsave(filename = paste(savDir, "Bottom10_Mean_VD_static.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)

#######################################################
# Bottom 10 visibility autoplay only
#######################################################

auto_b = vis_dur_proj[vis_dur_proj$Type == "autoplay", ][(length(vis_dur_proj[vis_dur_proj$Type == "autoplay",]$mvd)-9):length(vis_dur_proj[vis_dur_proj$Type == "autoplay",]$mvd),]
auto_b$ID = 1:nrow(auto)

p = ggplot(auto_b, aes(x = reorder(Ad.title, -mvd), y = mvd, fill = BIC))
(p + geom_bar(stat = 'identity', position="dodge")
+ labs(x = "Ad title", y = "Mean Visibility Duration (in sec)", fill = "Best-in-Class ad:")
+ geom_errorbar(aes(ymin=mvd-sevd, ymax=mvd+sevd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#A6A6A6"))
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Bottom 10 Mean Visibility Duration for autoplay (Facebook) ads")
+ theme(text = element_text(size=5))
+ coord_cartesian(ylim=c(0,45))
+theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1)))

ggsave(filename = paste(savDir, "Bottom10_Mean_VD_autoplay.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)


#######################################################
# Bottom 10 visibility clicktoplay only
#######################################################

click_b = vis_dur_proj[vis_dur_proj$Type == "clicktoplay", ][(length(vis_dur_proj[vis_dur_proj$Type == "clicktoplay",]$mvd)-9):length(vis_dur_proj[vis_dur_proj$Type == "clicktoplay",]$mvd),]
click_b$ID = 1:nrow(click)

click_b$Ad.title = as.character(click_b$Ad.title)
click_b$Ad.title[1] = "1278-1101 Binge On 11/10  (TW)"
click_b$Ad.title = as.factor(click_b$Ad.title)

p = ggplot(click_b, aes(x = reorder(Ad.title, -mvd), y = mvd, fill = BIC))
(p + geom_bar(stat = 'identity', position="dodge")
+ labs(x = "Ad title", y = "Mean Visibility Duration (in sec)", fill = "Best-in-Class ad:")
+ geom_errorbar(aes(ymin=mvd-sevd, ymax=mvd+sevd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#A6A6A6", "#811B7F"))
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Bottom 10 Mean Visibility Duration for clicktoplay (Twitter) ads")
+ theme(text = element_text(size=5))
+ coord_cartesian(ylim=c(0,45))
+theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1)))

ggsave(filename = paste(savDir, "Bottom10_Mean_VD_clicktoplay.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)

###############################################
# visibleplay duration autoclick vs clicktoplay
###############################################


vis_pdur = ddply(data, "Type", summarise, mvpd = mean(visible.play_Duration, na.rm = TRUE)/1000,
                      sevpd = (sd(visible.play_Duration, na.rm = TRUE)/sqrt(length(visible.play_Duration)))/1000)

vis_pdur = rbind(vis_pdur[vis_pdur$Type == "clicktoplay",], vis_pdur[vis_pdur$Type == "autoplay",])

p = ggplot(vis_pdur, aes(x = Type, y = mvpd, fill = Type))
(p + geom_bar(stat = 'identity')
+ labs(x = "Ad type", y = "Mean Visible-pay Duration (in sec)")
+ geom_errorbar(aes(ymin=mvpd-sevpd, ymax=mvpd+sevpd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#00B4FF", "#811B7F"))
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Mean Visible-play Duration for video ads across ad types")
+ theme(text = element_text(size=5)))
#+ coord_cartesian(ylim=c(450,510)))

ggsave(filename = paste(savDir, "Mean_VPD_video.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)

#######################################################
# calculate the mean visibleplay duration for each project
#######################################################

vis_pdur_proj = ddply(data, c("projectId", "elementId", "Ad.title", "AdMediaSrc", "AdBrandName", "Type", "BIC"), summarise, mvpd = mean(visible.play_Duration, na.rm = TRUE)/1000,
           sevpd = (sd(visible.play_Duration, na.rm = TRUE)/sqrt(length(visible.play_Duration)))/1000)

vis_pdur_proj = rbind(vis_pdur_proj[vis_pdur_proj$Type == "clicktoplay",], vis_pdur_proj[vis_pdur_proj$Type == "autoplay",])

vis_pdur_proj = vis_pdur_proj[order(vis_pdur_proj$Type, -vis_pdur_proj$mvpd),]

vis_pdur_proj$BIC = factor(vis_pdur_proj$BIC, c("Yes", "No"))

p = ggplot(vis_pdur_proj, aes(x = projectId, y = mvpd, fill = elementId))
(p + geom_bar(stat = 'identity', position="dodge")
+ labs(x = "Ad type", y = "Visibility duration (sec)")
+ geom_errorbar(aes(ymin=mvpd-sevpd, ymax=mvpd+sevpd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ theme(text = element_text(size=5)))
#+ coord_cartesian(ylim=c(450,510)))

#######################################################
# top 10 visible play autoplay only
#######################################################

auto_vplay = vis_pdur_proj[vis_pdur_proj$Type == "autoplay", ][1:10,]
auto_vplay$ID = 1:nrow(auto_vplay)

p = ggplot(auto_vplay, aes(x = reorder(Ad.title, -mvpd), y = mvpd, fill = BIC))
(p + geom_bar(stat = 'identity', position="dodge")
+ labs(x = "Ad title", y = "Mean Visible-play  Duration (in sec)", fill = "Best-in-Class ad:")
+ geom_errorbar(aes(ymin=mvpd-sevpd, ymax=mvpd+sevpd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#811B7F", "#A6A6A6"))
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Top 10 Visible-play Duration for autoplay (Facebook) ads")
+ theme(text = element_text(size=5))
+ coord_cartesian(ylim=c(0,30))
+theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1)))

ggsave(filename = paste(savDir, "Top10_Mean_VPD_auto.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)


auto_vplay = vis_pdur_proj[vis_pdur_proj$Type == "autoplay", ]#[1:10,]
auto_vplay$ID = 1:nrow(auto_vplay)

p = ggplot(auto_vplay[26:55,], aes(x = reorder(Ad.title, -mvpd), y = mvpd, fill = BIC))
(p + geom_bar(stat = 'identity', position="dodge")
+ labs(x = "Ad title", y = "Mean Visible-play  Duration (in sec)", fill = "Best-in-Class ad:")
+ geom_errorbar(aes(ymin=mvpd-sevpd, ymax=mvpd+sevpd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#811B7F", "#A6A6A6"))
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Visible-play Duration for autoplay (Facebook) ads (part 2)")
+ theme(text = element_text(size=5))
+ coord_cartesian(ylim=c(0,30))
+theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1)))

ggsave(filename = paste(savDir, "All_Mean_VPD_auto_new_2.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)

#######################################################
# top 10 visible play clicktoplay only
#######################################################

click_vplay = vis_pdur_proj[vis_pdur_proj$Type == "clicktoplay", ][1:10,]
click_vplay = click_vplay[-3,]
click_vplay$ID = 1:nrow(click_vplay)

p = ggplot(click_vplay, aes(x = reorder(Ad.title, -mvpd), y = mvpd, fill = BIC))
(p + geom_bar(stat = 'identity', position="dodge")
+ labs(x = "Ad title", y = "Mean Visible-play  Duration (in sec)", fill = "Best-in-Class ad:")
+ geom_errorbar(aes(ymin=mvpd-sevpd, ymax=mvpd+sevpd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#A6A6A6", "#811B7F"))
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Top 10 Visible-play Duration for clicktoplay (Twitter) ads")
+ theme(text = element_text(size=5))
+ coord_cartesian(ylim=c(0,20))
+theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1)))

ggsave(filename = paste(savDir, "Top10_Mean_VPD_click.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)


click_vplay = vis_pdur_proj[vis_pdur_proj$Type == "clicktoplay", ]
click_vplay = click_vplay[-3,]
click_vplay$ID = 1:nrow(click_vplay)

p = ggplot(click_vplay[26:55,], aes(x = reorder(Ad.title, -mvpd), y = mvpd, fill = BIC))
(p + geom_bar(stat = 'identity', position="dodge")
+ labs(x = "Ad title", y = "Mean Visible-play  Duration (in sec)", fill = "Best-in-Class ad:")
+ geom_errorbar(aes(ymin=mvpd-sevpd, ymax=mvpd+sevpd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#811B7F", "#A6A6A6"))
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Visible-play Duration for clicktoplay (Twitter) ads (part 2)")
+ theme(text = element_text(size=5))
+ coord_cartesian(ylim=c(0,20))
+theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1)))

ggsave(filename = paste(savDir, "All_Mean_VPD_click_new_2.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)

#######################################################
# Bottom 10 visible play autoplay only
#######################################################

auto_vplay_b = vis_pdur_proj[vis_pdur_proj$Type == "autoplay", ][(length(vis_pdur_proj[vis_pdur_proj$Type == "autoplay",]$mvpd)-9):length(vis_pdur_proj[vis_pdur_proj$Type == "autoplay",]$mvpd),]
auto_vplay_b$ID = 1:nrow(auto_vplay)

p = ggplot(auto_vplay_b, aes(x = reorder(Ad.title, -mvpd), y = mvpd, fill = BIC))
(p + geom_bar(stat = 'identity', position="dodge")
+ labs(x = "Ad title", y = "Mean Visible-play  Duration (in sec)", fill = "Best-in-Class ad:")
+ geom_errorbar(aes(ymin=mvpd-sevpd, ymax=mvpd+sevpd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#A6A6A6", "#811B7F"))
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Bottom 10 Visible-play Duration for autoplay (Facebook) ads")
+ theme(text = element_text(size=5))
+ coord_cartesian(ylim=c(0,30))
+theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1)))

ggsave(filename = paste(savDir, "Bottom10_Mean_VPD_auto.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)

#######################################################
# Bottom 10 visible play clicktoplay only
#######################################################

click_vplay_b = vis_pdur_proj[vis_pdur_proj$Type == "clicktoplay", ][(length(vis_pdur_proj[vis_pdur_proj$Type == "clicktoplay",]$mvpd)-9):length(vis_pdur_proj[vis_pdur_proj$Type == "clicktoplay",]$mvpd),]
click_vplay_b$ID = 1:nrow(click_vplay)

p = ggplot(click_vplay_b, aes(x = reorder(Ad.title, -mvpd), y = mvpd, fill = BIC))
(p + geom_bar(stat = 'identity', position="dodge")
+ labs(x = "Ad title", y = "Mean Visible-play  Duration (in sec)", fill = "Best-in-Class ad:")
+ geom_errorbar(aes(ymin=mvpd-sevpd, ymax=mvpd+sevpd), width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#811B7F", "#A6A6A6"))
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Bottom 10 Visible-play Duration for autoplay (Twitter) ads")
+ theme(text = element_text(size=5))
+ coord_cartesian(ylim=c(0,20))
+theme(axis.text.x=element_text(angle=60,hjust=1,vjust=1)))

ggsave(filename = paste(savDir, "Bottom10_Mean_VPD_click.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)

##################################################
# Means TOP 10 vs. Flop 10 visibility
##################################################

dodge <- position_dodge(width=0.9)

col1 = matrix(c(mean(vis_dur_proj[vis_dur_proj$Type == "static",]$mvd[1:10]), mean(vis_dur_proj[vis_dur_proj$Type == "static",]$mvd[(length(vis_dur_proj[vis_dur_proj$Type == "static",]$mvd)-9):length(vis_dur_proj[vis_dur_proj$Type == "static",]$mvd)]),
                         mean(vis_dur_proj[vis_dur_proj$Type == "autoplay",]$mvd[1:10]), mean(vis_dur_proj[vis_dur_proj$Type == "autoplay",]$mvd[(length(vis_dur_proj[vis_dur_proj$Type == "autoplay",]$mvd)-9):length(vis_dur_proj[vis_dur_proj$Type == "autoplay",]$mvd)]),
                         mean(vis_dur_proj[vis_dur_proj$Type == "clicktoplay",]$mvd[1:10]), mean(vis_dur_proj[vis_dur_proj$Type == "clicktoplay",]$mvd[(length(vis_dur_proj[vis_dur_proj$Type == "clicktoplay",]$mvd)-9):length(vis_dur_proj[vis_dur_proj$Type == "clicktoplay",]$mvd)])), ncol=1,byrow=TRUE)

col4 = matrix(c(sd(vis_dur_proj[vis_dur_proj$Type == "static",]$mvd[1:10])/sqrt(10), sd(vis_dur_proj[vis_dur_proj$Type == "static",]$mvd[(length(vis_dur_proj[vis_dur_proj$Type == "static",]$mvd)-9):length(vis_dur_proj[vis_dur_proj$Type == "static",]$mvd)])/sqrt(10),
                sd(vis_dur_proj[vis_dur_proj$Type == "autoplay",]$mvd[1:10])/sqrt(10), sd(vis_dur_proj[vis_dur_proj$Type == "autoplay",]$mvd[(length(vis_dur_proj[vis_dur_proj$Type == "autoplay",]$mvd)-9):length(vis_dur_proj[vis_dur_proj$Type == "autoplay",]$mvd)])/sqrt(10),
                sd(vis_dur_proj[vis_dur_proj$Type == "clicktoplay",]$mvd[1:10])/sqrt(10), sd(vis_dur_proj[vis_dur_proj$Type == "clicktoplay",]$mvd[(length(vis_dur_proj[vis_dur_proj$Type == "clicktoplay",]$mvd)-9):length(vis_dur_proj[vis_dur_proj$Type == "clicktoplay",]$mvd)]))/sqrt(10), ncol=1,byrow=TRUE)

col2 = matrix(c("static", "static", "autoplay", "autoplay", "clicktoplay", "clicktoplay"), ncol=1,byrow=TRUE)

col3 = matrix(c("Top 10", "Bottom 10", "Top 10", "Bottom 10","Top 10", "Bottom 10"), ncol=1,byrow=TRUE)

topbottom_vis = cbind(col1, col4, col2, col3)
colnames(topbottom_vis) = c("mean","se", "adType", "group")
topbottom_vis = as.data.frame(topbottom_vis)
topbottom_vis$mean = as.numeric(as.character(topbottom_vis$mean))
topbottom_vis$se = as.numeric(as.character(topbottom_vis$se))
# order the types
topbottom_vis$adType = (factor(topbottom_vis$adType, c("autoplay", "clicktoplay", "static")))
topbottom_vis$group = (factor(topbottom_vis$group, c("Top 10", "Bottom 10")))

p = ggplot(topbottom_vis, aes(x = adType, y = mean, fill = group))
(p + geom_bar(stat = 'identity', position=dodge)
+ labs(x = "AD Type", y = "Mean Visibility Duration (in sec)", fill = "Group")
+ geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = dodge, width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#811B7F", "#A6A6A6"))
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Top 10 vs. Bottom 10 Visibility Duration across all ad types")
+ theme(text = element_text(size=5)))
#+ coord_cartesian(ylim=c(450,510)))

ggsave(filename = paste(savDir, "TopBottom10_Mean_VD.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)

##################################################
# Means TOP 10 vs. Flop 10 visible-play
##################################################

col1 = matrix(c(mean(vis_pdur_proj[vis_pdur_proj$Type == "autoplay",]$mvpd[1:10]), mean(vis_pdur_proj[vis_pdur_proj$Type == "autoplay",]$mvpd[(length(vis_pdur_proj[vis_pdur_proj$Type == "autoplay",]$mvpd)-9):length(vis_pdur_proj[vis_pdur_proj$Type == "autoplay",]$mvpd)]),
                mean(vis_pdur_proj[vis_pdur_proj$Type == "clicktoplay",]$mvpd[1:10]), mean(vis_pdur_proj[vis_pdur_proj$Type == "clicktoplay",]$mvpd[(length(vis_pdur_proj[vis_pdur_proj$Type == "clicktoplay",]$mvpd)-9):length(vis_pdur_proj[vis_pdur_proj$Type == "clicktoplay",]$mvpd)])), ncol=1,byrow=TRUE)

col4 = matrix(c(sd(vis_pdur_proj[vis_pdur_proj$Type == "autoplay",]$mvpd[1:10])/sqrt(10), sd(vis_pdur_proj[vis_pdur_proj$Type == "autoplay",]$mvpd[(length(vis_pdur_proj[vis_pdur_proj$Type == "autoplay",]$mvpd)-9):length(vis_pdur_proj[vis_pdur_proj$Type == "autoplay",]$mvpd)])/sqrt(10),
                       sd(vis_pdur_proj[vis_pdur_proj$Type == "clicktoplay",]$mvpd[1:10])/sqrt(10), sd(vis_pdur_proj[vis_pdur_proj$Type == "clicktoplay",]$mvpd[(length(vis_pdur_proj[vis_pdur_proj$Type == "clicktoplay",]$mvpd)-9):length(vis_pdur_proj[vis_pdur_proj$Type == "clicktoplay",]$mvd)]))/sqrt(10), ncol=1,byrow=TRUE)


col2 = matrix(c("autoplay", "autoplay", "clicktoplay", "clicktoplay"), ncol=1,byrow=TRUE)

col3 = matrix(c("Top 10", "Bottom 10", "Top 10", "Bottom 10"), ncol=1,byrow=TRUE)

topbottom_vis_play = cbind(col1, col4, col2, col3)
colnames(topbottom_vis_play) = c("mean","se", "adType", "group")
topbottom_vis_play = as.data.frame(topbottom_vis_play)
topbottom_vis_play$mean = as.numeric(as.character(topbottom_vis_play$mean))
topbottom_vis_play$se = as.numeric(as.character(topbottom_vis_play$se))

# order the types
topbottom_vis_play$adType = (factor(topbottom_vis_play$adType, c("autoplay", "clicktoplay")))
topbottom_vis_play$group = (factor(topbottom_vis_play$group, c("Top 10", "Bottom 10")))

p = ggplot(topbottom_vis_play, aes(x = adType, y = mean, fill = group))
(p + geom_bar(stat = 'identity', position=dodge)
+ labs(x = "AD Type", y = "Mean Visible-play  Duration (in sec)", fill = "Group")
+ geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = dodge, width=.1)
#+ scale_fill_grey(start = .8, end = .3, guide=FALSE)
+ scale_fill_manual(values = c("#811B7F", "#A6A6A6"))
+ theme_bw()
#+ guides(fill = FALSE)
#+ coord_flip()
+ ggtitle("Top 10 vs. Bottom 10 Visible-play Duration across video ad types")
+ theme(text = element_text(size=5)))
#+ coord_cartesian(ylim=c(450,510)))

ggsave(filename = paste(savDir, "TopBottom10_Mean_VPD.jpg", sep = "", collapse = NULL),
       width = 4, height = 2.95)

##################################################
# write the data
##################################################

write.csv(vis_dur_proj[vis_dur_proj$Type == "autoplay",3:4], file = "C:/Users/Klein/Desktop/ads/computations/vis_dur_URL_auto.csv", append = FALSE)
write.csv(vis_dur_proj[vis_dur_proj$Type == "clicktoplay",3:4], file = "C:/Users/Klein/Desktop/ads/computations/vis_dur_URL_click.csv", append = FALSE)
write.csv(vis_dur_proj[vis_dur_proj$Type == "static",3:4], file = "C:/Users/Klein/Desktop/ads/computations/vis_dur_URL_static.csv", append = FALSE)

write.csv(vis_pdur_proj[vis_pdur_proj$Type == "clicktoplay",3:4], file = "C:/Users/Klein/Desktop/ads/computations/vis_pdur_URL_click.csv", append = FALSE)
write.csv(vis_pdur_proj[vis_pdur_proj$Type == "autoplay",3:4], file = "C:/Users/Klein/Desktop/ads/computations/vis_pdur_URL_auto.csv", append = FALSE)

WriteXLS(c("vis_dur", "vis_dur_stat", "vis_dur_proj", "vis_pdur", "vis_pdur_proj", "topbottom_vis", "topbottom_vis_play"), ExcelFileName = "C:/Users/Klein/Desktop/ads/computations/tables.xls", SheetNames = NULL, perl = "perl", col.names = TRUE)


##################################################
# naming stuff
##################################################


vis_dur_proj[5,3:4]

vis_dur_proj[4,3] = "Plenti points Handstand (TW)"
vis_dur_proj[5,3] = "Plenti points Handstand (TW)"
vis_dur_proj[7,3] = "Strong 4G LTE Signal"
vis_dur_proj[15,3] = "Plenti points Cartwheel (TW)"
vis_dur_proj[16,3] = "Plenti points Breakdance (TW)"
vis_dur_proj[17,3] = "Plenti points Breakdance RETEST (TW)"
vis_dur_proj[18,3] = "Plenti points Cartwheel RETEST (TW)"

vis_pdur_proj[5,3:4]


vis_pdur_proj[1,3] = "Plenti points Handstand (FB)"
vis_pdur_proj[2,3] = "Plenti points Handstand (TW)"
vis_pdur_proj[4,3] = "Strong 4G LTE Signal"
vis_pdur_proj[5,3] = "Plenti points Cartwheel (TW)"
vis_pdur_proj[6,3] = "Plenti points Breakdance (TW)"
vis_pdur_proj[7,3] = "Plenti points Breakdance RETEST (TW)"
vis_pdur_proj[8,3] = "Plenti points Cartwheel RETEST (TW)"

vis_dur_proj$Ad.title = as.character(vis_dur_proj$Ad.title)
vis_pdur_proj$Ad.title = as.character(vis_pdur_proj$Ad.title)
vis_dur_proj$Ad.title = as.factor(vis_dur_proj$Ad.title)
vis_pdur_proj$Ad.title = as.factor(vis_pdur_proj$Ad.title)

##################################################
# modeling
##################################################

# check normality
qqmath(~visibility_Duration|elementId, data = datata)


plot(visibility_Duration ~ BIC, data=data)
plot(visible.play_Duration ~ BIC, data=data)

# check the numbers
summary(data[data$Type == "static",])

# convert in seconds
data$VDsec = data$visibility_Duration/1000
data$VPDsec = data$visible.play_Duration/1000
data$HDsec = data$cursorHover_Duration/1000
data$ADsec = data$audible_Duration/1000

summary(glm(BIC ~ visibility_Duration * visible.play_Duration * cursorHover_Duration, family=binomial(logit), data=data))

# models
mstatic = glm(BIC ~ VDsec + HDsec, family=binomial(logit), data=data[data$Type == "static",])
summary(mstatic)

mauto = glm(BIC ~ VDsec + VPDsec + HDsec + ADsec, family=binomial(logit), data=data[data$Type == "autoplay",])
mauto = glm(BIC ~ VDsec + HDsec, family=binomial(logit), data=data[data$Type == "autoplay",])
summary(mauto)

mclick = glm(BIC ~ VDsec + VPDsec + HDsec + ADsec, family=binomial(logit), data=data[data$Type == "clicktoplay",])
summary(mclick)

# average predicted probability
testdata = data.frame(VDsec = 0, VPDsec = 0, HDsec = 0)
predict(mstatic, testdata, type="response")

m2 = glm(BIC ~ VDsec + VPDsec + HDsec, family=binomial(logit), data=data)
m1 = glm(BIC ~ VDsec, family=binomial(logit), data=data[data$Type == "static", ])

m1$coef
m1$fitted
m1$resid
m1$effects
anova(m1)
