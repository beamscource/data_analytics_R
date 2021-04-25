rm(list = ls())

# VOTs for 10 p productions p. 101(67) Mayer 1995
p1 <- c(38,42,41,50,61)
p2 <- c(52,69,66,92,69)
p3 <- c(48,73,56,46,68)
p4 <- c(64,46,51,54,40)
p5 <- c(80,48,53,45,60)
p6 <- c(91,68,68,85,109)
p7 <- c(56,79,60,84,60)
p8 <- c(45,41,57,55,76)
p9 <- c(38,53,51,58,58)
p10 <- c(81,98,56,67,85)
p <- c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
mp = mean(p)
sdp = sd(p)

# VOTs for 10 b productions p. 101(67) Mayer 1995
b1 <- c(-90,-74,-10,-122,-8)
b1_ <- c()
b2 <- c(15,14,15,8,20)
b3 <- c(10,12,21,13,21)
b4 <- c(7,6,8,7,5)
b5 <- c(12,12,11,17,11)
b6 <- c(6,10,11,11,11)
b7 <- c(7,16,15,17,14)
b8 <- c(7,-94,7,7,7)
b8_ <- c(7,7,7,7)
b9 <- c(-118,-105,-21,-72,-62)
b9_ <- c()
b10 <- c(10,6,6,10,6)
b <- c(b1_,b2,b3,b4,b5,b6,b7,b8_,b9_,b10)
mb = mean(b)
sdb = sd(b)

######################
### histogramm with full VOT for p/b
### data from Mayer

hist(p, col = "red", xlim=c(0,120), ylim=c(0,10), main="Productions of /b/ and /p/", xlab="VOT")
par(new=TRUE) # hold on the plot
hist(b, col = "blue", xlim=c(0,120), ylim=c(0,10), main="", xlab="")

#######################
### plot for labials

labials = cbind(p,b)
labials = as.data.frame(labials)
labials$subject = c("2","2","2","2","2","3","3","3","3","3",
                    "4","4","4","4","4", "5","5","5","5","5",
                    "7","7","7","7","7", "8","8","8","8","8",
                    "10","10","10","10","10")
labial$subject = as.factor(labials$subject)
colnames(labials) = c("/p/", "/b/", "Subject")


library(reshape2)
dentals = melt (labials, id=c("Subject"))

colnames(labials) = c("Subject", "Consonant", "Duration")


library(languageR)
xylowess.fnc(Duration ~ Subject|Consonant, data = labials,
             span=2, ylab = "VOT in msec", 
             xlab = "Subjects")

# VOTs for 10 k productions p. 101(67) Mayer 1995
k1 <- c(50,60,68,69,71)
k2 <- c(82,111,75,93,88)
k3 <- c(57,62,43,43,53)
k4 <- c(62,51,57,57,63)
k5 <- c(73,74,74,70,76)
k6 <- c(100,76,94,75,98)
k7 <- c(64,52,78,78,61)
k8 <- c(75,76,63,58,97)
k9 <- c(80,73,81,93,79)
k10 <- c(85,106,103,95,85)
k <- c(k1,k2,k3,k4,k5,k6,k7,k8,k9,k10)
mk = mean(k)
sdk = sd(k)

# VOTs for 10 g productions p. 101(67) Mayer 1995
g1 <- c(-130,17,-164,23,24)
g1_ <- c(17,23,24)
g2 <- c(23,17,22,22,17)
g3 <- c(23,19,23,12,2)
g4 <- c(23,29,21,28,16)
g5 <- c(25,14,22,16,12)
g6 <- c(19,21,16,14,18)
g7 <- c(18,20,22,28,17)
g8 <- c(14,15,17,11,17)
g9 <- c(12,-102,-82,-200,-66)
g9_ <- c(12)
g10 <- c(19,15,19,17,24)
g <- c(g1_,g2,g3,g4,g5,g6,g7,g8,g9_,g10)
mg = mean(g)
sdg = sd(g)

### boxplot for k/g
boxplot(k2,g2,k3,g3,k4,g4,k5,g5,k6,g6,k7,g7,k8,g8,k10,g10)

######################
### histogramm with full VOT for k/g
### data from Mayer

hist(k, col = "red", xlim=c(0,120), ylim=c(0,10), main="Productions of /g/ and /k/", xlab="VOT")
par(new=TRUE) # hold on the plot
hist(g, col = "blue", xlim=c(0,120), ylim=c(0,10), main="", xlab="")


# VOTs for 10 t productions p. 101(67) Mayer 1995
t1 <- c(37,46,59,55,49)
t2 <- c(79,73,72,105,72)
t3 <- c(59,53,54,52,64)
t4 <- c(51,40,58,53,45)
t5 <- c(66,54,41,44,66)
t6 <- c(59,88,108,86,70)
t7 <- c(68,52,65,67,72)
t8 <- c(78,68,61,88,81)
t9 <- c(47,54,62,59,52)
t10 <- c(80,77,70,84,87)
t <- c(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)
mt = mean(t)
sdt = sd(t)

# VOTs for 10 d productions p. 101(67) Mayer 1995
d1 <- c(16,-92,-109,-146,-102)
d1_ <- c(16)
d2 <- c(15,12,14,15,14)
d3 <- c(23,23,2,22,21)
d4 <- c(8,8,8,9,9)
d5 <- c(11,12,13,13,14)
d6 <- c(13,10,11,11,11)
d7 <- c(19,14,19,18,19)
d8 <- c(2,11,12,12,15)
d9 <- c(20,-61,-60,-70,-81)
d9_ <- c(20)
d10 <- c(13,12,12,12,11)
d <- c(d1_,d2,d3,d4,d5,d6,d7,d8,d9_,d10)
md = mean(d)
sdd = sd(d)

######################
### histogramm with full VOT for t/d
### data from Mayer

hist(t, col = "red", xlim=c(0,120), ylim=c(0,10), main="Productions of /d/ and /t/", xlab="VOT")
par(new=TRUE) # hold on the plot
hist(d, col = "blue", xlim=c(0,120), ylim=c(0,10), main="", xlab="")

#######################
### plot for dentals

dentals = cbind(t,d)
dentals = as.data.frame(dentals)
dentals$subject = c("2","2","2","2","2","3","3","3","3","3",
                    "4","4","4","4","4", "5","5","5","5","5",
                    "7","7","7","7","7", "8","8","8","8","8",
                    "10","10","10","10","10")
dentals$subject = as.factor(dentals$subject)
colnames(dentals) = c("/t/", "/d/", "Subject")


library(reshape2)
dentals = melt (dentals, id=c("Subject"))

colnames(dentals) = c("Subject", "Consonant", "Duration")


library(languageR)
xylowess.fnc(Duration ~ Subject|Consonant, data = dentals,
             span=2, ylab = "VOT in msec", 
             xlab = "Subjects")


############################
### normal density function for VOT
############################

normal.density.function <- function(x,mu=0,sigma=1){
  1/(sqrt(2*pi)*sigma)*exp(-((x - mu)^2/(2*sigma^2)))}


plot(function(x) normal.density.function(x, md, sdd), 0, 120,
     main = "",ylim=c(0,.1),
     ylab="density",xlab="VOT")

par(new=TRUE)
plot(function(x) normal.density.function(x, mt, sdt), 0, 120,
     ylim=c(0,.1),
     ylab="density",xlab="VOT")

segments(25, 0, 25, 0.1)
segments(40, 0, 40, 0.1)

par(new=TRUE)

plot(function(x) normal.density.function(x, 25, sdd), 0, 120, col="red",
     ylim=c(0,.1),
     ylab="density",xlab="VOT")

plot(function(x) normal.density.function(x, 55, sdt), 0, 120, col="red",
     ylim=c(0,.1),
     ylab="density",xlab="VOT")
