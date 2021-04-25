
##################################################################
#### mean table
##################################################################

M = aggregate(response.vot~gender+response, big.frame[big.frame$vot.step == "none",], mean)
M[,3] = round(M[,3], digits = 2)
names(M) <- c("Gender", "Syllable", "Voice-oncet time")


##################################################################
#### means for every participant
##################################################################

df = data.frame(Subject_ID=character(),
                Gender=character(),
                Syllable=character(),
                Voice_onset_time=character(),
                SD=character(),
                stringsAsFactors=FALSE)

names(df) <- c("Subject's ID", "Gender", "Syllable", "Voice-onset time", "SD")

for (i in 1:length(fileList)){
  
  m = aggregate(response.vot~gender+response, big.frame[big.frame$vot.step == "none"
                                                           & big.frame$subject.ID == IdList[i],], mean)
  m[,3] = round(m[,3], digits = 2)
  
  SD = aggregate(response.vot~gender+response, big.frame[big.frame$vot.step == "none"
                                                        & big.frame$subject.ID == IdList[i],], sd)
  SD[,3] = round(SD[,3], digits = 2)
  
  subid = big.frame[big.frame$subject.ID == IdList[i],][1,1]
  
  df = rbind(df,cbind(cbind(subid,m), SD[,3]))
  
  
}