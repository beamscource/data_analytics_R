##################################################################
# reading data
# Author: Eugen Klein, Dezember 2014
##################################################################

# clear workspace
rm(list = ls())

# load the gdata package to read xls-files
library(gdata)
# load plyr for data frame manipulatios
library(plyr)

# main directory
mainDir = "C:/Users/Eugen/Desktop/data_processing/data/"
# save directory
savDir = "C:/Users/Eugen/Desktop/data_processing/plots/"
# create file list
fileList = dir(mainDir)
IdList = 1:40

##################################################################
#### read all data
##################################################################

for (s in 1:length(fileList)){
  # current file's path
  currentFile = paste(mainDir, fileList[s], sep = "", collapse = NULL)
  # read data table for current file
  data=read.xls(currentFile, sheet = 1, header = TRUE)
  
  # add info with the observation number
  # (trial nuber independend from block number)
  data$observation = id(data)
  
  # delete rows with NA's in RTs
  data = data[!is.na(data$rt),]
  
  # convert RTs to msec
  data$rt = data$rt*1000
  #convert VOTs to msec
  data$response.vot = data$response.vot*1000
  # convert the vowel length to msec
  data$vowel.length = data$vowel.length*1000
  
  # include preceding RT
  data$pre.rt = c(mean(data$rt), head(data$rt, -1))
  
  # include preceding VOT
  data$pre.vot = c(mean(data$response.vot), head(data$response.vot, -1))
  
  ##################################################################
  #  center around zero and sort distractor categories based on the pivot vots
  ##################################################################
  
  #     # pivot vot for ka
  #     piv.ka = mean(data[data$response == 'ka' & data$distractor.condition == 'none',]$response.vot)
  #     
  #     # round pivot vot
  #     piv.ka = round(piv.ka, digits = 2)
  
#   # pivot vot for ka
#   piv.ka = mean(data[data$response == 'ka',]$response.vot)
#   
#   # round pivot vot
#   piv.ka = round(piv.ka, digits = 2)
    
  #     # pivot vot for ta
  #     piv.ta = mean(data[data$response == 'ta' & data$distractor.condition == 'none',]$response.vot)
  #     
  #     # round pivot vot
  #     piv.ta = round(piv.ta, digits = 2)
  
#   # pivot vot for ta
#   piv.ta = mean(data[data$response == 'ta',]$response.vot)
#   
#   # round pivot vot
#   piv.ta = round(piv.ta, digits = 2)
#   
#   # create empty vector for response-distractor pairs
#   data$pairs = rep(NA, length(data$vot.step))
  
#       # create empty vector for attractor assignment
#       data$attractor = rep(NA, length(data$vot.step))
#   
#       # create empty vector for generalized steps
#       data$steps = rep(NA, length(data$vot.step))
    
    ##################################################################
    
#     # center ka responses
#     data$response.vot[data$response == 'ka'] = ((data$response.vot[data$response == 'ka']
#                                                 - piv.ka))
#     
#     data$distractor.vot[data$response == 'ka'] = (data$vot.step[data$response == 'ka']
#                                                 - piv.ka)
#     
#     # center ta responses
#     data$response.vot[data$response == 'ta'] = ((data$response.vot[data$response == 'ta']
#                                                 - piv.ta))
#     
#     data$distractor.vot[data$response == 'ta'] = (data$vot.step[data$response == 'ta']
#                                             - piv.ta)
    
    ##################################################################
    
#     # create colum for pairs ka-ka
#     data[!is.na(data$vot.step) & data$response == 'ka' &
#            data$distractor.condition == 'match',]$pairs = 'ka-ka'
#     # create colum for pairs ka-ka
#     data[data$response == 'ka' &
#            data$distractor.condition == 'none',]$pairs = 'ka-ka'
    
#     # sort all distractors into two attractor categories for ka-ka-responses
#     for (i in 1:length(data$vot.step[!is.na(data$vot.step) &
#                                        data$response == 'ka' &
#                                        data$distractor.condition == 'match']))
#       if (data[!is.na(data$vot.step) &
#                  data$response == 'ka' &
#                  data$distractor.condition == 'match', ]$vot.step[i] < piv.ka){
#         data[!is.na(data$vot.step) & data$response == 'ka' &
#                data$distractor.condition == 'match',]$attractor[i] = 'left'
#       } else {
#         data[!is.na(data$vot.step) & data$response == 'ka' &
#                data$distractor.condition == 'match',]$attractor[i] = 'right'
#       }
#     
#     # convert attractor categories into descrete values for left attractors
#     level = levels(as.factor(data[!is.na(data$vot.step) &
#                                     data$response == 'ka' &
#                                     data$distractor.condition == 'match' &
#                                     data$attractor == 'left',]$vot.step))
#     
#     # mirror level vector for left attractors
#     level = level[length(level):1]
#     
#     l = length(as.integer(level))
#     
#     # sort both attractor categories in several steps for ka-ka-responses
#     if (!is.na(level)){
#       
#       indx = 0
#       
#       while ((l-indx) != 0){
#         for (i in 1:length(data[!is.na(data$vot.step) &
#                                   data$response == 'ka' &
#                                   data$distractor.condition == 'match' &
#                                   data$attractor == 'left',]$vot.step)){
#           
#           if (data[!is.na(data$vot.step) & data$response == 'ka' &
#                      data$distractor.condition == 'match' &
#                      data$attractor == 'left',]$vot.step[i] == as.integer(level)[l-indx]){
#             
#             data[!is.na(data$vot.step) & data$response == 'ka' &
#                    data$distractor.condition == 'match' &
#                    data$attractor == 'left',]$steps[i] = -(l-indx)
#           }
#         }
#         
#         indx = indx + 1
#         
#       }
#     }
#     
#     # convert attractor categories into descrete values for right attractors
#     level = levels(as.factor(data[!is.na(data$vot.step) &
#                                     data$response == 'ka' &
#                                     data$distractor.condition == 'match' &
#                                     data$attractor == 'right',]$vot.step))
#     
#     l = length(as.integer(level))
#     
#     # sort both attractor categories in several steps for ka-ka-responses
#     if (!is.na(level)){
#       
#       indx = 0
#       
#       while ((l-indx) != 0){
#         for (i in 1:length(data[!is.na(data$vot.step) &
#                                   data$response == 'ka' &
#                                   data$distractor.condition == 'match' &
#                                   data$attractor == 'right',]$vot.step)){
#           
#           if (data[!is.na(data$vot.step) & data$response == 'ka' &
#                      data$distractor.condition == 'match' &
#                      data$attractor == 'right',]$vot.step[i] == as.integer(level)[l-indx]){
#             
#             data[!is.na(data$vot.step) & data$response == 'ka' &
#                    data$distractor.condition == 'match' &
#                    data$attractor == 'right',]$steps[i] = (l-indx)
#           }
#         }
#         
#         indx = indx + 1
#         
#       }
#     }
    
    #################################################################
    
#     # create colum for pairs ka-ta
#     data[!is.na(data$vot.step) & data$response == 'ka' &
#            data$distractor.condition == 'mismatch',]$pairs = 'ka-ta'
    
#     # sort all distractors in two attractor categories for ka-ta-responses
#     for (i in 1:length(data$vot.step[!is.na(data$vot.step) &
#                                        data$response == 'ka' &
#                                        data$distractor.condition == 'mismatch']))
#       if (data[!is.na(data$vot.step) &
#                  data$response == 'ka' &
#                  data$distractor.condition == 'mismatch', ]$vot.step[i] < piv.ka){
#         data[!is.na(data$vot.step) & data$response == 'ka' &
#                data$distractor.condition == 'mismatch',]$attractor[i] = 'left'
#       } else {
#         data[!is.na(data$vot.step) & data$response == 'ka' &
#                data$distractor.condition == 'mismatch',]$attractor[i] = 'right'
#       }
#     
#     # convert attractor categories into descrete values for left attractors
#     level = levels(as.factor(data[!is.na(data$vot.step) &
#                                     data$response == 'ka' &
#                                     data$distractor.condition == 'mismatch' &
#                                     data$attractor == 'left',]$vot.step))
#    
#     # mirror level vector for left attractors
#     level = level[length(level):1]
#     
#     l = length(as.integer(level))
#     
#     # sort both attractor categories in several steps for ka-ta-responses
#     if (!is.na(level)){
#       
#       indx = 0
#       
#       while ((l-indx) != 0){
#         for (i in 1:length(data[!is.na(data$vot.step) &
#                                   data$response == 'ka' &
#                                   data$distractor.condition == 'mismatch' &
#                                   data$attractor == 'left',]$vot.step)){
#           
#           if (data[!is.na(data$vot.step) & data$response == 'ka' &
#                      data$distractor.condition == 'mismatch' &
#                      data$attractor == 'left',]$vot.step[i] == as.integer(level)[l-indx]){
#             
#             data[!is.na(data$vot.step) & data$response == 'ka' &
#                    data$distractor.condition == 'mismatch' &
#                    data$attractor == 'left',]$steps[i] = -(l-indx)
#           }
#         }
#         
#         indx = indx + 1
#         
#       }
#     }
#     
#     # convert attractor categories into descrete values for right attractors
#     level = levels(as.factor(data[!is.na(data$vot.step) &
#                                     data$response == 'ka' &
#                                     data$distractor.condition == 'mismatch' &
#                                     data$attractor == 'right',]$vot.step))
#     
#     l = length(as.integer(level))
#     
#     # sort both attractor categories in several steps for ka-ta-responses
#     if (!is.na(level)){
#       
#       indx = 0
#       
#       while ((l-indx) != 0){
#         for (i in 1:length(data[!is.na(data$vot.step) &
#                                   data$response == 'ka' &
#                                   data$distractor.condition == 'mismatch' &
#                                   data$attractor == 'right',]$vot.step)){
#           
#           if (data[!is.na(data$vot.step) & data$response == 'ka' &
#                      data$distractor.condition == 'mismatch' &
#                      data$attractor == 'right',]$vot.step[i] == as.integer(level)[l-indx]){
#             
#             data[!is.na(data$vot.step) & data$response == 'ka' &
#                    data$distractor.condition == 'mismatch' &
#                    data$attractor == 'right',]$steps[i] = (l-indx)
#           }
#         }
#         
#         indx = indx + 1
#         
#       }
#     }
    
    ##################################################################
    
#     # create colum for pairs ta-ta
#     data[!is.na(data$vot.step) & data$response == 'ta' &
#            data$distractor.condition == 'match',]$pairs = 'ta-ta'

#     # create colum for pairs ta-ta
#     data[data$response == 'ta' &
#            data$distractor.condition == 'none',]$pairs = 'ta-ta'
    
#     # sort all distractors in two attractor categories for ta-ta-responses
#     for (i in 1:length(data$vot.step[!is.na(data$vot.step) &
#                                        data$response == 'ta' &
#                                        data$distractor.condition == 'match']))
#       if (data[!is.na(data$vot.step) &
#                  data$response == 'ta' &
#                  data$distractor.condition == 'match', ]$vot.step[i] < piv.ta){
#         data[!is.na(data$vot.step) & data$response == 'ta' &
#                data$distractor.condition == 'match',]$attractor[i] = 'left'
#       } else {
#         data[!is.na(data$vot.step) & data$response == 'ta' &
#                data$distractor.condition == 'match',]$attractor[i] = 'right'
#       }
#     
#     # convert attractor categories into descrete values for left attractors
#     level = levels(as.factor(data[!is.na(data$vot.step) &
#                                     data$response == 'ta' &
#                                     data$distractor.condition == 'match' &
#                                     data$attractor == 'left',]$vot.step))
#     
#     # mirror level vector for left attractors
#     level = level[length(level):1]
#     
#     l = length(as.integer(level))
#     
#     # sort both attractor categories in several steps for ta-ta-responses
#     if (!is.na(level)){
#       
#       indx = 0
#       
#       while ((l-indx) != 0){
#         for (i in 1:length(data[!is.na(data$vot.step) &
#                                   data$response == 'ta' &
#                                   data$distractor.condition == 'match' &
#                                   data$attractor == 'left',]$vot.step)){
#           
#           if (data[!is.na(data$vot.step) & data$response == 'ta' &
#                      data$distractor.condition == 'match' &
#                      data$attractor == 'left',]$vot.step[i] == as.integer(level)[l-indx]){
#             
#             data[!is.na(data$vot.step) & data$response == 'ta' &
#                    data$distractor.condition == 'match' &
#                    data$attractor == 'left',]$steps[i] = -(l-indx)
#           }
#         }
#         
#         indx = indx + 1
#         
#       }
#     }
#     
#     # convert attractor categories into descrete values for right attractors
#     level = levels(as.factor(data[!is.na(data$vot.step) &
#                                     data$response == 'ta' &
#                                     data$distractor.condition == 'match' &
#                                     data$attractor == 'right',]$vot.step))
#     
#     l = length(as.integer(level))
#     
#     # sort both attractor categories in several steps for ta-ta-responses
#     if (!is.na(level)){
#       
#       indx = 0
#       
#       while ((l-indx) != 0){
#         for (i in 1:length(data[!is.na(data$vot.step) &
#                                   data$response == 'ta' &
#                                   data$distractor.condition == 'match' &
#                                   data$attractor == 'right',]$vot.step)){
#           
#           if (data[!is.na(data$vot.step) & data$response == 'ta' &
#                      data$distractor.condition == 'match' &
#                      data$attractor == 'right',]$vot.step[i] == as.integer(level)[l-indx]){
#             
#             data[!is.na(data$vot.step) & data$response == 'ta' &
#                    data$distractor.condition == 'match' &
#                    data$attractor == 'right',]$steps[i] = (l-indx)
#           }
#         }
#         
#         indx = indx + 1
#         
#       }
#     }
    
    #################################################################
    
#     # create colum for pairs ta-ka
#     data[!is.na(data$vot.step) & data$response == 'ta' &
#            data$distractor.condition == 'mismatch',]$pairs = 'ta-ka'
    
#     # sort all distractors in two attractor categories for ta-ka-responses
#     for (i in 1:length(data$vot.step[!is.na(data$vot.step) &
#                                        data$response == 'ta' &
#                                        data$distractor.condition == 'mismatch']))
#       if (data[!is.na(data$vot.step) &
#                  data$response == 'ta' &
#                  data$distractor.condition == 'mismatch', ]$vot.step[i] < piv.ta){
#         data[!is.na(data$vot.step) & data$response == 'ta' &
#                data$distractor.condition == 'mismatch',]$attractor[i] = 'left'
#       } else {
#         data[!is.na(data$vot.step) & data$response == 'ta' &
#                data$distractor.condition == 'mismatch',]$attractor[i] = 'right'
#       }
#     
#     # convert attractor categories into descrete values for left attractors
#     level = levels(as.factor(data[!is.na(data$vot.step) &
#                                     data$response == 'ta' &
#                                     data$distractor.condition == 'mismatch' &
#                                     data$attractor == 'left',]$vot.step))
#     
#     # mirror level vector for left attractors
#     level = level[length(level):1]
#     
#     l = length(as.integer(level))
#     
#     # sort both attractor categories in several steps for ta-ka-responses
#     if (!is.na(level)){
#       
#       indx = 0
#       
#       while ((l-indx) != 0){
#         for (i in 1:length(data[!is.na(data$vot.step) &
#                                   data$response == 'ta' &
#                                   data$distractor.condition == 'mismatch' &
#                                   data$attractor == 'left',]$vot.step)){
#           
#           if (data[!is.na(data$vot.step) & data$response == 'ta' &
#                      data$distractor.condition == 'mismatch' &
#                      data$attractor == 'left',]$vot.step[i] == as.integer(level)[l-indx]){
#             
#             data[!is.na(data$vot.step) & data$response == 'ta' &
#                    data$distractor.condition == 'mismatch' &
#                    data$attractor == 'left',]$steps[i] = -(l-indx)
#           }
#         }
#         
#         indx = indx + 1
#         
#       }
#     }
#     
#     # convert attractor categories into descrete values for right attractors
#     level = levels(as.factor(data[!is.na(data$vot.step) &
#                                     data$response == 'ta' &
#                                     data$distractor.condition == 'mismatch' &
#                                     data$attractor == 'right',]$vot.step))
#     
#     l = length(as.integer(level))
#     
#     # sort both attractor categories in several steps for ta-ka-responses
#     if (!is.na(level)){
#       
#       indx = 0
#       
#       while ((l-indx) != 0){
#         for (i in 1:length(data[!is.na(data$vot.step) &
#                                   data$response == 'ta' &
#                                   data$distractor.condition == 'mismatch' &
#                                   data$attractor == 'right',]$vot.step)){
#           
#           if (data[!is.na(data$vot.step) & data$response == 'ta' &
#                      data$distractor.condition == 'mismatch' &
#                      data$attractor == 'right',]$vot.step[i] == as.integer(level)[l-indx]){
#             
#             data[!is.na(data$vot.step) & data$response == 'ta' &
#                    data$distractor.condition == 'mismatch' &
#                    data$attractor == 'right',]$steps[i] = (l-indx)
#           }
#         }
#         
#         indx = indx + 1
#         
#       }
#     }
  
  ##################################################################
  #  combine data  from all participants to one frame
  ##################################################################
  
  # add participant's ID to a list
  IdList[s] = strsplit(strsplit(fileList[s], split = "\\.")[[1]][1], split = "_")[[1]][2]
  
  # construct big frame
  if (s == 1){
    big = data
  } else {
    big = rbind(big, data)
  }
}

#big[big$distractor.condition == 'none',]$distractor.vot = 0

# convert variables to factors
# big$vot.step = as.factor(big$vot.step)
big$soa = as.factor(big$soa)
big$subject.ID = as.factor(big$subject.ID)
big$distractor.condition = as.factor(big$distractor.condition)
# big$observation = as.factor(big$observation)
# big$trial = as.factor(big$trial)
big$block = as.factor(big$block)
#big$attractor = as.factor(big$attractor)

rm(list = c("currentFile", "data", "mainDir", "s"))