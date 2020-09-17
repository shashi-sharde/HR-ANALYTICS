library(dplyr)
library(ggplot2)
library(esquisse)
library(lubridate)
library(knitr)
library(mlr)
library(tidyverse)
library(mice)
library(VIM)
library(xlsx)
library(C50)
library(ggplot2)
library(caret)
library(tree)
test1 <- read.csv("C:\\Users\\shashi\\Desktop\\HR Analytics\\test1.csv")
View(test1)
setwd("C:\\Users\\shashi\\Desktop\\ HR Analytics")
predict_data<- function(hr_tree, test1){
  summary(test1)
  str(test1)
  length(test1)
  
  impute1<- mice(test1[,2:33] , m=5 , seed=123)
  
  print(impute1)
  
  impute1$imp$Nursing_room
  summary(impute1$imp$Nursing_room)
  
  test_hr<- mice::complete(impute1,1)
  #View(test_hr)
  is.na(test_hr)

  write.csv(test_hr, "Newtest.csv")
  
  
  ####Prediction of data using hr_tree model from training_full 
  
  final.pred<- predict(object = hr_tree, test_hr, type = "class")
  final.pred
  summary(final.pred)
  
  Submission.final<- as.data.frame(final.pred)
  names(Submission.final)<- c("prediction1")
  
  Submission.final$Id<- 1:nrow(Submission.final)
  View(Submission.final)
  
  prediction.data<- Submission.final[,c(2,1)]
  prediction.data$prediction1<- as.numeric(levels(prediction.data$prediction1))[prediction.data$prediction1]
  
  str(prediction.data)
  
  write.csv(prediction.data, "Submission_dt_final.csv", row.names = FALSE)
  
}

