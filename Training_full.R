#Required Packages
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
library(tidyverse)
library(ggplot2)
library(caret)
library(tree)
train1 <- read.csv("C:/Users/shashi/Desktop/HR Analytics/train1.csv")
View(train1)
setwd("C:\\Users\\shashi\\Desktop\\New HR Analytics")
Build_Model<- function(train1){
  
  str(train1)
  summary(train1)
  
  length(train1)
  summary(train1)
  attach(train1)
  md.pattern(train1)
  md.pairs(train1)
    
  impute<- mice(train1[,2:34] , m=5 , seed=123)
  
  print(impute)
  #### here we can see that it used the pmm method.
  # pmm = predictive mean matching.
  #### 
  impute$imp$Nursing_room
  train_hr<- mice::complete(impute,3)
  View(train_hr)
  is.na(train_hr)
  write.csv(train_hr, "Newtrain.csv")
  
  
  attach(train_hr)
  str(train_hr)
  train_hr$Accept_offer<- factor(train_hr$Accept_offer)
  
  ##### Using tree function 
  # Building a model on training data 
  hr_tree <- tree(Accept_offer~.,data=train_hr)
  plot(hr_tree)
  text(hr_tree,pretty = 0)
  
  summary(hr_tree)
  hr_tree
}

