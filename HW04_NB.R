#  Company    : Stevens 
#  Project    : R Homework 4 
#  Purpose    : Naive Bayes
#  First Name	: Aman
#  Last Name	: Pawar
#  Id			    : 10477801
#  Date       : March 8, 2022
#  Comments   : Homework Assignment 4 submission


library(e1071)
rm(list=ls())
temp<-read.csv("breast-cancer-wisconsin.csv",header = TRUE,na.strings = c("?"))


is.factor(temp$F1)
bc<-na.omit(temp)

idx<-sort(sample(nrow(bc),as.integer(.70*nrow(bc))))

training<-bc[idx,]

test<-bc[-idx,]


nBayes <- naiveBayes(factor(Class)~., data =training[,-1])

## Naive Bayes classification using all variables 

category_all<-predict(nBayes,test[,-1]  )


table(NBayes=category_all,Survived=test$Class)
NB_wrong<-sum(category_all!=test$Class )
NB_error_rate<-NB_wrong/length(category_all)
NB_error_rate

