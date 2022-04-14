#  Company    : Stevens 
#  Project    : R Homework 3 
#  Purpose    : KNN classification of the data
#  First Name	: Aman
#  Last Name	: Pawar
#  Id			    : 10477801
#  Date       : March 1, 2022
#  Comments   : Homework Assignment 3 submission



rm(list=ls())


data()
#name<-file.choose()
temp<-read.csv("breast-cancer-wisconsin.csv",header = TRUE,na.strings = c("?"))


installed.packages()
install.packages("kknn")


library(kknn)

data(temp)
View(temp)


temp<-temp[!is.na(temp$F6),]
View(temp)

#idx<-sort(sample(nrow(temp),as.integer(.70*nrow(temp))))
#training<-temp[idx,]
#test<-temp[-idx,]



#Generate a number that is 70% of the total number of rows in dataset.
ran <- sort(sample(nrow(temp),as.integer(.70*nrow(temp)))) 

#the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

#Run nomalization on first 9 coulumns of dataset because they are the predictors
temp_norm <- as.data.frame(lapply(temp[,c(1,2,3,4,5,7,8,9)], nor))

summary(temp_norm)

#extract training set
temp_train <- temp_norm[ran,] 
#extract testing set
temp_test <- temp_norm[-ran,] 
#extract 10th column of train dataset because it will be used as an argument in knn function.
temp_target_category <- temp[ran,10]
#extract 10th column if test dataset to measure the accuracy
temp_test_category <- temp[-ran,10]
#load the package class
library(class)



#For k=3 developing the classification models for diagnosis
#run knn function
pr <- knn(temp_train,temp_test,cl=temp_target_category,k=3)

#create confusion matrix
tab <- table(pr,temp_test_category)
View(tab)

#this function divides the correct predictions by total number of predictions that tell us how accurate the model is.
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)



#For k=5 developing the classification models for diagnosis
#run knn function
pr <- knn(temp_train,temp_test,cl=temp_target_category,k=5)

#create confusion matrix
tab <- table(pr,temp_test_category)

#this function divides the correct predictions by total number of predictions that tell us how accurate the model is.
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)



#For k=10 developing the classification models for diagnosis
#run knn function
pr <- knn(temp_train,temp_test,cl=temp_target_category,k=10)

#create confusion matrix
tab <- table(pr,temp_test_category)

#this function divides the correct predictions by total number of predictions that tell us how accurate the model is.
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)



rm(list=ls())
