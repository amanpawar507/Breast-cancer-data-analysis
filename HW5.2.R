#  Company    : Stevens 
#  Project    : R Homework 5 
#  Purpose    : Cart methodology
#  First Name	: Aman
#  Last Name	: Pawar
#  Id			    : 10477801
#  Date       : March 22, 2022
#  Comments   : Homework Assignment 5 submission

rm(list=ls())
filename<-file.choose()
temp<- read.csv(filename,header = TRUE,na.strings = "?", colClasses=c("Class" = "factor"))
#temp<-read.csv("breast-cancer-wisconsin.csv",header = TRUE,na.strings = "?", colClasses=c("Class" = "factor"))
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("RColorBrewer")
library(rpart)
library(rpart.plot)  	
library(rattle)           
library(RColorBrewer)     

index<-sort(sample(nrow(temp),round(.30*nrow(temp))))
training<-temp[-index,]
test<-temp[index,]

CART_class<-rpart( Class~.,data=training[,-1])
rpart.plot(CART_class)
CART_predict2<-predict(CART_class,test, type="class")
df<-as.data.frame(cbind(test,CART_predict2))
table(Actual=test[,"Class"],CART=CART_predict2)

CART_wrong<-sum(test[,"Class"]!=CART_predict2)

error_rate=CART_wrong/length(test$Class)
error_rate
