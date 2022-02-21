#  Purpose    : EDA Analysis of data set
#  First Name	: Aman
#  Last Name	: Pawar
#  Id			    : 10477801
#  Date       : February 13, 2022

rm(list=ls())


data()
#name<-file.choose()
temp<-read.csv("breast-cancer-wisconsin.csv",header = TRUE,na.strings = c("?"))

#Question 1 Summary of each column
summary(temp)

#Question 2  Identifying missing values
is.na(temp)
which(is.na(temp$F6))

#Question 3	Replacing the missing values with the "mean" of the column.
temp$F6[is.na(temp$F6)] <- mean(temp$F6, na.rm = TRUE)  
View(temp)

#Alternate answer using in built library
#temp <- na.aggregate(temp)                             
#View(temp)


#Question 4	Displaying the frequency table of "Class" vs. F6
Freq<-with(temp,table(Class,F6))
View(Freq)

#Question 5	Displaying the scatter plot of F1 to F6, one pair at a time
plot(temp[2:7],main = "scatter plot of F1 to F6",col = "blue")

#Question 6	Show histogram & box plot for columns F7 to F9
hist(temp$F7,main = "histogram plot for columns F7",col = "red")
hist(temp$F8,main = "histogram plot for columns F8",col = "green")
hist(temp$F9,main = "histogram plot for columns F9",col = "blue")
boxplot(temp[8:10],main = "box plot for columns F7 to F9",col = "yellow")


rm(list=ls())
#name<-file.choose()
temp<-read.csv("/Users/aakashirengbam/Downloads/KDD/breast-cancer-wisconsin.csv",header = TRUE,na.strings = c("?"))
temp_clean<-na.omit(temp)

