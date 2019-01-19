#histograms 
# load libraries 
library(mlbench) 
library(caret) 
set.seed(12345) 
data(iris) 
# create histograms for each attribute 
par(mfrow=c(1,4)) 
for(i in 1:4) { 
  hist(iris[,i], main=names(iris)[i]) 
}
#desity plots 
library(lattice) 
# load dataset 
data(iris) 
# create a panel of simpler density plots by attribute 
par(mfrow=c(1,4)) 
for(i in 1:4) { 
  plot(density(iris[,i]), main=names(iris)[i]) 
}
#box plot 
data(iris) 
# Create separate boxplots for each attribute 
par(mfrow=c(1,4)) 
for(i in 1:4) { 
  boxplot(iris[,i], main=names(iris)[i]) 
}
#scatter plot 
# load the data 
data(iris) 
# pair-wise scatterplots of all 4 attributes 
pairs(iris) 
#box plot by class value. 
# load the caret library 
library(caret) 
# load the iris dataset 
data(iris) 
# box and whisker plots for each attribute by class value 
x <- iris[,1:4] 
y <- iris[,5] 
featurePlot(x=x, y=y, plot="box") 
#scale 
#transform calculates the standard deviation for an attribute and divides each value by that standard deviation. 
# load libraries 
library(caret) 
# load the dataset 
data(iris) 
# summarize data 
summary(iris[,1:4]) 
# calculate the pre-process parameters from the dataset 
preprocessParams <- preProcess(iris[,1:4], method=c("scale")) 
# summarize transform parameters 
print(preprocessParams) 
# transform the dataset using the parameters 
transformed <- predict(preprocessParams, iris[,1:4]) 
# summarize the transformed dataset 
summary(transformed) 
#Center 
#The center transform calculates the mean for an attribute and subtracts it from each value. 
library(caret) 
# load the dataset 
data(iris) 
# summarize data 
summary(iris[,1:4]) 
# calculate the pre-process parameters from the dataset 
preprocessParams <- preProcess(iris[,1:4], method=c("center")) 
# summarize transform parameters 
print(preprocessParams) 
# transform the dataset using the parameters 
transformed <- predict(preprocessParams, iris[,1:4]) 
# summarize the transformed dataset 
summary(transformed) 
#Normalize 
#Data values can be scaled into the range of [0, 1] which is called normalization. 
library(caret) 
# load the dataset 
data(iris) 
# summarize data 
summary(iris[,1:4]) 
# calculate the pre-process parameters from the dataset 
preprocessParams <- preProcess(iris[,1:4], method=c("range")) 
# summarize transform parameters 
print(preprocessParams) 
# transform the dataset using the parameters 
transformed <- predict(preprocessParams, iris[,1:4]) 
# summarize the transformed dataset 
summary(transformed) 
rm(list=ls()) 
#Principal Component Analysis 
#Transform the data to the principal components. 

#The transform keeps components above the variance threshold (default=0.95) or the number of components can be specified (pcaComp). 
library(mlbench) 
# load the dataset 
data(iris) 
# summarize dataset 
summary(iris) 
# calculate the pre-process parameters from the dataset 
preprocessParams <- preProcess(iris, method=c("center", "scale", "pca")) 
# summarize transform parameters 
print(preprocessParams) 
# transform the dataset using the parameters 
transformed <- predict(preprocessParams, iris) 
# summarize the transformed dataset 
summary(transformed) 
#Descriptive Statistics for PimaIndiansDiabetes.csv 
library(mlbench) 
# load the dataset 
data(iris) 
# display first 20 rows of data 
head(iris, n=20) 
#Dimensions of the Data 
# display the dimensions of the dataset 
dim(iris) 
#Data Types 
# list types for each attribute 
sapply(iris, class) 
#Class Distribution 
# distribution of class variable 
y <- iris$Species 
cbind(freq=table(y), percentage=prop.table(table(y))*100) 
#Data Summary 
summary(iris) 
#Standard Deviations 
# calculate standard deviation for all attributes 
sapply(iris[,1:4], sd) 
#Correlations 
library(mlbench) 
# load the dataset 
data(iris) 
# calculate a correlation matrix for numeric variables 
correlations <- cor(iris[,1:4]) 
# display the correlation matrix 
print(correlations)

