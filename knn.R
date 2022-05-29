
#----------------------------------- ORIGNAL DATASET-------------------------------------------
library(reshape2)

Data<- read.csv("train_sample.csv")
Data$is_attributed <- as.factor(Data$is_attributed)


# Data0 <- Data[Data$is_attributed == 1,]
# 
# for (i in c(1:100)){
#   Data <- rbind(Data, Data0)
# }
# 
#      
# nrow(Data[Data$is_attributed == 1,])
#SampleSize <- round(0.10*nrow(Data1))
#SampleIndex <- sample(1:nrow(Data1),size=SampleSize)
#Data1 <- Data1[SampleIndex,]



#scale the first 5 features

minimums <- apply(Data[,1:5], 2, min)
print (minimums)

ranges <- apply(Data[,1:5],2,max) - apply(Data[,1:5],2,min)
print (ranges)

DataScaled <- scale(Data[,1:5],minimums,ranges)
DataScaled <- as.data.frame(DataScaled)


DataScaled$is_attributed <- Data$is_attributed
#DataScaled$is_attributed <- as.factor(Data$is_attributed)

#install.packages('performanceEstimation')


#create train and test sets

SampleSize <- round(0.75*nrow(DataScaled))
SampleIndex <- sample(1:nrow(DataScaled),size=SampleSize)

TrainSet <- DataScaled[SampleIndex,]
TestSet <- DataScaled[-SampleIndex,]


#extract is_attributed (6th column)of dataset because it will be used as 'cl' argument in knn function
target_category <- DataScaled[SampleIndex, 6]
#target_category <- DataScaled[SampleIndex, 5]

#extract 6th column of the test dataset to mesure the accuracy
test_category <- DataScaled[-SampleIndex, 6]
#test_category <- DataScaled[-SampleIndex, 5]


#remove is_attributed column from datasets
TrainSet <- TrainSet[,c(1:5)]
TestSet <- TestSet[,c(1:5)]

#knn model using Euclidean distance

library(class)
library(caret)

PredictTrain <- knn(TrainSet,TrainSet,cl=target_category,k=316)
plot(PredictTrain)

PredictTest <- knn(TrainSet,TestSet,cl=target_category,k=316)
plot(PredictTest)

#base rate for train set
BaseError <- mean(target_category == 1)
BaseError
#base rate for test set
BaseError1 <- mean(test_category == 1)
BaseError1

#train error
TrainError <- mean(PredictTrain != target_category)
TrainError

#test error
TestError <- mean(PredictTest != test_category)
TestError


#FPRate
FPrate <- sum(PredictTest != test_category 
              & test_category == 0) / sum(test_category == 0)
FPrate

#FNRate
FNrate <- sum(PredictTest != test_category 
              & test_category == 1) / sum(test_category == 1)
FNrate

#TPRate
TPrate <- sum(PredictTest == test_category 
              & test_category == 0) / sum(test_category == 0)
TPrate

#TNRate
TNrate <- sum(PredictTest == test_category 
              & test_category == 1) / sum(test_category == 1)
TNrate


#----------------------------------- SMOTE DATASET-------------------------------------------
library(reshape2)
library(performanceEstimation)


Data1<- read.csv("smoteDataset.csv")
Data1$is_attributed <- as.factor(Data1$is_attributed)

#scale the first 5 features

minimums <- apply(Data1[,2:6], 2, min)
print (minimums)

ranges <- apply(Data1[,2:6],2,max) - apply(Data1[,2:6],2,min)
print (ranges)

DataScaled <- scale(Data1[,2:6],minimums,ranges)
DataScaled <- as.data.frame(DataScaled)


DataScaled$is_attributed <- Data1$is_attributed

#install.packages('performanceEstimation')
library(performanceEstimation)


#create train and test sets
SampleSize <- round(0.75*nrow(DataScaled))
SampleIndex <- sample(1:nrow(DataScaled),size=SampleSize)

TrainSet <- DataScaled[SampleIndex,]
TestSet <- DataScaled[-SampleIndex,]


#extract is_attributed (6th column)of dataset because it will be used as 'cl' argument in knn function
target_category <- DataScaled[SampleIndex, 6]

#extract 6th column of the test dataset to mesure the accuracy
test_category <- DataScaled[-SampleIndex, 6]


#remove is_attributed column from datasets
TrainSet <- TrainSet[,c(1:5)]
TestSet <- TestSet[,c(1:5)]

#knn model using Euclidean distance

library(class)
library(caret)

PredictTrain <- knn(TrainSet,TrainSet,cl=target_category,k=316)
plot(PredictTrain)

PredictTest <- knn(TrainSet,TestSet,cl=target_category,k=316)
plot(PredictTest)


#base error rate
BaseError <- mean(target_category == 1)
BaseError
BaseError1 <- mean(test_category == 1)
BaseError1

#train error
TrainError <- mean(PredictTrain != target_category)
TrainError

#test error
TestError <- mean(PredictTest != test_category)
TestError


#FPRate
FPrate <- sum(PredictTest != test_category 
              & test_category == 0) / sum(test_category == 0)
FPrate

#FNRate
FNrate <- sum(PredictTest != test_category 
              & test_category == 1) / sum(test_category == 1)
FNrate

#TPRate
TPrate <- sum(PredictTest == test_category 
              & test_category == 0) / sum(test_category == 0)
TPrate

#TNRate
TNrate <- sum(PredictTest == test_category 
              & test_category == 1) / sum(test_category == 1)
TNrate



