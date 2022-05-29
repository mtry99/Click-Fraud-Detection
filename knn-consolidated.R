
#----------------------------------- CONSOLIDATED DATASET-------------------------------------------
library(reshape2)

Data1<- read.csv("train_sample.csv")
# we use count since across ip, device, app, channel, the is_attributed field will be the same
Consolidated <- count(Data1, c("ip", "device", "app", "channel", "os", "is_attributed"))

Data <- Consolidated


#scale the 6 features

list <- c(1,2,3,4,5,7)
minimums <- apply(Data[,list], 2, min)
print (minimums)

ranges <- apply(Data[,list],2,max) - apply(Data[,list],2,min)
print (ranges)

DataScaled <- scale(Data[,list],minimums,ranges)
DataScaled <- as.data.frame(DataScaled)

DataScaled$is_attributed <- as.factor(Data$is_attributed)


#install.packages('performanceEstimation')


#create train and test sets

SampleSize <- round(0.75*nrow(DataScaled))
SampleIndex <- sample(1:nrow(DataScaled),size=SampleSize)

TrainSet <- DataScaled[SampleIndex,]
TestSet <- DataScaled[-SampleIndex,]


#extract is_attributed (6th column)of dataset because it will be used as 'cl' argument in knn function
target_category <- TrainSet[, 7]

#extract 6th column of the test dataset to mesure the accuracy
test_category <- TestSet[, 7]



#remove is_attributed column from datasets
TrainSet <- TrainSet[,c(1:6)]
TestSet <- TestSet[,c(1:6)]

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